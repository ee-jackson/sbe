#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 00_clean-data.R
## Desc: Clean the raw data file
## Date created: 2024-11-08

# Packages ----------------------------------------------------------------

library("tidyverse")
library("here")
library("janitor")
library("zen4R")
library("WorldFlora")


# Get data ----------------------------------------------------------------

# download SBE data from Zenodo (if it doesn't already exist)
data_dir <- here::here("data", "raw")
sbe_file <- file.path(data_dir, "SBE_compiled_data_2002-2024.csv")

if (!file.exists(sbe_file)) {
	dir.create(
		data_dir,
		recursive = TRUE,
		showWarnings = FALSE
	)

	zen4R::download_zenodo(
		doi = "https://doi.org/10.5281/zenodo.10815814",
		path = data_dir
	)
}

data_sbe <-
	read_csv(
		sbe_file
	) |>
	clean_names() |>
	rename(
		old_new = o_n,
		planting_date = plantingdate,
		survey_date = surveydate,
		height_apex = heightapex
	) |>
	mutate(across(c(old_new, survival), str_trim)) |>
	mutate(
		plot = ifelse(
			is.na(pl),
			NA,
			formatC(pl, width = 3, format = "d", flag = "0")
		),
		line = ifelse(
			is.na(li),
			NA,
			formatC(li, width = 2, format = "d", flag = "0")
		),
		position = ifelse(
			is.na(po),
			NA,
			formatC(po, width = 3, format = "d", flag = "0")
		),
		sample = ifelse(
			is.na(sample),
			NA,
			formatC(sample, width = 2, format = "d", flag = "0")
		)
	) |>
	mutate(
		planting_date = dmy(planting_date),
		survey_date = dmy(survey_date),
		census_id = as.factor(paste(data_origin, sample, sep = "_")),
		plant_id = paste(plot, line, position, old_new, sep = "_"),
		survival = case_when(
			survival == "yes" ~ 1,
			survival == "no" ~ 0,
			.default = NA
		)
	) |>
	mutate(
		plant_id = case_when(
			is.na(position) ~ paste(plant_id, species, sep = "_"),
			.default = plant_id
		)
	) |>
	mutate(
		cohort = case_when(
			old_new == "O" ~ 1,
			old_new == "N" ~ 2
		)
	) |>
	select(
		plant_id,
		plot,
		species_mix,
		line,
		position,
		cohort,
		genus,
		species,
		genus_species,
		planting_date,
		census_id,
		survey_date,
		survival,
		height_apex,
		diam1,
		diam2,
		dbh1,
		dbh2
	)


# Clean species -----------------------------------------------------------

# Some plants switch species
sp_switch_plants <-
	data_sbe |>
	group_by(plant_id) |>
	summarise(n_distinct(genus_species, na.rm = TRUE)) |>
	filter(`n_distinct(genus_species, na.rm = TRUE)` > 1) |>
	pull(plant_id)

print(paste(length(sp_switch_plants), "plants switch species"))

# Assume most recent species ID is accurate
sp_index <-
	data_sbe |>
	filter(!is.na(genus_species)) |>
	filter(plant_id %in% sp_switch_plants) |>
	group_by(plant_id) |>
	slice_max(survey_date) |>
	select(plant_id, genus, species, genus_species)

data_sbe <-
	data_sbe |>
	rows_update(sp_index, by = "plant_id", unmatched = "ignore")


# Clean census ------------------------------------------------------------

data_sbe <-
	data_sbe |>
	# we only want full censuses
	filter(str_detect(census_id, "full_measurement")) |>
	mutate(
		census_no = case_when(
			census_id == "full_measurement_01" ~ "01",
			census_id == "full_measurement_02" ~ "02",
			census_id == "full_measurement_03" ~ "03",
			.default = census_id
		)
	)


# Clean survey date -------------------------------------------------------

# get median survey dates for each census + plot
all_med_dates_pl <-
	data_sbe |>
	group_by(census_id, census_no, plot) |>
	summarise(median_date = median(survey_date, na.rm = TRUE))

# also get median survey dates for each census
# as plot 13 consistently has no survey date
all_med_dates_cen <-
	data_sbe |>
	group_by(census_id, census_no) |>
	summarise(median_date = median(survey_date, na.rm = TRUE))

# replace NAs with median
data_sbe <-
	data_sbe |>
	left_join(all_med_dates_pl, by = c("census_id", "census_no", "plot")) |>
	mutate(
		survey_date = case_when(
			is.na(survey_date) ~ median_date,
			.default = survey_date
		)
	) |>
	select(-median_date) |>
	left_join(all_med_dates_cen, by = c("census_id", "census_no")) |>
	mutate(
		survey_date = case_when(
			is.na(survey_date) ~ median_date,
			.default = survey_date
		)
	) |>
	select(-median_date)


# Backfill dead plants ----------------------------------------------------

# concatenate plot and line as not complete cases in each census
data_sbe <-
	data_sbe |>
	mutate(plot_line = paste(plot, line, sep = "_"))

# get unique plant ids found in each plot and line
plants_in_plots <-
	data_sbe |>
	filter(!str_detect(plant_id, "NA")) |>
	select(plot_line, plant_id) |>
	distinct() |>
	group_by(plot_line) |>
	nest(.key = "id_list") |>
	ungroup()

# key to pass to backfilling function
keys <-
	data_sbe |>
	select(census_id, census_no, plot_line) |>
	distinct() |>
	left_join(plants_in_plots, by = c("plot_line"))

# function to backfill missing trees as dead
backfill_trees <- function(census_name, census, plot_no, tree_ids, data) {
	data |>
		filter(
			census_id == census_name,
			census_no == census,
			plot_line == plot_no
		) |>
		full_join(tree_ids, by = "plant_id") |>
		mutate(survival = replace_na(survival, 0)) |>
		ungroup() |>
		tidyr::fill(plot, census_id, census_no, plot_line)
}

# run function over all keys
data_backfilled <-
	pmap(
		.f = backfill_trees,
		.l = list(
			census_name = keys$census_id,
			census = keys$census_no,
			plot_no = keys$plot_line,
			tree_ids = keys$id_list
		),
		data = data_sbe
	) |>
	bind_rows()

# fill missing plant level data for backfilled plants
data_backfilled <-
	data_backfilled |>
	dplyr::group_by(plant_id) |>
	arrange(census_no, .by_group = TRUE) |>
	tidyr::fill(species_mix:planting_date, .direction = "updown") |>
	ungroup()

# "new" cohort of plants were first surveyed in census "02"
data_backfilled <-
	data_backfilled |>
	filter(
		!(cohort == 2 &
			census_no %in% c("01"))
	)

# add survey dates for for backfilled plants
data_backfilled <-
	data_backfilled |>
	left_join(all_med_dates_pl, by = c("census_id", "census_no", "plot")) |>
	mutate(
		survey_date = case_when(
			is.na(survey_date) ~ median_date,
			.default = survey_date
		)
	) |>
	select(-median_date) |>
	left_join(all_med_dates_cen, by = c("census_id", "census_no")) |>
	mutate(
		survey_date = case_when(
			is.na(survey_date) ~ median_date,
			.default = survey_date
		)
	) |>
	select(-median_date)


# Clean Lazarus trees -----------------------------------------------------

lazarus_ids <- data_backfilled |>
	group_by(plant_id) |>
	filter(survival == "1" & lag(survival, order_by = survey_date) == "0") |>
	pull(plant_id) |>
	unique()

print(paste("There are", length(lazarus_ids), "Lazarus trees", sep = " "))

last_alive_dates <-
	data_backfilled |>
	filter(survival == "1") |>
	group_by(plant_id) |>
	slice_max(lubridate::ymd(survey_date), with_ties = FALSE) |>
	select(plant_id, survey_date) |>
	rename(last_alive = survey_date)

# Assuming that the most recent time a tree was found alive was correct
# and previous records of survival == 0 were incorrect
data_backfilled <-
	data_backfilled |>
	filter(plant_id %in% lazarus_ids) |>
	left_join(last_alive_dates, by = "plant_id") |>
	mutate(
		survival = case_when(
			survey_date <= last_alive ~ 1,

			survey_date > last_alive ~ 0,

			is.na(survey_date) ~ NA
		)
	) |>
	select(-last_alive) |>
	bind_rows(filter(data_backfilled, !plant_id %in% lazarus_ids))


# Clean growth ------------------------------------------------------------

data_backfilled <-
	data_backfilled |>
	rowwise() |>
	mutate(
		dbh_mm = mean(c(dbh1, dbh2), na.rm = TRUE),
		dbase_mm = mean(c(diam1, diam2), na.rm = TRUE)
	) |>
	ungroup()


# Create time variable ----------------------------------------------------

data_backfilled <-
	data_backfilled |>
	group_by(plant_id) |>
	slice_min(survey_date, with_ties = FALSE) |>
	select(plant_id, survey_date) |>
	rename(first_survey = survey_date) |>
	ungroup() |>
	right_join(data_backfilled)

# data_backfilled <-
#   data_backfilled |>
#   rowwise() |>
#   mutate(
#     days =
#       survey_date - first_survey) |>
#   ungroup() |>
#   mutate(years = as.numeric(days, units = "weeks")/52.25,
#          days_num = as.numeric(days))

# Add col indicating whether climber cut ----------------------------------

data_backfilled <-
	data_backfilled |>
	mutate(
		climber_cut = case_when(
			plot == "05" |
				plot == "11" |
				plot == "14" ~ 1,
			.default = 0
		)
	)


# Predict missing basal diameter ------------------------------------------

n_missing_base <-
	data_backfilled |>
	filter(is.na(dbase_mm) & !is.na(dbh_mm)) |>
	nrow()

paste(
	round(
		n_missing_base /
			nrow(filter(data_backfilled, survival == 1)) *
			100,
		digits = 2
	),
	"% trees are missing basal diameter",
	sep = " "
)

# Taper model 1 from Cushman et al. 2014, doi: 10.1111/2041-210X.12187
get_basal <- function(dbh, pom, b1) {
	dbh /
		exp(b1 * (pom - 1))
}

# Choose b1 parameter which minimises RMSE in basal diameter for our data
get_rmse <- function(df, b1) {
	# make predictions for basal diameter
	predictions <-
		with(df, get_basal(dbh = dbh_mm, pom = 1.3, b1))

	# get model errors
	errors <-
		with(df, dbase_mm - predictions)

	# return the rmse
	return(sqrt(sum(errors^2, na.rm = TRUE) / (length(errors))))
}

optimiser_results <-
	optim(
		method = "Brent",
		par = c(0),
		lower = -5,
		upper = 5,
		fn = function(x) {
			get_rmse(df = data_backfilled, x[1])
		}
	)

data_backfilled <-
	data_backfilled |>
	mutate(
		dbase_mm = case_when(
			is.na(dbase_mm) & !is.na(dbh_mm) ~
				get_basal(dbh = dbh_mm, pom = 1.3, b1 = optimiser_results$par),
			.default = dbase_mm
		)
	)


# Create treatment --------------------------------------------------------

data_backfilled <-
	data_backfilled |>
	mutate(
		treatment = case_when(
			plot %in%
				c(
					"005",
					"011",
					"014",
					"022",
					"029",
					"032",
					"040",
					"046",
					"049",
					"064",
					"115",
					"111",
					"100",
					"091",
					"085",
					"075"
				) ~ "16-species-cut",
			str_detect(species_mix, "monoculture") ~ "1-species",
			str_detect(species_mix, "4-species") ~ "4-species",
			str_detect(species_mix, "16-species") ~ "16-species"
		)
	)


# Remove mis-planted species ----------------------------------------------

# sometimes the wrong species has been planted
# removing these individuals since would affect our diversity analyses

# getting lists of species which *should* be present in each plot
pl_sp_1 <-
	data_backfilled |>
	filter(treatment == "1-species") |>
	group_by(plot, genus_species) |>
	summarise(n = n_distinct(plant_id), .groups = "drop_last") |>
	slice_max(n = 1, order_by = n) |>
	select(-n)

pl_sp_4 <-
	data_backfilled |>
	filter(treatment == "4-species") |>
	group_by(plot, genus_species) |>
	summarise(n = n_distinct(plant_id), .groups = "drop_last") |>
	slice_max(n = 4, order_by = n) |>
	select(-n)

pl_sp_16 <-
	data_backfilled |>
	filter(treatment == "16-species" | treatment == "16-species-cut") |>
	group_by(plot, genus_species) |>
	summarise(n = n_distinct(plant_id), .groups = "drop_last") |>
	slice_max(n = 16, order_by = n) |>
	select(-n)

sp_lists <- bind_rows(pl_sp_1, pl_sp_4, pl_sp_16)

# removing species which are not in the list for each plot
data_backfilled <-
	inner_join(x = data_backfilled, y = sp_lists)


# Get new species names --------------------------------------------------

# download world flora data from Zenodo
taxa_file <- file.path(data_dir, "classification.csv")

if (!file.exists(taxa_file)) {
	dir.create(
		data_dir,
		recursive = TRUE,
		showWarnings = FALSE
	)

	zen4R::download_zenodo(
		doi = "https://doi.org/10.5281/zenodo.10815814",
		path = data_dir
	)

	utils::unzip(
		here::here("data", "raw", "_DwC_backbone_R.zip"),
		exdir = data_dir
	)
}

orig_sp <-
	data_backfilled |>
	select(genus_species) |>
	distinct() |>
	mutate(old_name = str_replace(genus_species, "_", " "))

wfo <-
	WFO.match(
		spec.data = as.data.frame(orig_sp),
		WFO.file = here::here("data", "raw", "classification.csv"),
		no.dates = TRUE,
		spec.name = "old_name"
	)

# finds one unique matching name for each submitted name
wfo_sp <-
	wfo |>
	WFO.one(priority = "Accepted") |>
	select(
		old_name,
		genus_species,
		family,
		tribe,
		subgenus,
		scientificName,
		scientificNameAuthorship
	)

wfo_sp |>
	select(-genus_species) |>
	janitor::clean_names() |>
	write_csv(here::here("data", "derived", "species_list.csv"))

data_backfilled <-
	data_backfilled |>
	left_join(wfo_sp) |>
	mutate(
		genus_species = str_replace(scientificName, " ", "_")
	)

# Save --------------------------------------------------------------------

data_backfilled <-
	data_backfilled |>
	mutate(
		census_yr = case_when(
			census_no == "01" ~ "2003-2007",
			census_no == "02" ~ "2011-2013",
			census_no == "03" ~ "2023-2024",
			.default = census_no
		)
	) |>
	select(
		plant_id,
		treatment,
		species_mix,
		plot,
		line,
		position,
		cohort,
		genus_species,
		planting_date,
		first_survey,
		census_no,
		census_yr,
		survey_date,
		survival,
		height_apex,
		dbh_mm,
		dbase_mm
	) |>
	distinct() |>
	filter(!if_all(c(survival, dbh_mm, dbase_mm, height_apex), is.na)) |>
	filter(!str_detect(plant_id, "NA")) |>
	filter(!is.na(genus_species)) |>
	filter(treatment != "16-species-cut") |> # not using liana-cut plots
	mutate(across(
		c(
			treatment,
			species_mix,
			plant_id,
			plot,
			cohort,
			genus_species,
			line,
			position,
			census_no,
			census_yr
		),
		as.factor
	))

saveRDS(data_backfilled, here::here("data", "derived", "data_cleaned.rds"))
