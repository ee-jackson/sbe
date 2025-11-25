#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 00_clean-data.R
## Desc: Clean the raw data file
## Date created: 2024-11-08


# Packages ----------------------------------------------------------------

library("tidyverse")
library("here")
library("janitor")


# Get data ----------------------------------------------------------------

data_sbe <-
  read_csv(
    here::here("data", "raw", "SBE_compiled_data_2002-2024.csv")
  ) %>%
  clean_names() %>%
  rename(old_new = o_n,
         planting_date = plantingdate,
         survey_date = surveydate,
         height_apex = heightapex) %>%
  mutate(across(c(old_new, survival), str_trim)) %>%

  mutate(plot = ifelse(is.na(pl), NA,
                       formatC(pl,
                               width = 3,
                               format = "d",
                               flag = "0")),
         line = ifelse(is.na(li), NA,
                       formatC(li,
                               width = 2,
                               format = "d",
                               flag = "0")),
         position = ifelse(is.na(po), NA,
                           formatC(po,
                                   width = 3,
                                   format = "d",
                                   flag = "0")),
         sample = ifelse(is.na(sample), NA,
                         formatC(sample,
                                 width = 2,
                                 format = "d",
                                 flag = "0"))
  ) %>%

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
  ) %>%
  mutate(plant_id = case_when(
    is.na(position) ~ paste(plant_id, species, sep = "_"),
    .default = plant_id
  )) %>%
  # Making cols match the primary forest data
  select(plant_id, plot, species_mix, line, position, old_new,
         genus, species, genus_species,
         planting_date, census_id, survey_date,
         survival, height_apex, diam1, diam2, dbh1, dbh2)


# Clean species -----------------------------------------------------------

# Some plants switch species
sp_switch_plants <-
  data_sbe %>%
  group_by(plant_id) %>%
  summarise(n_distinct(genus_species, na.rm = TRUE)) %>%
  filter(`n_distinct(genus_species, na.rm = TRUE)` > 1) %>%
  pull(plant_id)

# Assume most recent species ID is accurate
sp_index <-
  data_sbe %>%
  filter(! is.na(genus_species)) %>%
  filter(plant_id %in% sp_switch_plants) %>%
  group_by(plant_id) %>%
  slice_max(survey_date) %>%
  select(plant_id, genus, species, genus_species)

data_sbe <-
  data_sbe %>%
  rows_update(sp_index, by = "plant_id", unmatched = "ignore")


# Clean census ------------------------------------------------------------

data_sbe <-
  data_sbe %>%
  filter(census_id != "intensive_01") %>% # this is a duplicate
  mutate(census_no = case_when(
    census_id == "full_measurement_01" ~ "01",
    census_id == "intensive_02" ~ "02",
    census_id == "intensive_03" ~ "03",
    census_id == "intensive_04" ~ "04",
    census_id == "intensive_05" ~ "05",
    census_id == "intensive_06" ~ "06",
    census_id == "intensive_07" ~ "07",
    census_id == "full_measurement_02" ~ "08",
    census_id == "climber_01" ~ "09",
    census_id == "climber_02" ~ "10",
    census_id == "climber_03" ~ "11",
    census_id == "climber_04" ~ "12",
    census_id == "climber_05" ~ "13",
    census_id == "climber_06" ~ "14",
    census_id == "climber_07" ~ "15",
    census_id == "climber_08" ~ "16",
    census_id == "climber_09" ~ "17",
    census_id == "climber_10" ~ "18",
    census_id == "climber_11" ~ "19",
    census_id == "intensive_08" ~ "20",
    census_id == "climber_12" ~ "21",
    census_id == "intensive_09" ~ "22",
    census_id == "climber_13" ~ "23",
    census_id == "climber_14" ~ "24",
    census_id == "intensive_10" ~ "25",
    census_id == "full_measurement_03" ~ "26",
    .default = census_id
  ))


# Clean survey date -------------------------------------------------------

# get median survey dates for each census + plot
all_med_dates_pl <-
  data_sbe %>%
  group_by(census_id, census_no, plot) %>%
  summarise(median_date = median(survey_date, na.rm = TRUE))

# also get median survey dates for each census
# as plot 13 consistently has no survey date
all_med_dates_cen <-
  data_sbe %>%
  group_by(census_id, census_no) %>%
  summarise(median_date = median(survey_date, na.rm = TRUE))

# replace NAs with median
data_sbe <-
  data_sbe %>%
  left_join(all_med_dates_pl,
             by = c("census_id", "census_no",
                    "plot")) %>%
  mutate(survey_date = case_when(
    is.na(survey_date) ~ median_date,
    .default = survey_date
  )) %>%
  select(-median_date) %>%
  left_join(all_med_dates_cen,
            by = c("census_id",
                   "census_no")) %>%
  mutate(survey_date = case_when(
    is.na(survey_date) ~ median_date,
    .default = survey_date
  )) %>%
  select(-median_date)


# Backfill dead plants ----------------------------------------------------


# concatenate plot and line as not complete cases in each census
data_sbe <-
  data_sbe %>%
  mutate(plot_line = paste(plot, line, sep = "_"))

# get unique plant ids found in each plot and line
plants_in_plots <-
  data_sbe %>%
  filter(! str_detect(plant_id, "NA")) %>%
  select(plot_line, plant_id) %>%
  distinct() %>%
  group_by(plot_line) %>%
  nest(.key = "id_list") %>%
  ungroup()

# key to pass to backfilling function
keys <-
  data_sbe %>%
  select(census_id, census_no, plot_line) %>%
  distinct() %>%
  left_join(plants_in_plots, by = c("plot_line"))

# function to backfill missing trees as dead
backfill_trees <- function(census_name, census, plot_no,
                           tree_ids, data) {
  data %>%
    filter(census_id == census_name,
           census_no == census,
           plot_line == plot_no) %>%
    full_join(tree_ids, by = "plant_id") %>%
    mutate(survival = replace_na(survival, 0)) %>%
    ungroup() %>%
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
  ) %>%
  bind_rows()

# fill missing plant level data for backfilled plants
data_backfilled <-
  data_backfilled %>%
  dplyr::group_by(plant_id) %>%
  arrange(census_no, .by_group = TRUE) %>%
  tidyr::fill(species_mix:planting_date, .direction = "updown") %>%
  ungroup()

# "new" cohort of plants were first surveyed in census "06"
data_backfilled <-
  data_backfilled %>%
  filter(!
           (old_new == "N" &
              census_no %in% c("01", "02", "03", "04", "05") )
  )

# add survey dates for for backfilled plants
data_backfilled <-
  data_backfilled %>%
  left_join(all_med_dates_pl,
            by = c("census_id", "census_no",
                   "plot")) %>%
  mutate(survey_date = case_when(
    is.na(survey_date) ~ median_date,
    .default = survey_date
  )) %>%
  select(-median_date) %>%
  left_join(all_med_dates_cen,
            by = c("census_id", "census_no")) %>%
  mutate(survey_date = case_when(
    is.na(survey_date) ~ median_date,
    .default = survey_date
  )) %>%
  select(-median_date)


# Clean Lazarus trees -----------------------------------------------------

lazarus_ids <- data_backfilled %>%
  group_by(plant_id) %>%
  filter(survival == "1" & lag(survival, order_by = survey_date) == "0") %>%
  pull(plant_id) %>%
  unique()

paste("There are", length(lazarus_ids), "Lazarus trees", sep = " ")

last_alive_dates <-
  data_backfilled %>%
  filter(survival == "1") %>%
  group_by(plant_id) %>%
  slice_max(lubridate::ymd(survey_date), with_ties = FALSE) %>%
  select(plant_id, survey_date) %>%
  rename(last_alive = survey_date)

# Assuming that the most recent time a tree was found alive was correct
# and previous records of survival == 0 were incorrect
data_backfilled <-
  data_backfilled %>%
  filter(plant_id %in% lazarus_ids) %>%
  left_join(last_alive_dates,
            by = "plant_id") %>%
  mutate(
    survival = case_when(
      survey_date <= last_alive ~ 1,

      survey_date > last_alive ~ 0,

      is.na(survey_date) ~ NA
    )
  ) %>%
  select(- last_alive) %>%
  bind_rows(filter(data_backfilled, ! plant_id %in% lazarus_ids))


# Clean growth ------------------------------------------------------------

data_backfilled <-
  data_backfilled %>%
  rowwise() %>%
  mutate(
    dbh_mm = mean(c(dbh1, dbh2), na.rm = TRUE),
    dbase_mm = mean(c(diam1, diam2), na.rm = TRUE)
  ) %>%
  ungroup()


# Create time variable ----------------------------------------------------

data_backfilled <-
  data_backfilled %>%
  group_by(plant_id) %>%
  slice_min(survey_date, with_ties = FALSE) %>%
  select(plant_id, survey_date) %>%
  rename(first_survey = survey_date) %>%
  ungroup() %>%
  right_join(data_backfilled)

# data_backfilled <-
#   data_backfilled %>%
#   rowwise() %>%
#   mutate(
#     days =
#       survey_date - first_survey) %>%
#   ungroup() %>%
#   mutate(years = as.numeric(days, units = "weeks")/52.25,
#          days_num = as.numeric(days))


# Clean survival ----------------------------------------------------------

# remove left censored trees?

left_censored <-
  data_backfilled %>%
  filter(survival == 0 &
             first_survey == survey_date) %>%
  select(plant_id) %>%
  distinct()

paste(pull(count(left_censored), 1),
      "trees died before the first survey", sep = " ")

# data_backfilled <-
#   data_backfilled %>%
#   filter(!plant_id %in% left_censored$plant_id)


# Clean cohort ------------------------------------------------------------

data_backfilled <-
  data_backfilled %>%
  mutate(cohort = case_when(
    old_new == "O" ~ 1,
    old_new == "N" ~ 2
  ))


# Create treatment --------------------------------------------------------

data_backfilled <-
  data_backfilled %>%
  mutate(treatment = case_when(
  plot %in% c("005", "011", "014", "022", "029", "032", "040", "046",
              "049", "064", "115", "111", "100", "091", "085", "075") ~ "16-species-cut",
  str_detect(species_mix, "4-species") ~ "4-species",
  str_detect(species_mix, "16-species") ~ "16-species",
  str_detect(species_mix, "monoculture") ~ "monoculture"
))

# Save --------------------------------------------------------------------

data_backfilled <-
  data_backfilled %>%
  select(plant_id, treatment, species_mix, plot, line, position, cohort,
         genus, species, genus_species,
         planting_date, first_survey, census_no, census_id, survey_date,
         survival, height_apex, dbh_mm, dbase_mm) %>%
  distinct() %>%
  filter(! if_all(c(survival, dbh_mm, dbase_mm, height_apex), is.na)) %>%
  filter(! str_detect(plant_id, "NA")) %>%
  filter(! is.na(genus_species)) %>%
  mutate(across(c(treatment, species_mix, plant_id, plot, cohort, genus_species,
                  line, position, census_no), as.factor))

saveRDS(data_backfilled,
        here::here("data", "derived", "data_cleaned.rds"))
