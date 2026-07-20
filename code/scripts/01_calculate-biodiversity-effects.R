#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 01_calculate-biodiversity-effects.R
## Desc: Calculates the net, complementarity and selection effects of biodiversity
## Date created: 2026-07-17

# Packages ----------------------------------------------------------------

library("tidyverse")
library("here")
library("densize")
library("janitor")


# Data -------------------------------------------------------------------

data <-
	readRDS(here::here("data", "derived", "data_cleaned.rds")) |>
	filter(treatment != "16-species-cut") # not using liana-cut plots

# Calculate the initial density ------------------------------------------

# reconsructs the intended experimental planting allocation
init_dens_perf <-
	data |>
	select(plot, treatment, genus_species) |>
	distinct() |>
	mutate(
		perc = case_when(
			treatment == "1-species" ~ 1,
			treatment == "4-species" ~ 0.25,
			treatment == "16-species" ~ 0.0625
		)
	) |>
	mutate(n = 1320 * perc) |>
	select(-c(treatment, perc)) |>
	pivot_wider(
		id_cols = plot,
		names_from = genus_species,
		values_from = n,
		names_sort = TRUE
	) |>
	mutate(across(everything(), \(x) replace_na(x, 0))) |>
	arrange(plot) |>
	column_to_rownames(var = "plot")

# counts seedlings actually present at census 2
init_dens <-
	data |>
	filter(census_no == "02") |>
	group_by(plot, genus_species) |>
	arrange(plot, genus_species) |>
	summarise(n = n_distinct(plant_id), .groups = "drop") |>
	pivot_wider(
		id_cols = plot,
		names_from = genus_species,
		values_from = n,
		names_sort = TRUE
	) |>
	mutate(across(everything(), \(x) replace_na(x, 0))) |>
	arrange(plot) |>
	column_to_rownames(var = "plot")


# Calculate the final density --------------------------------------------

final_dens <-
	data |>
	filter(census_no == "03") |>
	group_by(plot, genus_species) |>
	arrange(plot, genus_species) |>
	summarise(n = sum(survival), .groups = "drop") |>
	pivot_wider(
		id_cols = plot,
		names_from = genus_species,
		values_from = n,
		names_sort = TRUE
	) |>
	mutate(across(everything(), \(x) replace_na(x, 0))) |>
	arrange(plot) |>
	column_to_rownames(var = "plot")


# Calculate the final yield ----------------------------------------------

final_yield <-
	data |>
	filter(census_no == "03") |>
	mutate(dbase_m = dbase_mm / 1000) |>
	mutate(basal_area_m2 = pi * (dbase_m / 2)^2) |>
	group_by(plot, genus_species) |>
	arrange(plot, genus_species) |>
	summarise(sum = sum(basal_area_m2, na.rm = TRUE), .groups = "drop") |>
	pivot_wider(
		id_cols = plot,
		names_from = genus_species,
		values_from = sum,
		names_sort = TRUE
	) |>
	mutate(across(everything(), \(x) replace_na(x, 0))) |>
	arrange(plot) |>
	column_to_rownames(var = "plot")


# Calculate the biodiversity effects -------------------------------------

densize_out <-
	densize::densize(
		init.dens = init_dens_perf,
		final.dens = final_dens,
		final.yield = final_yield
	) |>
	as_tibble()

metadata <-
	data |>
	select(plot, treatment) |>
	distinct() |>
	filter(treatment != "1-species") |>
	arrange(plot)

result <-
	bind_cols(densize_out, metadata) |>
	clean_names() |>
	mutate(
		compl = dens_compl + size_compl,
		selec = dens_selec + size_selec,
		net = compl + selec
	) |>
	mutate(
		treatment = fct_relevel(
			treatment,
			"4-species",
			"16-species"
		)
	)

glimpse(result)


# Save output ------------------------------------------------------------

saveRDS(result, here::here("data", "derived", "biodiv_effects.rds"))
