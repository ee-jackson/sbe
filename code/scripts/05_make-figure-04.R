#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-figure-04.R
## Desc: Make figure of liana_cut and genera_structure models
## Date created: 2026-10-06

# Packages ----------------------------------------------------------------

library("tidyverse")
library("here")

source(here::here("code", "functions", "plotting.R"))
ggplot2::theme_set(theme_sbe())


# Data -------------------------------------------------------------------

means <- read_csv(here("output", "results", "means_out.csv"))

plot_data <- read_rds(here::here("data", "derived", "data_plot_lvl.rds"))

plot_data_long <-
	plot_data |>
	mutate(
		liana_treatment = if_else(
			species_mix == "16-species",
			as.character(liana_treatment),
			NA_character_
		),
		across(
			c(struc_complexity, generic_richness),
			as.character
		)
	) |>
	pivot_longer(
		cols = c(basal_area, seedling_density),
		names_to = "response",
		values_to = "value"
	) |>
	pivot_longer(
		cols = c(liana_treatment, struc_complexity, generic_richness),
		names_to = "marginal_effect",
		values_to = "group",
		values_drop_na = TRUE
	)


# Draw figure ------------------------------------------------------------

p <- means |>
	mutate(
		group = coalesce(liana_treatment, struc_complexity, generic_richness)
	) |>
	mutate(
		colour = case_when(
			marginal_effect == "liana_treatment" ~ "16-species",
			marginal_effect == "struc_complexity" |
				marginal_effect == "generic_richness" ~ "4-species"
		)
	) |>
	ggplot(aes(y = group)) +
	geom_pointrange(
		aes(
			x = estimate,
			xmin = asymp.LCL,
			xmax = asymp.UCL,
			colour = colour
		),
		shape = 21,
		size = 0.25,
		fill = "white"
	) +
	geom_point(
		aes(x = value),
		data = plot_data_long,
		#data = filter_out(plot_data_long, response == "basal_area" & value > 3),
		shape = "|",
		size = 1,
		alpha = .4,
		position = position_nudge(y = -.15),
		show.legend = FALSE
	) +
	facet_grid(
		marginal_effect ~ response,
		scales = "free",
		labeller = sbe_labeller
	) +
	labs(x = "Estimated marginal mean", y = "", colour = "") +
	scale_colour_sbe()

png(
	here::here("output", "figures", "figure_04.png"),
	width = 8,
	height = 10,
	res = 600,
	pointsize = 6,
	units = "cm",
	bg = "white",
	type = "cairo"
)
p
dev.off()
