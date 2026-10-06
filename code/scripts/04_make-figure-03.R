#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-figure-03.R
## Desc: Make figure of sp. richness model results
## Date created: 2026-09-29

set.seed(20732) # for the jittered points

# Packages ----------------------------------------------------------------

library("tidyverse")
library("here")

source(here::here("code", "functions", "plotting.R"))
ggplot2::theme_set(theme_sbe())


# Data -------------------------------------------------------------------

predictions <- read_csv(here("output", "results", "predictions_out.csv"))

plot_data <- read_rds(here::here("data", "derived", "data_plot_lvl.rds"))

plot_data_long <-
	plot_data |>
	filter(liana_treatment == "not_cut") |>
	pivot_longer(
		cols = c(basal_area, seedling_density),
		names_to = "response",
		values_to = "value"
	) |>
	filter_out(response == "basal_area" & value > 3)

# Draw figure ------------------------------------------------------------

p <-
	predictions |>
	filter(model_set == "species_richness_models") |>
	ggplot(
		aes(
			x = log_base4_species_richness,
			y = estimate_response
		)
	) +
	geom_jitter(
		data = plot_data_long,
		aes(y = value, colour = as.character(species_richness)),
		alpha = 0.3,
		width = 0.15,
		size = 0.5,
		show.legend = FALSE
	) +
	geom_ribbon(
		aes(group = response, ymin = conf_low_response, ymax = conf_high_response),
		alpha = 0.15
	) +
	geom_line() +
	geom_point(
		aes(colour = as.character(species_richness)),
		size = 3,
		show.legend = FALSE
	) +
	facet_wrap(
		vars(response),
		scales = "free_y",
		labeller = sbe_labeller
	) +
	scale_x_continuous(
		breaks = 0:2,
		labels = c("1", "4", "16")
	) +
	labs(
		x = "Species richness",
		y = NULL
	) +
	theme_sbe() +
	scale_colour_sbe()

png(
	here::here("output", "figures", "figure_03.png"),
	width = 8,
	height = 5,
	res = 600,
	pointsize = 6,
	units = "cm",
	bg = "white",
	type = "cairo"
)
p
dev.off()
