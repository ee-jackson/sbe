#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-figure-05.R
## Desc: Make figure of biodiversity partition model results
## Date created: 2026-10-06

# Packages ----------------------------------------------------------------

library("tidyverse")
library("here")

source(here::here("code", "functions", "plotting.R"))
ggplot2::theme_set(theme_sbe())


# Data -------------------------------------------------------------------

part_data <- read_rds(here::here("data", "derived", "biodiv_effects.rds"))

predictions <- read_csv(here("output", "results", "predictions_out.csv"))

part_data_long <-
	part_data |>
	pivot_longer(
		cols = c(net, selec, compl, dens_compl, dens_selec, size_compl, size_selec),
		names_to = "response",
		values_to = "value"
	) |>
	mutate(
		species_richness = factor(
			parse_number(as.character(treatment)),
			ordered = TRUE
		),
		response = case_when(
			response == "compl" ~ "complementarity",
			response == "selec" ~ "selection",
			response == "dens_compl" ~ "complementarity_density",
			response == "size_compl" ~ "complementarity_size",
			response == "dens_selec" ~ "selection_density",
			response == "size_selec" ~ "selection_size",
			.default = response
		)
	) |>
	mutate(
		response = factor(
			response,
			levels = c(
				"net",
				"complementarity",
				"complementarity_density",
				"complementarity_size",
				"selection",
				"selection_density",
				"selection_size"
			),
			ordered = TRUE
		)
	)


# Draw figure 05 ---------------------------------------------------------

p <-
	predictions |>
	filter(
		model_set == "biodiversity_partition_models",
		response %in% c("net", "complementarity", "selection")
	) |>
	mutate(
		response = factor(
			response,
			levels = c("net", "complementarity", "selection"),
			ordered = TRUE
		)
	) |>
	ggplot(
		aes(
			y = as.factor(species_richness),
			x = estimate_response
		)
	) +
	geom_vline(xintercept = 0, linetype = 3, colour = "#D55E00FF") +
	geom_pointrange(
		aes(
			xmin = conf_low_response,
			xmax = conf_high_response,
			colour = as.factor(species_richness)
		),
		shape = 21,
		size = 0.25,
		fill = "white",
		show.legend = FALSE
	) +
	geom_point(
		aes(x = value),
		data = filter(
			part_data_long,
			response %in% c("net", "complementarity", "selection")
		),
		shape = "|",
		size = 1,
		alpha = .4,
		position = position_nudge(y = -.25),
		show.legend = FALSE
	) +
	facet_wrap(
		vars(response),
		ncol = 1,
		labeller = sbe_labeller
	) +
	labs(
		y = "Species richness",
		x = NULL
	) +
	theme_sbe()


# Draw figure 06 ---------------------------------------------------------

p2 <-
	predictions |>
	filter(
		model_set == "biodiversity_partition_models",
		!response %in% c("net", "complementarity", "selection")
	) |>
	ggplot(
		aes(
			y = as.factor(species_richness),
			x = estimate_response
		)
	) +
	geom_vline(xintercept = 0, linetype = 3, colour = "#D55E00FF") +
	geom_pointrange(
		aes(
			xmin = conf_low_response,
			xmax = conf_high_response,
			colour = as.factor(species_richness)
		),
		shape = 21,
		size = 0.25,
		fill = "white",
		show.legend = FALSE
	) +
	geom_point(
		aes(x = value),
		data = filter(
			part_data_long,
			!response %in% c("net", "complementarity", "selection")
		),
		shape = "|",
		size = 1,
		alpha = .4,
		position = position_nudge(y = -.3),
		show.legend = FALSE
	) +
	facet_wrap(
		vars(response),
		ncol = 1,
		labeller = sbe_labeller
	) +
	labs(
		y = "Species richness",
		x = NULL
	) +
	theme_sbe()


png(
	here::here("output", "figures", "figure_05.png"),
	width = 8,
	height = 8,
	res = 600,
	pointsize = 6,
	units = "cm",
	bg = "white",
	type = "cairo"
)
p
dev.off()

png(
	here::here("output", "figures", "figure_06.png"),
	width = 8,
	height = 9,
	res = 600,
	pointsize = 6,
	units = "cm",
	bg = "white",
	type = "cairo"
)
p2
dev.off()
