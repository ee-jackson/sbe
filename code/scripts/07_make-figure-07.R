#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-figure-07.R
## Desc: Make figure of single species plots
## Date created: 2026-10-27

# Packages ----------------------------------------------------------------

library("tidyverse")
library("here")
library("ggtext")

source(here::here("code", "functions", "plotting.R"))
ggplot2::theme_set(theme_sbe())


# Data -------------------------------------------------------------------

plot_data <-
	read_rds(here::here("data", "derived", "data_plot_lvl.rds")) |>
	mutate(species_mix = str_replace(species_mix, "_", " ")) |>
	mutate(
		species_mix = case_when(
			!species_mix %in% c("16-species", "4-species") ~ paste(
				"*",
				species_mix,
				"*",
				sep = ""
			),
			.default = species_mix
		)
	)


# Draw figure 07 ---------------------------------------------------------

p <-
	plot_data |>
	filter(
		treatment == "1-species" |
			treatment == "16-species"
	) |>
	ggplot(aes(
		x = basal_area,
		y = species_mix,
		colour = treatment,
		group = species_mix
	)) +
	geom_point(size = 1.5, alpha = 0.7) +
	geom_line(orientation = "x", alpha = 0.3) +
	scale_colour_sbe() +
	guides(colour = guide_legend(nrow = 1)) +
	theme_sbe() +
	theme(
		axis.text.y = element_markdown(),
		legend.location = "plot",
		legend.justification = "left"
	) +
	labs(y = "", x = "Basal area (m²)", colour = "")


png(
	here::here("output", "figures", "figure_07.png"),
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
