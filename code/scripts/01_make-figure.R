#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 01_make-figure.R
## Desc: Draw a descriptive figure of the full experiment to date
## Date created: 2026-08-18

# Packages ----------------------------------------------------------------

library("tidyverse")
library("here")

source(here::here("code", "functions", "plotting.R"))
ggplot2::theme_set(theme_sbe())

# Data -------------------------------------------------------------------

data <-
	readRDS(here::here("data", "derived", "data_cleaned.rds"))


# Summarise data ---------------------------------------------------------

summed_ba <-
	data |>
	mutate(dbase_m = dbase_mm / 1000) |>
	mutate(basal_area = pi * (dbase_m / 2)^2) |>
	group_by(census_no) |>
	summarise(
		sum_ba = sum(basal_area, na.rm = TRUE),
		density = sum(survival, na.rm = TRUE),
		date = median(survey_date)
	) |>
	add_row(
		census_no = '00',
		sum_ba = 0,
		density = 0,
		date = as.Date("2002-07-01")
	)


# Draw figure ------------------------------------------------------------

# summed basal area
p <-
	summed_ba |>
	ggplot(aes(x = date, y = sum_ba)) +
	coord_cartesian(
		xlim = c(as.Date("2002-01-01"), as.Date(as.Date("2026-01-01"))),
		expand = FALSE
	) +
	labs(x = "Year", y = expression("Summed basal area" ~ (m^2)), fill = "")

# seedling density - looks a bit odd beacuse of the 2nd round of planting
p_dens <-
	summed_ba |>
	ggplot(aes(x = date, y = density)) +
	geom_line(se = FALSE, stat = "smooth", span = 0.5) +
	geom_point(colour = "red", shape = 3) +
	coord_cartesian(
		xlim = c(as.Date("2002-01-01"), as.Date(as.Date("2026-01-01"))),
		expand = FALSE
	)

# Add annotations --------------------------------------------------------

# planting cohort 1
p <- p +
	geom_rect(
		aes(
			xmin = as.Date("2002-07-01"),
			xmax = as.Date("2003-09-01"),
			ymin = 0,
			ymax = Inf,
			fill = "Planting"
		),
		alpha = 0.1
	)

# planting cohort 2
p <- p +
	geom_rect(
		aes(
			xmin = as.Date("2009-01-01"),
			xmax = as.Date("2010-10-01"),
			ymin = 0,
			ymax = Inf,
			fill = "Planting"
		),
		alpha = 0.1
	)

# census 1
p <- p +
	geom_rect(
		aes(
			xmin = as.Date("2003-11-18"),
			xmax = as.Date("2006-12-30"),
			ymin = 0,
			ymax = Inf,
			fill = "Censusing"
		),
		alpha = 0.1
	)

# census 2
p <- p +
	geom_rect(
		aes(
			xmin = as.Date("2011-11-24"),
			xmax = as.Date("2013-09-24"),
			ymin = 0,
			ymax = Inf,
			fill = "Censusing"
		),
		alpha = 0.1
	)

# census 3
p <- p +
	geom_rect(
		aes(
			xmin = as.Date("2023-09-12"),
			xmax = as.Date("2024-05-28"),
			ymin = 0,
			ymax = Inf,
			fill = "Censusing"
		),
		alpha = 0.1
	)

p <- p +
	geom_line(se = FALSE, stat = "smooth")


# Save image -------------------------------------------------------------

png(
	here::here("output", "figures", "figure_02.png"),
	width = 8,
	height = 7,
	res = 600,
	pointsize = 6,
	units = "cm",
	bg = "white",
	type = "cairo"
)
p
dev.off()
