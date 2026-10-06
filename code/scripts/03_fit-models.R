#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: fit-models.R
## Desc: Fit basal-area, seedling-density, and biodiversity-effect models
## Date created: 2026-09-11

set.seed(20732)

# Packages ----------------------------------------------------------------

library("tidyverse")
library("glmmTMB")
library("broom.mixed")
library("DHARMa")
library("emmeans")


# Helper functions -------------------------------------------------------

fit_response_models <- function(
	data,
	rhs,
	responses,
	family_factories,
	dispformulas = NULL
) {
	if (is.null(dispformulas)) {
		dispformulas <- rep(list(~1), length(responses)) |>
			set_names(names(responses))
	}

	fit_one <- quietly(\(response, model_name) {
		glmmTMB::glmmTMB(
			stats::as.formula(paste(response, deparse1(rhs))),
			data = data,
			family = family_factories[[model_name]](),
			dispformula = dispformulas[[model_name]]
		)
	})

	results <- map2(responses, names(responses), fit_one)

	tibble(
		response = names(responses),
		fit = map(results, "result"),
		warning = map_chr(
			results,
			\(x) paste(unique(x$warnings), collapse = "\n")
		)
	)
}

inverse_link <- function(x, link) {
	case_when(
		link == "log" ~ exp(x),
		link == "identity" ~ x,
		.default = NA_real_
	)
}

tidy_fixed_effects <- function(fit) {
	broom.mixed::tidy(
		fit,
		effects = "fixed",
		component = "cond",
		conf.int = TRUE,
		conf.level = 0.95
	)
}

get_fixed_rhs <- function(fit) {
	fit |>
		formula(fixed.only = TRUE) |>
		terms() |>
		delete.response() |>
		formula()
}

has_factor_predictor <- function(fit) {
	predictors <- fit |>
		get_fixed_rhs() |>
		all.vars()

	model_data <- model.frame(fit)

	predictors |>
		intersect(names(model_data)) |>
		map_lgl(\(predictor) is.factor(model_data[[predictor]])) |>
		any()
}

make_emm_grids <- function(fit, rhs) {
	term_info <- terms(rhs)
	term_labels <- attr(term_info, "term.labels")
	term_orders <- attr(term_info, "order")

	if (any(term_orders > 1L)) {
		interaction_variables <- term_labels[term_orders > 1L] |>
			str_split(":", simplify = FALSE) |>
			unlist() |>
			unique()

		map_dfr(
			interaction_variables,
			\(variable) {
				tibble(
					marginal_effect = variable,
					emm_grid = list(
						emmeans::emmeans(
							fit,
							specs = reformulate(variable),
							weights = "equal",
							regrid = "response",
							level = 0.95
						)
					)
				)
			}
		)
	} else {
		tibble(
			marginal_effect = paste(term_labels, collapse = " + "),
			emm_grid = list(
				emmeans::emmeans(
					fit,
					specs = rhs,
					regrid = "response",
					level = 0.95
				)
			)
		)
	}
}

summarise_emmeans <- function(grid) {
	grid |>
		summary(infer = c(TRUE, FALSE)) |>
		as.data.frame() |>
		as_tibble() |>
		rename(estimate = response)
}

summarise_contrasts <- function(grid) {
	grid |>
		pairs(adjust = "none") |>
		summary(infer = c(TRUE, TRUE)) |>
		as.data.frame() |>
		as_tibble()
}

get_predictions <- function(model, response_name) {
	emmeans(
		model,
		~log_base4_species_richness,
		at = list(log_base4_species_richness = 0:2)
	) |>
		broom::tidy(conf.int = TRUE) |>
		mutate(
			log_base4_species_richness,
			species_richness = 4^log_base4_species_richness,
			estimate,
			conf.low,
			conf.high,
			.keep = "none"
		)
}

# Data --------------------------------------------------------------------

# Retain living seedlings from the most recent census and aggregate them to
# plot level before fitting models.
plot_data <- read_rds(
	here::here("data", "derived", "data_cleaned.rds")
) |>
	filter(
		survival == 1,
		census_no == "03"
	) |>
	mutate(
		basal_area = pi * (dbase_mm / 2000)^2 # basal area is in m2
	) |>
	group_by(
		plot,
		block,
		species_mix,
		treatment,
		generic_richness,
		struc_complexity
	) |>
	summarise(
		basal_area = sum(basal_area, na.rm = TRUE),
		seedling_density = sum(survival, na.rm = TRUE),
		.groups = "drop"
	) |>
	mutate(
		species_richness = parse_number(as.character(treatment)),
		log_base4_species_richness = log(species_richness, base = 4),
		liana_treatment = factor(
			if_else(
				coalesce(str_detect(treatment, "cut"), FALSE),
				"cut",
				"not_cut"
			)
		)
	)

# I will use this data for plotting later.
saveRDS(plot_data, here::here("data", "derived", "data_plot_lvl.rds"))

# A one-unit change represents a fourfold increase in species richness.
partition_data <- read_rds(
	here::here("data", "derived", "biodiv_effects.rds")
) |>
	mutate(
		species_richness = parse_number(as.character(treatment)),
		log_base4_species_richness = log(species_richness, base = 4)
	)


# Fit models --------------------------------------------------------------

# Using a lognormal response distribution for basal area (log link),
# negative binomial for seedling density (log link),
# and a t-distribution for the biodiversity partitioning effects (id link).

primary_responses <- c(
	basal_area = "basal_area",
	seedling_density = "seedling_density"
)

primary_family_factories <- list(
	basal_area = \() glmmTMB::lognormal(link = "log"),
	seedling_density = \() glmmTMB::nbinom2(link = "log")
)

# Exclude liana-cut plots when estimating the species-richness effect.
species_richness_models <- fit_response_models(
	data = filter(plot_data, treatment != "16-species-cut"),
	rhs = ~ log_base4_species_richness + (1 | block),
	responses = primary_responses,
	family_factories = primary_family_factories,
	dispformulas = list(
		basal_area = ~1,
		seedling_density = ~treatment
	)
)

# Restrict the liana-cut comparison to 16-species plots.
liana_cut_models <- fit_response_models(
	data = filter(plot_data, species_mix == "16-species"),
	rhs = ~ 0 + liana_treatment + (1 | block),
	responses = primary_responses,
	family_factories = primary_family_factories
)

# Estimate joint effects of canopy structure and generic richness.
genera_structure_models <- fit_response_models(
	data = filter(plot_data, treatment == "4-species"),
	rhs = ~ struc_complexity * generic_richness + (1 | block),
	responses = primary_responses,
	family_factories = primary_family_factories
)

partition_responses <- c(
	net = "net",
	complementarity = "compl",
	complementarity_size = "size_compl",
	complementarity_density = "dens_compl",
	selection = "selec",
	selection_size = "size_selec",
	selection_density = "dens_selec"
)

partition_family_factories <- rep(
	list(\() glmmTMB::t_family(link = "identity")),
	length(partition_responses)
) |>
	set_names(names(partition_responses))

biodiversity_partition_models <- fit_response_models(
	data = partition_data,
	rhs = ~ log_base4_species_richness + (1 | block),
	responses = partition_responses,
	family_factories = partition_family_factories
)


# Combine and diagnose models ---------------------------------------------

models_out <- bind_rows(
	species_richness_models = species_richness_models,
	liana_cut_models = liana_cut_models,
	genera_structure_models = genera_structure_models,
	biodiversity_partition_models = biodiversity_partition_models,
	.id = "model_set"
) |>
	mutate(
		formula = map_chr(fit, \(model) deparse1(formula(model))),
		fixed_rhs = map(fit, get_fixed_rhs),
		residuals = map(
			fit,
			\(model) DHARMa::simulateResiduals(model, plot = FALSE)
		)
	)

write_rds(
	models_out,
	here::here("output", "models", "models_out.rds")
)

pdf(
	here::here("output", "plots", "residual_plots.pdf"),
	onefile = TRUE,
	width = 11.69,
	height = 8.27
)

walk2(
	models_out$residuals,
	models_out$formula,
	\(residuals, formula_text) plot(residuals, title = formula_text)
)

dev.off()


# Extract model results ---------------------------------------------------

# Retain conditional-model coefficients on the link scale and transform
# them to the response scale using each model's inverse link.
coefficients_out <- models_out |>
	mutate(
		model_set,
		response,
		formula,
		link = map_chr(fit, \(model) stats::family(model)$link),
		result = map(fit, tidy_fixed_effects),
		.keep = "none"
	) |>
	unnest(result) |>
	rename(
		estimate_link = estimate,
		conf_low_link = conf.low,
		conf_high_link = conf.high
	) |>
	mutate(
		estimate_response = inverse_link(estimate_link, link),
		conf_low_response = inverse_link(conf_low_link, link),
		conf_high_response = inverse_link(conf_high_link, link)
	)

write_csv(
	coefficients_out,
	here::here("output", "results", "coefficients_out.csv")
)

# Keep model-level statistics separate because they have one row per model.
model_fit_out <- models_out |>
	mutate(
		model_set,
		response,
		formula,
		result = map(fit, broom.mixed::glance),
		.keep = "none"
	) |>
	unnest(result)

write_csv(
	model_fit_out,
	here::here("output", "results", "model_fit_stats_out.csv")
)


# Extract marginal means --------------------------------------------------

# Create reference grids only for models with categorical predictors.
emm_grids <- models_out |>
	filter(map_lgl(fit, has_factor_predictor)) |>
	mutate(
		emm_results = map2(fit, fixed_rhs, make_emm_grids)
	) |>
	select(model_set, response, formula, emm_results) |>
	unnest(emm_results)

means_out <- emm_grids |>
	mutate(result = map(emm_grid, summarise_emmeans)) |>
	select(-emm_grid) |>
	unnest(result)

write_csv(
	means_out,
	here::here("output", "results", "means_out.csv")
)

# Pairwise contrasts are intentionally unadjusted for multiple comparisons.
contrasts_out <- emm_grids |>
	mutate(result = map(emm_grid, summarise_contrasts)) |>
	select(-emm_grid) |>
	unnest(result)

write_csv(
	contrasts_out,
	here::here("output", "results", "contrasts_out.csv")
)


# Generate predictions ---------------------------------------------------

# Generate predictions only for models with continuous predictors.
predictions_out <-
	models_out |>
	filter_out(map_lgl(fit, has_factor_predictor)) |>
	mutate(
		model_set,
		response,
		formula,
		link = map_chr(fit, \(model) stats::family(model)$link),
		predictions = map2(fit, response, get_predictions),
		.keep = "none"
	) |>
	unnest(predictions) |>
	rename(
		estimate_link = estimate,
		conf_low_link = conf.low,
		conf_high_link = conf.high
	) |>
	mutate(
		estimate_response = inverse_link(estimate_link, link),
		conf_low_response = inverse_link(conf_low_link, link),
		conf_high_response = inverse_link(conf_high_link, link)
	) |>
	filter_out(
		model_set == "biodiversity_partition_models" &
			species_richness == 1
	)

write_csv(
	predictions_out,
	here::here("output", "results", "predictions_out.csv")
)
