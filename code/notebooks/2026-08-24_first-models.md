# First models - SBE 2nd decade
eleanorjackson
26 August, 2026

- [`lmer` models](#lmer-models)
- [`lm` models](#lm-models)
- [`glmmTMB` models](#glmmtmb-models)
- [Try block as a random effect](#try-block-as-a-random-effect)
- [Intercept only models](#intercept-only-models)

First step to fit:

`partitioning_component ~ treatment + block + (1 | species_mix)`

Later we will fit models for the 4-species plots only:

`partitioning_component ~ structural_complexity * generic_richness + block + (1 | species_mix)`

The 4-species analysis is treated as a separate subanalysis because
structural complexity and generic richness are only defined within that
subset.

``` r
library("tidyverse")
library("patchwork")
library("lme4")
library("glmmTMB")
library("broom.mixed")
library("DHARMa")
```

``` r
partitions <- readRDS(here::here("data", "derived", "biodiv_effects.rds"))
```

Create model fitting function that we can run over multiple
`partitioning_component`s

``` r
fit_models <- function(data, formula, method = c("lmer", "lm", "glmmTMB")) {
    responses <- c(
        m_NE = "net",
        m_CE = "compl",
        m_CE_size = "size_compl",
        m_CE_dens = "dens_compl",
        m_SE = "selec",
        m_SE_size = "size_selec",
        m_SE_dens = "dens_selec"
    )

    fit_one <- purrr::quietly(\(response) {
        if (method == "lmer") {
            lme4::lmer(
                stats::as.formula(
                    paste(response, formula)
                ),
                data = data
            )
        } else if (method == "glmmTMB") {
            glmmTMB::glmmTMB(
                stats::as.formula(
                    paste(response, formula)
                ),
                data = data,
                family = t_family(link = "identity")
            )
        } else {
            stats::lm(
                stats::as.formula(
                    paste(response, formula)
                ),
                data = data
            )
        }
    })

    results <- purrr::map(responses, fit_one)

    # check for warnings and singular fits
    tibble::tibble(
        name = names(responses),
        fit = purrr::map(results, "result"),
        singular = purrr::map_lgl(
            results,
            \(x) method == "lmer" && lme4::isSingular(x$result)
        ),
        warning = purrr::map_chr(
            results,
            \(x) {
                warnings <- x$warnings

                if (method == "lmer" && lme4::isSingular(x$result)) {
                    warnings <- c(
                        warnings,
                        "boundary (singular) fit: see help('isSingular')"
                    )
                }

                paste(unique(warnings), collapse = "\n")
            }
        )
    )
}
```

## `lmer` models

``` r
models_lmer <- fit_models(
    data = partitions,
    formula = "~ 0 + treatment + block + (1 | species_mix)",
    method = "lmer"
)
```

``` r
models_lmer |>
    dplyr::filter(warning != "")
```

    # A tibble: 1 × 4
      name      fit          singular warning                                       
      <chr>     <named list> <lgl>    <chr>                                         
    1 m_SE_dens <lmerMod>    TRUE     boundary (singular) fit: see help('isSingular…

The selection density effect model is giving us a sigular fit, but
others are ok.

``` r
models_lmer |>
    dplyr::filter(warning != "") |>
    pull(fit)
```

    $m_SE_dens
    Linear mixed model fit by REML ['lmerMod']
    Formula: dens_selec ~ 0 + treatment + block + (1 | species_mix)
       Data: data
    REML criterion at convergence: -114.392
    Random effects:
     Groups      Name        Std.Dev.
     species_mix (Intercept) 0.0000  
     Residual                0.0875  
    Number of obs: 64, groups:  species_mix, 17
    Fixed Effects:
     treatment4-species  treatment16-species           blocksouth  
               -0.01582             -0.08733             -0.03701  
    optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings 

The model estimates the species_mix variance as exactly zero – suggests
that the data don’t support between-mixture random-intercept variation
for this response.

Most of the `species_mix` levels have only 2 observations (4-species
plots), while one level has 32 observations (the 16-species plots). We
probably shouldn’t have it as a random effect, but I’m not sure it works
as a fixed effect either since it’s confounded with treatment. The
`species_mix` terms might absorb most of the treatment effect.

The cleanest model is probably `reponse ~ 0 + treatment + block`.

We could do a separate exploratory model with species composition for
only the 4-species plots, like `dens_selec ~ species_mix + block`, but
will leave this for now.

## `lm` models

``` r
models_lm <- fit_models(
    data = partitions,
    formula = "~ 0 + treatment + block",
    method = "lm"
)
```

Extract the coefficients and plot them

``` r
results_lm <-
    models_lm |>
    mutate(
        tidy = purrr::map(
            fit,
            tidy,
            effects = "fixed",
            conf.int = TRUE
        )
    ) |>
    unnest(tidy) |>
    mutate(
        term = fct_relevel(
            term,
            "treatment4-species",
            "treatment16-species",
            "blocksouth"
        )
    )
```

``` r
results_lm |>
    ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange(shape = 21, fill = "white") +
    labs(x = "Term", y = "Estimate ± CI [95%]") +
    geom_hline(yintercept = 0, color = "blue") +
    coord_flip() +
    facet_wrap(~name, ncol = 1)
```

![](figures/2026-08-24_first-models/unnamed-chunk-9-1.png)

``` r
models_lm <-
    models_lm |>
    mutate(
        resids = purrr::map(
            fit,
            DHARMa::simulateResiduals,
            plot = FALSE
        )
    )
```

``` r
purrr::walk2(
    models_lm$resids,
    models_lm$name,
    \(resids, name) plot(resids, title = name)
)
```

![](figures/2026-08-24_first-models/unnamed-chunk-11-1.png)

![](figures/2026-08-24_first-models/unnamed-chunk-11-2.png)

![](figures/2026-08-24_first-models/unnamed-chunk-11-3.png)

![](figures/2026-08-24_first-models/unnamed-chunk-11-4.png)

![](figures/2026-08-24_first-models/unnamed-chunk-11-5.png)

![](figures/2026-08-24_first-models/unnamed-chunk-11-6.png)

![](figures/2026-08-24_first-models/unnamed-chunk-11-7.png)

A fair ammount of deviation in the qq plots. Could be due to long tails
in the distribution of the response, see here:

``` r
partitions |>
    ggplot(aes(x = net)) +
    geom_density() +

    partitions |>
        ggplot(aes(x = compl)) +
    geom_density() +

    partitions |>
        ggplot(aes(x = selec)) +
    geom_density() +

    partitions |>
        ggplot(aes(x = size_compl)) +
    geom_density() +

    partitions |>
        ggplot(aes(x = dens_compl)) +
    geom_density() +

    partitions |>
        ggplot(aes(x = size_selec)) +
    geom_density() +

    partitions |>
        ggplot(aes(x = dens_selec)) +
    geom_density()
```

![](figures/2026-08-24_first-models/unnamed-chunk-12-1.png)

## `glmmTMB` models

Let’s try a t response distribution rather than Gaussian.

``` r
models_tmb <- fit_models(
    data = partitions,
    formula = "~ 0 + treatment + block",
    method = "glmmTMB"
)
```

``` r
models_tmb |>
    dplyr::filter(warning != "")
```

    # A tibble: 0 × 4
    # ℹ 4 variables: name <chr>, fit <named list>, singular <lgl>, warning <chr>

No warnings

``` r
results_tmb <-
    models_tmb |>
    mutate(
        tidy = purrr::map(
            fit,
            tidy,
            effects = "fixed",
            conf.int = TRUE
        )
    ) |>
    unnest(tidy) |>
    mutate(
        term = fct_relevel(
            term,
            "treatment4-species",
            "treatment16-species",
            "blocksouth"
        )
    )
```

``` r
results_tmb |>
    ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange(shape = 21, fill = "white") +
    labs(x = "Term", y = "Estimate ± CI [95%]") +
    geom_hline(yintercept = 0, color = "blue") +
    coord_flip() +
    facet_wrap(~name, ncol = 1) +
    ggtitle("glmmTMB") +

    results_lm |>
        ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange(shape = 21, fill = "white") +
    labs(x = "Term", y = "Estimate ± CI [95%]") +
    geom_hline(yintercept = 0, color = "blue") +
    coord_flip() +
    facet_wrap(~name, ncol = 1) +
    ggtitle("lm")
```

![](figures/2026-08-24_first-models/unnamed-chunk-16-1.png)

``` r
models_tmb <-
    models_tmb |>
    mutate(
        resids = purrr::map(
            fit,
            DHARMa::simulateResiduals,
            plot = FALSE
        )
    )
```

``` r
purrr::walk2(
    models_tmb$resids,
    models_tmb$name,
    \(resids, name) plot(resids, title = name)
)
```

![](figures/2026-08-24_first-models/unnamed-chunk-18-1.png)

![](figures/2026-08-24_first-models/unnamed-chunk-18-2.png)

![](figures/2026-08-24_first-models/unnamed-chunk-18-3.png)

![](figures/2026-08-24_first-models/unnamed-chunk-18-4.png)

![](figures/2026-08-24_first-models/unnamed-chunk-18-5.png)

![](figures/2026-08-24_first-models/unnamed-chunk-18-6.png)

![](figures/2026-08-24_first-models/unnamed-chunk-18-7.png)

Residuals looking much better!

## Try block as a random effect

Block has only 2 levels, but we are not specifically interested in block
so might be better to average over it – allow information to be shared
across blocks (partial pooling) – with the understanding that the
variance for the block random effect will be unreliable.

Would rather generalise to a broader population than make specific
comparisons between blocks.

``` r
models_rand_bl <- fit_models(
    data = partitions,
    formula = "~ 0 + treatment + (1|block)",
    method = "glmmTMB"
)
```

``` r
models_rand_bl |>
    dplyr::filter(warning != "")
```

    # A tibble: 0 × 4
    # ℹ 4 variables: name <chr>, fit <named list>, singular <lgl>, warning <chr>

No warnings

``` r
results_rand_bl <-
    models_rand_bl |>
    mutate(
        tidy = purrr::map(
            fit,
            tidy,
            effects = "fixed",
            conf.int = TRUE
        )
    ) |>
    unnest(tidy) |>
    mutate(
        term = fct_relevel(
            term,
            "treatment4-species",
            "treatment16-species"
        )
    )
```

``` r
results_tmb |>
    ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange(shape = 21, fill = "white") +
    labs(x = "Term", y = "Estimate ± CI [95%]") +
    geom_hline(yintercept = 0, color = "blue") +
    coord_flip() +
    facet_wrap(~name, ncol = 1) +
    ggtitle("glmmTMB - block fixed") +

    results_rand_bl |>
        ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange(shape = 21, fill = "white") +
    labs(x = "Term", y = "Estimate ± CI [95%]") +
    geom_hline(yintercept = 0, color = "blue") +
    coord_flip() +
    facet_wrap(~name, ncol = 1) +
    ggtitle("glmmTMB - block random")
```

![](figures/2026-08-24_first-models/unnamed-chunk-22-1.png)

``` r
models_rand_bl <-
    models_rand_bl |>
    mutate(
        resids = purrr::map(
            fit,
            DHARMa::simulateResiduals,
            plot = FALSE
        )
    )
```

``` r
purrr::walk2(
    models_rand_bl$resids,
    models_rand_bl$name,
    \(resids, name) plot(resids, title = name)
)
```

![](figures/2026-08-24_first-models/unnamed-chunk-24-1.png)

![](figures/2026-08-24_first-models/unnamed-chunk-24-2.png)

![](figures/2026-08-24_first-models/unnamed-chunk-24-3.png)

![](figures/2026-08-24_first-models/unnamed-chunk-24-4.png)

![](figures/2026-08-24_first-models/unnamed-chunk-24-5.png)

![](figures/2026-08-24_first-models/unnamed-chunk-24-6.png)

![](figures/2026-08-24_first-models/unnamed-chunk-24-7.png)

## Intercept only models

Is there a a biodiversity effect (regardless of treatment)?

``` r
models_intercept <- fit_models(
    data = partitions,
    formula = "~ 1 + (1|block)",
    method = "glmmTMB"
)
```

``` r
results_intercept <-
    models_intercept |>
    filter(name %in% c("m_NE", "m_CE", "m_SE")) |>
    mutate(
        tidy = purrr::map(
            fit,
            tidy,
            effects = "fixed",
            conf.int = TRUE
        )
    ) |>
    unnest(tidy) |>
    mutate(
        name = fct_relevel(
            name,
            "m_NE",
            "m_CE",
            "m_SE"
        )
    )
```

``` r
results_intercept |>
    ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange(shape = 21, fill = "white") +
    labs(x = "Term", y = "Estimate ± CI [95%]") +
    geom_hline(yintercept = 0, color = "blue") +
    coord_flip() +
    facet_wrap(~name, ncol = 1)
```

![](figures/2026-08-24_first-models/unnamed-chunk-27-1.png)
