# Estimate total basal area of seedlings from most recent census
eleanorjackson
2025-10-30

``` r
library("tidyverse")
library("here")
library("patchwork")
```

``` r
new_census <- 
  readRDS(here::here("data", "derived", "data_cleaned.rds")) %>% 
  filter(census_id == "full_measurement_03") # most recent census
```

Calculate basal area for each seedling using basal diameter measurements

``` r
seedling_ba <-
  new_census %>% 
  filter(survival == 1) %>% # only alive seedlings
  mutate(dbase_m = dbase_mm / 1000) %>% 
  mutate(basal_area = pi * (dbase_m/2)^2)
```

Summing basal area per treatment and calculating per ha

``` r
seedling_ba %>% 
  group_by(treatment) %>% 
  summarise(sum_basal_area = sum(basal_area, na.rm = TRUE),
            n_plots = n_distinct(plot)) %>% 
  mutate(basal_area_per_ha = (sum_basal_area / n_plots)/4 ) %>% # 1 plot is 4ha
  knitr::kable()
```

| treatment      | sum_basal_area | n_plots | basal_area_per_ha |
|:---------------|---------------:|--------:|------------------:|
| 16-species     |       13.50826 |      32 |         0.1055333 |
| 16-species-cut |        9.22940 |      16 |         0.1442094 |
| 4-species      |       12.01510 |      32 |         0.0938680 |
| monoculture    |       16.47040 |      32 |         0.1286750 |

``` r
seedling_ba %>% 
  mutate(treatment_simplified = 
           case_when(treatment == "16-species-cut" ~ "lianas+planting",
                     .default = "planting")) %>% 
  group_by(treatment_simplified) %>% 
  summarise(sum_basal_area = sum(basal_area, na.rm = TRUE),
            n_plots = n_distinct(plot)) %>% 
  mutate(basal_area_per_ha = (sum_basal_area / n_plots)/4 ) %>% # 1 plot is 4ha 
  knitr::kable()
```

| treatment_simplified | sum_basal_area | n_plots | basal_area_per_ha |
|:---------------------|---------------:|--------:|------------------:|
| lianas+planting      |        9.22940 |      16 |         0.1442094 |
| planting             |       41.99376 |      96 |         0.1093588 |
