# Estimate total basal area of seedlings from the second recent census
eleanorjackson
2025-11-04

Replicating
[2025-10-30_seedling-total-basal-area](2025-10-30_seedling-total-basal-area.md)
for the second full SBE census.

``` r
library("tidyverse")
library("here")
library("patchwork")
```

``` r
new_census <- 
  readRDS(here::here("data", "derived", "data_cleaned.rds")) %>% 
  filter(census_id == "full_measurement_02") # most recent census
```

Number of seedlings:

``` r
new_census %>% 
  group_by(survival) %>% 
  summarise(n_seedlings = n_distinct(plant_id))
```

    # A tibble: 2 × 2
      survival n_seedlings
         <dbl>       <int>
    1        0       15128
    2        1       39953

Note that seedlings which were never recorded as alive are not included
here (i.e., they died between planting and the first census, so we have
no size data for them).

``` r
new_census %>% 
  group_by(plot, treatment, survival) %>% 
  summarise(n_seedlings = n_distinct(plant_id),
            .groups = "drop") %>% 
  pivot_wider(names_from = survival, values_from = n_seedlings) %>% 
  rename(n_alive = `1`, n_dead = `0`) %>% 
  mutate(total = n_dead + n_alive) %>% 
  knitr::kable()
```

| plot | treatment      | n_dead | n_alive | total |
|:-----|:---------------|-------:|--------:|------:|
| 001  | 4-species      |     74 |     565 |   639 |
| 002  | monoculture    |    246 |     831 |  1077 |
| 003  | 16-species     |    285 |     407 |   692 |
| 004  | monoculture    |    340 |     848 |  1188 |
| 005  | 16-species-cut |    324 |     452 |   776 |
| 006  | 4-species      |    178 |     353 |   531 |
| 007  | monoculture    |    194 |     304 |   498 |
| 008  | 16-species     |    352 |     392 |   744 |
| 009  | 4-species      |    164 |     405 |   569 |
| 010  | monoculture    |    144 |     535 |   679 |
| 011  | 16-species-cut |    304 |     383 |   687 |
| 012  | 4-species      |     93 |     386 |   479 |
| 013  | monoculture    |    170 |     230 |   400 |
| 014  | 16-species-cut |    372 |     463 |   835 |
| 015  | 16-species     |    183 |     390 |   573 |
| 016  | 4-species      |    196 |     349 |   545 |
| 017  | 16-species     |    264 |     318 |   582 |
| 018  | monoculture    |    242 |     503 |   745 |
| 019  | 4-species      |    152 |     358 |   510 |
| 021  | 16-species     |    136 |     341 |   477 |
| 022  | 16-species-cut |     93 |     227 |   320 |
| 023  | monoculture    |    118 |     227 |   345 |
| 025  | 4-species      |    128 |     232 |   360 |
| 027  | monoculture    |    232 |     401 |   633 |
| 028  | 4-species      |     74 |     203 |   277 |
| 029  | 16-species-cut |    111 |     302 |   413 |
| 030  | 16-species     |     76 |     200 |   276 |
| 031  | 16-species     |    110 |     240 |   350 |
| 032  | 16-species-cut |    166 |     247 |   413 |
| 033  | 16-species     |    100 |     426 |   526 |
| 034  | monoculture    |      7 |      58 |    65 |
| 035  | 4-species      |     63 |     201 |   264 |
| 037  | monoculture    |    139 |     530 |   669 |
| 038  | 16-species     |     61 |     453 |   514 |
| 039  | 4-species      |     75 |     297 |   372 |
| 040  | 16-species-cut |     69 |     389 |   458 |
| 041  | 4-species      |    125 |     277 |   402 |
| 042  | 16-species     |    118 |     427 |   545 |
| 043  | monoculture    |     56 |     151 |   207 |
| 044  | monoculture    |    117 |     591 |   708 |
| 045  | 16-species     |    108 |     335 |   443 |
| 046  | 16-species-cut |     97 |     301 |   398 |
| 048  | 4-species      |    130 |     370 |   500 |
| 049  | 16-species-cut |     66 |     394 |   460 |
| 050  | monoculture    |    109 |     261 |   370 |
| 051  | 16-species     |    129 |     399 |   528 |
| 052  | monoculture    |     89 |     138 |   227 |
| 053  | 16-species     |    103 |     311 |   414 |
| 054  | 4-species      |     84 |     287 |   371 |
| 055  | 16-species     |     92 |     341 |   433 |
| 056  | 4-species      |     62 |     221 |   283 |
| 058  | 4-species      |    121 |     332 |   453 |
| 059  | monoculture    |    120 |     451 |   571 |
| 060  | 4-species      |    101 |     393 |   494 |
| 061  | monoculture    |     95 |     449 |   544 |
| 062  | 16-species     |    161 |     369 |   530 |
| 063  | 16-species     |    115 |     293 |   408 |
| 064  | 16-species-cut |    118 |     373 |   491 |
| 065  | monoculture    |    177 |     382 |   559 |
| 066  | 16-species     |    152 |     381 |   533 |
| 067  | 4-species      |    157 |     553 |   710 |
| 069  | monoculture    |     91 |     612 |   703 |
| 070  | monoculture    |    125 |     578 |   703 |
| 071  | 4-species      |    116 |     417 |   533 |
| 072  | 16-species     |    114 |     374 |   488 |
| 073  | 16-species     |    119 |     329 |   448 |
| 074  | 4-species      |    102 |     216 |   318 |
| 075  | 16-species-cut |    110 |     452 |   562 |
| 076  | 16-species     |     73 |     324 |   397 |
| 077  | 4-species      |    143 |     549 |   692 |
| 078  | monoculture    |     45 |     284 |   329 |
| 079  | 4-species      |     78 |     331 |   409 |
| 081  | monoculture    |    106 |     480 |   586 |
| 082  | 4-species      |     87 |     356 |   443 |
| 083  | 16-species     |    129 |     255 |   384 |
| 084  | monoculture    |    147 |     458 |   605 |
| 085  | 16-species-cut |    197 |     429 |   626 |
| 086  | monoculture    |    111 |      82 |   193 |
| 087  | 16-species     |    146 |     354 |   500 |
| 088  | 4-species      |    114 |     340 |   454 |
| 090  | monoculture    |    126 |     199 |   325 |
| 091  | 16-species-cut |    170 |     364 |   534 |
| 092  | 16-species     |    179 |     454 |   633 |
| 093  | 4-species      |    178 |     442 |   620 |
| 094  | 16-species     |    125 |     321 |   446 |
| 096  | 16-species     |    139 |     323 |   462 |
| 097  | 4-species      |    129 |     388 |   517 |
| 098  | monoculture    |    145 |     424 |   569 |
| 099  | 16-species     |    137 |     448 |   585 |
| 100  | 16-species-cut |    172 |     393 |   565 |
| 101  | monoculture    |    183 |     375 |   558 |
| 102  | 4-species      |    153 |     307 |   460 |
| 103  | 16-species     |    128 |     258 |   386 |
| 104  | monoculture    |     95 |     496 |   591 |
| 105  | 4-species      |    184 |     463 |   647 |
| 106  | 16-species     |    147 |     297 |   444 |
| 107  | monoculture    |    136 |     189 |   325 |
| 109  | 4-species      |    125 |     315 |   440 |
| 110  | monoculture    |    141 |     320 |   461 |
| 111  | 16-species-cut |     93 |     286 |   379 |
| 112  | 4-species      |     88 |     350 |   438 |
| 113  | 16-species     |    162 |     244 |   406 |
| 114  | monoculture    |    177 |     229 |   406 |
| 115  | 16-species-cut |     87 |     349 |   436 |
| 116  | 4-species      |    169 |     262 |   431 |
| 118  | monoculture    |    108 |     184 |   292 |
| 119  | 4-species      |    109 |     223 |   332 |
| 120  | 16-species     |     73 |     389 |   462 |
| 121  | monoculture    |     64 |     494 |   558 |
| 122  | 4-species      |     64 |     286 |   350 |
| 123  | 16-species     |     72 |     227 |   299 |
| 124  | 16-species     |     80 |     208 |   288 |

Mean and max seedling sizes:

``` r
new_census %>% 
  filter(survival == 1) %>% 
  summarise(max_dbh = max(dbh_mm, na.rm = TRUE),
            mean_dbh = mean(dbh_mm, na.rm = TRUE),
            max_basal = max(dbase_mm, na.rm = TRUE),
            mean_basal = mean(dbase_mm, na.rm = TRUE)) %>% 
  knitr::kable()
```

| max_dbh | mean_dbh | max_basal | mean_basal |
|--------:|---------:|----------:|-----------:|
|     810 |  20.0158 |       410 |   11.87788 |

Distribution of seedling sizes:

``` r
new_census %>% 
  filter(survival == 1) %>% 
  ggplot(aes(x = dbh_mm)) +
  geom_density() +
  
  new_census %>% 
  filter(survival == 1) %>% 
  ggplot(aes(x = dbase_mm)) +
  geom_density()
```

![](figures/2025-11-04_seedling-total-basal-area--cens2/unnamed-chunk-6-1.png)

``` r
new_census %>% 
  filter(survival == 1) %>% # only alive seedlings
  summarise(n_distinct(plant_id))
```

    # A tibble: 1 × 1
      `n_distinct(plant_id)`
                       <int>
    1                  39953

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
| 16-species     |       4.285453 |      32 |         0.0334801 |
| 16-species-cut |       2.262722 |      16 |         0.0353550 |
| 4-species      |       3.227593 |      32 |         0.0252156 |
| monoculture    |       4.267682 |      32 |         0.0333413 |

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
| lianas+planting      |       2.262722 |      16 |          0.035355 |
| planting             |      11.780728 |      96 |          0.030679 |
