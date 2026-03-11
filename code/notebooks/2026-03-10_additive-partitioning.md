# Additive partition of biodiversity effects
eleanorjackson
2026-03-11

``` r
library("tidyverse")
library("here")
library("patchwork")
```

[Loreau & Hector 2001:](https://doi.org/10.1126/science.1064088)

> We measure the net biodiversity effect, $𝚫Y$, by the difference
> between the observed yield of a mixture and its expected yield under
> the null hypothesis that there is no selection effect or
> complementarity effect. This expected value is the weighted (by the
> initial relative abundance of species in mixture) average of the
> monoculture yields for the component species. Positive selection
> occurs if species with higher-than-average monoculture yields dominate
> the mixtures. The selection effect is measured by the covariance
> between the monoculture yield of species and their change in relative
> yield in the mixture. Finally, a positive complementarity effect
> occurs if species yields in a mixture are on average higher than
> expected on the basis of the weighted average monoculture yield of the
> component species. These various effects can be related by additive
> partition as follows.

$M_i =$ yield of species $i$ in monoculture

$Y_{O,i} =$ observed yield of species $i$ in the mixture

$Y_O = ∑_i Y_{O,i} =$ total observed yield of the mixture

$RY_{E,i} =$ expected relative yield of species $i$ in the mixture,
which is simply its proportion seeded or planted

$RY_{O,i} = Y_{O,i}/M_i =$ observed relative yield of species $i$ in the
mixture

$Y_{E,i} = RY_{E,i}M_i =$ expected yield of species $i$ in the mixture

$Y_E = ∑_iY_{E,i} =$ total expected yield of the mixture

$𝚫Y = Y_O - Y_E =$ deviation from total expected yield in the mixture

$𝚫RY_i = RY_{O,i} - RY_{E,i} =$ deviation from expected relative yield
of species $i$ in the mixture

$N =$ number of species in the mixture

$$
𝚫Y = Y_O - Y_E = ∑_i RY_{O,i} M_i - ∑_i RY_{E,i} M_i = ∑_i 𝚫RY_iM_i = N\overline{𝚫RY} \overline{M} + N cov(𝚫RY,M)
$$

$N\overline{𝚫RY} \overline{M}$ is the complementarity effect and
$N cov(𝚫RY,M)$ is the selection effect.

``` r
data <- 
  readRDS(here::here("data", "derived", "data_cleaned.rds")) 
```

Calculate basal area

``` r
data <- 
  data %>% 
  mutate(dbase_m = dbase_mm / 1000) %>% 
  mutate(basal_area_m2 = pi * (dbase_m/2)^2)
```

Calculate yield (summed BA) of each species in each treatment

``` r
data_yield <- 
  data %>% 
  filter(str_detect(census_id, "full_measurement")) %>% 
  select(!survey_date:dbase_m & !census_no) %>% 
  pivot_wider(names_from = census_id,
              values_from = basal_area_m2) %>%
  # summed basal area  of each species in each treatment
  group_by(genus_species, treatment) %>%
  summarise(ba_m2_cenus_01 = sum(full_measurement_01, na.rm = TRUE),
            ba_m2_cenus_02 = sum(full_measurement_02, na.rm = TRUE),
            ba_m2_cenus_03 = sum(full_measurement_03, na.rm = TRUE),
            .groups = "drop") %>% 
  # yield
  mutate(yield = ba_m2_cenus_03 - ba_m2_cenus_01)
```

We now have $M_i$ (yield of species $i$ in monoculture) and $Y_{O,i}$
(observed yield of species $i$ in mixture):

``` r
data_yield %>% 
  select(genus_species, treatment, yield) %>% 
  glimpse()
```

    Rows: 64
    Columns: 3
    $ genus_species <fct> Dipterocarpus_conformis, Dipterocarpus_conformis, Dipter…
    $ treatment     <fct> 16-species, 16-species-cut, 4-species, monoculture, 16-s…
    $ yield         <dbl> 0.2883357, 0.1505064, 0.5505712, 0.3003661, 0.6960972, 0…

The total observed yield of the mixture $Y_O$ is the summed observed
yield of all species in mixture $∑_i Y_{O,i}$ :

``` r
Y_O <- 
  data_yield %>% 
  filter(treatment == "16-species" |
           treatment == "4-species") %>% 
  group_by(treatment) %>% 
  summarise(total_obs_yield = sum(yield))

glimpse(Y_O)
```

    Rows: 2
    Columns: 2
    $ treatment       <fct> 16-species, 4-species
    $ total_obs_yield <dbl> 13.17627, 11.72055

We can calculate observed relative yield of species $i$ in the mixture
as $RY_{O,i} = Y_{O,i}/M_i$ :

``` r
RY_Oi <- 
  data_yield %>% 
  select(genus_species, treatment, yield) %>% 
  pivot_wider(names_from = treatment,
              values_from = yield) %>% 
  # calculating separately for 16- and 4- species mix
  mutate(obs_rel_yield_16 = `16-species` / monoculture,
         obs_rel_yield_04 = `4-species` / monoculture) %>% 
  select(genus_species, obs_rel_yield_04, obs_rel_yield_16)

glimpse(RY_Oi)
```

    Rows: 16
    Columns: 3
    $ genus_species    <fct> Dipterocarpus_conformis, Dryobalanops_lanceolata, Hop…
    $ obs_rel_yield_04 <dbl> 1.83300020, 2.10923383, 3.14507241, 2.07857754, 0.186…
    $ obs_rel_yield_16 <dbl> 0.9599473, 1.1458336, 2.4294730, 0.8996051, 0.5999437…

The expected relative yield ($RY_{E,i}$) for each species in the
4-species mix is 0.25 and 0.0625 for each species in the 16-species mix
(i.e., 1/4 and 1/16).

To calculate the expected yield of species $i$ in the mixture
($Y_{E,i}$) we can do $RY_{E,i}M_i$ (the expected relative yield
multiplied by the yield of the species in monoculture):

``` r
RY_Ei <- 
  data_yield %>% 
  filter(treatment == "monoculture") %>% 
  mutate(exp_yield_04 = yield * 0.25,
         exp_yield_16 = yield * 0.0625) %>% 
  select(genus_species, exp_yield_04, exp_yield_16)

glimpse(RY_Ei)
```

    Rows: 16
    Columns: 3
    $ genus_species <fct> Dipterocarpus_conformis, Dryobalanops_lanceolata, Hopea_…
    $ exp_yield_04  <dbl> 0.07509153, 0.15187572, 0.05670068, 0.28153071, 0.269344…
    $ exp_yield_16  <dbl> 0.018772883, 0.037968930, 0.014175170, 0.070382676, 0.06…

and the total expected yield of the mixture ($Y_E$) is those values
summed across species $∑_iY_{E,i}$ :

``` r
Y_E <- 
  data_yield %>% 
  filter(treatment == "monoculture") %>% 
  mutate(exp_yield_04 = yield * 0.25,
         exp_yield_16 = yield * 0.0625) %>% 
  summarise(`4-species` = sum(exp_yield_04),
            `16-species` = sum(exp_yield_16)) %>% 
  pivot_longer(cols = everything(),
               names_to = "treatment",
               values_to = "total_exp_yield")

glimpse(Y_E)
```

    Rows: 2
    Columns: 2
    $ treatment       <chr> "4-species", "16-species"
    $ total_exp_yield <dbl> 4.027028, 1.006757

So now we can calculate $𝚫Y$, the deviation from total expected relative
yield in the mixture as: $Y_{O} - Y_{E}$

``` r
full_join(Y_E, Y_O) %>% 
  mutate(DY = total_obs_yield - total_exp_yield)
```

    # A tibble: 2 × 4
      treatment  total_exp_yield total_obs_yield    DY
      <chr>                <dbl>           <dbl> <dbl>
    1 4-species             4.03            11.7  7.69
    2 16-species            1.01            13.2 12.2 

and $𝚫RY_i$, the deviation from expected relative yield of species $i$
in the mixture as: $RY_{O,i} - RY_{E,i}$

``` r
DRY_i <- 
  full_join(RY_Ei, RY_Oi) %>% 
  mutate(DRY_i_04 = obs_rel_yield_04 - exp_yield_04,
         DRY_i_16 = obs_rel_yield_16 - exp_yield_16) %>% 
  select(genus_species, DRY_i_04, DRY_i_16)

glimpse(DRY_i)
```

    Rows: 16
    Columns: 3
    $ genus_species <fct> Dipterocarpus_conformis, Dryobalanops_lanceolata, Hopea_…
    $ DRY_i_04      <dbl> 1.75790866, 1.95735811, 3.08837173, 1.79704683, -0.08295…
    $ DRY_i_16      <dbl> 0.9411744, 1.1078647, 2.4152978, 0.8292224, 0.5326077, 1…

The complementarity effect is $N\overline{𝚫RY} \overline{M}$, the number
of species in the mixture ($N$) multiplied by the mean $𝚫RY_i$ and mean
$M_i$ across species.

``` r
M_i_mean <-
  data_yield %>% 
  filter(treatment == "monoculture") %>% 
  summarise(mean(yield)) %>% 
  pluck(1,1)

complementarity_effect <-
  DRY_i %>% 
  summarise(DRY_mean_04 = mean(DRY_i_04),
            DRY_mean_16 = mean(DRY_i_16)) %>% 
  mutate(`4-species` = 4 * DRY_mean_04 * M_i_mean,
         `16-species` = 16 * DRY_mean_16 * M_i_mean) %>% 
  pivot_longer(cols = c(`4-species`, `16-species`),
               names_to = "treatment",
               values_to = "complementarity_effect") %>% 
  select(treatment, complementarity_effect)

complementarity_effect
```

    # A tibble: 2 × 2
      treatment  complementarity_effect
      <chr>                       <dbl>
    1 4-species                    2.80
    2 16-species                  18.2 

and the selection effect $N cov(𝚫RY,M)$

``` r
M_i <- 
  data_yield %>% 
  filter(treatment == "monoculture") %>% 
  select(genus_species, yield)

selection_effect_04 <- 
  cov(x = DRY_i$DRY_i_04, y = M_i$yield) * 4

selection_effect_16 <- 
  cov(x = DRY_i$DRY_i_16, y = M_i$yield) * 16

selection_effect <-
  tibble(
    treatment = c("4-species", "16-species"),
    selection_effect = c(selection_effect_04, selection_effect_16)
  )

selection_effect
```

    # A tibble: 2 × 2
      treatment  selection_effect
      <chr>                 <dbl>
    1 4-species             -1.58
    2 16-species            -7.03
