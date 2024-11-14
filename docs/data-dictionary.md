# data-dictionary

## [SBE_compiled_data_2002-2024.csv](https://zenodo.org/doi/10.5281/zenodo.10815814)
This dataset contains the data collected from a number of experiments that were conducted within the Sabah Biodiversity Experiment (SBE) project site located within the Malua Forest Reserve in Sabah, Malaysia between 2002 to 2024. The data collected are measurements taken from 16 species of Dipterocarpaceae seedlings that were planted within the project site. These measurements largely comprise of the diameters at base and breast height of the main stems (in centimeters), the height of the main stem (in centimeters), densiometer readings to determine canopy openness, and the level of herbivory experienced by seedlings.

|variable                           |definition                        |
|:----------------------------------|:---------------------------------|
|`pl` | Plot number |
|`li`| Line number within each plot |
|`po`| Planting point on each line |
|`location`| Plot location /  seedling tag |
|`plantingdate`| Date of planting |
|`surveydate`| Date of survey |
|`genus`| Genus of the seedling being measured |
|`species`| Species of the seedling being measured |
|`genus_species`| The genus and species of the seedling combined |
|`SpeciesMix`| Type of species mixture (refer to planting design booklet below) |
|`survival`| Did the seedling survive when measurements were being taken? Y or N  |
|`O.N`| Was the seedling planted before the 1<sup>st</sup> or 2<sup>nd</sup> census? (O = Seedlings planted before 1<sup>st</sup> Census, N = Seedlings planted before 2nd census and after 1<sup>st</sup> census) |
|`data_origin`| `full_measurement`, `intensive` or `climber`. The experiment or study that the measurement originated from |
|`sample`| The i<sup>th</sup> sample being measured for a given study  |
|`insectdamage`| Level of leaf damage inflicted by herbivorous insects |
|`mammaldamage`| Level of leaf damage inflicted by herbivorous mammals |
|`treefalldamage`| Type of shoot damage inflicted by falling trees or branches |
|`heightapex`| Measure to the tip of leader shoot (to nearest cm) |
|`diam1`| Measure diameter of stem base at 5cm from the ground (to nearest cm) |
|`diam2`| Same as `diam1` but at right-angle to the first measurement |
|`dbh1`| Measure diameter of stem at 130cm from the ground (to nearest cm) |
|`dbh2`| As with `dbh1`, but at right-angle to the first measurement |
|`DN`| Hemispherical densiometer measurements taken facing North |
|`DE`| Hemispherical densiometer measurement taken facing East |
|`DS`| Hemispherical densiometer measurement taken facing South |
|`DW`| Hemispherical densiometer measurement taken facing West |

### Notes for users
1. Each unique combination of plot number, line number, planting point, and old/new status ("O.N") is a unique seedling.
2. There are about 600 seedlings with no scientific name within the dataset. All of these are seedlings that are planted right before the 2nd census, but did not survive until their measurement during the 2nd census. Therefore, these are left as unknown species.
3. The "data_origin" column refers to the studies conducted within the SBE project site which can be easily used by users for subsetting the dataframe for a specific study. These include the tree census or full measurement studies (1st,2nd and 3rd census/sample measurements; refer to Hector et al. 2011), intensive plot measurements (10 sample measurements; refer to Philipson et al. 2014), and climber cutting sample measurements (14 sample measurements; refer to O'Brien et al. 2019).
4. We also want to mention that there are 475 seedlings with no survey dates in the 2nd census.
