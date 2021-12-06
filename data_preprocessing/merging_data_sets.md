Merging Data Sets
================
Hun Lee (sl4836), Jimmy Kelliher (jmk2303), Tanvir Khan (tk2886), Tucker
Morgan (tlm2152), Zachary Katz (zak2132)
11/20/2021

``` r
# Load packages
library(tidyverse)
library(janitor)
```

Below, we read in monthly death rates and hospitalization rates due to
COVID-19, as well as rates of vaccination against COVID-19, courtesy of
DOHMH. Data are recorded for each Zip Code Tabulation Area (ZCTA) in
NYC, and values denote rates per 100,000 residents of a given ZCTA.
Next, we read in two crosswalks between geographies of NYC, courtesy of
Baruch College. The `zcta_puma_cross.csv` crosswalk enables us to merge
data between ZCTAs and PUMAs, neither of which is a strict sub-geography
of the other.

``` r
# Import hospitalization, death, and vaccination ZCTA data
death_data <- read_csv("./data/nyc_death_rate_zcta.csv")
hosp_data <- read_csv("./data/nyc_hosp_rate_zcta.csv")
vacc_data <- read_csv("./data/nyc_vacc_zcta.csv")

# Read in ZCTA/PUMA crosswalk
zcta_puma_cross <- read_csv("./data/zcta_puma_cross.csv")
```

Below, we are calculating the sum of death rate and hospitalization rate
for each ZCTA over the time interval from March 2020 to September 2021.

``` r
# Obtain death rate summed by ZCTA over the entire time interval
death_data_clean <-
  death_data %>% 
  janitor::clean_names() %>%
  select(deathrate_10001:deathrate_11697) %>%
  pivot_longer(deathrate_10001:deathrate_11697, 
               names_to = "zcta", 
               values_to = "death_rate", 
               names_prefix = "deathrate_") %>%
  group_by(zcta) %>%
  mutate(death_rate = replace_na(death_rate, 0)) %>% 
  summarise(death_rate_sum = 
              sum(death_rate))

# Deprecate original death data frame
rm(death_data)
```

``` r
# Obtain hospitalization rate summed by ZCTA over the entire time interval
hosp_data_clean <- 
  hosp_data %>% 
  janitor::clean_names() %>%
  select(hosprate_10001:hosprate_11697) %>%
  pivot_longer(hosprate_10001:hosprate_11697, 
               names_to = "zcta", 
               values_to = "hosp_rate", 
               names_prefix = "hosprate_") %>%
  group_by(zcta) %>%
  mutate(hosp_rate = replace_na(hosp_rate, 0)) %>% 
  summarise(hosp_rate_sum = 
              sum(hosp_rate))

# Deprecate original hospitalization data frame
rm(hosp_data)
```

Below, we clean `vacc_data` and extract only the percentage of partially
or fully vaccinated individuals in each ZCTA as of November 16, 2021.

``` r
# Obtain vaccination rate summed by ZCTA over the entire time interval
vacc_data_clean <- 
  vacc_data %>% 
  janitor::clean_names() %>% 
  select(modzcta, perc_1plus) %>% # We can make changes here if we want to include other variables
  rename(zcta = modzcta) %>% 
  mutate(zcta = as.character(zcta))

# Deprecate original vaccination data frame
rm(vacc_data)
```

Below, we merge our outcomes data:

``` r
# Merge hospitalization and death data
merged_outcomes <- 
  merge(death_data_clean, hosp_data_clean, by = "zcta")

# Merge vaccination data with hospitalization and death data
merged_outcomes <- 
  merge(merged_outcomes, vacc_data_clean, by = "zcta")

# Observe first few rows of merged outcomes data
# Some ZCTAs have vaccination rates exceeding 100%, but this has to do with migration between ZCTAs
head(merged_outcomes)
```

    ##    zcta death_rate_sum hosp_rate_sum perc_1plus
    ## 1 10001           86.9         688.0     132.18
    ## 2 10002          440.9        1477.7      88.06
    ## 3 10003           92.7         444.7      78.48
    ## 4 10004            0.0         807.4     141.68
    ## 5 10005            0.0         353.8      97.51
    ## 6 10006            0.0         266.2     141.02

The data frame we construct below includes one row for each ZCTA, and
the following variables besides ZCTA and PUMA identification number:

-   `per_in_puma`: percent of the ZCTA in the associated PUMA
-   `per_of_puma`: percentage of the associated PUMA that is occupied by
    the specified ZCTA

``` r
# Clean the ZCTA to PUMA crosswalk
zcta_puma_clean <-
  zcta_puma_cross %>% 
  mutate(zcta = 
           as.character(zcta10)) %>%
  rename(
    puma = puma10
  ) %>% 
  select(zcta, puma, per_in_puma, per_of_puma)

# Observe first few rows of crosswalk between ZCTAs and PUMAS
head(zcta_puma_clean)
```

    ## # A tibble: 6 × 4
    ##   zcta   puma per_in_puma per_of_puma
    ##   <chr> <dbl>       <dbl>       <dbl>
    ## 1 10451  3710       0.482       0.141
    ## 2 10451  3708       0.459       0.151
    ## 3 10451  3705       0.059       0.017
    ## 4 10452  3708       0.952       0.515
    ## 5 10452  3707       0.048       0.027
    ## 6 10453  3707       0.984       0.574

``` r
# Remove unnecessary crosswalk data frame
rm(zcta_puma_cross)
```

Below, we calculate `weighted_death_rate`, `weighted_hosp_rate`, and
`weighted_vacc_rate` for each PUMA in the following way:

1.  The outcome rate from each ZCTA is multiplied by the percentage of
    the ZCTA that is in the corresponding PUMA (`per_in_puma`).

2.  The resulting product is then multiplied by `per_in_puma`, which
    represents the percentage of the total PUMA that the specified ZCTA
    occupies.

3.  The resulting weighted products are summed for each PUMA to obtain
    the total `weighted_death_rate`, `weighted_hosp_rate`, and
    `weighted_vacc_rate` for each PUMA.

![Zcta Puma Map](zcta_puma_map.jpg)

``` r
# Obtain final weighted outcomes data by PUMA
outcome_puma <- 
  merge(zcta_puma_clean, merged_outcomes, by = "zcta") %>% # Merge cleaned ZCTA/PUMA mappings with outcomes data
  mutate(weighted_death_rate =  
           death_rate_sum * per_in_puma * per_of_puma,
         weighted_hosp_rate =
           hosp_rate_sum * per_in_puma * per_of_puma,
         weighted_vacc_rate = 
           perc_1plus * per_in_puma * per_of_puma) %>% # Calculate weighted outcomes
  select(weighted_death_rate,
         weighted_hosp_rate,
         weighted_vacc_rate,
         puma) %>%
  group_by(puma) %>%
  summarise(puma_death_rate = sum(weighted_death_rate),
            puma_hosp_rate = sum(weighted_hosp_rate),
            puma_vacc_per = sum(weighted_vacc_rate)) # Group by and sum over PUMAs

# Confirm that merged result has correct number of PUMAS (55)
outcome_puma %>% pull(puma) %>% n_distinct()
```

    ## [1] 55

``` r
# Observe first several rows of merged data farme
head(outcome_puma)
```

    ## # A tibble: 6 × 4
    ##    puma puma_death_rate puma_hosp_rate puma_vacc_per
    ##   <dbl>           <dbl>          <dbl>         <dbl>
    ## 1  3701            398.          1065.          55.8
    ## 2  3702            269.          1178.          47.2
    ## 3  3703            405.          1304.          58.3
    ## 4  3704            209.           686.          29.4
    ## 5  3705            202.           826.          33.2
    ## 6  3706            181.           772.          33.0

``` r
# Write merged data to a new CSV
write_csv(outcome_puma, "./data/outcome_puma.csv")
```

This gives us the `hosp_rate` and `death_rate` per 100,000 people in
each PUMA and the percentage of individuals who are partially or fully
vaccinated in each PUMA.
