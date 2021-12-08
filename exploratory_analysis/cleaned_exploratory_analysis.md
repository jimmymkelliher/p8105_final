Cleaned Exploratory Analysis
================
Zachary Katz
11/30/2021

# Data Preprocessing

The following code chunks are replicated from our
`cleaning_merged_data.Rmd` file to generate the data set from which
exploratory analyses can be conducted.

``` r
jimzip <- function(csv_file, path) {
  # create full path to csv file
  full_csv <- paste0(path, "/", csv_file)
  # append ".zip" to csv file
  zip_file <- paste0(full_csv, ".zip")
  # unzip file
  unzip(zip_file)
  # read csv
  data_extract <- read_csv(csv_file)
  # be sure to remove file once unzipped (it will live in working directory)
  on.exit(file.remove(csv_file))
  # output data
  data_extract
}

# Apply function to filtered census data CSV
census_data <- jimzip("census_filtered.csv", "./data")

# Observe first several rows of data frame
head(census_data) %>%  knitr::kable()
```

| multyear |  serial | hhwt |      cluster | countyfip | puma | strata | rent | hhincome | foodstmp | cihispeed | perwt | famsize | nchild | sex | age | race | hispan | bpl | ancestr1 | ancestr2 | citizen | language | hcovany | hcovpriv | hcovpub | educd | empstat | labforce |  occ |  ind |  inctot | ftotinc | incwage | incwelfr | poverty | occscore | pwpuma00 | tranwork |
|---------:|--------:|-----:|-------------:|----------:|-----:|-------:|-----:|---------:|---------:|----------:|------:|--------:|-------:|----:|----:|-----:|-------:|----:|---------:|---------:|--------:|---------:|--------:|---------:|--------:|------:|--------:|---------:|-----:|-----:|--------:|--------:|--------:|---------:|--------:|---------:|---------:|---------:|
|     2015 | 4100568 |   30 | 2.019041e+12 |        61 | 3806 | 380636 | 2806 |    93882 |        1 |        10 |    31 |       1 |      0 |   2 |  25 |    1 |      0 |  42 |       51 |      999 |       0 |       11 |       2 |        2 |       1 |   101 |       1 |        2 | 5940 | 7580 |   50718 |   50718 |   50718 |        0 |     381 |       25 |     3800 |       36 |
|     2015 | 4100568 |   30 | 2.019041e+12 |        61 | 3806 | 380636 | 2806 |    93882 |        1 |        10 |    49 |       1 |      0 |   2 |  27 |    1 |      0 |  39 |      148 |      153 |       0 |        1 |       2 |        2 |       1 |   114 |       1 |        2 |  726 | 8564 |   43164 |   43164 |   43164 |        0 |     325 |       24 |     3800 |       36 |
|     2015 | 4100570 |   20 | 2.019041e+12 |        47 | 4004 | 400436 |  453 |    16834 |        1 |        10 |    20 |       4 |      2 |   2 |  45 |    4 |      0 | 500 |      706 |      999 |       3 |       43 |       2 |        1 |       2 |    63 |       1 |        2 | 4850 | 4390 |    5503 |   16834 |    5503 |        0 |      62 |       24 |     3200 |       36 |
|     2015 | 4100570 |   20 | 2.019041e+12 |        47 | 4004 | 400436 |  453 |    16834 |        1 |        10 |    20 |       4 |      2 |   1 |  49 |    4 |      0 | 500 |      706 |      999 |       3 |       43 |       2 |        1 |       2 |    65 |       1 |        2 | 4030 | 8680 |   11331 |   16834 |   11331 |        0 |      62 |       16 |     3800 |       36 |
|     2015 | 4100570 |   20 | 2.019041e+12 |        47 | 4004 | 400436 |  453 |    16834 |        1 |        10 |    13 |       4 |      0 |   1 |  21 |    4 |      0 | 500 |      706 |      999 |       2 |       43 |       2 |        1 |       2 |    63 |       3 |        1 | 2545 | 7860 |       0 |   16834 |       0 |        0 |      62 |       33 |        0 |        0 |
|     2015 | 4100570 |   20 | 2.019041e+12 |        47 | 4004 | 400436 |  453 |    16834 |        1 |        10 |    18 |       4 |      0 |   2 |  10 |    4 |      0 |  36 |      706 |      999 |       0 |       43 |       2 |        1 |       2 |    17 |       0 |        0 |    0 |    0 | 9999999 |   16834 |  999999 |    99999 |      62 |        0 |        0 |        0 |

``` r
# Read in PUMA outcomes data
health_data <-
  read_csv("./data/outcome_puma.csv")

# Merge census data with PUMA outcomes data
merged_data <- merge(census_data, health_data, by = "puma")

# Deprecate census data alone
rm(census_data)

# Observe first several rows of merged census and health outcomes data
head(merged_data) %>% knitr::kable()
```

| puma | multyear |  serial | hhwt |      cluster | countyfip | strata | rent | hhincome | foodstmp | cihispeed | perwt | famsize | nchild | sex | age | race | hispan | bpl | ancestr1 | ancestr2 | citizen | language | hcovany | hcovpriv | hcovpub | educd | empstat | labforce |  occ |  ind | inctot | ftotinc | incwage | incwelfr | poverty | occscore | pwpuma00 | tranwork | puma\_death\_rate | puma\_hosp\_rate | puma\_vacc\_per |
|-----:|---------:|--------:|-----:|-------------:|----------:|-------:|-----:|---------:|---------:|----------:|------:|--------:|-------:|----:|----:|-----:|-------:|----:|---------:|---------:|--------:|---------:|--------:|---------:|--------:|------:|--------:|---------:|-----:|-----:|-------:|--------:|--------:|---------:|--------:|---------:|---------:|---------:|------------------:|-----------------:|----------------:|
| 3701 |     2017 | 4301368 |   25 | 2.019043e+12 |         5 | 370136 |    0 |   166870 |        1 |        20 |    18 |       2 |      0 |   1 |  58 |    1 |      0 | 465 |      999 |      999 |       2 |        1 |       2 |        2 |       1 |   116 |       1 |        2 | 2100 | 7270 |  83435 |  166870 |   83435 |        0 |     501 |       62 |     3800 |       36 |          398.2366 |         1064.624 |        55.79213 |
| 3701 |     2018 | 4429112 |   22 | 2.019044e+12 |         5 | 370136 | 1018 |   123192 |        1 |        10 |    22 |       2 |      0 |   1 |  52 |    2 |      0 | 600 |      522 |      999 |       2 |       60 |       2 |        2 |       1 |    81 |       1 |        2 | 9142 | 6190 | 107920 |  123192 |   85522 |        0 |     501 |       22 |     3800 |       10 |          398.2366 |         1064.624 |        55.79213 |
| 3701 |     2019 | 4496321 |   17 | 2.019045e+12 |         5 | 370136 | 2000 |   125000 |        1 |        10 |    16 |       1 |      0 |   2 |  59 |    1 |      2 | 110 |      200 |      261 |       0 |       12 |       2 |        2 |       1 |    81 |       1 |        2 | 5740 | 6870 | 125000 |  125000 |  125000 |        0 |     501 |       22 |     3800 |       31 |          398.2366 |         1064.624 |        55.79213 |
| 3701 |     2017 | 4272796 |    6 | 2.019043e+12 |         5 | 370136 |    0 |   140588 |        2 |        10 |     4 |       6 |      3 |   1 |  64 |    4 |      0 | 500 |      706 |      999 |       2 |       43 |       2 |        1 |       2 |   114 |       3 |        1 | 1050 | 8191 |  11264 |  140588 |       0 |        0 |     388 |       33 |        0 |        0 |          398.2366 |         1064.624 |        55.79213 |
| 3701 |     2015 | 4184953 |   19 | 2.019042e+12 |         5 | 370136 | 1058 |    52876 |        1 |        10 |    20 |       2 |      1 |   1 |  44 |    1 |      4 | 300 |      237 |      999 |       2 |       12 |       2 |        1 |       2 |    81 |       1 |        2 | 4220 | 7072 |  32373 |   52876 |   32373 |        0 |     309 |       19 |     3800 |       36 |          398.2366 |         1064.624 |        55.79213 |
| 3701 |     2016 | 4255659 |    9 | 2.019043e+12 |         5 | 370136 |    0 |    26101 |        1 |        10 |     9 |       1 |      0 |   2 |  64 |    1 |      0 |  36 |       50 |       32 |       0 |        1 |       2 |        2 |       1 |   114 |       1 |        2 | 2360 | 7890 |  26101 |   26101 |   12784 |        0 |     198 |       20 |     3100 |       10 |          398.2366 |         1064.624 |        55.79213 |

``` r
# Clean the merged census and outcomes data
# Each row represents one 
cleaned_data = 
  merged_data %>% 
  # Remove variables less useful for analysis or redundant (high probability of collinearity with remaining variables)
  select(-serial, -cluster, -strata, -multyear, -ancestr1, -ancestr2, -labforce, -occ, -ind, -incwage, -occscore, -pwpuma00, -ftotinc, -hcovpub) %>% 
  # Remove duplicate rows, if any
  distinct() %>% 
  # Rename variables
  rename(
    borough = countyfip,
    has_broadband = cihispeed,
    birthplace = bpl,
    education = educd,
    employment = empstat,
    personal_income = inctot,
    work_transport = tranwork,
    household_income = hhincome,
    on_foodstamps = foodstmp,
    family_size = famsize,
    num_children = nchild,
    US_citizen = citizen,
    puma_vacc_rate = puma_vacc_per,
    on_welfare = incwelfr,
    poverty_threshold = poverty
  ) %>% 
  # Recode variables according to data dictionary
  mutate(
    # Researched mapping for county
    borough = recode(
      borough,
      "5" = "Bronx",
      "47" = "Brooklyn",
      "61" = "Manhattan",
      "81" = "Queens",
      "85" = "Staten Island"
    ),
    rent = ifelse(
      rent == 9999, 0,
      rent
    ),
    household_income = ifelse(
      household_income %in% c(9999998,9999999), NA,
      household_income
    ),
    on_foodstamps = recode(
      on_foodstamps,
      "1" = "No",
      "2" = "Yes"
    ),
    has_broadband = case_when(
      has_broadband == "20" ~ "No",
      has_broadband != "20" ~ "Yes"
    ),
    sex = recode(
      sex,
      "1" = "Male",
      "2" = "Female"
    ),
    # Collapse Hispanic observation into race observation
    race = case_when(
      race == "1" ~ "White",
      race == "2" ~ "Black",
      race == "3" ~ "American Indian",
      race %in% c(4,5,6) ~ "Asian and Pacific Islander",
      race == 7 & hispan %in% c(1,2,3,4) ~ "Hispanic",
      race == 7 & hispan %in% c(0,9) ~ "Other",
      race %in% c(8,9) ~ "2+ races"
    ),
    birthplace = case_when(
      birthplace %in% 1:120 ~"US",
      birthplace %in% 121:950 ~ "Non-US",
      birthplace == 999 ~"Unknown"
    ),
    US_citizen = case_when(
      US_citizen %in% c(1,2) ~ "Yes",
      US_citizen %in% 3:8 ~"No",
      US_citizen %in% c(0,9) ~ "Unknown"
    ),
    # Chose languages based on highest frequency observed
    language = case_when(
      language == "1" ~ "English",
      language == "12" ~ "Spanish",
      language == "43" ~ "Chinese",
      language == "0" ~ "Unknown",
      language == "31" ~ "Hindi",
      !language %in% c(1,12,43,0,31) ~ "Other"
    ),
    # Collapse multiple health insurance variables into single variable
    health_insurance = case_when(
      hcovany == 1 ~ "None",
      hcovany == 2 & hcovpriv == 2 ~ "Private",
      hcovany == 2 & hcovpriv == 1 ~ "Public"
    ),
    education = case_when(
      education %in% 2:61 ~ "Less Than HS Graduate",
      education %in% 62:64 ~ "HS Graduate",
      education %in% 65:100 ~ "Some College",
      education %in% 110:113 ~ "Some College",
      education == 101 ~ "Bachelor's Degree",
      education %in% 114:116 ~ "Post-Graduate Degree",
      education %in% c(0,1,999) ~ "Unknown"
    ),
    employment = case_when(
      employment %in% c(0,3) ~ "Not in labor force",
      employment == 1 ~ "Employed",
      employment == 2 ~ "Unemployed"
    ),
    personal_income = ifelse(
      personal_income %in% c(9999998,9999999), NA,
      personal_income
    ),
    household_income = ifelse(
      household_income %in% c(9999998,9999999), NA,
      household_income
    ),
    on_welfare = case_when(
      on_welfare > 0 ~ "Yes",
      on_welfare == 0 ~ "No"
    ), 
    poverty_threshold = case_when(
      poverty_threshold >= 100 ~ "Above",
      poverty_threshold < 100 ~ "Below"
    ),
    work_transport = case_when(
      work_transport %in% c(31:37, 39) ~ "Public Transit",
      work_transport %in% c(10:20, 38) ~ "Private Vehicle",
      work_transport == 50 ~ "Bicycle",
      work_transport == 60 ~ "Walking",
      work_transport == 80 ~ "Worked From Home",
      work_transport %in% c(0, 70) ~ "Other"
    )
  ) %>% 
  # Eliminate columns no longer needed after transformation
  select(-hispan, -hcovany, -hcovpriv) %>% 
  # Relocate new columns
  relocate(health_insurance, .before = personal_income) %>% 
  relocate(poverty_threshold, .before = work_transport) %>% 
  relocate(on_welfare, .before = poverty_threshold) %>% 
  relocate(perwt, .before = hhwt) %>% 
  # Create factor variables where applicable
  mutate(across(.cols = c(puma, borough, on_foodstamps, has_broadband, sex, race, birthplace, US_citizen, language, health_insurance, education, employment, on_welfare, poverty_threshold, work_transport), as.factor))
```

# Exploratory Analysis

To accommodate our exploratory analysis, we develop a few particular
kinds of data frames from the primary cleaned data frame, as well as
functions to run quick, replicable analysis upon certain sets of
variables from particular data frames.

## Overview of Outcome Variables

### Outcomes by PUMA

How are key outcomes distributed across PUMAs?

<img src="cleaned_exploratory_analysis_files/figure-gfm/outcomes all PUMAs-1.png" width="90%" />

Which PUMAs have the worst and best outcomes?

<img src="cleaned_exploratory_analysis_files/figure-gfm/worst PUMA outcomes-1.png" width="90%" />

<img src="cleaned_exploratory_analysis_files/figure-gfm/best PUMA outcomes-1.png" width="90%" />

What about with PUMAs in the same order?

<img src="cleaned_exploratory_analysis_files/figure-gfm/outcomes sorted by puma-1.png" width="90%" />

How do key outcomes associate with each other at the PUMA level?

<img src="cleaned_exploratory_analysis_files/figure-gfm/key outcome associations-1.png" width="90%" />

<img src="cleaned_exploratory_analysis_files/figure-gfm/outcome correlations-1.png" width="90%" /><img src="cleaned_exploratory_analysis_files/figure-gfm/outcome correlations-2.png" width="90%" />

### Outcomes by Borough

Within each borough, how are PUMAs distributed on each key outcome?

<img src="cleaned_exploratory_analysis_files/figure-gfm/hospitalizations by borough-1.png" width="90%" />

<img src="cleaned_exploratory_analysis_files/figure-gfm/deaths by borough-1.png" width="90%" />

<img src="cleaned_exploratory_analysis_files/figure-gfm/vax by borough-1.png" width="90%" />

What proportion of PUMAs in a given borough were above or below the
citywide median on a given outcome?

| Borough       | Total PUMAs | % Above Hosp Median | % Above Death Median | % Above Vax Median |
|:--------------|------------:|--------------------:|---------------------:|-------------------:|
| Bronx         |          10 |                70.0 |                 50.0 |               20.0 |
| Brooklyn      |          18 |                22.2 |                 38.9 |               16.7 |
| Manhattan     |          10 |                30.0 |                 30.0 |               80.0 |
| Queens        |          14 |                85.7 |                 78.6 |               85.7 |
| Staten Island |           3 |                33.3 |                 33.3 |               66.7 |

% of PUMAs in Each Borough Above Citywide PUMA Median

### Outcomes by Demographic Combos

Can we determine which age/sex/race combos perform best and worst on
each outcome?

``` r
# Lowest hospitalization rates
race_age_sex %>% 
  filter(outcome == "hosp_rate") %>% 
  mutate(
    outcome_rate = outcome_rate / 100
  ) %>% 
  arrange(outcome_rate) %>% 
  select(race, age_class, sex, outcome, outcome_rate) %>% 
  head() %>% 
  knitr::kable()
```

| race  | age\_class | sex    | outcome    | outcome\_rate |
|:------|:-----------|:-------|:-----------|--------------:|
| White | 21-30      | Female | hosp\_rate |      8.761509 |
| White | 31-40      | Male   | hosp\_rate |      8.859089 |
| White | 31-40      | Female | hosp\_rate |      8.908691 |
| White | 21-30      | Male   | hosp\_rate |      8.963492 |
| White | 41-50      | Male   | hosp\_rate |      9.275372 |
| White | 41-50      | Female | hosp\_rate |      9.347574 |

``` r
# Highest hospitalization rates
race_age_sex %>% 
  filter(outcome == "hosp_rate") %>% 
  mutate(
    outcome_rate = outcome_rate / 100
  ) %>% 
  arrange(desc(outcome_rate)) %>% 
  select(race, age_class, sex, outcome, outcome_rate) %>% 
  head() %>% 
  knitr::kable()
```

| race                       | age\_class | sex    | outcome    | outcome\_rate |
|:---------------------------|:-----------|:-------|:-----------|--------------:|
| American Indian            | 81-90      | Male   | hosp\_rate |      12.72494 |
| Other                      | 61-70      | Male   | hosp\_rate |      12.16952 |
| Other                      | 11-20      | Male   | hosp\_rate |      12.11757 |
| Other                      | 41-50      | Male   | hosp\_rate |      12.00270 |
| Other                      | 61-70      | Female | hosp\_rate |      11.85198 |
| Asian and Pacific Islander | 91-100     | Male   | hosp\_rate |      11.73874 |

``` r
# Lowest death rates
race_age_sex %>% 
  filter(outcome == "death_rate") %>% 
  mutate(
    outcome_rate = outcome_rate / 100
  ) %>% 
  arrange(outcome_rate) %>% 
  select(race, age_class, sex, outcome, outcome_rate) %>% 
  head() %>% 
  knitr::kable()
```

| race  | age\_class | sex    | outcome     | outcome\_rate |
|:------|:-----------|:-------|:------------|--------------:|
| White | 21-30      | Female | death\_rate |      2.376629 |
| White | 31-40      | Male   | death\_rate |      2.424339 |
| White | 21-30      | Male   | death\_rate |      2.435737 |
| White | 31-40      | Female | death\_rate |      2.455473 |
| Other | 81-90      | Male   | death\_rate |      2.541018 |
| White | 41-50      | Male   | death\_rate |      2.564677 |

``` r
# Highest death rates
race_age_sex %>% 
  filter(outcome == "death_rate") %>% 
  mutate(
    outcome_rate = outcome_rate / 100
  ) %>% 
  arrange(desc(outcome_rate)) %>% 
  select(race, age_class, sex, outcome, outcome_rate) %>% 
  head() %>% 
  knitr::kable()
```

| race                       | age\_class | sex  | outcome     | outcome\_rate |
|:---------------------------|:-----------|:-----|:------------|--------------:|
| 2+ races                   | 91-100     | Male | death\_rate |      3.525028 |
| Asian and Pacific Islander | 91-100     | Male | death\_rate |      3.386789 |
| American Indian            | 81-90      | Male | death\_rate |      3.246158 |
| Other                      | 11-20      | Male | death\_rate |      3.228600 |
| Other                      | 41-50      | Male | death\_rate |      3.220782 |
| Other                      | 71-80      | Male | death\_rate |      3.202419 |

``` r
# Lowest vax rates
race_age_sex %>% 
  filter(outcome == "vax_rate") %>% 
  mutate(
    outcome_rate = outcome_rate
  ) %>% 
  arrange(outcome_rate) %>% 
  select(race, age_class, sex, outcome, outcome_rate) %>% 
  head() %>% 
  knitr::kable()
```

| race            | age\_class | sex    | outcome   | outcome\_rate |
|:----------------|:-----------|:-------|:----------|--------------:|
| American Indian | 71-80      | Male   | vax\_rate |      49.32433 |
| Black           | 11-20      | Female | vax\_rate |      49.35445 |
| Black           | &lt;11     | Male   | vax\_rate |      49.47578 |
| Black           | 11-20      | Male   | vax\_rate |      49.48017 |
| Black           | 31-40      | Female | vax\_rate |      49.49496 |
| Black           | &lt;11     | Female | vax\_rate |      49.51926 |

``` r
# Highest vax rates
race_age_sex %>% 
  filter(outcome == "vax_rate") %>% 
  mutate(
    outcome_rate = outcome_rate
  ) %>% 
  arrange(desc(outcome_rate)) %>% 
  select(race, age_class, sex, outcome, outcome_rate) %>% 
  head() %>% 
  knitr::kable()
```

| race                       | age\_class | sex    | outcome   | outcome\_rate |
|:---------------------------|:-----------|:-------|:----------|--------------:|
| Asian and Pacific Islander | 91-100     | Female | vax\_rate |      66.13596 |
| Asian and Pacific Islander | 71-80      | Female | vax\_rate |      65.89982 |
| Asian and Pacific Islander | 71-80      | Male   | vax\_rate |      65.73426 |
| Asian and Pacific Islander | 31-40      | Male   | vax\_rate |      65.61553 |
| Asian and Pacific Islander | 31-40      | Female | vax\_rate |      65.57662 |
| 2+ races                   | 81-90      | Male   | vax\_rate |      65.47910 |

## Associations between Predictors and Outcomes

How do key predictors correlate with key outcomes at the PUMA level?

<img src="cleaned_exploratory_analysis_files/figure-gfm/correlations predictors vs outcomes-1.png" width="90%" />

For each of the four most correlated variables (excluding obvious
redundancies) with each outcome, can we explore more precisely the
relationship between outcome and predictor?

First, across PUMAs:

<img src="cleaned_exploratory_analysis_files/figure-gfm/PUMA hospitalization rate vs predictor-1.png" width="90%" />

<img src="cleaned_exploratory_analysis_files/figure-gfm/PUMA death rate vs predictor-1.png" width="90%" />

<img src="cleaned_exploratory_analysis_files/figure-gfm/PUMA vax rate vs predictor-1.png" width="90%" />

Then, across all interviews, for key demographic predictors:

<img src="cleaned_exploratory_analysis_files/figure-gfm/hospitalization rate by demo-1.png" width="90%" />

<img src="cleaned_exploratory_analysis_files/figure-gfm/death rate by demo-1.png" width="90%" />

<img src="cleaned_exploratory_analysis_files/figure-gfm/vax rate by demo-1.png" width="90%" />

And finally across all interviews for key socioeconomic predictors:

<img src="cleaned_exploratory_analysis_files/figure-gfm/hosp rate by ses-1.png" width="90%" />

<img src="cleaned_exploratory_analysis_files/figure-gfm/death rate by ses-1.png" width="90%" />

<img src="cleaned_exploratory_analysis_files/figure-gfm/vax rate by ses-1.png" width="90%" />

## Associations between Predictors and Outcomes by Borough

What kinds of disparities occur within each borough on key outcome rates
across levels of a predictor?

First, hospitalizations:

<img src="cleaned_exploratory_analysis_files/figure-gfm/hospitalization borough disparities-1.png" width="90%" /><img src="cleaned_exploratory_analysis_files/figure-gfm/hospitalization borough disparities-2.png" width="90%" /><img src="cleaned_exploratory_analysis_files/figure-gfm/hospitalization borough disparities-3.png" width="90%" />

Then, deaths:

<img src="cleaned_exploratory_analysis_files/figure-gfm/death borough disparities-1.png" width="90%" /><img src="cleaned_exploratory_analysis_files/figure-gfm/death borough disparities-2.png" width="90%" /><img src="cleaned_exploratory_analysis_files/figure-gfm/death borough disparities-3.png" width="90%" />

And finally, vaccinations:

<img src="cleaned_exploratory_analysis_files/figure-gfm/vax borough disparities-1.png" width="90%" /><img src="cleaned_exploratory_analysis_files/figure-gfm/vax borough disparities-2.png" width="90%" /><img src="cleaned_exploratory_analysis_files/figure-gfm/vax borough disparities-3.png" width="90%" />

A note for later – we could try this for key SES indicators as well,
such as:

<img src="cleaned_exploratory_analysis_files/figure-gfm/vax borough ses disparity-1.png" width="90%" />

And finally, we can visualize outcomes on a given predictor across
boroughs in the following way – for simplicity’s sake, only demographic
variables and outcomes included here.

<img src="cleaned_exploratory_analysis_files/figure-gfm/borough predictor heatmap hosp-1.png" width="90%" />

<img src="cleaned_exploratory_analysis_files/figure-gfm/borough predictor heatmap death-1.png" width="90%" />

<img src="cleaned_exploratory_analysis_files/figure-gfm/borough predictor heatmap vax-1.png" width="90%" />
