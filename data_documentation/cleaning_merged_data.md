Data Cleaning
================
Zak and Tanvir
11/23/2021

<!------------------------------------------------------------------------------
Preamble
------------------------------------------------------------------------------->
<!------------------------------------------------------------------------------
Overview
------------------------------------------------------------------------------->

## Unzipping the Census Data

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

## Merging the Outcome and Census Data

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

## Cleaning the Merged Data

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
      hcovany == 2 && hcovpriv == 2 ~ "Private",
      hcovany == 2 && hcovpriv == 1 ~ "Public"
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

# View first few rows of full cleaned data frame
cleaned_data %>% head()
```

    ##   puma perwt hhwt borough rent household_income on_foodstamps has_broadband
    ## 1 3701    18   25   Bronx    0           166870            No            No
    ## 2 3701    22   22   Bronx 1018           123192            No           Yes
    ## 3 3701    16   17   Bronx 2000           125000            No           Yes
    ## 4 3701     4    6   Bronx    0           140588           Yes           Yes
    ## 5 3701    20   19   Bronx 1058            52876            No           Yes
    ## 6 3701     9    9   Bronx    0            26101            No           Yes
    ##   family_size num_children    sex age                       race birthplace
    ## 1           2            0   Male  58                      White     Non-US
    ## 2           2            0   Male  52                      Black     Non-US
    ## 3           1            0 Female  59                      White         US
    ## 4           6            3   Male  64 Asian and Pacific Islander     Non-US
    ## 5           2            1   Male  44                      White     Non-US
    ## 6           1            0 Female  64                      White         US
    ##   US_citizen language            education         employment health_insurance
    ## 1        Yes  English Post-Graduate Degree           Employed          Private
    ## 2        Yes    Other         Some College           Employed          Private
    ## 3    Unknown  Spanish         Some College           Employed          Private
    ## 4        Yes  Chinese Post-Graduate Degree Not in labor force          Private
    ## 5        Yes  Spanish         Some College           Employed          Private
    ## 6    Unknown  English Post-Graduate Degree           Employed          Private
    ##   personal_income on_welfare poverty_threshold  work_transport puma_death_rate
    ## 1           83435         No             Above  Public Transit        398.2366
    ## 2          107920         No             Above Private Vehicle        398.2366
    ## 3          125000         No             Above  Public Transit        398.2366
    ## 4           11264         No             Above           Other        398.2366
    ## 5           32373         No             Above  Public Transit        398.2366
    ## 6           26101         No             Above Private Vehicle        398.2366
    ##   puma_hosp_rate puma_vacc_rate
    ## 1       1064.624       55.79213
    ## 2       1064.624       55.79213
    ## 3       1064.624       55.79213
    ## 4       1064.624       55.79213
    ## 5       1064.624       55.79213
    ## 6       1064.624       55.79213

``` r
# Examine structure of cleaned data frame to ensure proper variable types, distributions, and missingness
str(cleaned_data)
```

    ## 'data.frame':    356073 obs. of  26 variables:
    ##  $ puma             : Factor w/ 55 levels "3701","3702",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ perwt            : num  18 22 16 4 20 9 14 30 38 17 ...
    ##  $ hhwt             : num  25 22 17 6 19 9 20 30 38 17 ...
    ##  $ borough          : Factor w/ 5 levels "Bronx","Brooklyn",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ rent             : num  0 1018 2000 0 1058 ...
    ##  $ household_income : num  166870 123192 125000 140588 52876 ...
    ##  $ on_foodstamps    : Factor w/ 2 levels "No","Yes": 1 1 1 2 1 1 2 1 1 1 ...
    ##  $ has_broadband    : Factor w/ 2 levels "No","Yes": 1 2 2 2 2 2 2 2 1 2 ...
    ##  $ family_size      : num  2 2 1 6 2 1 6 1 4 3 ...
    ##  $ num_children     : num  0 0 0 3 1 0 3 0 0 0 ...
    ##  $ sex              : Factor w/ 2 levels "Female","Male": 2 2 1 2 2 1 1 1 1 2 ...
    ##  $ age              : num  58 52 59 64 44 64 43 62 26 71 ...
    ##  $ race             : Factor w/ 7 levels "2+ races","American Indian",..: 7 4 7 3 7 7 1 7 5 7 ...
    ##  $ birthplace       : Factor w/ 2 levels "Non-US","US": 1 1 2 1 1 2 1 2 1 2 ...
    ##  $ US_citizen       : Factor w/ 3 levels "No","Unknown",..: 3 3 2 3 3 2 3 2 1 2 ...
    ##  $ language         : Factor w/ 6 levels "Chinese","English",..: 2 4 5 1 5 2 5 2 5 2 ...
    ##  $ education        : Factor w/ 6 levels "Bachelor's Degree",..: 4 5 5 4 5 4 4 1 2 1 ...
    ##  $ employment       : Factor w/ 3 levels "Employed","Not in labor force",..: 1 1 1 2 1 1 1 1 1 2 ...
    ##  $ health_insurance : Factor w/ 2 levels "None","Private": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ personal_income  : num  83435 107920 125000 11264 32373 ...
    ##  $ on_welfare       : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ poverty_threshold: Factor w/ 2 levels "Above","Below": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ work_transport   : Factor w/ 6 levels "Bicycle","Other",..: 4 3 4 2 4 3 2 4 4 2 ...
    ##  $ puma_death_rate  : num  398 398 398 398 398 ...
    ##  $ puma_hosp_rate   : num  1065 1065 1065 1065 1065 ...
    ##  $ puma_vacc_rate   : num  55.8 55.8 55.8 55.8 55.8 ...

``` r
skimr::skim(cleaned_data)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | cleaned\_data |
| Number of rows                                   | 356073        |
| Number of columns                                | 26            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| factor                                           | 15            |
| numeric                                          | 11            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: factor**

| skim\_variable     | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                                      |
|:-------------------|-----------:|---------------:|:--------|----------:|:-------------------------------------------------|
| puma               |          0 |              1 | FALSE   |        55 | 411: 12034, 400: 11827, 410: 11001, 410: 10606   |
| borough            |          0 |              1 | FALSE   |         5 | Bro: 124597, Que: 109374, Bro: 52474, Man: 48946 |
| on\_foodstamps     |          0 |              1 | FALSE   |         2 | No: 283311, Yes: 72762                           |
| has\_broadband     |          0 |              1 | FALSE   |         2 | Yes: 318233, No: 37840                           |
| sex                |          0 |              1 | FALSE   |         2 | Fem: 188432, Mal: 167641                         |
| race               |          0 |              1 | FALSE   |         7 | Whi: 163114, Bla: 80607, Asi: 58555, His: 36657  |
| birthplace         |          0 |              1 | FALSE   |         2 | US: 218520, Non: 137553                          |
| US\_citizen        |          0 |              1 | FALSE   |         3 | Unk: 218521, Yes: 86011, No: 51541               |
| language           |          0 |              1 | FALSE   |         6 | Eng: 180942, Spa: 66114, Oth: 55472, Chi: 24192  |
| education          |          0 |              1 | FALSE   |         6 | Les: 105206, HS : 68568, Som: 65024, Bac: 63366  |
| employment         |          0 |              1 | FALSE   |         3 | Not: 173075, Emp: 171505, Une: 11493             |
| health\_insurance  |          0 |              1 | FALSE   |         2 | Pri: 332771, Non: 23302                          |
| on\_welfare        |          0 |              1 | FALSE   |         2 | No: 292544, Yes: 63529                           |
| poverty\_threshold |          0 |              1 | FALSE   |         2 | Abo: 289312, Bel: 66761                          |
| work\_transport    |          0 |              1 | FALSE   |         6 | Oth: 189949, Pub: 90957, Pri: 50763, Wal: 15132  |

**Variable type: numeric**

| skim\_variable    | n\_missing | complete\_rate |      mean |        sd |        p0 |      p25 |      p50 |       p75 |       p100 | hist  |
|:------------------|-----------:|---------------:|----------:|----------:|----------:|---------:|---------:|----------:|-----------:|:------|
| perwt             |          0 |           1.00 |     23.61 |     17.91 |      1.00 |    12.00 |    18.00 |     29.00 |     437.00 | ▇▁▁▁▁ |
| hhwt              |          0 |           1.00 |     22.65 |     16.77 |      1.00 |    12.00 |    18.00 |     28.00 |     363.00 | ▇▁▁▁▁ |
| rent              |          0 |           1.00 |    787.66 |    950.11 |      0.00 |     0.00 |   270.00 |   1460.00 |    4155.00 | ▇▃▂▁▁ |
| household\_income |      15169 |           0.96 | 119315.44 | 141054.16 | -14400.00 | 38688.00 | 82336.00 | 148917.00 | 3370815.00 | ▇▁▁▁▁ |
| family\_size      |          0 |           1.00 |      3.24 |      1.99 |      1.00 |     2.00 |     3.00 |      4.00 |      19.00 | ▇▂▁▁▁ |
| num\_children     |          0 |           1.00 |      0.52 |      0.98 |      0.00 |     0.00 |     0.00 |      1.00 |       9.00 | ▇▁▁▁▁ |
| age               |          0 |           1.00 |     39.89 |     22.73 |      0.00 |    22.00 |    38.00 |     58.00 |      95.00 | ▆▇▇▆▂ |
| personal\_income  |      55735 |           0.84 |  47056.34 |  81553.09 |  -7613.00 |  6109.00 | 23988.00 |  59351.00 | 1630109.00 | ▇▁▁▁▁ |
| puma\_death\_rate |          0 |           1.00 |    283.37 |    103.82 |     56.65 |   210.30 |   274.78 |    345.96 |     628.34 | ▂▇▆▂▁ |
| puma\_hosp\_rate  |          0 |           1.00 |   1017.75 |    318.06 |    394.54 |   772.39 |   987.23 |   1236.26 |    1707.95 | ▃▇▇▆▃ |
| puma\_vacc\_rate  |          0 |           1.00 |     57.09 |     15.51 |     28.98 |    46.76 |    56.37 |     69.29 |     103.77 | ▃▇▆▁▁ |

The Census also doesn’t reach everyone; it samples the population. As a
result, we need to weight our statistics by how many households look
like a particular household observed, or how many persons look like a
particular person observed. We also want to group by PUMA.

``` r
# Check total people in NYC by summing over person-weight column
cleaned_data %>% 
  select(perwt) %>% 
  sum()
```

    ## [1] 8408346

``` r
# Example data frame with weightings for summary stats over each PUMA
nyc_hh_summary = cleaned_data %>% 
  # Note: do we need to filter to one individual per household for household weightings?
  group_by(puma) %>%
  summarize(
    median_household_income = weighted.median(household_income, hhwt, na.rm = TRUE),
    perc_foodstamps = sum(hhwt[on_foodstamps == "Yes"]) * 100 / sum(hhwt),
    perc_broadband = sum(hhwt[has_broadband == "Yes"]) * 100 / sum(hhwt),
    perc_male = sum(perwt[sex == "Male"]) * 100 / sum(perwt),
    median_age = weighted.median(age, perwt, na.rm = TRUE),
    perc_white = sum(perwt[race == "White"]) * 100 / sum(perwt),
    perc_foreign_born = sum(perwt[birthplace == "Non-US"]) * 100 / sum(perwt),
    perc_citizen = sum(perwt[US_citizen == "Yes"]) * 100 / sum(perwt),
    perc_english = sum(perwt[language == "English"]) * 100 / sum(perwt),
    perc_college = sum(perwt[education %in% c("Some College", "Bachelor's Degree", "Post-Graduate Degree")]) * 100 / sum(perwt),
    perc_unemployed = sum(perwt[employment == "Unemployed"]) * 100 / sum(perwt),
    perc_insured = sum(perwt[health_insurance %in% c("Private", "Public")]) * 100 / sum(perwt),
    median_personal_income = weighted.median(personal_income, perwt, na.rm = TRUE),
    perc_welfare = sum(perwt[on_welfare == "Yes"]) * 100 / sum(perwt),
    perc_poverty = sum(perwt[poverty_threshold == "Below"]) * 100 / sum(perwt),
    perc_public_transit = sum(perwt[work_transport == "Public Transit"]) * 100 / sum(perwt),
    covid_hosp_rate = median(puma_hosp_rate),
    covid_vax_rate = median(puma_vacc_rate),
    covid_death_rate = median(puma_death_rate)
  )

# View first few rows of PUMA-summarized data frame
nyc_hh_summary %>% head()
```

    ## # A tibble: 6 × 20
    ##   puma  median_household_in… perc_foodstamps perc_broadband perc_male median_age
    ##   <fct>                <dbl>           <dbl>          <dbl>     <dbl>      <dbl>
    ## 1 3701                 69000            24.5           89.5      46.9         39
    ## 2 3702                 68610            28.2           82.9      45.1         36
    ## 3 3703                 77588            15.9           86.6      46.7         43
    ## 4 3704                 64662            23.9           81.6      47.7         37
    ## 5 3705                 36500            50.7           87.6      46.2         30
    ## 6 3706                 43490            46.2           82.2      48.3         32
    ## # … with 14 more variables: perc_white <dbl>, perc_foreign_born <dbl>,
    ## #   perc_citizen <dbl>, perc_english <dbl>, perc_college <dbl>,
    ## #   perc_unemployed <dbl>, perc_insured <dbl>, median_personal_income <dbl>,
    ## #   perc_welfare <dbl>, perc_poverty <dbl>, perc_public_transit <dbl>,
    ## #   covid_hosp_rate <dbl>, covid_vax_rate <dbl>, covid_death_rate <dbl>

``` r
# Examine structure of summarized data frame to ensure proper variable types, distributions, and missingness
str(nyc_hh_summary)
```

    ## tibble [55 × 20] (S3: tbl_df/tbl/data.frame)
    ##  $ puma                   : Factor w/ 55 levels "3701","3702",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ median_household_income: num [1:55] 69000 68610 77588 64662 36500 ...
    ##  $ perc_foodstamps        : num [1:55] 24.5 28.2 15.9 23.9 50.7 ...
    ##  $ perc_broadband         : num [1:55] 89.5 82.9 86.6 81.6 87.6 ...
    ##  $ perc_male              : num [1:55] 46.9 45.1 46.7 47.7 46.2 ...
    ##  $ median_age             : num [1:55] 39 36 43 37 30 32 30 32 34 31 ...
    ##  $ perc_white             : num [1:55] 46.5 14.1 41.2 32.7 15.5 ...
    ##  $ perc_foreign_born      : num [1:55] 34.5 43 22.7 37.9 33.5 ...
    ##  $ perc_citizen           : num [1:55] 20.3 27.9 16.9 23 16.5 ...
    ##  $ perc_english           : num [1:55] 41.7 66.5 58.4 41.1 33.3 ...
    ##  $ perc_college           : num [1:55] 48 39.8 45.1 40.3 28.4 ...
    ##  $ perc_unemployed        : num [1:55] 3.45 4.53 3.58 4.14 5.29 ...
    ##  $ perc_insured           : num [1:55] 93.9 92.6 94.1 92.3 91.4 ...
    ##  $ median_personal_income : num [1:55] 22357 20859 25030 20362 11264 ...
    ##  $ perc_welfare           : num [1:55] 20.2 22.1 16.7 20.2 29.2 ...
    ##  $ perc_poverty           : num [1:55] 22.7 18.6 16.3 19 39.3 ...
    ##  $ perc_public_transit    : num [1:55] 24.8 21.6 19.9 23.6 23 ...
    ##  $ covid_hosp_rate        : num [1:55] 1065 1178 1304 686 826 ...
    ##  $ covid_vax_rate         : num [1:55] 55.8 47.2 58.3 29.4 33.2 ...
    ##  $ covid_death_rate       : num [1:55] 398 269 405 209 202 ...

``` r
skimr::skim(nyc_hh_summary)
```

|                                                  |                  |
|:-------------------------------------------------|:-----------------|
| Name                                             | nyc\_hh\_summary |
| Number of rows                                   | 55               |
| Number of columns                                | 20               |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                  |
| Column type frequency:                           |                  |
| factor                                           | 1                |
| numeric                                          | 19               |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                  |
| Group variables                                  | None             |

Data summary

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                    |
|:---------------|-----------:|---------------:|:--------|----------:|:-------------------------------|
| puma           |          0 |              1 | FALSE   |        55 | 370: 1, 370: 1, 370: 1, 370: 1 |

**Variable type: numeric**

| skim\_variable            | n\_missing | complete\_rate |     mean |       sd |       p0 |      p25 |      p50 |      p75 |      p100 | hist  |
|:--------------------------|-----------:|---------------:|---------:|---------:|---------:|---------:|---------:|---------:|----------:|:------|
| median\_household\_income |          0 |              1 | 80444.70 | 34782.11 | 34316.00 | 61043.50 | 71221.00 | 92067.00 | 183448.00 | ▆▇▂▁▂ |
| perc\_foodstamps          |          0 |              1 |    22.51 |    12.72 |     2.44 |    13.14 |    20.96 |    29.90 |     52.88 | ▇▇▇▂▃ |
| perc\_broadband           |          0 |              1 |    88.99 |     3.58 |    79.08 |    87.60 |    89.50 |    92.01 |     94.36 | ▁▂▆▇▇ |
| perc\_male                |          0 |              1 |    47.69 |     1.91 |    43.29 |    46.41 |    47.45 |    48.84 |     52.56 | ▂▇▇▃▁ |
| median\_age               |          0 |              1 |    36.56 |     3.92 |    28.00 |    34.00 |    36.00 |    39.00 |     45.00 | ▂▃▇▃▂ |
| perc\_white               |          0 |              1 |    42.67 |    22.95 |     4.09 |    25.53 |    38.86 |    60.83 |     91.17 | ▆▇▅▃▅ |
| perc\_foreign\_born       |          0 |              1 |    38.15 |    12.01 |    16.96 |    28.50 |    37.89 |    46.64 |     64.53 | ▇▇▇▆▃ |
| perc\_citizen             |          0 |              1 |    22.18 |     7.76 |    10.26 |    15.75 |    20.59 |    28.23 |     38.26 | ▇▇▅▆▂ |
| perc\_english             |          0 |              1 |    48.27 |    17.71 |    13.67 |    34.77 |    47.49 |    66.10 |     75.27 | ▃▆▆▃▇ |
| perc\_college             |          0 |              1 |    46.24 |    13.67 |    26.43 |    39.64 |    45.13 |    51.34 |     82.98 | ▃▇▂▁▂ |
| perc\_unemployed          |          0 |              1 |     3.30 |     1.00 |     1.70 |     2.54 |     3.06 |     3.91 |      5.73 | ▅▇▆▃▂ |
| perc\_insured             |          0 |              1 |    92.45 |     3.16 |    82.31 |    91.28 |    92.58 |    94.08 |     97.76 | ▁▁▃▇▃ |
| median\_personal\_income  |          0 |              1 | 27291.41 | 15870.98 | 10359.00 | 18975.00 | 22001.43 | 28048.67 |  75537.00 | ▇▅▁▁▁ |
| perc\_welfare             |          0 |              1 |    19.75 |     4.76 |     8.86 |    17.03 |    19.62 |    21.82 |     33.36 | ▂▅▇▃▁ |
| perc\_poverty             |          0 |              1 |    19.66 |     9.09 |     6.67 |    12.24 |    17.51 |    25.99 |     43.25 | ▇▆▅▃▁ |
| perc\_public\_transit     |          0 |              1 |    26.71 |     7.03 |    11.39 |    22.07 |    26.30 |    32.67 |     38.03 | ▃▅▇▆▇ |
| covid\_hosp\_rate         |          0 |              1 |  1001.35 |   334.77 |   394.54 |   761.66 |   977.86 |  1253.81 |   1707.95 | ▃▇▇▆▃ |
| covid\_vax\_rate          |          0 |              1 |    56.70 |    15.87 |    28.98 |    46.70 |    56.37 |    68.63 |    103.77 | ▅▇▆▂▁ |
| covid\_death\_rate        |          0 |              1 |   279.71 |   112.21 |    56.65 |   206.96 |   268.83 |   345.51 |    628.34 | ▂▇▆▂▁ |
