P8105: Data Science I
================
Constructing Unbiased Group Means<br>Jimmy Kelliher (UNI: jmk2303)

-   [Overview and Setting Up the
    Data](#overview-and-setting-up-the-data)
    -   [Unzipping the Census Data](#unzipping-the-census-data)
    -   [Merging the Outcome Data](#merging-the-outcome-data)
    -   [Cleaning the Data](#cleaning-the-data)
-   [Motivation](#motivation)
    -   [The Threat of Overfitting](#the-threat-of-overfitting)
    -   [The Threat of Reverse
        Causality](#the-threat-of-reverse-causality)
-   [Aggregating the Input Data](#aggregating-the-input-data)
    -   [Helper Functions](#helper-functions)
    -   [The Croon-Veldhoven Procedure](#the-croon-veldhoven-procedure)

<!------------------------------------------------------------------------------
Preamble
------------------------------------------------------------------------------->

# Overview and Setting Up the Data

In this script, we constructed adjusted PUMA-level means of our data to
address the potential threat of bias. We begin by providing motivation
as to why the interview-level census data should be aggregated to the
level of PUMAs, and we conduct the aggregation in a manner that reduces
bias in a subsequent regression.

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
```

## Merging the Outcome Data

``` r
# Read in PUMA outcomes data
health_data <-
  read_csv("./data/outcome_puma.csv")

# Merge census data with PUMA outcomes data
merged_data <- merge(census_data, health_data, by = "puma")

# Deprecate census data alone
rm(census_data)
```

## Cleaning the Data

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
  mutate(across(.cols = c(puma, borough, on_foodstamps, has_broadband, sex, race, birthplace, US_citizen, language, health_insurance, education, employment, on_welfare, poverty_threshold, work_transport), as.factor)) %>% 
  # Ensure consistent use of percentages
  mutate(
    puma_death_rate = puma_death_rate / 100,
    puma_hosp_rate = puma_hosp_rate / 100
  )
```

# Motivation

## The Threat of Overfitting

When considering a regression to predict health outcomes, our primary
limitation is that outcomes are recorded at the PUMA-level. This is
unfortunate, but without access to anonymized health records, this is
the best we can do, so let’s work with it!

To that end, we must be careful to avoid over-fitting our model. For
example, suppose we have a two-state model with states *s*∈ {0, 1}, and
for all individuals *i*∈ {1, …, *n*}, we observe binary outcome
*y*<sub>*i*, *s*</sub> = *s*. Further suppose we run the regression

        *y*<sub>*i*, *s*</sub> = *α* + *β*
*I*<sub>{*s* = 1}</sub> + *ε*<sub>*i*, *s*</sub>.

Trivially, we will find that *α̂* = 0 and *β̂* = 1. Moreover, we will find
that *R*<sup>2</sup> = 1, but this is deceptive! While the model enjoys
a perfect linear fit, it does not say anything meaningful: “If we know
the value for each group, we can predict the value for each group!”
Below is an example of this in the context of our data.

``` r
# example of overfitting
example_overfit <- lm(
    puma_hosp_rate ~ puma
  , data = cleaned_data
)

# using puma to predict puma-level data generates a bad model
summary(example_overfit) %>%
  broom::glance() %>%
  knitr::kable()
```

| r.squared | adj.r.squared | sigma |  statistic | p.value |  df | df.residual |   nobs |
|----------:|--------------:|------:|-----------:|--------:|----:|------------:|-------:|
|         1 |             1 |     0 | 7.4486e+25 |       0 |  54 |      356018 | 356073 |

## The Threat of Reverse Causality

Thus, for any independent variable *x* that we consider to predict
health outcome *y*, we must ensure that *x* exhibits sufficient
heterogeneity within PUMAs. If not, *x* is just a predictor of where
someone lives, and not of their probability of observing health outcome
*y*.

``` r
# vaccination rate regressed on income
example <-
  lm(
    puma_vacc_rate ~ personal_income
  , data = cleaned_data
  , weights = perwt
  ) %>%
  summary()

# income regressed on PUMA
example_validate <-
  lm(
    personal_income ~ puma
  , data = cleaned_data
  , weights = perwt
  ) %>%
  summary()

# coefficients of vaccination rate regressed on income
example %>%broom::tidy() %>% knitr::kable()
```

| term             |   estimate | std.error |  statistic | p.value |
|:-----------------|-----------:|----------:|-----------:|--------:|
| (Intercept)      | 56.6748780 | 0.0330904 | 1712.72710 |       0 |
| personal\_income |  0.0000325 | 0.0000003 |   94.72168 |       0 |

``` r
# performance statistics
example %>%broom::glance() %>% knitr::kable()
```

| r.squared | adj.r.squared |    sigma | statistic | p.value |  df | df.residual |   nobs |
|----------:|--------------:|---------:|----------:|--------:|----:|------------:|-------:|
| 0.0290073 |     0.0290041 | 75.77679 |  8972.196 |       0 |   1 |      300336 | 300338 |

``` r
# validation via income regressed on PUMA
example_validate %>%broom::glance() %>% knitr::kable()
```

| r.squared | adj.r.squared |    sigma | statistic | p.value |  df | df.residual |   nobs |
|----------:|--------------:|---------:|----------:|--------:|----:|------------:|-------:|
| 0.1242383 |     0.1240808 | 376668.4 |  788.8721 |       0 |  54 |      300283 | 300338 |

While the PUMA-level vaccination rate and personal income are indeed
linearly associated, we also see that PUMA explains personal income.
Given this potential reverse causality, it is vital to aggregate our
census data before running a regression. To do this, we follow Croon and
Veldhoven (2007) to construct adjusted group means that will reduce the
bias in our aggregated regression.

# Aggregating the Input Data

## Helper Functions

``` r
# function that computes a sum of weights
wsum  <- function(w) {
  sum(w, na.rm = TRUE)
}

# function that returns weighted mean
wmean <- function(v, w = rep(1, length(v)), remove_na = TRUE) {
  sum(w * v, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

# function that returns (possibly weighted) product of vector with its transpose
vvt <- function(v) {
  u <- as.vector(v)
  u %*% t(u)
}

# function that applies vvt to rows of a data frame
vvt_map <- function(df) {
  pmap(df, ~ vvt(c(...)))
}

# function that applies vvt to difference of data frames, then adds matrices
pbyp <- function(df) {
  Reduce("+", vvt_map(df))
}
```

## The Croon-Veldhoven Procedure

``` r
# construct dataset of indices (pumas) and inputs (predictors)
inputs_plus_index <-
  cleaned_data %>%
  # remove extraneous variables
  select(-c(
      # outputs are already observed at the group level
      puma_death_rate, puma_hosp_rate, puma_vacc_rate
      # miscellaneous
    , hhwt, borough
      # US_citizen is precisely the same as birthplace, and hence collinear
    , US_citizen
  )) %>%
  # replace missing data with weighted group means
  mutate(
    personal_income = replace_na(
        personal_income
      , wmean(personal_income, perwt)
    )
    , household_income = replace_na(
        household_income
      , wmean(household_income, perwt)
    )
  )

# extract inputs
inputs <- inputs_plus_index %>% select(-c(puma, perwt))
# feed inputs to model.matrix to convert factors to dummies
inputs <- as_tibble(model.matrix(~ ., inputs)[ , -1])

# redefine inputs_plus_index to include model.matrix output
inputs_plus_index <-
  inputs_plus_index %>%
  select(puma) %>%
  bind_cols(inputs)

# compute relevant statistics for ANOVA
w  <- pull(cleaned_data, perwt) / sum(pull(cleaned_data, perwt)) # normalized weights
n  <- nrow(inputs_plus_index)                                    # NYC population
g  <- length(unique(pull(inputs_plus_index, puma)))              # number of PUMAs
ng <-                                                            # PUMA populations
  inputs_plus_index %>%
  group_by(puma) %>%
  summarize(ng = n()) %>%
  pull(ng)

# identify citywide means
means_observed  <-
  inputs %>%
  mutate(across(
      everything()
    , ~ replace(.x, TRUE, wmean(.x, w))
  ))

# identify observed puma-level means
group_means_observed <-
  inputs_plus_index %>%
  mutate(perwt = pull(cleaned_data, perwt)) %>%
  group_by(puma) %>%
  # use helper function to apply weighting across numeric columns
  mutate(across(where(is.numeric), ~ replace(.x, TRUE, wmean(.x, perwt))
  )) %>%
  ungroup() %>%
  select(-c(puma, perwt))

# the above matrices have the same dimensionality as our inputs matrix
# we additionally require the collapsed versions of these matrices
means_vector <- means_observed %>% distinct()
group_means_vector <- group_means_observed %>% distinct()

# compute ANOVA statistics
msa <- pbyp(w^(0.5) * (group_means_observed - means_observed)) # between SSE (weighted)
mse <- pbyp(w^(0.5) * (group_means_observed - inputs))         # within SSE (weighted)
between_error <- (msa - mse) / n                               # between variation
within_error  <-  mse                                          # within variation

# define a function that applies croon-veldhoven weighting
jimcol <- function(k, x) {
  # weight matrix 
  W <- solve(between_error + within_error / k) %*% between_error
  # convex combination of citywide mean and observed mean, based on W
  u <- unlist(means_vector) %*% (diag(ncol(inputs)) - W) + unlist(x) %*% W
  # output adjusted mean vector
  u
}

# apply weighting function to group means to consruct unbiased group means
adj_group_means <-
  inputs_plus_index %>%
  mutate(perwt = pull(cleaned_data, perwt)) %>%
  select(puma, perwt) %>%
  group_by(puma) %>%
  summarize(group_pop = sum(perwt)) %>%
  cbind(group_means_vector) %>%
  nest(group_means_vector = -c(puma, group_pop)) %>%
  # apply function to estimate group means for each puma
  mutate(adj_group_means = map2(group_pop, group_means_vector, jimcol)) %>%
  unnest(adj_group_means) %>%
  pull(adj_group_means) %>%
  as_tibble()

# name matrix columns as those of observed inputs
names(adj_group_means) <- names(group_means_vector)

# append indices and population weights to unbiased group mean data
unbiased_group_means <-
  inputs_plus_index %>%
  mutate(perwt = pull(cleaned_data, perwt)) %>%
  select(puma, perwt) %>%
  group_by(puma) %>%
  summarize(group_pop = sum(perwt)) %>%
  cbind(adj_group_means) %>%
  ungroup()  %>%
  janitor::clean_names()

# output result
head(unbiased_group_means) %>% knitr::kable()
```

| puma | group\_pop |      rent | household\_income | on\_foodstamps\_yes | has\_broadband\_yes | family\_size | num\_children | sex\_male |      age | race\_american\_indian | race\_asian\_and\_pacific\_islander | race\_black | race\_hispanic | race\_other | race\_white | birthplace\_us | language\_english | language\_hindi | language\_other | language\_spanish | language\_unknown | education\_hs\_graduate | education\_less\_than\_hs\_graduate | education\_post\_graduate\_degree | education\_some\_college | education\_unknown | employment\_not\_in\_labor\_force | employment\_unemployed | health\_insurance\_private | personal\_income | on\_welfare\_yes | poverty\_threshold\_below | work\_transport\_other | work\_transport\_private\_vehicle | work\_transport\_public\_transit | work\_transport\_walking | work\_transport\_worked\_from\_home |
|:-----|-----------:|----------:|------------------:|--------------------:|--------------------:|-------------:|--------------:|----------:|---------:|-----------------------:|------------------------------------:|------------:|---------------:|------------:|------------:|---------------:|------------------:|----------------:|----------------:|------------------:|------------------:|------------------------:|------------------------------------:|----------------------------------:|-------------------------:|-------------------:|----------------------------------:|-----------------------:|---------------------------:|-----------------:|-----------------:|--------------------------:|-----------------------:|----------------------------------:|---------------------------------:|-------------------------:|------------------------------------:|
| 3701 |     110016 |  924.2170 |          114193.0 |           0.2243431 |           0.8871624 |     3.328378 |     0.5227393 | 0.4807539 | 37.22959 |              0.0036322 |                           0.1755820 |   0.2716981 |      0.1007933 |   0.0086214 |   0.4064615 |      0.6003337 |         0.5013679 |       0.0363664 |       0.1530536 |         0.1723310 |         0.0614135 |               0.2012814 |                           0.3105409 |                         0.0948015 |                0.1751352 |          0.0355887 |                         0.4733084 |              0.0324845 |                  0.9161792 |         46682.21 |        0.1953106 |                 0.1831953 |              0.5191957 |                         0.1220037 |                        0.2756562 |                0.0541162 |                           0.0215648 |
| 3702 |     151067 |  935.4120 |          106761.8 |           0.2313543 |           0.9155232 |     3.183746 |     0.5067718 | 0.4865298 | 37.80187 |              0.0024175 |                           0.1636617 |   0.1301883 |      0.1875897 |   0.0084507 |   0.4651172 |      0.6447790 |         0.3971393 |       0.0434670 |       0.1620713 |         0.2725492 |         0.0645985 |               0.1993663 |                           0.3151518 |                         0.1099687 |                0.1714112 |          0.0366029 |                         0.4929084 |              0.0284434 |                  0.9191024 |         44113.06 |        0.1986043 |                 0.2186417 |              0.5320215 |                         0.0996032 |                        0.2912960 |                0.0463175 |                           0.0237115 |
| 3703 |     119529 | 1032.1367 |          116734.1 |           0.2638998 |           0.8953900 |     3.454844 |     0.5503979 | 0.4804294 | 36.09352 |              0.0040850 |                           0.1831152 |   0.2375749 |      0.1279615 |   0.0096865 |   0.4044683 |      0.5388238 |         0.4393629 |       0.0384363 |       0.1641281 |         0.2118702 |         0.0718842 |               0.1801875 |                           0.3232494 |                         0.1197418 |                0.1493730 |          0.0437204 |                         0.4760255 |              0.0320975 |                  0.9150358 |         46714.50 |        0.2158947 |                 0.2103241 |              0.5224124 |                         0.1007106 |                        0.2911655 |                0.0539696 |                           0.0236797 |
| 3704 |     126664 |  936.1718 |          118101.9 |           0.2377836 |           0.9215612 |     3.278162 |     0.5044178 | 0.4768163 | 37.73571 |              0.0027141 |                           0.1609478 |   0.2675658 |      0.0909628 |   0.0103237 |   0.4380434 |      0.6029847 |         0.5018593 |       0.0299833 |       0.1325290 |         0.1932621 |         0.0655637 |               0.1895536 |                           0.3044901 |                         0.1235233 |                0.1606752 |          0.0388292 |                         0.4798509 |              0.0295077 |                  0.9198649 |         47951.42 |        0.2013912 |                 0.2054159 |              0.5261335 |                         0.1186382 |                        0.2766251 |                0.0495118 |                           0.0222097 |
| 3705 |     171016 |  905.5632 |          123918.3 |           0.0927768 |           0.8778438 |     3.263593 |     0.4821918 | 0.4866007 | 40.81002 |              0.0032069 |                           0.1826325 |   0.2568923 |      0.0328278 |   0.0151186 |   0.4718517 |      0.5583668 |         0.5597872 |       0.0460307 |       0.1503090 |         0.1262995 |         0.0491624 |               0.2130535 |                           0.2417526 |                         0.1333715 |                0.1766074 |          0.0284029 |                         0.4350905 |              0.0231478 |                  0.9166286 |         47543.78 |        0.1448138 |                 0.0898442 |              0.4694772 |                         0.1540024 |                        0.3014550 |                0.0438268 |                           0.0228592 |
| 3706 |     131986 |  816.9890 |          121248.2 |           0.1532570 |           0.9136376 |     3.123549 |     0.4802748 | 0.4742848 | 39.42301 |              0.0038065 |                           0.1636181 |   0.3138695 |      0.0115262 |   0.0091782 |   0.4636045 |      0.6443204 |         0.5771303 |       0.0286903 |       0.1576062 |         0.1046874 |         0.0574438 |               0.1906708 |                           0.2745733 |                         0.1254084 |                0.1825582 |          0.0342982 |                         0.4906344 |              0.0235905 |                  0.9300694 |         47516.12 |        0.1827171 |                 0.1603317 |              0.5291671 |                         0.1457973 |                        0.2559379 |                0.0426178 |                           0.0201182 |
