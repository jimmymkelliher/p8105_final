P8105: Data Science I
================
Analysis Playground<br>Jimmy Kelliher (UNI: jmk2303)

-   [Unzipping the Census Data](#unzipping-the-census-data)
-   [Merging the Outcome Data](#merging-the-outcome-data)
-   [Illustrating with an Example
    Analysis](#illustrating-with-an-example-analysis)

<!------------------------------------------------------------------------------
Preamble
------------------------------------------------------------------------------->
<!------------------------------------------------------------------------------
Overview
------------------------------------------------------------------------------->

This file provides a template by which we create our final dataset.
Essentially, because of the size of our census data, we cannot store the
final dataset locally. Instead, we create a dataset of health outcomes
at the PUMA level (see `data_documentation/merging_data_sets.Rmd`), and
we merge this with our unzipped census data, which exists at the
individual level and indicates the PUMA in which each individual
resides.

# Unzipping the Census Data

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

census_data <- jimzip("census_filtered.csv", "./data")
head(census_data) %>%  knitr::kable()
```

| multyear |  serial | hhwt |      cluster | countyfip | puma | strata | rent | hhincome | foodstmp | cihispeed | perwt | famsize | nchild | sex | age | race | hispan | bpl | ancestr1 | ancestr2 | citizen | yrsusa1 | language | hcovany | hcovpriv | hcovpub | educ | empstat | labforce |  occ |  ind |  inctot | ftotinc | incwage | incwelfr | poverty | occscore | pwpuma00 | tranwork |
|---------:|--------:|-----:|-------------:|----------:|-----:|-------:|-----:|---------:|---------:|----------:|------:|--------:|-------:|----:|----:|-----:|-------:|----:|---------:|---------:|--------:|--------:|---------:|--------:|---------:|--------:|-----:|--------:|---------:|-----:|-----:|--------:|--------:|--------:|---------:|--------:|---------:|---------:|---------:|
|     2015 | 4100568 |   30 | 2.019041e+12 |        61 | 3806 | 380636 | 2806 |    93882 |        1 |        10 |    31 |       1 |      0 |   2 |  25 |    1 |      0 |  42 |       51 |      999 |       0 |       0 |       11 |       2 |        2 |       1 |   10 |       1 |        2 | 5940 | 7580 |   50718 |   50718 |   50718 |        0 |     381 |       25 |     3800 |       36 |
|     2015 | 4100568 |   30 | 2.019041e+12 |        61 | 3806 | 380636 | 2806 |    93882 |        1 |        10 |    49 |       1 |      0 |   2 |  27 |    1 |      0 |  39 |      148 |      153 |       0 |       0 |        1 |       2 |        2 |       1 |   11 |       1 |        2 |  726 | 8564 |   43164 |   43164 |   43164 |        0 |     325 |       24 |     3800 |       36 |
|     2015 | 4100570 |   20 | 2.019041e+12 |        47 | 4004 | 400436 |  453 |    16834 |        1 |        10 |    20 |       4 |      2 |   2 |  45 |    4 |      0 | 500 |      706 |      999 |       3 |      11 |       43 |       2 |        1 |       2 |    6 |       1 |        2 | 4850 | 4390 |    5503 |   16834 |    5503 |        0 |      62 |       24 |     3200 |       36 |
|     2015 | 4100570 |   20 | 2.019041e+12 |        47 | 4004 | 400436 |  453 |    16834 |        1 |        10 |    20 |       4 |      2 |   1 |  49 |    4 |      0 | 500 |      706 |      999 |       3 |      11 |       43 |       2 |        1 |       2 |    6 |       1 |        2 | 4030 | 8680 |   11331 |   16834 |   11331 |        0 |      62 |       16 |     3800 |       36 |
|     2015 | 4100570 |   20 | 2.019041e+12 |        47 | 4004 | 400436 |  453 |    16834 |        1 |        10 |    13 |       4 |      0 |   1 |  21 |    4 |      0 | 500 |      706 |      999 |       2 |      11 |       43 |       2 |        1 |       2 |    6 |       3 |        1 | 2545 | 7860 |       0 |   16834 |       0 |        0 |      62 |       33 |        0 |        0 |
|     2015 | 4100570 |   20 | 2.019041e+12 |        47 | 4004 | 400436 |  453 |    16834 |        1 |        10 |    18 |       4 |      0 |   2 |  10 |    4 |      0 |  36 |      706 |      999 |       0 |       0 |       43 |       2 |        1 |       2 |    1 |       0 |        0 |    0 |    0 | 9999999 |   16834 |  999999 |    99999 |      62 |        0 |        0 |        0 |

# Merging the Outcome Data

``` r
health_data <-
  read_csv("./data/outcome_puma.csv") %>%
  rename(puma = puma10)

merged_data <- merge(census_data, health_data, by = "puma")
rm(census_data)

head(merged_data) %>% knitr::kable()
```

| puma | multyear |  serial | hhwt |      cluster | countyfip | strata | rent | hhincome | foodstmp | cihispeed | perwt | famsize | nchild | sex | age | race | hispan | bpl | ancestr1 | ancestr2 | citizen | yrsusa1 | language | hcovany | hcovpriv | hcovpub | educ | empstat | labforce |  occ |  ind | inctot | ftotinc | incwage | incwelfr | poverty | occscore | pwpuma00 | tranwork | puma\_death\_rate | puma\_hosp\_rate | puma\_vacc\_rate |
|-----:|---------:|--------:|-----:|-------------:|----------:|-------:|-----:|---------:|---------:|----------:|------:|--------:|-------:|----:|----:|-----:|-------:|----:|---------:|---------:|--------:|--------:|---------:|--------:|---------:|--------:|-----:|--------:|---------:|-----:|-----:|-------:|--------:|--------:|---------:|--------:|---------:|---------:|---------:|------------------:|-----------------:|-----------------:|
| 3701 |     2017 | 4301368 |   25 | 2.019043e+12 |         5 | 370136 |    0 |   166870 |        1 |        20 |    18 |       2 |      0 |   1 |  58 |    1 |      0 | 465 |      999 |      999 |       2 |      19 |        1 |       2 |        2 |       1 |   11 |       1 |        2 | 2100 | 7270 |  83435 |  166870 |   83435 |        0 |     501 |       62 |     3800 |       36 |          34.09364 |         53.23122 |         55.79213 |
| 3701 |     2018 | 4429112 |   22 | 2.019044e+12 |         5 | 370136 | 1018 |   123192 |        1 |        10 |    22 |       2 |      0 |   1 |  52 |    2 |      0 | 600 |      522 |      999 |       2 |      27 |       60 |       2 |        2 |       1 |    8 |       1 |        2 | 9142 | 6190 | 107920 |  123192 |   85522 |        0 |     501 |       22 |     3800 |       10 |          34.09364 |         53.23122 |         55.79213 |
| 3701 |     2019 | 4496321 |   17 | 2.019045e+12 |         5 | 370136 | 2000 |   125000 |        1 |        10 |    16 |       1 |      0 |   2 |  59 |    1 |      2 | 110 |      200 |      261 |       0 |      50 |       12 |       2 |        2 |       1 |    8 |       1 |        2 | 5740 | 6870 | 125000 |  125000 |  125000 |        0 |     501 |       22 |     3800 |       31 |          34.09364 |         53.23122 |         55.79213 |
| 3701 |     2017 | 4272796 |    6 | 2.019043e+12 |         5 | 370136 |    0 |   140588 |        2 |        10 |     4 |       6 |      3 |   1 |  64 |    4 |      0 | 500 |      706 |      999 |       2 |      26 |       43 |       2 |        1 |       2 |   11 |       3 |        1 | 1050 | 8191 |  11264 |  140588 |       0 |        0 |     388 |       33 |        0 |        0 |          34.09364 |         53.23122 |         55.79213 |
| 3701 |     2015 | 4184953 |   19 | 2.019042e+12 |         5 | 370136 | 1058 |    52876 |        1 |        10 |    20 |       2 |      1 |   1 |  44 |    1 |      4 | 300 |      237 |      999 |       2 |      23 |       12 |       2 |        1 |       2 |    8 |       1 |        2 | 4220 | 7072 |  32373 |   52876 |   32373 |        0 |     309 |       19 |     3800 |       36 |          34.09364 |         53.23122 |         55.79213 |
| 3701 |     2016 | 4255659 |    9 | 2.019043e+12 |         5 | 370136 |    0 |    26101 |        1 |        10 |     9 |       1 |      0 |   2 |  64 |    1 |      0 |  36 |       50 |       32 |       0 |       0 |        1 |       2 |        2 |       1 |   11 |       1 |        2 | 2360 | 7890 |  26101 |   26101 |   12784 |        0 |     198 |       20 |     3100 |       10 |          34.09364 |         53.23122 |         55.79213 |

# Illustrating with an Example Analysis

``` r
example <-
  merged_data %>%
  # restrict to white New Yorkers
  filter(race %in% c(1)) %>%
  # group by puma, race, and outcomme of interest
  group_by(puma, race, puma_hosp_rate) %>%
  # summarize predictors of interest by taking weighted averages
  summarize(
      age    = weighted.mean(age,     perwt, na.rm = TRUE)
    , educ   = weighted.mean(educ,    perwt, na.rm = TRUE)
    , inctot = weighted.mean(inctot,  perwt, na.rm = TRUE)
  )

# run a linear regression on stratified sample
summary(lm(puma_hosp_rate ~ age + educ + inctot, data = example))
```

    ## 
    ## Call:
    ## lm(formula = puma_hosp_rate ~ age + educ + inctot, data = example)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -28.003  -6.997   0.111   5.595  38.845 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  8.494e+01  3.036e+01   2.798  0.00724 ** 
    ## age          1.170e+00  4.348e-01   2.692  0.00958 ** 
    ## educ        -1.037e+01  1.925e+00  -5.388 1.83e-06 ***
    ## inctot      -8.078e-06  5.260e-06  -1.536  0.13076    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 12.53 on 51 degrees of freedom
    ## Multiple R-squared:  0.4642, Adjusted R-squared:  0.4327 
    ## F-statistic: 14.73 on 3 and 51 DF,  p-value: 4.941e-07
