Pulling Data Sets
================
Hun Lee (sl4836), Jimmy Kelliher (jmk2303), Tanvir Khan (tk2886), Tucker
Morgan (tlm2152), Zachary Katz (zak2132)
11/22/2021

``` r
library(tidyverse)
library(httr)
library(gdata)
library(RCurl)
library(readxl)
```

The predictor variables in our analysis will come from United States
Census data and the American Community Survey (ACS) 2019 five-year
estimate via [IPUMS](https://usa.ipums.org/usa/). This data includes
demographic and macroeconomic data by Public Use Microdata Area (PUMA)
for New York City. Predictor variables include:

-   Rent
-   Household income
-   Food stamps
-   High-speed internet
-   Family size
-   Sex
-   Age
-   Race / Ethnicity
-   Citizenship
-   Health coverage
-   Education
-   Employment Status
-   Personal income
-   Poverty status

Below, we bring in monthly health outcomes from [NYC Department of
Health and Mental
Hygiene](https://github.com/nychealth/coronavirus-data) (DOHMH) as of
11/18/2021. We will primarily analyze data related to COVID-19
hospitalizations and deaths.

``` r
hosp_rate_url = "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/hosprate-by-modzcta.csv"

hosp_rate_zcta <- read_csv(hosp_rate_url)

death_rate_url = "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/deathrate-by-modzcta.csv"

death_rate_zcta <- read_csv(death_rate_url)
```

``` r
write_csv(hosp_rate_zcta, "./data/nyc_hosp_rate_zcta.csv")
write_csv(death_rate_zcta, "./data/nyc_death_rate_zcta.csv")
```

Below, we import vaccination data from
[nyc.gov](https://www1.nyc.gov/site/doh/covid/covid-19-data-vaccines.page)
containing information on the number and estimated percentage of NYC
residents who are fully or partially vaccinated by Modified ZCTA
(MODZCTA) as of 11/17/2021.

``` r
vacc_url = "https://raw.githubusercontent.com/nychealth/covid-vaccine-data/main/people/coverage-by-modzcta-allages.csv"

nyc_vacc_zcta <- read_csv(vacc_url)
head(nyc_vacc_zcta)
```

    ## # A tibble: 6 × 13
    ##   DATE       NEIGHBORHOOD_NAME   BOROUGH MODZCTA Label AGE_GROUP POP_DENOMINATOR
    ##   <date>     <chr>               <chr>     <dbl> <chr> <chr>               <dbl>
    ## 1 2021-11-21 Chelsea/NoMad/West… Manhat…   10001 1000… All ages           27613.
    ## 2 2021-11-21 Chinatown/Lower Ea… Manhat…   10002 10002 All ages           75323.
    ## 3 2021-11-21 East Village/Grame… Manhat…   10003 10003 All ages           53978.
    ## 4 2021-11-21 Financial District  Manhat…   10004 10004 All ages            2972.
    ## 5 2021-11-21 Financial District  Manhat…   10005 10005 All ages            8757.
    ## 6 2021-11-21 Financial District  Manhat…   10006 10006 All ages            3382.
    ## # … with 6 more variables: COUNT_PARTIALLY_CUMULATIVE <dbl>,
    ## #   COUNT_FULLY_CUMULATIVE <dbl>, COUNT_1PLUS_CUMULATIVE <dbl>,
    ## #   PERC_PARTIALLY <dbl>, PERC_FULLY <dbl>, PERC_1PLUS <dbl>

``` r
write_csv(nyc_vacc_zcta, "./data/nyc_vacc_zcta.csv")
```

Finally, we will import a crosswalk data set from [Baruch
College](https://www.baruch.cuny.edu/confluence/display/geoportal/NYC+Geographies)
to be utilized in relating ZCTAs and PUMAs.

``` r
zcta_puma_url = "http://faculty.baruch.cuny.edu/geoportal/resources/nyc_geog/nyc_zcta10_to_puma10.xls"

zcta_puma_cross <- read.xls(zcta_puma_url)
```

``` r
write_csv(zcta_puma_cross, "./data/zcta_puma_cross.csv")
```
