Pulling Data Sets
================
Hun Lee (sl4836), Jimmy Kelliher (jmk2303), Tanvir Khan (tk2886), Tucker
Morgan (tlm2152), Zachary Katz (zak2132)
11/17/2021

``` r
library(tidyverse)
library(httr)
library(readxl)
```

Below, we import United States Census data from the American Community
Survey (ACS) 2019 five-year estimate via
[IPUMS](https://usa.ipums.org/usa/). This data includes demographic and
macroeconomic data by Public Use Microdata Area (PUMA) for New York
City.

``` r
# space for Jimmy to import updated IPUMS
```

Next, we bring in monthly health outcomes from [NYC Department of Health
and Mental Hygiene](https://github.com/nychealth/coronavirus-data)
(DOHMH) as of 11/18/2021. We will primarily analyze data related to
COVID-19 hospitalizations and deaths.

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
    ## 1 2021-11-18 Chelsea/NoMad/West… Manhat…   10001 1000… All ages           27613.
    ## 2 2021-11-18 Chinatown/Lower Ea… Manhat…   10002 10002 All ages           75323.
    ## 3 2021-11-18 East Village/Grame… Manhat…   10003 10003 All ages           53978.
    ## 4 2021-11-18 Financial District  Manhat…   10004 10004 All ages            2972.
    ## 5 2021-11-18 Financial District  Manhat…   10005 10005 All ages            8757.
    ## 6 2021-11-18 Financial District  Manhat…   10006 10006 All ages            3382.
    ## # … with 6 more variables: COUNT_PARTIALLY_CUMULATIVE <dbl>,
    ## #   COUNT_FULLY_CUMULATIVE <dbl>, COUNT_1PLUS_CUMULATIVE <dbl>,
    ## #   PERC_PARTIALLY <dbl>, PERC_FULLY <dbl>, PERC_1PLUS <dbl>

``` r
write_csv(nyc_vacc_zcta, "./data/nyc_vacc_zcta.csv")
```

Below, we import broadband adoption and infrastructure data by zip code
from [NYC Open
Data](https://data.cityofnewyork.us/City-Government/Broadband-Adoption-and-Infrastructure-by-Zip-Code/qz5f-yx82/data).

``` r
nyc_broadband <- 
  GET("https://data.cityofnewyork.us/resource/qz5f-yx82.json") %>% 
  content("text") %>% 
  jsonlite::fromJSON() %>%
  as_tibble()
head(nyc_broadband)
```

    ## # A tibble: 6 × 17
    ##   oid   zip_code home_broadband_adoption mobile_broadband_a… no_internet_access…
    ##   <chr> <chr>    <chr>                   <chr>               <chr>              
    ## 1 0     83       0.8343                  0.8442              0.0747             
    ## 2 1     10001    0.8258                  0.8359              0.091              
    ## 3 2     10002    0.541                   0.6496              0.3092             
    ## 4 3     10003    0.8002                  0.8579              0.0803             
    ## 5 4     10004    0.9255                  0.9625              0.008              
    ## 6 5     10005    0.9225                  0.9575              0.0122             
    ## # … with 12 more variables: no_home_broadband_adoption <chr>,
    ## #   no_mobile_broadband_adoption <chr>, no_home_broadband_adoption_1 <chr>,
    ## #   no_mobile_broadband_adoption_1 <chr>, commercial_fiber_max_isp <chr>,
    ## #   public_computer_center_count <chr>, workstations_in_pccs <chr>,
    ## #   avg_training_hrs_per_week <chr>, public_wi_fi_count <chr>,
    ## #   poles_reserved_by_mobile <chr>, pole_with_equipment_installed <chr>,
    ## #   density_of_poles_reserved <chr>

``` r
write_csv(nyc_broadband, "./data/nyc_broadband.csv")
```

Finally, we will import crosswalk data sets from [Baruch
College](https://www.baruch.cuny.edu/confluence/display/geoportal/NYC+Geographies)
to be utilized in relating ZCTA’s, zip codes, and PUMAs.

``` r
zcta_puma_url = "http://faculty.baruch.cuny.edu/geoportal/resources/nyc_geog/nyc_zcta10_to_puma10.xls"

zcta_puma_cross <- read_excel(zcta_puma_url) # issue here, link on the website seems broken

zcta_zip_url = "http://faculty.baruch.cuny.edu/geoportal/resources/nyc_geog/zip_to_zcta10_nyc_revised.xls"

zcta_zip_cross <- read_excel(zcta_zip_url) # issue here, link on the website seems broken
```
