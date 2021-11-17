Pulling Data Sets
================
Jimmy Kelliher (jmk2303), Hun Lee (sl4836), Tanvir Khan (tk2886), Tucker
Morgan (tlm2152), Zachary Katz (zak2132)
11/17/2021

``` r
library(tidyverse)
library(httr)
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
    ## 1 2021-11-16 Chelsea/NoMad/West… Manhat…   10001 1000… All ages           27613.
    ## 2 2021-11-16 Chinatown/Lower Ea… Manhat…   10002 10002 All ages           75323.
    ## 3 2021-11-16 East Village/Grame… Manhat…   10003 10003 All ages           53978.
    ## 4 2021-11-16 Financial District  Manhat…   10004 10004 All ages            2972.
    ## 5 2021-11-16 Financial District  Manhat…   10005 10005 All ages            8757.
    ## 6 2021-11-16 Financial District  Manhat…   10006 10006 All ages            3382.
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
