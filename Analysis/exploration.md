P8105: Data Science I
================
Exploration of Datasets<br>Jimmy Kelliher (UNI: jmk2303)

-   [NYC Census Data](#nyc-census-data)
-   [Health Outcomes](#health-outcomes)
-   [Crosswalk](#crosswalk)

<!-------------------------------------------------------------------------------
Preamble
-------------------------------------------------------------------------------->
<!-------------------------------------------------------------------------------
Census Data
-------------------------------------------------------------------------------->

# NYC Census Data

We pull demographic data from the 2019 5-year ACS for NYC. Below, we
further filter by taking a random sample of 1,000 observations.

``` r
read_csv("nyc_census_2019_sample.csv") %>%
  janitor::clean_names() %>%
  head(10) %>%
  knitr::kable()
```

|  x1 |     x2 | year | multyear | sample |  serial |     cbserial | hhwt |      cluster | statefip | countyfip | puma | strata |  gq | pernum | perwt | sex | age | race | raced | hispan | hispand | bpl |  bpld | language | languaged | speakeng | tribe | tribed | racnum | empstat | empstatd | labforce | uhrswork |  inctot | ftotinc | incss | incwelfr | poverty |
|----:|-------:|-----:|---------:|-------:|--------:|-------------:|-----:|-------------:|---------:|----------:|-----:|-------:|----:|-------:|------:|----:|----:|-----:|------:|-------:|--------:|----:|------:|---------:|----------:|---------:|------:|-------:|-------:|--------:|---------:|---------:|---------:|--------:|--------:|------:|---------:|--------:|
|   1 | 347105 | 2019 |     2019 | 201903 | 4518593 | 2.019001e+12 |   14 | 2.019045e+12 |       36 |        61 | 3804 | 380436 |   1 |      1 |    14 |   1 |  58 |    2 |   200 |      0 |       0 |  36 |  3600 |        1 |       100 |        3 |     0 |      0 |      1 |       3 |       30 |        1 |        0 |    7700 |    7700 |  7700 |        0 |      94 |
|   2 | 353241 | 2019 |     2019 | 201903 | 4525674 | 2.019001e+12 |   24 | 2.019045e+12 |       36 |        61 | 3801 | 380136 |   1 |      2 |    44 |   1 |  30 |    7 |   700 |      4 |     460 | 260 | 26010 |       12 |      1200 |        4 |     0 |      0 |      1 |       1 |       10 |        2 |       43 |   56000 |   84960 |     0 |        0 |     319 |
|   3 |  69939 | 2019 |     2015 | 201903 | 4183011 | 2.015001e+12 |   13 | 2.019042e+12 |       36 |        81 | 4103 | 410336 |   1 |      2 |    12 |   2 |  51 |    1 |   100 |      0 |       0 | 455 | 45500 |        1 |       100 |        3 |     0 |      0 |      1 |       1 |       10 |        2 |       40 |   28057 |   84170 |     0 |        0 |     314 |
|   4 | 314605 | 2019 |     2019 | 201903 | 4481838 | 2.019001e+12 |   12 | 2.019045e+12 |       36 |         5 | 3707 | 370736 |   1 |      1 |    13 |   1 |  64 |    1 |   100 |      4 |     460 | 260 | 26010 |        1 |       100 |        3 |     0 |      0 |      1 |       3 |       30 |        1 |        0 |   78200 |   78200 | 26200 |        0 |     501 |
|   5 |    403 | 2019 |     2015 | 201903 | 4100979 | 2.015000e+12 |   38 | 2.019041e+12 |       36 |        81 | 4104 | 410436 |   1 |      8 |    46 |   1 |   8 |    8 |   826 |      4 |     420 |  36 |  3600 |       12 |      1200 |        4 |     0 |      0 |      2 |       0 |        0 |        0 |        0 | 9999999 |  256719 | 99999 |    99999 |     458 |
|   6 | 158127 | 2019 |     2017 | 201903 | 4286076 | 2.017000e+12 |   17 | 2.019043e+12 |       36 |        47 | 4011 | 401136 |   1 |      2 |    22 |   1 |  33 |    2 |   200 |      0 |       0 |  36 |  3600 |        1 |       100 |        3 |     0 |      0 |      1 |       1 |       10 |        2 |       30 |   15644 |  324979 |     0 |        0 |     501 |
|   7 |  41455 | 2019 |     2015 | 201903 | 4149699 | 2.015001e+12 |   13 | 2.019041e+12 |       36 |        81 | 4108 | 410836 |   1 |      3 |    18 |   1 |  60 |    6 |   665 |      0 |       0 | 521 | 52130 |       45 |      4500 |        6 |     0 |      0 |      1 |       1 |       10 |        2 |       40 |   49639 |  124637 |     0 |        0 |     392 |
|   8 | 161846 | 2019 |     2017 | 201903 | 4290192 | 2.017000e+12 |   13 | 2.019043e+12 |       36 |        81 | 4109 | 410936 |   1 |      4 |    20 |   1 |  62 |    4 |   400 |      0 |       0 | 500 | 50000 |       43 |      4302 |        1 |     0 |      0 |      1 |       3 |       30 |        1 |        0 |     521 |  173023 |     0 |      521 |     501 |
|   9 | 196229 | 2019 |     2017 | 201903 | 4328066 | 2.017001e+12 |    9 | 2.019043e+12 |       36 |        47 | 4016 | 401636 |   1 |      1 |     9 |   2 |  68 |    1 |   100 |      0 |       0 | 465 | 46500 |       18 |      1800 |        4 |     0 |      0 |      1 |       1 |       10 |        2 |       48 |   66226 |   66226 | 13767 |        0 |     501 |
|  10 | 340999 | 2019 |     2019 | 201903 | 4511743 | 2.019001e+12 |   22 | 2.019045e+12 |       36 |        61 | 3805 | 380536 |   1 |      2 |    22 |   2 |  31 |    1 |   100 |      0 |       0 |  36 |  3600 |        1 |       100 |        3 |     0 |      0 |      1 |       1 |       10 |        2 |       40 |   80000 |  150100 |     0 |        0 |     501 |

# Health Outcomes

We pull hospitalizations and deaths per 100,000 residents in a given
ZCTA from NYC DOHMH.

``` r
read_csv("nyc_health_outcomes_2020.csv") %>%
  janitor::clean_names() %>%
  head(10) %>%
  knitr::kable()
```

|  zcta | hospitalizations | deaths |
|------:|-----------------:|-------:|
| 10001 |            253.5 |   36.2 |
| 10002 |            212.4 |  101.0 |
| 10003 |            152.1 |   59.3 |
| 10004 |            437.3 |    0.0 |
| 10005 |            194.1 |    0.0 |
| 10006 |            207.0 |    0.0 |
| 10007 |            128.7 |    0.0 |
| 10009 |            167.8 |   82.2 |
| 10010 |            207.0 |   48.0 |
| 10011 |            158.8 |   62.4 |

# Crosswalk

Because the census data is observed at the PUMA geography, and because
the health data is observed at the ZCTA geography, we pull a crosswalk
between these two geographies. Note that neither geography is a strict
subset of the other.

``` r
read_csv("nyc_puma_zcta_crosswalk.csv") %>%
  janitor::clean_names() %>%
  head(10) %>%
  knitr::kable()
```

|  zcta | stateco | alloc | puma | pumaname                                                                     |   pop | per\_in\_puma | per\_of\_puma |
|------:|--------:|:------|-----:|:-----------------------------------------------------------------------------|------:|--------------:|--------------:|
| 10451 |   36005 | x     | 3710 | NYC-Bronx Community District 1 & 2–Hunts Point, Longwood & Melrose           | 22027 |         0.482 |         0.141 |
| 10451 |   36005 | NA    | 3708 | NYC-Bronx Community District 4–Concourse, Highbridge & Mount Eden            | 21002 |         0.459 |         0.151 |
| 10451 |   36005 | NA    | 3705 | NYC-Bronx Community District 3 & 6–Belmont, Crotona Park East & East Tremont |  2684 |         0.059 |         0.017 |
| 10452 |   36005 | x     | 3708 | NYC-Bronx Community District 4–Concourse, Highbridge & Mount Eden            | 71729 |         0.952 |         0.515 |
| 10452 |   36005 | NA    | 3707 | NYC-Bronx Community District 5–Morris Heights, Fordham South & Mount Hope    |  3642 |         0.048 |         0.027 |
| 10453 |   36005 | x     | 3707 | NYC-Bronx Community District 5–Morris Heights, Fordham South & Mount Hope    | 77074 |         0.984 |         0.574 |
| 10453 |   36005 | NA    | 3706 | NYC-Bronx Community District 7–Bedford Park, Fordham North & Norwood         |  1235 |         0.016 |         0.010 |
| 10454 |   36005 | x     | 3710 | NYC-Bronx Community District 1 & 2–Hunts Point, Longwood & Melrose           | 37337 |         1.000 |         0.239 |
| 10455 |   36005 | x     | 3710 | NYC-Bronx Community District 1 & 2–Hunts Point, Longwood & Melrose           | 39665 |         1.000 |         0.254 |
| 10456 |   36005 | x     | 3705 | NYC-Bronx Community District 3 & 6–Belmont, Crotona Park East & East Tremont | 42529 |         0.491 |         0.264 |
