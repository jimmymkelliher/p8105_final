Cleaned Exploratory Analysis
================
Zachary Katz
11/30/2021

# Data Preparation

# Exploratory Analysis

## Overview of Outcome Variables

### Outcomes by Demographic

Which distinct race/sex/age group combinations have the best and worst
outcomes?

| Race  | Age Group | Sex    | % Hospitalized |
|:------|:----------|:-------|---------------:|
| White | (20,30\]  | Female |          0.876 |
| White | (30,40\]  | Male   |          0.886 |
| White | (30,40\]  | Female |          0.891 |
| White | (20,30\]  | Male   |          0.896 |
| White | (40,50\]  | Male   |          0.928 |
| White | (40,50\]  | Female |          0.935 |

Lowest hospitalization rates (per 100)

| Race                       | Age Group | Sex    | % Hospitalized |
|:---------------------------|:----------|:-------|---------------:|
| American Indian            | (80,90\]  | Male   |          1.272 |
| Other                      | (60,70\]  | Male   |          1.217 |
| Other                      | (10,20\]  | Male   |          1.212 |
| Other                      | (40,50\]  | Male   |          1.200 |
| Other                      | (60,70\]  | Female |          1.185 |
| Asian and Pacific Islander | (90,100\] | Male   |          1.174 |

Highest hospitalization rates (per 100)

| Race  | Age Group | Sex    | % Deceased |
|:------|:----------|:-------|-----------:|
| White | (20,30\]  | Female |      0.238 |
| White | (30,40\]  | Male   |      0.242 |
| White | (20,30\]  | Male   |      0.244 |
| White | (30,40\]  | Female |      0.246 |
| Other | (80,90\]  | Male   |      0.254 |
| White | (40,50\]  | Male   |      0.256 |

Lowest death rates (per 100)

| Race                       | Age Group | Sex    | % Deceased |
|:---------------------------|:----------|:-------|-----------:|
| American Indian            | (80,90\]  | Male   |      1.272 |
| Other                      | (60,70\]  | Male   |      1.217 |
| Other                      | (10,20\]  | Male   |      1.212 |
| Other                      | (40,50\]  | Male   |      1.200 |
| Other                      | (60,70\]  | Female |      1.185 |
| Asian and Pacific Islander | (90,100\] | Male   |      1.174 |

Highest death rates (per 100)

| Race            | Age Group | Sex    | % Vaccinated |
|:----------------|:----------|:-------|-------------:|
| American Indian | (70,80\]  | Male   |       49.324 |
| Black           | (10,20\]  | Female |       49.362 |
| Black           | \[0,10\]  | Male   |       49.474 |
| Black           | (10,20\]  | Male   |       49.481 |
| Black           | (30,40\]  | Female |       49.494 |
| Black           | \[0,10\]  | Female |       49.519 |

Lowest vaccination rates (per 100)

| Race                       | Age Group | Sex    | % Vaccinated |
|:---------------------------|:----------|:-------|-------------:|
| Asian and Pacific Islander | (90,100\] | Female |       66.102 |
| Asian and Pacific Islander | (70,80\]  | Female |       65.900 |
| Asian and Pacific Islander | (70,80\]  | Male   |       65.734 |
| Asian and Pacific Islander | (30,40\]  | Male   |       65.616 |
| Asian and Pacific Islander | (30,40\]  | Female |       65.577 |
| 2+ races                   | (80,90\]  | Male   |       65.479 |

Highest vaccination rates (per 100)

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

## Associations between Predictors and Outcomes

How do key predictors correlate with key outcomes at the PUMA level?

<img src="cleaned_exploratory_analysis_files/figure-gfm/correlations predictors vs outcomes-1.png" width="90%" />
