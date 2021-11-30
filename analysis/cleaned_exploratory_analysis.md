Cleaned Exploratory Analysis
================
Zachary Katz
11/30/2021

# Data Preparation

# Exploratory Analysis

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
