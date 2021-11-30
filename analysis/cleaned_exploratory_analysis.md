Cleaned Exploratory Analysis
================
Zachary Katz
11/30/2021

# Data Preparation

# Exploratory Analysis

## Overview of Outcome Variables

### Outcomes by Demographic Combination

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
