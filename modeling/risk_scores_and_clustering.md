Risk Scores and Clustering
================
Zachary Katz and Hun Lee
12/6/2021

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
```

``` r
health_data <- read_csv("./data/outcome_puma.csv")

merged_data <- merge(census_data, health_data, by = "puma")
rm(census_data, health_data)
```

``` r
# Clean the merged census and outcomes data
cleaned_data = 
  merged_data %>% 
  # Remove variables less useful for analysis, including ones with high correlation with remaining variables
  select(-multyear, -ancestr1, -ancestr2, -labforce, -occ, -ind, -incwage, -occscore, -pwpuma00, -ftotinc, -hcovpub) %>% 
  # Remove duplicate rows
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
      hcovany == 2 & hcovpriv == 2 ~ "Private",
      hcovany == 2 & hcovpriv == 1 ~ "Public"
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
  # Convert hospitalization and death rates to pure percentages to match vax rate
  mutate(
    puma_hosp_rate = puma_hosp_rate / 1000,
    puma_death_rate = puma_death_rate / 1000
  ) %>% 
  # Eliminate columns no longer needed after transformation
  select(-hispan, -hcovany, -hcovpriv) %>% 
  # Relocate new columns
  relocate(health_insurance, .before = personal_income) %>% 
  relocate(poverty_threshold, .before = work_transport) %>% 
  relocate(on_welfare, .before = poverty_threshold) %>% 
  relocate(perwt, .before = cluster) %>% 
  # Create factor variables where applicable
  mutate(across(.cols = c(puma, borough, on_foodstamps, has_broadband, sex, race, birthplace, US_citizen, language, health_insurance, education, employment, on_welfare, poverty_threshold, work_transport), as.factor)) %>% 
  # Change levels of certain key factors for later analysis
  mutate(
    health_insurance = factor(health_insurance,
                              levels = c("None", "Public", "Private")),
    education = factor(education,
                       levels = c("Less Than HS Graduate", "HS Graduate", "Some College", "Bachelor's Degree", "Post-Graduate Degree", "Unknown"))
  )

rm(merged_data)
```

``` r
# Example data frame with weightings for summary stats over each PUMA
nyc_puma_summary = cleaned_data %>% 
  # Note: do we need to filter to one individual per household for household weightings?
  group_by(puma) %>%
  summarize(
    total_people = sum(perwt),
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
```

## Prediction Modeling

### Risk Scoring

We want to develop a method to score PUMAs on risk of not achieving herd
immunity from vaccination. Let’s say that herd immunity occurs at 70%
vaccination rate, for our purposes.

``` r
# 1 indicates BELOW 70% vaccination rate
logistic_df = nyc_puma_summary %>% 
  mutate(
    below_herd_vax = as.factor(ifelse(covid_vax_rate >= 70, 0, 1))
  ) %>% 
  select(-puma, -total_people, -covid_hosp_rate, -covid_death_rate, -covid_vax_rate)

# Define predictors matrix
x = model.matrix(below_herd_vax ~ ., logistic_df)[,-1]

# Define outcomes
y = logistic_df$below_herd_vax
```

``` r
library(caret) # Note: new libraries are loaded here due to potential function masking from the `caret` library
library(mlbench)
```

``` r
set.seed(777)
vax_cv <- trainControl(method = "repeatedcv", number = 5, repeats = 100, 
                       savePredictions = T
                       )

lasso_model <- train(below_herd_vax ~ ., data = logistic_df,
                     method = "glmnet",
                     trControl = vax_cv,
                     tuneGrid = expand.grid(
                       .alpha = 1,
                       .lambda = 10^seq(-5, 5, length = 10)),
                     family = "binomial")

lasso_model
```

    ## glmnet 
    ## 
    ## 55 samples
    ## 16 predictors
    ##  2 classes: '0', '1' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold, repeated 100 times) 
    ## Summary of sample sizes: 44, 44, 44, 44, 44, 44, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   lambda        Accuracy   Kappa    
    ##   1.000000e-05  0.8335121  0.5056866
    ##   1.291550e-04  0.8351667  0.5076780
    ##   1.668101e-03  0.8449424  0.5295549
    ##   2.154435e-02  0.8807242  0.5557529
    ##   2.782559e-01  0.8007727  0.0000000
    ##   3.593814e+00  0.8007727  0.0000000
    ##   4.641589e+01  0.8007727  0.0000000
    ##   5.994843e+02  0.8007727  0.0000000
    ##   7.742637e+03  0.8007727  0.0000000
    ##   1.000000e+05  0.8007727  0.0000000
    ## 
    ## Tuning parameter 'alpha' was held constant at a value of 1
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were alpha = 1 and lambda = 0.02154435.

``` r
coef <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda)

sub_lasso <- subset(lasso_model$pred, lasso_model$pred$lambda == lasso_model$bestTune$lambda)

caret::confusionMatrix(table(sub_lasso$pred, sub_lasso$obs))
```

    ## Confusion Matrix and Statistics
    ## 
    ##    
    ##        0    1
    ##   0  638  195
    ##   1  462 4205
    ##                                          
    ##                Accuracy : 0.8805         
    ##                  95% CI : (0.8717, 0.889)
    ##     No Information Rate : 0.8            
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.5893         
    ##                                          
    ##  Mcnemar's Test P-Value : < 2.2e-16      
    ##                                          
    ##             Sensitivity : 0.5800         
    ##             Specificity : 0.9557         
    ##          Pos Pred Value : 0.7659         
    ##          Neg Pred Value : 0.9010         
    ##              Prevalence : 0.2000         
    ##          Detection Rate : 0.1160         
    ##    Detection Prevalence : 0.1515         
    ##       Balanced Accuracy : 0.7678         
    ##                                          
    ##        'Positive' Class : 0              
    ## 

### Getting risk prediction for each PUMA

``` r
lambda <- lasso_model$bestTune$lambda
lasso_fit = glmnet(x, y, lambda = lambda, family = "binomial")
risk_predictions = (round((predict(lasso_fit, x, type = "response"))*100, 1))

puma <- nyc_puma_summary %>% 
  select(puma)

vax <- logistic_df %>% 
  select(below_herd_vax)

puma_risk_scores <- 
  bind_cols(puma, vax, as.vector(risk_predictions)) %>%
  rename(risk_prediciton = ...3)

puma_risk_scores
```

    ## # A tibble: 55 × 3
    ##    puma  below_herd_vax risk_prediciton
    ##    <fct> <fct>                    <dbl>
    ##  1 3701  1                         98.2
    ##  2 3702  1                         99.7
    ##  3 3703  1                         99  
    ##  4 3704  1                         99  
    ##  5 3705  1                        100  
    ##  6 3706  1                         99.8
    ##  7 3707  1                        100  
    ##  8 3708  1                         99.7
    ##  9 3709  1                         99.7
    ## 10 3710  1                        100  
    ## # … with 45 more rows

## Clustering

We might try to see how our PUMAs cluster on predictors, and how those
clusters are associated with distributions of particular outcomes.

Let’s start with K-means clustering.

``` r
# Define tibble of predictors only
predictors = nyc_puma_summary %>% 
  select(-puma, -total_people, -covid_hosp_rate, -covid_vax_rate, -covid_death_rate)

# Define tibble of outcomes only
outcomes = nyc_puma_summary %>% 
  select(covid_hosp_rate, covid_death_rate, covid_vax_rate)

# Define tibble of pumas only
pumas = nyc_puma_summary %>% 
  select(puma)

# Fit 3 clusters on predictors
kmeans_fit = 
  kmeans(x = predictors, centers = 3)

# Add clusters to data frame of predictors and bind with PUMA and outcomes data
predictors = 
  broom::augment(kmeans_fit, predictors)

# Bind columns
full_df = cbind(pumas, outcomes, predictors)

# Summary df
summary_df = full_df %>% 
  group_by(.cluster) %>% 
  summarize(
    median_hosp = median(covid_hosp_rate),
    median_death = median(covid_death_rate),
    median_vax = median(covid_vax_rate)
  )

# Plot predictor clusters against outcomes
# Example: try hospitalization vs vaccination
ggplot(data = full_df, aes(x = covid_hosp_rate, y = covid_vax_rate, color = .cluster)) + 
  geom_point() + 
  geom_point(data = summary_df, aes(x = median_hosp, y = median_vax), color = "black", size = 4) +
  geom_point(data = summary_df, aes(x = median_hosp, y = median_vax, color = .cluster), size = 2.75)
```

<img src="risk_scores_and_clustering_files/figure-gfm/initial clustering-1.png" width="90%" />

We could also scale predictors and omit NAs.

``` r
# Scale predictors
for_clustering = predictors %>% 
  select(-.cluster) %>% 
  na.omit() %>% 
  scale()

# Evaluate Euclidean distances between observations
distance = get_dist(for_clustering)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
```

<img src="risk_scores_and_clustering_files/figure-gfm/scaled clustering-1.png" width="90%" />

``` r
# Cluster with three centers
k_scaled = kmeans(for_clustering, centers = 3)

# Visualize cluster plot with reduction to two dimensions
fviz_cluster(k_scaled, data = for_clustering)
```

<img src="risk_scores_and_clustering_files/figure-gfm/scaled clustering-2.png" width="90%" />

``` r
# Bind with outcomes and color clusters
full_df = for_clustering %>% 
  as_tibble() %>% 
  cbind(outcomes, pumas) %>% 
  mutate(
    cluster = k_scaled$cluster
  )

summary_df = full_df %>% 
  group_by(cluster) %>% 
  summarize(
    median_hosp = median(covid_hosp_rate),
    median_death = median(covid_death_rate),
    median_vax = median(covid_vax_rate)
  )

ggplot(data = full_df, aes(x = covid_hosp_rate, y = covid_vax_rate, color = factor(cluster))) + 
  geom_point() + 
  geom_point(data = summary_df, aes(x = median_hosp, y = median_vax), color = "black", size = 4) +
  geom_point(data = summary_df, aes(x = median_hosp, y = median_vax, color = factor(cluster)), size = 2.75)
```

<img src="risk_scores_and_clustering_files/figure-gfm/scaled clustering-3.png" width="90%" />

We may want to evaluate this method’s clustering quality as follows:

``` r
# Check where elbow occurs using WSS method
fviz_nbclust(for_clustering, kmeans, method = "wss")
```

<img src="risk_scores_and_clustering_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

``` r
# Check for optimal number of clusters using silhouette method
fviz_nbclust(for_clustering, kmeans, method = "silhouette")
```

<img src="risk_scores_and_clustering_files/figure-gfm/unnamed-chunk-2-2.png" width="90%" />

``` r
# Check fnumber of clusters that minimize gap statistic
gap_stat = clusGap(for_clustering, FUN = kmeans, nstart = 25, K.max = 20, B = 50)
fviz_gap_stat(gap_stat)
```

<img src="risk_scores_and_clustering_files/figure-gfm/unnamed-chunk-2-3.png" width="90%" />

It seems that two clusters may actually work better than three, when
clustering on predictors.

``` r
# Scale predictors
for_clustering = predictors %>% 
  select(-.cluster) %>% 
  na.omit() %>% 
  scale()

# Evaluate Euclidean distances between observations
distance = get_dist(for_clustering)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
```

<img src="risk_scores_and_clustering_files/figure-gfm/scaled - two clusters-1.png" width="90%" />

``` r
# Cluster with three centers
k_scaled2 = kmeans(for_clustering, centers = 2)

# Visualize cluster plot with reduction to two dimensions
fviz_cluster(k_scaled2, data = for_clustering)
```

<img src="risk_scores_and_clustering_files/figure-gfm/scaled - two clusters-2.png" width="90%" />

``` r
# Bind with outcomes and color clusters
full_df = for_clustering %>% 
  as_tibble() %>% 
  cbind(outcomes, pumas) %>% 
  mutate(
    cluster = k_scaled2$cluster
  )

summary_df = full_df %>% 
  group_by(cluster) %>% 
  summarize(
    median_hosp = median(covid_hosp_rate),
    median_death = median(covid_death_rate),
    median_vax = median(covid_vax_rate)
  )

ggplot(data = full_df, aes(x = covid_hosp_rate, y = covid_vax_rate, color = factor(cluster))) + 
  geom_point() + 
  geom_point(data = summary_df, aes(x = median_hosp, y = median_vax), color = "black", size = 4) +
  geom_point(data = summary_df, aes(x = median_hosp, y = median_vax, color = factor(cluster)), size = 2.75)
```

<img src="risk_scores_and_clustering_files/figure-gfm/scaled - two clusters-3.png" width="90%" />
