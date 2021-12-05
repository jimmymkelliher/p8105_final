P8105: Data Science I
================
Regression<br>Jimmy Kelliher (UNI: jmk2303)

<!------------------------------------------------------------------------------
Preamble
------------------------------------------------------------------------------->
<!------------------------------------------------------------------------------
Overview
------------------------------------------------------------------------------->

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

| r.squared | adj.r.squared | sigma |    statistic | p.value |  df | df.residual |   nobs |
|----------:|--------------:|------:|-------------:|--------:|----:|------------:|-------:|
|         1 |             1 |     0 | 1.136853e+26 |       0 |  54 |      356148 | 356203 |

Thus, for any independent variable *x* that we consider to predict
health outcome *y*, we must ensure that *x* exhibits sufficient
heterogeneity within PUMAs. If not, *x* is just a predictor of where
someone lives, and not of their probability of observing health outcome
*y*.

``` r
example_validate_vax <-
  lm(
    puma_vacc_rate ~ personal_income
  , data = cleaned_data
  , weights = perwt
  ) %>%
  summary()

example_validate_puma <-
  lm(
    personal_income ~ puma
  , data = cleaned_data
  , weights = perwt
  ) %>%
  summary()

example_validate_vax %>%broom::tidy() %>% knitr::kable()
```

| term             |   estimate | std.error |  statistic | p.value |
|:-----------------|-----------:|----------:|-----------:|--------:|
| (Intercept)      | 56.6747072 | 0.0330822 | 1713.14550 |       0 |
| personal\_income |  0.0000325 | 0.0000003 |   94.73751 |       0 |

``` r
example_validate_vax %>%broom::glance() %>% knitr::kable()
```

| r.squared | adj.r.squared |   sigma | statistic | p.value |  df | df.residual |   nobs |
|----------:|--------------:|--------:|----------:|--------:|----:|------------:|-------:|
| 0.0290053 |      0.029002 | 75.7679 |  8975.195 |       0 |   1 |      300458 | 300460 |

``` r
example_validate_puma %>%broom::glance() %>% knitr::kable()
```

| r.squared | adj.r.squared |    sigma | statistic | p.value |  df | df.residual |   nobs |
|----------:|--------------:|---------:|----------:|--------:|----:|------------:|-------:|
| 0.1241917 |     0.1240342 | 376612.6 |  788.8543 |       0 |  54 |      300405 | 300460 |

``` r
# function that computes a sum of weights
wsum  <- function(w) {
  sum(w, na.rm = TRUE)
}

# function that returns weighted mean
wmean <- function(v, w = rep(1, length(v)), remove_na = TRUE) {
  sum(w * v, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

test <-
  cleaned_data %>%
  #select(puma, perwt, household_income, personal_income, age) %>%
  select(-c(
    serial, hhwt, cluster, borough, strata,
    US_citizen, puma_death_rate, puma_hosp_rate, puma_vacc_rate
  )) %>%
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

X <- test %>% select(-c(puma, perwt))
X <- as_tibble(model.matrix(~ ., X)[ , -1])

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

w  <- pull(test, perwt) / sum(pull(test, perwt))
n  <- nrow(test) # wsum(w)
g  <- length(unique(pull(test, puma)))
ng <- test %>% group_by(puma) %>% summarize(ng = n()) %>% pull(ng)
#ng <- test %>% group_by(puma) %>% summarize(ng = wsum(perwt)) %>% pull(ng)

Xbar  <-
  X %>%
  mutate(across(
      everything()
    , ~ replace(.x, TRUE, wmean(.x, pull(test, perwt)))
  ))

Xbarg <-
  cbind(puma = pull(test, puma), w = w, X) %>%
  group_by(puma) %>%
  mutate(across(
      everything()
    , ~ replace(.x, TRUE, wmean(.x, w))
  )) %>%
  ungroup() %>%
  select(-c(puma, w))

xbar <- Xbar[1, ]

xbarg <-
  cbind(puma = pull(test, puma), w = w, X) %>%
  group_by(puma) %>%
  mutate(across(
      everything()
    , ~ replace(.x, TRUE, wmean(.x, w))
  )) %>%
  distinct() %>%
  ungroup() %>%
  select(-c(puma, w))

msa <- pbyp(w^(0.5) * (Xbarg - Xbar)) # / (g - 1)
mse <- pbyp(w^(0.5) * (X - Xbarg)) # / (n - g)

within_error  <- mse
between_error <- n * (g - 1) * (msa - mse) / (n^2 - sum(ng^2))
#between_error <- (msa - mse) / n

jimcol <- function(k, x) {
  W <- solve(between_error + within_error / k) %*% between_error
  u <- unlist(xbar) %*% (diag(ncol(X)) - W) + unlist(x) %*% W
  u
}

ubarg <-
  test %>%
  select(puma, perwt) %>%
  group_by(puma) %>%
  summarize(group_pop = sum(perwt)) %>%
  cbind(xbarg) %>%
  nest(xbarg = -c(puma, group_pop)) %>%
  mutate(ubarg = map2(group_pop, xbarg, jimcol)) %>%
  unnest(ubarg) %>%
  pull(ubarg) %>%
  as_tibble()
```

    ## Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
    ## Using compatibility `.name_repair`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
names(ubarg) <- names(xbarg)

unbiased_group_means <-
  test %>%
  select(puma, perwt) %>%
  group_by(puma) %>%
  summarize(group_pop = sum(perwt)) %>%
  cbind(ubarg) %>%
  ungroup()  %>%
  janitor::clean_names()

write_csv(unbiased_group_means, "./data/unbiased_group_means.csv")

#lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "HC0"))
```
