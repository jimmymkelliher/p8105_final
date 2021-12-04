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
