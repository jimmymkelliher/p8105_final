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
we run the regression

        *y*<sub>*s*</sub> = *α* + *β*
*I*<sub>{*s* = 1}</sub> + *ε*<sub>*s*</sub>.

Trivially, we will find that *α̂* = *y*<sub>0</sub> and
*β̂* = *y*<sub>1</sub> − *y*<sub>0</sub>. Moreover, we will find that
*R*<sup>2</sup> = 1, but this is deceptive! While the model enjoys a
perfect linear fit, it does not say anything meaningful: “If we know the
value for each group, we can predict the value for each group!” Below is
an example of this in the context of our data.

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
heterogeneity within PUMAs.
