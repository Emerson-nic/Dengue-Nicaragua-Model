[Versión en Español](README.es.md) 

# Dengue-Nicaragua-Model

Statistical model for predicting dengue outbreaks in Nicaragua at the departmental level. This project uses weekly epidemiological surveillance data and climate covariates to estimate the probability that a given week crosses the epidemic outbreak threshold within each department.

## What the Model Does

The response variable `is_brote` is binary. It is defined using the endemic channel approach established by the Pan American Health Organization (PAHO): a week is classified as an outbreak (`1`) when reported cases exceed the 75th percentile of the historical distribution for that specific department and epidemiological week.

The model is a Generalized Linear Mixed Model (GLMM) with a logit link function, fitted using `glmmTMB`. Alternative link functions were tested during model selection but discarded because they failed residual diagnostic checks. The logit specification represents the final, robust model.

### Formula

``` r
is_brote ~ Niño + Rain_acc3 + Temperature + ln_casos_semana_anterior +  
    (1 | DEPARTAMENTO) + ar1(tiempo_factor + 0 | DEPARTAMENTO)
```

Main components:

- Fixed effects: El Niño indicator, 3-week accumulated rainfall, mean temperature, and the log of cases from the previous week.
- Random intercept by department, to absorb unobserved regional heterogeneity.
- AR(1) residual structure within each department, to account for the week-to-week memory of transmission.

## Data

Weekly records from 18 departments, starting in 2014. Variables include case counts, temperature, rainfall, accumulated rainfall over 3 weeks, and an ENSO indicator. Cases from the previous week and their log transform are included as a control for epidemic inertia.

## Main Findings

- Previous-week cases dominate the prediction. Doubling them multiplies the odds of an outbreak by roughly 20 (95% CI: 16 to 24).
- El Niño weeks show about 49% higher odds of outbreak compared to regular weeks.
- Each additional millimeter of accumulated rainfall over 3 weeks raises the odds by roughly 0.3%.
- Temperature is only marginally significant (p = 0.078). This is likely because temperatures across Nicaraguan departments stay within a range that favors *Aedes aegypti* development year-round, so the variable carries little discriminating power.
- The AR(1) coefficient is 0.96, meaning transmission in a given week is almost fully inherited from the previous week within the same department.
- No collinearity issues (VIF near 1 for all predictors).
- Residual diagnostics with DHARMa pass the uniformity, dispersion, and outlier tests. Durbin-Watson on aggregated residuals gives p = 0.21, so no leftover temporal autocorrelation.

### Authors

Emerson Lopez - Lead Author / Maintainer 

Co-author: [to be added]

### Citation

If you use this model or the code in this repository, cite it as:

```text
Lopez, E. (2026). Dengue-Nicaragua-Model: A generalized linear mixed model for dengue outbreak prediction in Nicaragua [Source code]. GitHub. [https://github.com/Emerson-nic/Dengue-Nicaragua-Model](https://github.com/Emerson-nic/Dengue-Nicaragua-Model)
```
