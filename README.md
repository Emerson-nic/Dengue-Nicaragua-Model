[Versión en Español](README.es.md) 

# Dengue-Nicaragua-Model

Statistical analysis of factors associated with dengue outbreaks in Nicaragua at the departmental level. This project uses weekly epidemiological surveillance data and climate covariates to estimate the probability that a given week is classified as an outbreak within each department. The goal is inference on associations, not prediction.

## What the Model Does

The response variable `brote` is binary. It is defined using the endemic channel approach established by the Pan American Health Organization (PAHO): a week is classified as an outbreak (`1`) when reported cases exceed the 75th percentile of the historical distribution for that specific department.

The model is a Generalized Linear Mixed Model (GLMM) with a logit link function, fitted using `glmmTMB`. It includes monthly seasonality, a long-term annual trend, epidemic inertia, and an AR(1) structure to account for temporal dependence.

### Formula

``` r
brote ~ nino34_12s + Rain_acc3 + Temperature + ln_casos_semana_anterior +
    mes + year +
    (1 | DEPARTAMENTO) + ar1(tiempo_factor + 0 | DEPARTAMENTO)
```

Main components:

- Fixed effects: 12-week moving average of the Niño 3.4 SST anomaly (`nino34_12s`), 3-week accumulated rainfall, mean temperature, log of cases from the previous week, month (`mes`), and year (`year`).
- Random intercept by department, to absorb unobserved regional heterogeneity.
- AR(1) residual structure within each department, to account for the week-to-week memory of transmission.

## Data

Weekly records from 18 departments for 2014-2022. The gap-filled series has 8,316 department-weeks (462 weeks per department) with no missing values. After constructing lags and the 3-week rainfall accumulation, 8,280 department-weeks remain for the model. Variables include case counts, temperature, rainfall, accumulated rainfall over 3 weeks, the Niño 3.4 anomaly, previous-week cases and their log transform, month, and year.

## Main Findings

- Previous-week cases dominate the association. Doubling them multiplies the odds of an outbreak by 16.77 (95% CI: 13.50 to 20.83).
- El Niño does not show a robust independent association. The odds ratio is 1.141 (95% CI: 0.908 to 1.434; p = 0.256) with the year trend in the model, and 1.20 (95% CI: 0.987 to 1.46; p = 0.067) without it.
- Each additional millimeter of accumulated rainfall over 3 weeks raises the odds by about 0.24% (OR = 1.00239; 95% CI: 1.00098 to 1.00380; p = 0.00091). An increase of 100 mm raises the odds by approximately 27% (95% CI: 10% to 46%).
- Each additional degree Celsius raises the odds by approximately 27% (OR = 1.271; 95% CI: 1.091 to 1.481; p = 0.0021).
- The AR(1) coefficient is 0.960, meaning transmission in a given week is almost fully inherited from the previous week within the same department.
- There are no problematic collinearity issues: the maximum VIF is 4.65 for month and 2.17 for year, and adjusted VIFs remain below 2.2.
- Residual diagnostics with DHARMa pass the uniformity (p = 0.31), dispersion (p = 0.69), and outlier bootstrap (p = 0.12) tests. Durbin-Watson on aggregated residuals gives 1.86 (p = 0.13), so no leftover temporal autocorrelation.
- These results describe associations and should not be interpreted as a predictive or forecasting model.

### Authors

Emerson Lopez - Lead Author / Maintainer

### Citation

If you use this model or the code in this repository, cite it as:

```text
Lopez, E. (2026). Dengue-Nicaragua-Model: A generalized linear mixed model for dengue outbreak risk in Nicaragua [Source code]. GitHub. [https://github.com/Emerson-nic/Dengue-Nicaragua-Model](https://github.com/Emerson-nic/Dengue-Nicaragua-Model)
```
