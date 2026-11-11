rm(list = ls())

#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-16"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               glmmTMB, #generalized linear mixed model (GLMM)
               performance, #model diagnostics and evaluation metrics (vif)
               ggeffects, #marginal predictions and confidence interval estimation plot
               broom.mixed, #tidying model outputs into clean tabular formats for coefficients
               DHARMa, #evaluation of residuals
               patchwork, #combining charts  
               itsadug, #Dharma needs this
               # xml2,
               RcppRoll, #Acum sum for this one
               pROC #ROC curves and AUC for model comparison
)

dengue <- readr::read_csv("Data/Csv/Etapas_dengue.csv")

#centering constants are fixed on the full series before lag construction
mean_temperature <- mean(dengue$Temperature, na.rm = TRUE)
mean_nino <- mean(dengue$nino34_12s, na.rm = TRUE)

# alternative thresholds and climate lags ----

#p75 is already stored in the cleaned file and p90 is built here for comparison
#climate covariates are rescaled to avoid numerical instability in the p90 models
dengue_modelos <- dengue %>%
  dplyr::arrange(DEPARTAMENTO, calendar_start_date) %>%
  dplyr::group_by(DEPARTAMENTO) %>%
  dplyr::mutate(
    threshold_p90 = floor(quantile(dengue_total, 0.90, na.rm = TRUE)), #90th percentile as the alternative threshold
    brote_p75 = factor(as.integer(brote == 1)), #outbreak with the original p75 threshold
    brote_p90 = factor(as.integer(dengue_total > threshold_p90)), #outbreak with the p90 threshold
    Rain_lag2_100 = dplyr::lag(Rain, 2) / 100, #weekly rainfall two weeks before, per 100 mm
    Rain_lag3_100 = dplyr::lag(Rain, 3) / 100, #weekly rainfall three weeks before, per 100 mm
    Rain_lag4_100 = dplyr::lag(Rain, 4) / 100, #weekly rainfall four weeks before, per 100 mm
    Temp_lag2_c = dplyr::lag(Temperature, 2) - mean_temperature, #centered temperature two weeks before
    Temp_lag3_c = dplyr::lag(Temperature, 3) - mean_temperature, #centered temperature three weeks before
    Temp_lag4_c = dplyr::lag(Temperature, 4) - mean_temperature #centered temperature four weeks before
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    tiempo_factor = factor(calendar_start_date), #time index for the AR(1) structure
    mes = factor(mes), #month factor for seasonality
    year = factor(year), #year factor for the long-term trend
    year_num = lubridate::year(calendar_start_date) - 2014, #linear year for temporal validation
    Rain100 = Rain_acc3 / 100, #accumulated rainfall per 100 mm
    Temp_c = Temperature - mean_temperature, #centered mean temperature
    nino_c = nino34_12s - mean_nino #centered Niño 3.4 anomaly
  ) %>%
  tidyr::drop_na(
    brote_p75, brote_p90, nino_c, Rain100, Temp_c,
    Rain_lag2_100, Rain_lag3_100, Rain_lag4_100,
    Temp_lag2_c, Temp_lag3_c, Temp_lag4_c,
    ln_casos_semana_anterior
  )

#outbreak frequency under both thresholds
print(
  dengue_modelos %>%
    dplyr::summarise(
      n_semanas = dplyr::n(),
      brotes_p75 = sum(brote_p75 == "1"),
      pct_p75 = round(100 * mean(brote_p75 == "1"), 1),
      brotes_p90 = sum(brote_p90 == "1"),
      pct_p90 = round(100 * mean(brote_p90 == "1"), 1)
    )
)

# main models with p75 and p90 ----

#glmmTMB 1.1.14 supports priors, but rescaling removed the extreme coefficients
#so the reported models retain maximum likelihood estimation
#sensitivity models use a linear annual trend to stabilize estimation and allow temporal validation
#inertia-only reference for p75
m_p75_inercia <- glmmTMB::glmmTMB(
  brote_p75 ~
    ln_casos_semana_anterior +
    mes + #monthly seasonality
    year_num + #long-term annual trend
    (1 | DEPARTAMENTO) + #random intercept by department
    ar1(tiempo_factor + 0 | DEPARTAMENTO), #temporal autocorrelation
  family = binomial(link = "logit"),
  data = dengue_modelos
)

#p75 with the original climate covariates
m_p75_original <- glmmTMB::glmmTMB(
  brote_p75 ~
    nino_c +
    Rain100 +
    Temp_c +
    ln_casos_semana_anterior +
    mes +
    year_num +
    (1 | DEPARTAMENTO) +
    ar1(tiempo_factor + 0 | DEPARTAMENTO),
  family = binomial(link = "logit"),
  data = dengue_modelos
)

#p75 with rainfall and temperature lagged 2 to 4 weeks
m_p75_lags <- glmmTMB::glmmTMB(
  brote_p75 ~
    nino_c +
    Rain_lag2_100 + Rain_lag3_100 + Rain_lag4_100 + #rainfall lags per 100 mm
    Temp_lag2_c + Temp_lag3_c + Temp_lag4_c + #centered temperature lags
    ln_casos_semana_anterior +
    mes +
    year_num +
    (1 | DEPARTAMENTO) +
    ar1(tiempo_factor + 0 | DEPARTAMENTO),
  family = binomial(link = "logit"),
  data = dengue_modelos
)

#inertia-only reference for p90
m_p90_inercia <- glmmTMB::glmmTMB(
  brote_p90 ~
    ln_casos_semana_anterior +
    mes +
    year_num +
    (1 | DEPARTAMENTO) +
    ar1(tiempo_factor + 0 | DEPARTAMENTO),
  family = binomial(link = "logit"),
  data = dengue_modelos
)

#p90 with the original climate covariates
m_p90_original <- glmmTMB::glmmTMB(
  brote_p90 ~
    nino_c +
    Rain100 +
    Temp_c +
    ln_casos_semana_anterior +
    mes +
    year_num +
    (1 | DEPARTAMENTO) +
    ar1(tiempo_factor + 0 | DEPARTAMENTO),
  family = binomial(link = "logit"),
  data = dengue_modelos
)

#p90 with rainfall and temperature lagged 2 to 4 weeks
m_p90_lags <- glmmTMB::glmmTMB(
  brote_p90 ~
    nino_c +
    Rain_lag2_100 + Rain_lag3_100 + Rain_lag4_100 + #rainfall lags per 100 mm
    Temp_lag2_c + Temp_lag3_c + Temp_lag4_c + #centered temperature lags
    ln_casos_semana_anterior +
    mes +
    year_num +
    (1 | DEPARTAMENTO) +
    ar1(tiempo_factor + 0 | DEPARTAMENTO),
  family = binomial(link = "logit"),
  data = dengue_modelos
)

# model diagnostics ----

#convergence and Hessian checks for every main model
diagnostico_modelo <- function(modelo) {
  coeficientes <- glmmTMB::fixef(modelo)$cond
  errores <- tryCatch(
    sqrt(diag(stats::vcov(modelo)$cond)),
    error = function(e) rep(NA_real_, length(coeficientes))
  )
  tibble::tibble(
    pdHess = isTRUE(modelo$sdr$pdHess), #TRUE means a positive-definite Hessian
    convergencia = modelo$fit$convergence, #0 means successful convergence
    gradiente_max = max(abs(modelo$sdr$gradient), na.rm = TRUE), #large values flag numerical problems
    coef_max_abs = max(abs(coeficientes), na.rm = TRUE), #very large values flag separation
    coef_max_term = names(coeficientes)[which.max(abs(coeficientes))], #term responsible for the maximum
    se_na = anyNA(errores) #undefined standard errors flag a singular Hessian
  )
}

modelos_principales <- list(
  "p75 inercia" = m_p75_inercia,
  "p75 original" = m_p75_original,
  "p75 rezagos" = m_p75_lags,
  "p90 inercia" = m_p90_inercia,
  "p90 original" = m_p90_original,
  "p90 rezagos" = m_p90_lags
)

diagnosticos_principales <- purrr::imap_dfr(modelos_principales, \(modelo, nombre) {
  diagnostico_modelo(modelo) %>%
    dplyr::mutate(modelo = nombre, .before = 1)
})

print(diagnosticos_principales, width = Inf)

#full glmmTMB diagnostics for the lagged models
purrr::walk(
  list(m_p75_lags, m_p90_lags),
  \(modelo) print(glmmTMB::diagnose(modelo))
)

# population-level AUC ----

#population predictions exclude random effects, so AUC is comparable across models
auc_poblacional <- function(modelo, datos, respuesta) {
  probabilidad <- stats::predict(modelo, newdata = datos, type = "response", re.form = NA) #fixed-effect predictions
  curva_roc <- pROC::roc(
    response = respuesta,
    predictor = probabilidad,
    levels = c(0, 1),
    direction = "<",
    quiet = TRUE
  )
  as.numeric(pROC::auc(curva_roc)) #area under the ROC curve
}

#comparison table for the p75/p90 by original/lags matrix
modelos_comparacion <- list(
  list(nombre = "p75 inercia", umbral = "p75", especificacion = "inercia", modelo = m_p75_inercia, brote = "brote_p75"),
  list(nombre = "p75 original", umbral = "p75", especificacion = "original", modelo = m_p75_original, brote = "brote_p75"),
  list(nombre = "p75 rezagos", umbral = "p75", especificacion = "rezagos", modelo = m_p75_lags, brote = "brote_p75"),
  list(nombre = "p90 inercia", umbral = "p90", especificacion = "inercia", modelo = m_p90_inercia, brote = "brote_p90"),
  list(nombre = "p90 original", umbral = "p90", especificacion = "original", modelo = m_p90_original, brote = "brote_p90"),
  list(nombre = "p90 rezagos", umbral = "p90", especificacion = "rezagos", modelo = m_p90_lags, brote = "brote_p90")
)

comparacion_modelos <- purrr::map_dfr(modelos_comparacion, \(x) {
  tibble::tibble(
    modelo = x$nombre,
    umbral = x$umbral,
    especificacion = x$especificacion,
    AIC = stats::AIC(x$modelo), #Akaike information criterion
    AUC = auc_poblacional(x$modelo, dengue_modelos, as.integer(dengue_modelos[[x$brote]] == "1")),
    logLik = as.numeric(stats::logLik(x$modelo)) #maximized log-likelihood
  )
}) %>%
  dplyr::group_by(umbral) %>%
  dplyr::mutate(
    delta_AIC = AIC[especificacion == "inercia"] - AIC, #positive values mean a better fit
    delta_AUC = AUC - AUC[especificacion == "inercia"] #positive values mean better discrimination
  ) %>%
  dplyr::ungroup()

print(comparacion_modelos, width = Inf)

#likelihood ratio tests against the inertia-only reference
#original and lagged climate models are not nested; their comparison uses AIC
print(stats::anova(m_p75_inercia, m_p75_original))
print(stats::anova(m_p75_inercia, m_p75_lags))
print(stats::anova(m_p90_inercia, m_p90_original))
print(stats::anova(m_p90_inercia, m_p90_lags))

# odds ratios for the lagged climate terms ----

#odds ratios are extracted separately for p75 and p90
or_rezagos <- dplyr::bind_rows(
  broom.mixed::tidy(
    m_p75_lags,
    effects = "fixed",
    component = "cond",
    exponentiate = TRUE,
    conf.int = TRUE
  ) %>%
    dplyr::mutate(umbral = "p75"),
  broom.mixed::tidy(
    m_p90_lags,
    effects = "fixed",
    component = "cond",
    exponentiate = TRUE,
    conf.int = TRUE
  ) %>%
    dplyr::mutate(umbral = "p90")
) %>%
  dplyr::filter(term %in% c(
    "Rain_lag2_100", "Rain_lag3_100", "Rain_lag4_100",
    "Temp_lag2_c", "Temp_lag3_c", "Temp_lag4_c"
  )) %>%
  dplyr::select(umbral, term, estimate, conf.low, conf.high, p.value)

print(or_rezagos, width = Inf)

# temporal validation 2014-2019 / 2020-2022 ----

#the linear year term allows predictions for years not observed during training
entrenamiento <- dengue_modelos %>%
  dplyr::filter(year_num <= 5) #2014-2019

prueba <- dengue_modelos %>%
  dplyr::filter(year_num >= 6) #2020-2022

#validation models keep the same structure and refit on the training years
v_p75_inercia <- update(m_p75_inercia, data = entrenamiento)
v_p75_original <- update(m_p75_original, data = entrenamiento)
v_p75_lags <- update(m_p75_lags, data = entrenamiento)
v_p90_inercia <- update(m_p90_inercia, data = entrenamiento)
v_p90_original <- update(m_p90_original, data = entrenamiento)
v_p90_lags <- update(m_p90_lags, data = entrenamiento)

modelos_validacion <- list(
  list(nombre = "p75 inercia", umbral = "p75", especificacion = "inercia", modelo = v_p75_inercia, brote = "brote_p75"),
  list(nombre = "p75 original", umbral = "p75", especificacion = "original", modelo = v_p75_original, brote = "brote_p75"),
  list(nombre = "p75 rezagos", umbral = "p75", especificacion = "rezagos", modelo = v_p75_lags, brote = "brote_p75"),
  list(nombre = "p90 inercia", umbral = "p90", especificacion = "inercia", modelo = v_p90_inercia, brote = "brote_p90"),
  list(nombre = "p90 original", umbral = "p90", especificacion = "original", modelo = v_p90_original, brote = "brote_p90"),
  list(nombre = "p90 rezagos", umbral = "p90", especificacion = "rezagos", modelo = v_p90_lags, brote = "brote_p90")
)

validacion_temporal <- purrr::map_dfr(modelos_validacion, \(x) {
  tibble::tibble(
    modelo = x$nombre,
    umbral = x$umbral,
    especificacion = x$especificacion,
    pdHess = isTRUE(x$modelo$sdr$pdHess), #Hessian check for each validation model
    AUC_entrenamiento = auc_poblacional(
      x$modelo,
      entrenamiento,
      as.integer(entrenamiento[[x$brote]] == "1")
    ),
    AUC_prueba = auc_poblacional(
      x$modelo,
      prueba,
      as.integer(prueba[[x$brote]] == "1")
    )
  )
}) %>%
  dplyr::group_by(umbral) %>%
  dplyr::mutate(
    delta_AUC_prueba = AUC_prueba - AUC_prueba[especificacion == "inercia"] #positive values mean better test discrimination
  ) %>%
  dplyr::ungroup()

print(validacion_temporal, width = Inf)

#model without previous-week cases and its diagnostics ----

#this model uses year_num, as the other sensitivity models, and scaled climate covariates
m_p75_sin_inercia <- glmmTMB::glmmTMB(
  brote_p75 ~
    nino_c +
    Rain100 +
    Temp_c +
    mes +
    year_num + #linear trend used in the sensitivity models
    (1 | DEPARTAMENTO) +
    ar1(tiempo_factor + 0 | DEPARTAMENTO),
  family = binomial(link = "logit"),
  data = dengue_modelos
)

print(diagnostico_modelo(m_p75_sin_inercia))
print(glmmTMB::diagnose(m_p75_sin_inercia))

print(
  broom.mixed::tidy(
    m_p75_sin_inercia,
    effects = "fixed",
    component = "cond",
    exponentiate = TRUE,
    conf.int = TRUE
  ) %>%
    dplyr::filter(term == "nino_c"),
  width = Inf
)
