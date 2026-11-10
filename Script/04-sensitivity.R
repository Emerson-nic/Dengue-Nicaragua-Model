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

# alternative threshold p90 ----

#the outbreak definition changes from p75 to p90 within each department
dengue_p90 <- dengue %>%
  dplyr::group_by(DEPARTAMENTO) %>%
  dplyr::mutate(
    threshold_p90 = floor(quantile(dengue_total, 0.90, na.rm = TRUE)), #90th percentile as the alternative threshold
    brote_p90 = factor(as.integer(dengue_total > threshold_p90)) #binary outbreak with the p90 threshold
  ) %>%
  dplyr::ungroup()

#climate lags from 2 to 4 weeks ----

#lags are built inside each department to avoid mixing territories
dengue_p90 <- dengue_p90 %>%
  dplyr::arrange(DEPARTAMENTO, calendar_start_date) %>%
  dplyr::group_by(DEPARTAMENTO) %>%
  dplyr::mutate(
    Rain_lag2 = dplyr::lag(Rain, 2), #rainfall two weeks before
    Rain_lag3 = dplyr::lag(Rain, 3), #rainfall three weeks before
    Rain_lag4 = dplyr::lag(Rain, 4), #rainfall four weeks before
    Temperature_lag2 = dplyr::lag(Temperature, 2), #temperature two weeks before
    Temperature_lag3 = dplyr::lag(Temperature, 3), #temperature three weeks before
    Temperature_lag4 = dplyr::lag(Temperature, 4) #temperature four weeks before
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    tiempo_factor = factor(calendar_start_date), #time index for the AR(1) structure
    mes = factor(mes), #month factor for seasonality
    year = factor(year) #year factor for the long-term trend
  ) %>%
  tidyr::drop_na(
    brote_p90, Rain_lag2, Rain_lag3, Rain_lag4,
    Temperature_lag2, Temperature_lag3, Temperature_lag4,
    ln_casos_semana_anterior
  )

#models with the alternative threshold ----

#inertia-only model: epidemic memory plus the same time controls
modelo_inercia_p90 <- glmmTMB::glmmTMB(
  brote_p90 ~
    ln_casos_semana_anterior +
    mes + #monthly seasonality
    year + #long-term annual trend
    (1 | DEPARTAMENTO) + #random intercept by department
    ar1(tiempo_factor + 0 | DEPARTAMENTO), #temporal autocorrelation
  family = binomial(link = "logit"),
  data = dengue_p90
)

#climate model: inertia plus rainfall and temperature lagged 2 to 4 weeks
modelo_clima_p90 <- glmmTMB::glmmTMB(
  brote_p90 ~
    Rain_lag2 + Rain_lag3 + Rain_lag4 + #rainfall lags
    Temperature_lag2 + Temperature_lag3 + Temperature_lag4 + #temperature lags
    ln_casos_semana_anterior +
    mes +
    year +
    (1 | DEPARTAMENTO) +
    ar1(tiempo_factor + 0 | DEPARTAMENTO),
  family = binomial(link = "logit"),
  data = dengue_p90
)

#in-sample discrimination using fitted probabilities
auc_p90 <- function(modelo) {
  probabilidad <- stats::fitted(modelo) #fitted outbreak probabilities
  curva_roc <- pROC::roc(
    response = as.integer(dengue_p90$brote_p90 == "1"), #event indicator for ROC
    predictor = probabilidad,
    levels = c(0, 1),
    direction = "<",
    quiet = TRUE
  )
  as.numeric(pROC::auc(curva_roc)) #area under the ROC curve
}

#comparison against the inertia-only model
modelos_p90 <- list(
  "solo inercia" = modelo_inercia_p90,
  "inercia + clima" = modelo_clima_p90
)

comparacion_p90 <- purrr::imap_dfr(modelos_p90, \(modelo_ajustado, nombre) {
  tibble::tibble(
    modelo = nombre,
    AIC = stats::AIC(modelo_ajustado), #Akaike information criterion
    AUC = auc_p90(modelo_ajustado), #area under the ROC curve
    logLik = as.numeric(stats::logLik(modelo_ajustado)) #maximized log-likelihood
  )
}) %>%
  dplyr::mutate(
    delta_AIC = AIC[modelo == "solo inercia"] - AIC, #positive values mean a better fit
    delta_AUC = AUC - AUC[modelo == "solo inercia"] #positive values mean better discrimination
  )

print(comparacion_p90, width = Inf)

#likelihood ratio test for the joint contribution of the climate lags
print(stats::anova(modelo_inercia_p90, modelo_clima_p90))

