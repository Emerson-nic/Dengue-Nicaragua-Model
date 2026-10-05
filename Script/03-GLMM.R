rm(list = ls())

#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-16"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               glmmTMB, #generalized linear mixed model (GLMM)
               performance, #model diagnostics and evaluation metrics (vif)
               ggeffects,#marginal predictions and confidence interval estimation plot
               broom.mixed, #tidying model outputs into clean tabular formats for coefficients
               DHARMa, #evaluation of residuals
               patchwork, #combining charts  
               itsadug, #Dharma needs this
               # xml2,
               RcppRoll #Acum sum for this one
)

dengue <- readr::read_csv("Data/Csv/dengue_dataframe.csv")

# interpolation and variables ---- 

dengue_tsbl <- dengue %>%
  dplyr::mutate(DEPARTAMENTO = factor(DEPARTAMENTO)) %>%
  tsibble::as_tsibble(key = DEPARTAMENTO, index = calendar_start_date, regular = TRUE) %>%
  tsibble::fill_gaps(.full = TRUE) %>%
  tsibble::group_by_key() %>%
  dplyr::mutate(
    dengue_total = imputeTS::na_interpolation(dengue_total, option = "linear"),
    Temperature = imputeTS::na_interpolation(Temperature, option = "linear"),
    Rain = imputeTS::na_interpolation(Rain, option = "linear")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Niño = dplyr::case_when(
      Year %in% c(2015, 2016, 2019) ~ 1,
      TRUE ~ 0
    ),
    Niño = as.factor(Niño) 
  )

dengue_tsbl <- dengue_tsbl %>%
  tibble::as_tibble() %>%
  # dplyr::mutate(week = lubridate::isoweek(calendar_start_date)) %>%
  # dplyr::arrange(DEPARTAMENTO, calendar_start_date) %>%
  dplyr::group_by(DEPARTAMENTO) %>%
  dplyr::mutate(
    Rain_acc3 = RcppRoll::roll_sum(Rain, n = 3, align = "right", fill = NA),
    casos_semana_anterior = dplyr::lag(dengue_total, n = 1),
    ln_dengue = log(dengue_total+1),
    ln_casos_semana_anterior = dplyr::lag(ln_dengue, n = 1)
  ) 

dengue_tsbl$tiempo_factor <- as.factor(dengue_tsbl$calendar_start_date)

dengue_tsbl %>%
  dplyr::select(dengue_total) %>%
  dplyr::filter(dengue_total == 0) %>%
  tibble::as_tibble() %>%
  print(n=91)

dengue_tsbl <- dengue_tsbl %>%
  stats::na.omit(Rain_acc3)

dengue_preparado <- dengue_tsbl %>%
  dplyr::group_by(DEPARTAMENTO) %>%
  dplyr::mutate(
    threshold = floor(quantile(dengue_total, 0.75, na.rm = TRUE)),
    is_brote = as.factor(ifelse(dengue_total > threshold, 1, 0))
  ) %>%
  dplyr::ungroup()

tabla_umbrales <- dengue_preparado %>%
  dplyr::group_by(DEPARTAMENTO) %>%
  dplyr::summarise(
    n_semanas = dplyr::n(),
    threshold = dplyr::first(threshold),   
    min_casos = min(dengue_total, na.rm = TRUE),
    median_casos = median(dengue_total, na.rm = TRUE),
    max_casos = max(dengue_total, na.rm = TRUE),
    n_brotes = sum(is_brote == "1", na.rm = TRUE),
    pct_brotes = round(100 * mean(is_brote == "1", na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(threshold))

print(tabla_umbrales, n = 30)

dengue_etapa1 <- dengue_preparado

readr::write_csv(dengue_etapa1,"Data/Csv/Etapas_dengue.csv")

dengue_etapa2 <- dengue_preparado %>%
  dplyr::filter(is_brote == "1")

dengue_etapa2 <- dengue_etapa2 %>%
  dplyr::mutate(
    exceso_brote = round(dengue_total - threshold)
  )

# models ----

# dplyr::glimpse(dengue_etapa1)

## model 1 stage 1 ----

modelo_etapa_1 <- glmmTMB(
  is_brote ~
    Niño + 
    Rain_acc3 +             
    Temperature +            
    ln_casos_semana_anterior +
    (1 | DEPARTAMENTO) +  #random intercept by department
    ar1(tiempo_factor + 0 | DEPARTAMENTO), 
  family = binomial(link = "logit"),
  data = dengue_etapa1
)

summary(modelo_etapa_1)

#or ratios
effect_model_1 <- broom.mixed::tidy(
  modelo_etapa_1,
  effects = "fixed",
  component = "cond",
  exponentiate = TRUE,
  conf.int = TRUE
) |>
  dplyr::mutate(
    cambio_odds_porc  = (estimate - 1) * 100,
    ic_bajo_odds_porc = (conf.low - 1) * 100,
    ic_alto_odds_porc = (conf.high - 1) * 100
    #odd_porc is in percentage term
  )

if(F){
  "
  calculating the total variance in a dataframe 
  with more than 2k observations breaks r
  
  "
}

#pseudo R-squared
# print(performance::r2(modelo_etapa_1)) #this crash my pc

#vif
print(performance::check_collinearity(modelo_etapa_1))

#icc
# print(performance::icc(modelo_etapa_2))

#impact plot

plot_lluvia_1 <- ggeffects::ggpredict(modelo_etapa_1, 
                                    terms = "Rain_acc3 [all]",
                                    type = "fixed"
                                    # interval = "prediction"
                                    #bias_correction = TRUE #another crash
) |> 
  plot() +
  ggplot2::labs(
    title = "Impacto de la Precipitación en la Magnitud del Brote",
    x = "Lluvia Acumulada - 3 semanas (mm)",
    y = "Probabilidad de Brote"
  ) +
  ggplot2::theme_minimal()

# print(plot_lluvia_1)

plot_niño_1 <- ggeffects::ggpredict(modelo_etapa_1, 
                                  terms = "Niño [all]",
                                  #bias_correction = TRUE
) |> 
  plot() +
  ggplot2::labs(
    title = "Impacto del Niño en la Magnitud del Brote",
    x = "Evento Climatico del Niño (1 para Epoca del Niño)",
    y = "Probabilidad de Brote"
  ) +
  ggplot2::theme_minimal()

# print(plot_niño_1)

plot_casos_1 <- ggeffects::ggpredict(modelo_etapa_1, 
                                    terms = "ln_casos_semana_anterior [all]",
                                    #bias_correction = TRUE
) |> 
  plot() +
  ggplot2::labs(
    title = "Impacto de Casos en la Magnitud del Brote",
    x = "Casos de la Semena Anterior (ln)",
    y = "Probabilidad de Brote"
  ) +
  ggplot2::theme_minimal()

# print(plot_casos_1)

#marge plots

efectos_1 <- plot_lluvia_1 + plot_niño_1 + 
  patchwork::plot_layout(ncol = 2) +
  patchwork::plot_annotation(
    # title = "Probabilidad de Detonar un Brote Epidémico de Dengue",
    # subtitle = "Predicciones marginales (Componente Binomial del Hurdle Model)",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14))
  )

print(efectos_1)

ggplot2::ggsave(
  filename = "Plots/Modelo_1_Pobabilidad_del_brote_1.pdf",
  plot = efectos_1,
  width = 12,
  height = 5,
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = "Plots/Modelo_1_Pobabilidad_del_brote_2.pdf",
  plot = plot_casos_1,
  width = 12,
  height = 5,
  dpi = 300,
  bg = "white"
)

#residuals

set.seed(57971643) #if u are femboy call
res_dharma_etapa1 <- DHARMa::simulateResiduals(
  modelo_etapa_1,
  n = 1000, 
  plot = TRUE
)

pdf(
  file = "Plots/Modelo_1_redual_res.pdf", 
  width = 12, 
  height = 5
)

plot(res_dharma_etapa1)

dev.off()

DHARMa::testResiduals(res_dharma_etapa1)
DHARMa::testDispersion(res_dharma_etapa1)
DHARMa::testUniformity(res_dharma_etapa1)
DHARMa::testOutliers(res_dharma_etapa1)
DHARMa::testCategorical(res_dharma_etapa1, catPred = dengue_etapa1$Niño)

# DHARMa::plotResiduals(res_dharma_etapa1, form = dengue_etapa1$Rain_acc3)
# DHARMa::plotResiduals(res_dharma_etapa1, form = dengue_etapa1$Temperature)
# DHARMa::plotResiduals(res_dharma_etapa1, form = dengue_etapa1$ln_casos_semana_anterior)

#autocorrelation

var_espacial_1 <- glmmTMB::VarCorr(modelo_etapa_1)
matriz_ar1_1 <- attr(var_espacial_1$cond$DEPARTAMENTO.1, "correlation")
inercia_ar1_1 <- matriz_ar1_1[1, 2]

cat("rho:",inercia_ar1_1)

res_temporales_etapa1 <- DHARMa::recalculateResiduals(
  res_dharma_etapa1, 
  group = dengue_etapa1$calendar_start_date
)

DHARMa::testTemporalAutocorrelation(
  res_temporales_etapa1, 
  time = unique(dengue_etapa1$calendar_start_date)
)


## model 2 stage 2 ----

modelo_etapa_2 <- glmmTMB(
  dengue_total ~ 
    Niño + 
    Rain_acc3 +              
    Temperature +            
    ln_casos_semana_anterior +
    (1 | DEPARTAMENTO) +
    ar1(tiempo_factor + 0 | DEPARTAMENTO),
  family = truncated_nbinom2,
  data = dengue_etapa2
)

summary(modelo_etapa_2)

#extraction Incidence Rate Ratios (irr)

# coeficientes_log_2 <- glmmTMB::fixef(modelo_etapa_2)$cond
# irr_estimados_2 <- exp(coeficientes_log_2)
# intervalos_confianza_2 <- exp(confint(modelo_etapa_2, 
#                                       parm = "beta_")[, 1:2])
# 
# tabla_irr_2 <- cbind(IRR = irr_estimados_2, intervalos_confianza_2)
# print(round(tabla_irr_2, 6))

effect_model_2 <- broom.mixed::tidy(
  modelo_etapa_2,
  effects = "fixed",
  exponentiate = TRUE,
  conf.int = TRUE
) |>
  dplyr::mutate(
    cambio_porc = (estimate - 1) ,
    ic_bajo_porc = (conf.low - 1) ,
    ic_alto_porc = (conf.high - 1)
  )

if(F){
  "
  calculating the total variance in a dataframe 
  with more than 2k observations breaks r
  
  "
}

#pseudo R-squared
# print(performance::r2(modelo_etapa_2)) #this crash my pc 

#vif
print(performance::check_collinearity(modelo_etapa_2))

#icc
# print(performance::icc(modelo_etapa_2))

#impact plot

plot_lluvia <- ggeffects::ggpredict(modelo_etapa_2, 
                                    terms = "Rain_acc3 [all]",
                                    type = "fixed"
                                    # interval = "prediction"
                                    #bias_correction = TRUE #another crash
                                    ) |> 
  plot() +
  ggplot2::labs(
    title = "Impacto de la Precipitación en la Magnitud del Brote",
    x = "Lluvia Acumulada - 3 semanas (mm)",
    y = "Casos Esperados de Dengue"
  ) +
  ggplot2::theme_minimal()

# print(plot_lluvia)

plot_temp <- ggeffects::ggpredict(modelo_etapa_2, 
                                  terms = "Temperature [all]",
                                  #bias_correction = TRUE
                                  ) |> 
  plot() +
  ggplot2::labs(
    title = "Impacto de la Temperatura en la Magnitud del Brote",
    x = "Temperatura Promedio (°C)",
    y = "Casos Esperados de Dengue"
  ) +
  ggplot2::theme_minimal()

# print(plot_temp)

plot_casos <- ggeffects::ggpredict(modelo_etapa_2, 
                                     terms = "ln_casos_semana_anterior [all]",
                                     #bias_correction = TRUE
) |> 
  plot() +
  ggplot2::labs(
    title = "Impacto de Casos en la Magnitud del Brote",
    x = "Casos de la Semena Anterior (ln)",
    y = "Casos Esperados de Dengue"
  ) +
  ggplot2::theme_minimal()

#marge plots

efectos_2 <- plot_lluvia + plot_temp + plot_casos +
  patchwork::plot_layout(ncol = 3) +
  patchwork::plot_annotation(
    # title = "Impacto Microclimático en la Magnitud de Brotes de Dengue",
    # subtitle = "Predicciones marginales poblacionales (GLMM nbinom2)",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14))
  )

print(efectos_2)

# ggplot2::ggsave(
#   filename = "Modelo_2_Impacto_Climatico_Magnitud.pdf",
#   plot = figura_publicacion,
#   width = 12,
#   height = 5,
#   dpi = 300,
#   bg = "white"
# )

#residuals

set.seed(57971643) #if u are femboy call
res_dharma_etapa2 <- DHARMa::simulateResiduals(
  modelo_etapa_2,
  n = 1000, 
  plot = TRUE
)

DHARMa::testResiduals(res_dharma_etapa2)
DHARMa::testDispersion(res_dharma_etapa2)
# DHARMa::testZeroInflation(res_dharma_etapa2)
DHARMa::testOutliers(res_dharma_etapa2)
DHARMa::testUniformity(res_dharma_etapa2)
DHARMa::testQuantiles(res_dharma_etapa2)
DHARMa::plotResiduals(res_dharma_etapa2, form = fitted(modelo_etapa_2), quantreg = TRUE)

# DHARMa::plotResiduals(res_dharma_etapa2, form = dengue_etapa2$Rain_acc3)
# DHARMa::plotResiduals(res_dharma_etapa2, form = dengue_etapa2$Temperature)
# DHARMa::plotResiduals(res_dharma_etapa2, form = dengue_etapa2$ln_casos_semana_anterior)
# 
# DHARMa::plotResiduals(res_dharma_etapa2, form = dengue_etapa2$Temperature, quantreg = TRUE)
# DHARMa::plotResiduals(res_dharma_etapa2, form = dengue_etapa2$Rain_acc3, quantreg = TRUE)
# DHARMa::plotResiduals(res_dharma_etapa2, form = dengue_etapa2$ln_casos_semana_anterior, quantreg = TRUE)

DHARMa::plotQQunif(res_dharma_etapa2)
DHARMa::plotResiduals(res_dharma_etapa2, form = fitted(modelo_etapa_2))
DHARMa::testCategorical(res_dharma_etapa2, catPred = dengue_etapa2$Niño)
DHARMa::testCategorical(res_dharma_etapa2, catPred = dengue_etapa2$DEPARTAMENTO)

#autocorrelation

var_espacial_2 <- glmmTMB::VarCorr(modelo_etapa_2)
matriz_ar1_2 <- attr(var_espacial_2$cond$DEPARTAMENTO.1, "correlation")
inercia_ar1_2 <- matriz_ar1_2[1, 2]

cat("rho:",inercia_ar1_2)

res_temporales_etapa2 <- DHARMa::recalculateResiduals(
  res_dharma_etapa2, 
  group = dengue_etapa2$calendar_start_date 
)

DHARMa::testTemporalAutocorrelation(
  res_temporales_etapa2, 
  time = unique(dengue_etapa2$calendar_start_date)
)

### sentibility analisis model 2 ----

m2_disp1 <- update(modelo_etapa_2, dispformula = ~ ln_casos_semana_anterior)
m2_disp2 <- update(modelo_etapa_2, dispformula = ~ DEPARTAMENTO)
m2_noar <- update(modelo_etapa_2, . ~ . - ar1(tiempo_factor + 0 | DEPARTAMENTO))

dengue_etapa2$exceso0 <- dengue_etapa2$exceso_brote - 1
m2_exc <- update(modelo_etapa_2, exceso0 ~ ., family = nbinom2, data = dengue_etapa2)

#convergence
lista_modelos <- list(
  "0_base (tnbinom2 + AR1)" = modelo_etapa_2,
  "1_disp ~ lag" = m2_disp1,
  "2_disp ~ DEPARTAMENTO" = m2_disp2,
  "3_sin AR1" = m2_noar,
  "4_exceso (nbinom2 + AR1)" = m2_exc
)

convergencia <- purrr::imap_dfr(lista_modelos, \(m, nm) {
  tibble::tibble(
    modelo = nm,
    pdHess = isTRUE(m$sdr$pdHess),   # TRUE = Hessiana OK
    AIC = AIC(m),
    logLik = as.numeric(logLik(m))
  )
})
print(convergencia)

BIC(modelo_etapa_2, m2_disp1, m2_disp2, m2_noar#, m2_exc
    )

#coefficient table
sens_model_2 <- purrr::imap_dfr(lista_modelos, \(m, nm) {
  broom.mixed::tidy(
    m,
    effects = "fixed",
    component = "cond",           
    exponentiate = TRUE,
    conf.int = TRUE
  ) |>
    dplyr::mutate(modelo = nm)
}) |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::mutate(
    cambio_porc  = estimate - 1,
    ic_bajo_porc = conf.low - 1,
    ic_alto_porc = conf.high - 1,
    signif_95    = conf.low > 1 | conf.high < 1   
  )

tabla_sens <- sens_model_2 |>
  dplyr::mutate(
    irr_ic = sprintf("%.3f (%.3f-%.3f)", estimate, conf.low, conf.high)
  ) |>
  dplyr::select(term, modelo, irr_ic) |>
  tidyr::pivot_wider(names_from = modelo, values_from = irr_ic)
print(tabla_sens, width = Inf)


resumen_estabilidad <- sens_model_2 |>
  dplyr::filter(modelo != "4_exceso (nbinom2 + AR1)") |> 
  dplyr::group_by(term) |>
  dplyr::summarise(
    irr_min        = min(estimate),
    irr_max        = max(estimate),
    rango_relativo = (irr_max - irr_min) / irr_min,
    mismo_signo    = all(estimate > 1) | all(estimate < 1),
    signif_en_todos = all(signif_95),
    .groups = "drop"
  )
print(resumen_estabilidad)

#forest plot ----
fig_sens_2 <- ggplot2::ggplot(
  sens_model_2,
  ggplot2::aes(x = estimate, y = modelo, xmin = conf.low, xmax = conf.high)
) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  ggplot2::geom_pointrange() +
  ggplot2::facet_wrap(~ term, scales = "free_x") +
  ggplot2::labs(
    x = "IRR (IC 95%)", y = NULL,
    title = "Sensibilidad de los efectos - Modelo 2 (magnitud del brote)"
  ) +
  ggplot2::theme_bw()
print(fig_sens_2)

## model 3 dispformula ----

modelo_disp <- glmmTMB(
  dengue_total ~ 
    Niño + 
    Rain_acc3 +              
    Temperature +            
    ln_casos_semana_anterior +
    (1 | DEPARTAMENTO) +
    ar1(tiempo_factor + 0 | DEPARTAMENTO),
  dispformula = ~ ln_casos_semana_anterior,
  family = truncated_nbinom2,
  data = dengue_etapa2
)

summary(modelo_disp)


effect_model_2 <- broom.mixed::tidy(
  modelo_disp,
  effects = "fixed",
  exponentiate = TRUE,
  conf.int = TRUE
) |>
  dplyr::mutate(
    cambio_porc = (estimate - 1) ,
    ic_bajo_porc = (conf.low - 1) ,
    ic_alto_porc = (conf.high - 1)
  )

#pseudo R-squared
# print(performance::r2(modelo_disp)) #this crash my pc 

#vif
print(performance::check_collinearity(modelo_disp))

#icc
# print(performance::icc(modelo_disp))

#impact plot

plot_lluvia_disp <- ggeffects::ggpredict(modelo_disp, 
                                    terms = "Rain_acc3 [all]",
                                    type = "fixed"
                                    # interval = "prediction"
                                    #bias_correction = TRUE #another crash
) |> 
  plot() +
  ggplot2::labs(
    title = "Impacto de la Precipitación en la Magnitud del Brote",
    x = "Lluvia Acumulada - 3 semanas (mm)",
    y = "Casos Esperados de Dengue"
  ) +
  ggplot2::theme_minimal()

# print(plot_lluvia)

plot_temp_disp <- ggeffects::ggpredict(modelo_disp, 
                                  terms = "Temperature [all]",
                                  #bias_correction = TRUE
) |> 
  plot() +
  ggplot2::labs(
    title = "Impacto de la Temperatura en la Magnitud del Brote",
    x = "Temperatura Promedio (°C)",
    y = "Casos Esperados de Dengue"
  ) +
  ggplot2::theme_minimal()

# print(plot_temp)

plot_casos_disp <- ggeffects::ggpredict(modelo_disp, 
                                   terms = "ln_casos_semana_anterior [all]",
                                   #bias_correction = TRUE
) |> 
  plot() +
  ggplot2::labs(
    title = "Impacto de Casos en la Magnitud del Brote",
    x = "Casos de la Semena Anterior (ln)",
    y = "Casos Esperados de Dengue"
  ) +
  ggplot2::theme_minimal()

#marge plots

efectos_disp <- plot_lluvia + plot_temp + plot_casos +
  patchwork::plot_layout(ncol = 3) +
  patchwork::plot_annotation(
    # title = "Impacto Microclimático en la Magnitud de Brotes de Dengue",
    # subtitle = "Predicciones marginales poblacionales (GLMM nbinom2)",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14))
  )

print(efectos_disp)

# ggplot2::ggsave(
#   filename = "Modelo_2_Impacto_Climatico_Magnitud.pdf",
#   plot = figura_publicacion,
#   width = 12,
#   height = 5,
#   dpi = 300,
#   bg = "white"
# )

#residuals

set.seed(57971643) #if u are femboy call
res_dharma_disp <- DHARMa::simulateResiduals(
  modelo_disp,
  n = 1000, 
  plot = TRUE
)

DHARMa::testResiduals(res_dharma_disp)
DHARMa::testCategorical(res_dharma_etapa2, catPred = dengue_etapa2$Niño)
DHARMa::testCategorical(res_dharma_etapa2, catPred = dengue_etapa2$DEPARTAMENTO)

#autocorrelation

var_disp <- glmmTMB::VarCorr(modelo_disp)
matriz_ar1_disp <- attr(var_disp$cond$DEPARTAMENTO.1, "correlation")
inercia_ar1_disp <- matriz_ar1_disp[1, 2]

cat("rho:",inercia_ar1_disp)

res_temporales_disp <- DHARMa::recalculateResiduals(
  res_dharma_disp, 
  group = dengue_etapa2$calendar_start_date 
)

DHARMa::testTemporalAutocorrelation(
  res_temporales_disp, 
  time = unique(dengue_etapa2$calendar_start_date)
)

## model 4 hurdle----

# modelo_hurdle <- glmmTMB(
#   dengue_total ~ 
#     Niño + 
#     Rain_acc3 + 
#     Temperature + 
#     ln_casos_semana_anterior +
#     (1 | DEPARTAMENTO) + 
#     ar1(tiempo_factor + 0 | DEPARTAMENTO),
#   
#   ziformula = 
#     ~ Niño + 
#     Rain_acc3 + 
#     Temperature + 
#     ln_casos_semana_anterior,
#   family = nbinom2,
#   data = dengue_preparado
# )
# 
# summary(modelo_hurdle)

#It is better not to use model 3



