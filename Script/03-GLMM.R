#rm(list = ls())

#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-16"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               glmmTMB, 
               performance,
               ggeffects,
               broom.mixed,
               DHARMa, #evaluation of residuals
               mgcViz, #DHARMa use mgcViz
              # gratia, #tools for extracting smoothed and derivative functions for spines
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

effect_model_1 <- broom.mixed::tidy(
  modelo_etapa_1,
  effects = "fixed",
  exponentiate = TRUE,
  conf.int = TRUE
) |>
  dplyr::mutate(
    cambio_pct = (estimate - 1) * 100,
    ic_bajo_pct = (conf.low - 1) * 100,
    ic_alto_pct = (conf.high - 1) * 100
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

# print(plot_temp_1)

#marge plots

efectos_1 <- plot_lluvia_1 + plot_niño_1 + plot_casos_1 + 
  patchwork::plot_layout(ncol = 3) +
  patchwork::plot_annotation(
    # title = "Probabilidad de Detonar un Brote Epidémico de Dengue",
    # subtitle = "Predicciones marginales (Componente Binomial del Hurdle Model)",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14))
  )

print(efectos_1)

# ggplot2::ggsave(
#   filename = "Modelo_1_Pobabilidad_del_brote.pdf",
#   plot = figura_publicacion,
#   width = 12,
#   height = 5,
#   dpi = 300,
#   bg = "white"
# )

#residuals

set.seed(57971643) #if u are femboy call
res_dharma_etapa1 <- DHARMa::simulateResiduals(
  modelo_etapa_1,
  n = 1000, 
  plot = TRUE
)

DHARMa::testResiduals(res_dharma_etapa1)
DHARMa::testDispersion(res_dharma_etapa1)
DHARMa::testUniformity(res_dharma_etapa1)
DHARMa::testOutliers(res_dharma_etapa1)

# DHARMa::plotResiduals(res_dharma_etapa1, form = dengue_etapa1$Rain_acc3)
# DHARMa::plotResiduals(res_dharma_etapa1, form = dengue_etapa1$Temperature)
# DHARMa::plotResiduals(res_dharma_etapa1, form = dengue_etapa1$ln_casos_semana_anterior)

#autocorrelation

var_espacial_1 <- glmmTMB::VarCorr(modelo_etapa_2)
matriz_ar1_1 <- attr(var_espacial_2$cond$DEPARTAMENTO.1, "correlation")
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
    cambio_pct = (estimate - 1) * 100,
    ic_bajo_pct = (conf.low - 1) * 100,
    ic_alto_pct = (conf.high - 1) * 100
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

## model 3 hurdle----

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
