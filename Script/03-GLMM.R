#rm(list = ls())

#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-16"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               glmmTMB, 
               performance,
               ggeffects,
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
    threshold = quantile(dengue_total, 0.75, na.rm = TRUE),
    is_brote = as.factor(ifelse(dengue_total > threshold, 1, 0))
  ) %>%
  dplyr::ungroup()


dengue_etapa1 <- dengue_preparado

dengue_etapa2 <- dengue_preparado %>%
  dplyr::filter(is_brote == "1")

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

#residuals

res_dharma_etapa1 <- DHARMa::simulateResiduals(
  modelo_etapa_1,
  n = 1000, 
  plot = TRUE
)

DHARMa::testResiduals(res_dharma_etapa1)
DHARMa::testDispersion(res_dharma_etapa1)

DHARMa::plotResiduals(res_dharma_etapa1, form = dengue_etapa1$Rain_acc3)
DHARMa::plotResiduals(res_dharma_etapa1, form = dengue_etapa1$Temperature)
DHARMa::plotResiduals(res_dharma_etapa1, form = dengue_etapa1$ln_casos_semana_anterior)

#autocorrelation
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
  family = nbinom2(link = "log"),
  data = dengue_etapa2
)

summary(modelo_etapa_2)

#extraction Incidence Rate Ratios (irr)

coeficientes_log <- glmmTMB::fixef(modelo_etapa_2)$cond
irr_estimados <- exp(coeficientes_log)
intervalos_confianza <- exp(confint(modelo_etapa_2, parm = "beta_")[, 1:2])

tabla_irr <- cbind(IRR = irr_estimados, intervalos_confianza)
print(round(tabla_irr, 3))

#pseudo R-squared
print(performance::r2(modelo_etapa_2))

#vif
print(performance::check_collinearity(modelo_etapa_2))

#impact plot

plot_lluvia <- ggeffects::ggpredict(modelo_etapa_2, terms = "Rain_acc3 [all]") |> 
  plot() +
  ggplot2::labs(
    title = "Impacto de la Precipitación en la Magnitud del Brote",
    x = "Lluvia Acumulada - 3 semanas (mm)",
    y = "Casos Esperados de Dengue"
  ) +
  ggplot2::theme_minimal()

print(plot_lluvia)

plot_temp <- ggeffects::ggpredict(modelo_etapa_2, terms = "Temperature [all]") |> 
  plot() +
  ggplot2::labs(
    title = "Impacto de la Temperatura en la Magnitud del Brote",
    x = "Temperatura Promedio (°C)",
    y = "Casos Esperados de Dengue"
  ) +
  ggplot2::theme_minimal()

print(plot_temp)

#residuals

res_dharma_etapa2 <- DHARMa::simulateResiduals(
  modelo_etapa_2,
  n = 1000, 
  plot = TRUE
)

DHARMa::testResiduals(res_dharma_etapa2)
DHARMa::testDispersion(res_dharma_etapa2)

DHARMa::plotResiduals(res_dharma_etapa2, form = dengue_etapa2$Rain_acc3)
DHARMa::plotResiduals(res_dharma_etapa2, form = dengue_etapa2$Temperature)
DHARMa::plotResiduals(res_dharma_etapa2, form = dengue_etapa2$ln_casos_semana_anterior)

#autocorrelation
res_temporales_etapa2 <- DHARMa::recalculateResiduals(
  res_dharma_etapa2, 
  group = dengue_etapa2$calendar_start_date 
)

DHARMa::testTemporalAutocorrelation(
  res_temporales_etapa2, 
  time = unique(dengue_etapa2$calendar_start_date)
)
