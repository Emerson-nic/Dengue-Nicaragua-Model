#rm(list = ls())

#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-16"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               DHARMa, #evaluation of residuals
               mgcViz, #DHARMa use mgcViz
               mgcv, #generalized additive models
               gratia, #tools for extracting smoothed and derivative functions
               patchwork, #combining charts  
               itsadug, #Dharma needs this
               RcppRoll #Acum sum for this one
)

dengue <- readr::read_csv("Data/Csv/dengue_dataframe.csv")

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

dengue <- dengue_tsbl %>%
  tibble::as_tibble() %>%
  dplyr::mutate(week = lubridate::isoweek(calendar_start_date)) %>%
  dplyr::arrange(DEPARTAMENTO, calendar_start_date) %>%
  dplyr::group_by(DEPARTAMENTO) %>%
  dplyr::mutate(
    Rain_acc3 = RcppRoll::roll_sum(Rain, n = 3, align = "right", fill = NA),
    casos_semana_anterior = dplyr::lag(dengue_total, n = 1)
  ) %>%
  dplyr::mutate(inicio_serie = dplyr::row_number() == 1) %>%
  dplyr::ungroup()

non_zeros <- dengue_tsbl %>%
  dplyr::filter(dengue_total > 0)

non_zeros <- non_zeros %>%
  tibble::as_tibble() %>%
  dplyr::mutate(week = lubridate::isoweek(calendar_start_date)) %>%
  dplyr::arrange(DEPARTAMENTO, calendar_start_date) %>%
  dplyr::group_by(DEPARTAMENTO) %>%
  dplyr::mutate(
    Rain_acc3 = RcppRoll::roll_sum(Rain, n = 3, align = "right", fill = NA),
    casos_semana_anterior = dplyr::lag(dengue_total, n = 1)
  ) %>%
  dplyr::mutate(inicio_serie = dplyr::row_number() == 1) %>%
  dplyr::ungroup()

dengue <- dengue %>%
  tidyr::drop_na(Rain_acc3) %>%
  dplyr::group_by(DEPARTAMENTO) %>%
  dplyr::mutate(inicio_serie = dplyr::row_number() == 1) %>%
  dplyr::ungroup()

non_zeros <- non_zeros %>%
  tidyr::drop_na(Rain_acc3) %>%
  dplyr::mutate(inicio_serie = dplyr::row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Year = dplyr::coalesce(Year, lubridate::year(calendar_start_date)))

dplyr::glimpse(non_zeros)

#mamita, filter zeros only remove 91 obs
#so now the final dataframe has 8189 obs, its great

#MODELS ----
#this model is only for get the rho
model_banano <- bam(
  (dengue_total > 0) ~
    Niño +
    s(Temperature, k = 8) +
    s(casos_semana_anterior, k=8) +
    s(week, bs = "cc", k = 20)+ #estacionalidad promedio nacional por dep (ciclica) 
    s(Rain, k = 8) +
    s(Rain_acc3, k = 8) +
    s(Year, k = 9) +
    s(DEPARTAMENTO, bs = "re"),#intercepto aleatorio por depto
  # family = binomial(),
  data = dengue,
  method = "fREML",
  knots = list(week = c(0, 52)),
  discrete = TRUE
)

# summary(model_banano)
rho_estimado <- itsadug::start_value_rho(model_banano)
print(rho_estimado)

# itsadug::acf_resid(
#   model_banano,
#   split_pred = "DEPARTAMENTO",
#   main = "ACF residuos por departamento"
# )


## model 1 bam - are there one? ----

model_magnitud<- bam(
  (dengue_total > 0) ~
    Niño +
    s(casos_semana_anterior, k=40) +
    s(Temperature, k = 8) +
    s(week, bs = "cc", k = 20)+ #estacionalidad promedio nacional por dep (ciclica) 
    # s(Rain, k = 8) +
    s(Rain_acc3, k = 8) +
    s(Year, k = 9) +
    s(DEPARTAMENTO, bs = "re"),#intercepto aleatorio por depto
  family = binomial(),
  data = dengue,
  method = "fREML",
  knots = list(week = c(0, 52)),
  # rho = rho_estimado,
  # AR.start = dengue$inicio_serie,
  discrete = TRUE
)

#edf (effective degrees of freedom)
#if edf = 1 straight line
#if edf > 1 the impact is curved

#Ref.df (reference degrees of freedom)
#chi.sq + ref.df is used to check whether that 
#form is statistically significant

summary(model_magnitud)
mgcv::gam.check(model_magnitud)

gratia::draw(model_magnitud, residuals = TRUE)
gratia::draw(model_magnitud, residuals = F)

gratia::draw(model_magnitud,ci_level=0.95, select = "s(Rain)", residuals = TRUE)

gratia::draw(model_magnitud, 
             select = "s(casos_semana_anterior)", 
             ci_level = 0.95, 
             residuals = F,
             constant = coef(model_magnitud)[1], 
             fun = plogis)

gratia::draw(model_magnitud,ci_level=0.95, select = "s(casos_semana_anterior)", residuals = F)

derivas_model_magnitud <- gratia::derivatives(model_magnitud)
# print(derivas_model_magnitud)
gratia::draw(derivas_model_magnitud)

#concurvity() measures the equivalent of multicollinearity. 
#the metric ranges from 0 (perfect) to 1 (total redundancy).
mgcv::concurvity(model_magnitud, full = T)
mgcv::concurvity(model_magnitud, full = F)

#autocorrelation
itsadug::acf_resid(
  model_magnitud,
  split_pred = "DEPARTAMENTO",
  main = "ACF residuos por departamento"
)

#residuals
res_dharma_presencia <- DHARMa::simulateResiduals(
  model_magnitud,
  n = 1000,
  plot = TRUE   
)

# res_t_presencia <- DHARMa::recalculateResiduals(res_dharma_presencia, 
#                                                 group = model_magnitud$model$DEPARTAMENTO
#                                                 )
# plot(res_t_presencia)

#   - QQ plot: points on the diagonal [x]
#   - resid vs fitted: flat cloud without a pattern [x]
#   - uniformidad: straight line on [0, 1] [x]

DHARMa::testResiduals(res_dharma_presencia)     
DHARMa::testZeroInflation(res_dharma_presencia)  


## model 2 bam - how many? ----

model_magnitud <- bam(
  dengue_total ~
    Niño +
    s(log(casos_semana_anterior), k=20) +
    s(week, bs = "cc", k = 20) +
    s(Temperature, k = 5) +
    #s(week, DEPARTAMENTO, bs = "fs", k = 10) +
    # s(Rain, k = 5) +
    s(Year, k = 9) +
    s(Rain_acc3, k = 8) +
    s(DEPARTAMENTO, bs = "re"),
  family = tw(),
  data = non_zeros,
  method = "fREML",
  knots = list(week = c(0, 53)),
  # rho = 0.87,
  # AR.start = non_zeros$inicio_serie,
  discrete = TRUE
)

summary(model_magnitud)
gam.check(model_magnitud)


mgcv::concurvity(model_magnitud, full = T)
mgcv::concurvity(model_magnitud, full = F)

#autocorrelation
itsadug::acf_resid(
  model_magnitud,
  split_pred = "DEPARTAMENTO",
  main = "ACF residuos por departamento"
)

#residuals
res_dharma_model_magnitud <- DHARMa::simulateResiduals(
  model_magnitud,
  n = 1000,
  plot = TRUE
)

#   - QQ plot: points on the diagonal [x]
#   - resid vs fitted: flat cloud without a pattern [x]
#   - uniformidad: straight line on [0, 1] [x]

DHARMa::testResiduals(res_dharma_model_magnitud)
DHARMa::testUniformity(res_dharma_model_magnitud)
DHARMa::testDispersion(res_dharma_model_magnitud)
# DHARMa::testZeroInflation(res_dharma_model_magnitud)
DHARMa::testOutliers(res_dharma_model_magnitud)

DHARMa::testCategorical(res_dharma_model_magnitud, non_zeros$DEPARTAMENTO)
DHARMa::plotResiduals(res_dharma_model_magnitud, non_zeros$Year)
DHARMa::plotResiduals(res_dharma_model_magnitud, non_zeros$week)
