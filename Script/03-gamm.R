rm(list = ls())

#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-16"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               DHARMa, #evaluation of residuals
               mgcViz, #DHARMa use mgcViz
               mgcv, #generalized additive models
               gratia, #tools for extracting smoothed and derivative functions
               patchwork #combining charts  
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
  dplyr::ungroup()

dengue <- dengue_tsbl %>%
  tibble::as_tibble() %>%
  dplyr::mutate(week = lubridate::isoweek(calendar_start_date)) %>%
  dplyr::arrange(DEPARTAMENTO, calendar_start_date) %>%
  dplyr::group_by(DEPARTAMENTO) %>%
  dplyr::mutate(inicio_serie = dplyr::row_number() == 1) %>%
  dplyr::ungroup()

#MODELS ----

## model 1 bam - are there one? ----

model_magnitud<- bam(
  (dengue_total > 0) ~
    s(week, bs = "cc", k = 20) + #estacionalidad promedio nacional (ciclica)
    s(Temperature, k = 5) +
    s(week, DEPARTAMENTO, bs = "fs", k = 10) +
    s(Rain, k = 5) +
    s(Year, k = 9) +
    s(DEPARTAMENTO, bs = "re"),#intercepto aleatorio por depto
  family = binomial(),
  data = dengue,
  method = "fREML",
  knots = list(week = c(0, 53)),
  rho = 0.87,                       
  AR.start = dengue$inicio_serie,
  discrete = TRUE
)

#edf (effective degrees of freedom)
#if edf = 1 straight line
#if edf > 1 the impact is curved

#Ref.df (reference degrees of freedom)
#chi.sq + ref.df is used to check whether that 
#form is statistically significant

summary(model_magnitud)
gam.check(model_magnitud)

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

#   - QQ plot: points on the diagonal [x]
#   - resid vs fitted: flat cloud without a pattern [x]
#   - uniformidad: straight line on [0, 1] [x]

DHARMa::testResiduals(res_dharma_presencia)     
DHARMa::testUniformity(res_dharma_presencia)     
DHARMa::testDispersion(res_dharma_presencia)     
DHARMa::testZeroInflation(res_dharma_presencia)  
DHARMa::testOutliers(res_dharma_presencia)




## model 2 bam - how many? ----

model_magnitud <- bam(
  dengue_total ~
    s(week, bs = "cc", k = 20) +
    s(Temperature, k = 5) +
    s(week, DEPARTAMENTO, bs = "fs", k = 10) +
    s(Rain, k = 5) +
    s(Year, k = 9) +
    s(DEPARTAMENTO, bs = "re"),
  family = nb(),
  data = dengue %>% dplyr::filter(dengue_total > 0),
  method = "fREML",
  knots = list(week = c(0, 53)),
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
DHARMa::testZeroInflation(res_dharma_model_magnitud)  
DHARMa::testOutliers(res_dharma_model_magnitud)

