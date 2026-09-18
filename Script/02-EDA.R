rm(list = ls())

#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-16"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               DataExplorer, 
               skimr, 
               summarytools,
               ggplot2,
               feasts, 
               tsibble
)

dengue <- readr::read_csv("Data/Csv/dengue_dataframe.csv")

#eda ----

## report ----
skimr::skim(dengue)                 
DataExplorer::create_report(dengue) 

dengue %>%
  dplyr::group_by(DEPARTAMENTO) %>%
  dplyr::summarise(
    n_semanas = n(),
    min_fecha = min(calendar_start_date),
    max_fecha = max(calendar_start_date),
    dengue_medio = mean(dengue_total),
    dengue_sd = sd(dengue_total),
    dengue_max = max(dengue_total),
    pct_ceros = mean(dengue_total == 0) * 100
  ) %>%
  dplyr::arrange(desc(dengue_medio))


dengue_plot <- dengue %>%
  ggplot2::ggplot(aes(x = calendar_start_date, y = dengue_total)) +
  ggplot2::geom_line(color = "steelblue") +
  ggplot2::facet_wrap(~ DEPARTAMENTO, scales = "free_y", ncol = 3) +
  ggplot2::labs(x = NULL, y = "Casos de dengue") +
  ggplot2::theme_minimal()

print(dengue_plot)

rain_plot <- dengue %>%
  ggplot2::ggplot(aes(x = calendar_start_date, y = Rain)) +
  ggplot2::geom_line(color = "steelblue") +
  ggplot2::facet_wrap(~ DEPARTAMENTO, scales = "free_y", ncol = 3) +
  ggplot2::labs(x = NULL, y = "Lluvia") +
  ggplot2::theme_minimal()

print(rain_plot)

temperature_plot <- dengue %>%
  ggplot2::ggplot(aes(x = calendar_start_date, y = Temperature)) +
  ggplot2::geom_line(color = "steelblue") +
  ggplot2::facet_wrap(~ DEPARTAMENTO, scales = "free_y", ncol = 3) +
  ggplot2::labs(x = NULL, y = "Temperatura (grados Celsius)") +
  ggplot2::theme_minimal()

print(temperature_plot)

##seasonality ----

dengue_tsbl <- dengue %>%
  tsibble::as_tsibble(key = DEPARTAMENTO, index = calendar_start_date, regular = TRUE)

dengue_tsbl %>%
  tsibble::scan_gaps() %>%
  dplyr::count(DEPARTAMENTO, sort = TRUE)

dengue_tsbl %>%
  tsibble::scan_gaps() %>%
  dplyr::count(calendar_start_date, sort = TRUE)

dengue_tsbl_full <- dengue_tsbl %>%
  tsibble::fill_gaps(.full = TRUE) %>%
  tsibble::group_by_key() %>%
  dplyr::mutate(
    dengue_total = imputeTS::na_interpolation(dengue_total, option = "linear")
  ) %>%
  dplyr::ungroup()

dengue_stl <- dengue_tsbl_full %>%
  fabletools::model(STL(dengue_total ~ season(period = 52) + trend(window = 52))) %>%
  fabletools::components()

summary(dengue_stl)

##outliers ----

###z-score, if z > 3.5 are outliers  (Iglewicz & Hoaglin), maybe
outliers_stl <- dengue_stl %>%
  tibble::as_tibble() %>%
  dplyr::group_by(DEPARTAMENTO) %>%
  dplyr::mutate(
    mediana = median(remainder, na.rm = TRUE),
    mad = mad(remainder, na.rm = TRUE),
    z_rob = 0.6745 * (remainder - mediana) / mad,
    es_outlier = abs(z_rob) > 3.5
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(es_outlier) %>%
  dplyr::arrange(dplyr::desc(abs(z_rob)))

outliers_stl %>% dplyr::count(DEPARTAMENTO, sort = TRUE)

outlier_plot <- dengue_stl %>%
  tibble::as_tibble() %>%
  ggplot2::ggplot(ggplot2::aes(x = calendar_start_date, y = dengue_total)) +
  ggplot2::geom_line(color = "steelblue", alpha = 0.7) +
  ggplot2::geom_point(data = outliers_stl, color = "red", size = 1.5) +
  ggplot2::facet_wrap(~ DEPARTAMENTO, scales = "free_y", ncol = 3) +
  ggplot2::labs(x = NULL, y = "Casos de dengue") +
  ggplot2::theme_minimal()

print(outlier_plot)

if(F){
  "
  
  non-outliers, dataframe is clean
  
  "
}






