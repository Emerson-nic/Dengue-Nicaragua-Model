
if(F){
  "
  
  VARIABLES:
  y = dengue
  temperature (celsius - mean)
  rain (sum)
  week , bs='cc'
  
  "
}

#load library ---- 
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-09-16"))
#this install 2026-09-03 librery
if (!require("pacman")) install.packages("pacman")

pacman::p_load(readxl,
               tidyverse,
               janitor,
               dplyr,
               zoo
)

#load csv ----

if(F){
  "
  
  CSV'S DENGUE  CITE:
  Clarke J, Lim A, Gupte P, Pigott DM, van Panhuis WG, Brady OJ. 
  OpenDengue: data from the OpenDengue database. Version [1.3]. 
  figshare; 2025. Available from: https://doi.org/10.6084/m9.figshare.24259573.
  
  Website: https://opendengue.org/
  
  
  "
}

##filter dengue ----

dengue_data <- readr::read_csv("Data/Data-Source/filtered_data_PAHO_1789598881378.csv")

dengue_week <- dengue_data %>%
  dplyr::filter(T_res == "Week")

dengue_no_se <- dengue_week %>%
  dplyr::rename(DEPARTAMENTO = "adm_1_name") %>%
  dplyr::select(DEPARTAMENTO,
                "calendar_start_date",
                "calendar_end_date",
                Year,
                dengue_total
                ) %>%
  tibble::as_tibble() %>%
  print(n=18)

dengue_na <- dengue_no_se %>%
  dplyr::filter(calendar_start_date >= zoo::as.Date("2014-01-05")) %>%
  tibble::as_tibble() %>%
  print(n =18)

dengue <- dengue_na %>%
  stats::na.omit(DEPARTAMENTO)


message("date range starts from 2014-01-05 to 2022-11-06")

dengue_tsbl <- dengue %>%
  dplyr::mutate(DEPARTAMENTO = factor(DEPARTAMENTO)) %>%
  tsibble::as_tsibble(key = DEPARTAMENTO, index = calendar_start_date, regular = TRUE) %>%
  tsibble::fill_gaps(.full = TRUE) %>%
  tsibble::group_by_key() %>%
  dplyr::mutate(
    dengue_total = imputeTS::na_interpolation(dengue_total, option = "linear"),
  ) %>%
  dplyr::ungroup() 

#with this interpolation the total is 8316 (before 8280)

#ONI 

Sys.setlocale("LC_TIME", "C") 

sst_url <- "https://www.cpc.ncep.noaa.gov/data/indices/wksst9120.for"
                             
lineas <- readLines(sst_url)
lineas <- lineas[grepl("^\\s*[0-9]{2}[A-Z]{3}[0-9]{4}", lineas)]

sst <- read.table(
  text = gsub("-", " -", lineas),     
  col.names = c("fecha", "Nino1+2_sst", "Nino1+2_ssta", 
                "Nino3_sst", "Nino3_ssta", "Nino34_sst", "Nino34_ssta", 
                "Nino4_sst", "Nino4_ssta")
)

nino_sem <- sst %>% 
  dplyr::mutate(
    fecha = as.Date(fecha, "%d%b%Y"),
    calendar_start_date = lubridate::floor_date(fecha, "week", week_start = 7)
  ) %>%
  dplyr::arrange(calendar_start_date) 

dplyr::glimpse(nino_sem)

nino <- nino_sem %>%
  dplyr::mutate(
    nino34_12s = RcppRoll::roll_mean(Nino34_ssta, n = 12, align = "right", fill = NA)
  ) %>%
  dplyr::select(calendar_start_date, nino34_12s)

#note
if(F){
  "
  SSTA_{t} = SST_{t} - SST_{climatology}
  where the climatology is the historical average
  
  "
}

#extract temperature 

if(F){
  "
  
  TEMPERATURE DATA FROM:
  Open-Meteo
  
  WEBSITE: https://open-meteo.com/en/docs/historical-weather-api
  
  
  "
}

coords_deptos <- tibble::tribble(
  ~DEPARTAMENTO, ~lat,  ~lon,
  "BILWI", 14.0351, -83.3888,
  "BOACO", 12.4722, -85.6586,
  "CARAZO", 11.8540, -86.2000,
  "CHINANDEGA",  12.6294, -87.1311,
  "CHONTALES",  12.1063, -85.3645,
  "ESTELI",  13.0918, -86.3538,
  "GRANADA", 11.9299, -85.9560,
  "JINOTEGA", 13.0988, -86.0022,
  "LEON",  12.4379, -86.8780,
  "MADRIZ", 13.4667, -86.5833,
  "MANAGUA", 12.1364, -86.2514,
  "MASAYA",  11.9744, -86.0942,
  "MATAGALPA", 12.9256, -85.9175,
  "NUEVA SEGOVIA", 13.6333, -86.4833,
  "REGION AUTONOMA DEL ATLANTICO SUR", 12.0137, -83.7635, # Bluefields
  "RIO SAN JUAN", 11.1097,  -84.7797, # San Carlos
  "RIVAS",  11.4372, -85.8263,
  "ZELAYA CENTRAL", 12.0000, -84.2833 # El Rama 
)

## temperature's script test ----

managua_url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive?",
  "latitude=12.1364&longitude=-86.2514",
  "&start_date=2014-01-01&end_date=2022-12-31",
  "&daily=temperature_2m_mean,precipitation_sum&timezone=auto&format=csv"
)

test_managua <- read_csv(managua_url, skip = 8,
                         col_names = FALSE, 
                         show_col_types = FALSE) 

colnames(test_managua)[1] <- "calendar_start_date"
colnames(test_managua)[2] <- "Temperature"
colnames(test_managua)[3] <- "Rain"

managuita <- test_managua[-(1:6), ]

#first date starts in 2014-01-05
#last date is 2022-11-12
managuita_date <- managuita %>%
  dplyr::filter(Temperature <= zoo::as.Date("2022-11-12"))
  

## temperature funcion ----

descargar_clima_depto <- function(depto, lat, lon) {
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", lat, "&longitude=", lon,
    "&start_date=2014-01-01&end_date=2022-12-31",
    "&daily=temperature_2m_mean,precipitation_sum&timezone=auto&format=csv"
  )
  
  Sys.sleep(10) #wait 2 seconds
  
  lineas <- readr::read_lines(url)
  
  lineas_datos <- lineas[grepl("^\\d{4}-\\d{2}-\\d{2}", lineas)]
  
  df <- readr::read_csv(
    I(lineas_datos),
    col_names = c("fecha", "temp", "rain"),
    show_col_types = FALSE
  ) %>%
    dplyr::mutate(
      fecha = as.Date(fecha),
      calendar_start_date = lubridate::floor_date(fecha, unit = "week", week_start = 7),
      DEPARTAMENTO = depto
    ) %>%
    dplyr::group_by(DEPARTAMENTO, calendar_start_date) %>%
    dplyr::summarise(
      Temperature = mean(temp, na.rm = TRUE),
      Rain = sum(rain, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(calendar_start_date <= as.Date("2022-11-12"))
  
  return(df)
}

clima_panel <- purrr::pmap_dfr(
  list(coords_deptos$DEPARTAMENTO, coords_deptos$lat, coords_deptos$lon),
  descargar_clima_depto
)

#margining all dataframes ----

denguito <- dengue %>%
  dplyr::inner_join(clima_panel,
                    by = c("calendar_start_date", "DEPARTAMENTO"))

denguesote <- denguito %>%
  dplyr::inner_join(nino, by = "calendar_start_date")


readr::write_csv(denguesote, "Data/Csv/dengue_dataframe.csv")

