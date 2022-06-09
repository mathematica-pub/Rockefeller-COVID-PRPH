suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(readxl)
  library(dplyr)
  library(lubridate)
  library(openxlsx)
  library(stringr)
  #library(tabulizer)
  library(jsonlite)
  library(r2d3)
  library(leaflet)
  library(raster)
  library(sp)
  library(Cairo)
  library(zoo)
  library(DT)
  library(naniar)
  library(assertr)
  library(datasets)
  library(RSocrata)
  library(httr)
})

source(here::here("src/processing.R"), encoding = "utf-8")
options(error = browser)

df_dashboard <- write_data()
df_demo_site <- write_demo_site_data(min_date = min(df_dashboard$date),
                                     max_date = max(df_dashboard$date))

df_dashboard %>% 
  filter(is.na(population)) %>% 
  dplyr::select(region, subregion_1, subregion_2, subregion_3) %>% 
  unique() %>% 
  write_csv(here::here("data", "missing_population_geos.csv"))

assertthat::are_equal(df_dashboard %>% 
  filter(tpr > 1) %>% 
  nrow(), 0)
