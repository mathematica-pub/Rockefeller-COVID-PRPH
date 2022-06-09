suppressWarnings(suppressPackageStartupMessages({
  
  # shiny
  library(shiny)
  library(shinyalert)
  library(shinydashboard)
  library(shinyjs)
  library(shinythemes)
  library(shinyWidgets)

  # tidyverse
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  
  # geo
  library(leaflet)
  library(sf)
  
  # viz
  library(Cairo)
  library(naniar)
  library(plotly)
  library(r2d3)
  
  # utility
  library(configr)
  library(data.table)
  library(DT)
  library(here)
  library(readxl)
  library(zoo)
  
}))

options(
  lifecycle_disable_warnings = T,
  lifecycle_verbosity        = "quiet"
)


# global parameters ----------------------------------------------------------
source(here::here("src/helper.R"))
source(here::here("src/map_helper.R"))

today <- Sys.Date()

# geospatial assets
geo_sf_lvl0 <- readRDS(here::here("data", "shapefiles", "lvl-0-country.Rds")) %>% mutate(NAME_0 = case_when(NAME_0 == "Swaziland" ~ "Eswatini",
                                                                                                   TRUE ~ NAME_0))
geo_sf_lvl1 <- readRDS(here::here("data", "shapefiles", "lvl-1-sub-national.Rds"))
geo_sf_lvl2 <- readRDS(here::here("data", "shapefiles", "lvl-2-district.Rds"))
geo_sf_lvl3 <- data.table::fread(here::here("data", "geography-demo-site-coordinates.csv"))

# indicator definitions
# indicators_raw must be defined before text.R
core_indicators_raw <- readxl::read_xlsx("data/indicators.xlsx", sheet = 1)
core_indicators <- build_indicator_html(core_indicators_raw, core = TRUE)
sup_indicators_raw <- readxl::read_xlsx("data/indicators.xlsx", sheet = 2) 
sup_indicators <- build_indicator_html(sup_indicators_raw, core = FALSE, 
                                       corelist = c( "BIPOC share of vaccinations to date, at least one dose",
                                                     "Proportion Fully Vaccinated in the County (All Ages)",
                                                     "Proportion BIPOC vaccinated (ages 5+, at least one dose), estimated",
                                                     "Proportion vaccinated by race/ethnicity (ages 5+, at least one dose), estimated",
                                                     "Proportion of Vaccinations with Race/Ethnicity Information Reported",
                                                     "Total BIPOC Population vaccinated, at least one dose",
                                                     "Proportion Who Would Accept a Covid-19 Vaccine"))
indicators_raw <- bind_rows(core_indicators_raw, sup_indicators_raw)

source(here::here("src/text.R"))

pop_year <- 2019 # year used for denominator of per-capita indicators
cases_threshold1 <- 10 # cases threshold of 10 per million
cases_threshold2 <- 100 # cases threshold of 100 per million
cases_high_threshold <- 250 # red level of cases on map
tpr_threshold1 <- 0.05 # tpr threshold of 0.05 (5%)
tpr_threshold2 <- 0.075 # tpr threshold of 0.05 (5%)
tpr_high_threshold <- 0.10 # red level of cases on map
test_threshold_case_multiplier <- 20 # value multiplied by active cases to determine testing threshold
test_threshold_minimum <- 200 # lowest valid testing threshold (per million)
test_threshold_multiplier_low <- 10 # to identify "red" level threshold on map, multiply active cases by this number
vpeople_high_threshold <- 0.7 # "blue" (>70%) and "yellow" (60-70%) on map
vpeople_threshold1 <- 0.4 # "orange" (40-60%) and "red" (<40%) on map
vpeople_threshold2 <- 0.6 # new threshold - "orange" (40-60%) and "yellow" (60-70%) on map
tevx_threshold <- 90000000
vpeople_display_threshold <- .7 # threshold requested by RF for line graphs
vaccine_accept_threshold <- 0.9
bipoc_vacc_pct_threshold1 <- 0.4
bipoc_vacc_pct_threshold2 <- 0.6
bipoc_vacc_pct_threshold3 <- 0.7
booster_fully_vac_threshold <- 0.15 # threshold of prop fully vaccinated to show a boosted line
vacc_acc_threshold <- 0.7 # vaccine acceptance: Proportion who will "probably" or "definitely" get vaccinated

one_million <- 1000000
rolling_days <- 7
pandemic_first_date <- ymd("2020-03-01") # our interpretation of date pandemic began
num_waves <- 7 # TODO: use these wave parameters in data processing
first_wave_date <- ymd("2020-07-19")

# list of pages on the region level that consist of a single country.
# these are treated differently from other regions
one_country_regions <- c("India", "United States")

wave_dates <- c(first_wave_date) 
for (x in 2:num_waves){
  wave_dates <- c(wave_dates, first_wave_date + 14*(x-1))  
}

theme_minimal2 <- function (base_size = 11, base_family = "", base_line_size = base_size/22,
                            base_rect_size = base_size/22)
{
  theme_bw(base_size = base_size, base_family = base_family,
           base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(legend.background = element_blank(),
          axis.ticks.y = element_blank(),
          legend.key = element_blank(), panel.background = element_blank(),
          panel.border = element_blank(), strip.background = element_blank(),
          plot.background = element_blank(), complete = TRUE)
}

# define colors
red <- "#D02B27"
navy <- "#0B2949"
yellow <- "#F1B51C"

light_grey <- "#bfbfbf"
dark_grey <- "#5B6771"
green <- "#7fa29a"
beige <- "#E0D4B5"
light_green <- "#17A673"
teal <- "#189394"
orange <- "#ef8302"
gold <- "#f7ab00"
electric_teal <- "#21ccfb"

bg_color <- "#F5F1E8"

# title style for secondary indicator charts
sub_chart_title_style <- "
        font-size:16px;
        font-weight: 600;
        letter-spacing:1px;
        text-align: left;
        display:table-cell; 
        vertical-align:middle; 
        color:#000000; 
        text-decoration: underline;"

# helps render plots nicely
options(shiny.usecairo=TRUE)

DASH_LEVELS = c("Region", "Country", "Sub-national")
TIME_LEVELS <- c("Two weeks", "One month", "Three months", "Six months", "Twelve months", "Eighteen months", "Since pandemic began")
DISTRICT_LEVELS <- c("TOTAL")

beh_map <- c(
  "Mask wearing" = "wear_mask",
  "Hand washing" = "wash_hands", 
  "Self-isolating" = "self_isolate", 
  "Physical distancing" = "phys_dist"
)
beh_map_wrap <- c(
  "Mask \nwearing" = "wear_mask",
  "Hand \nwashing" = "wash_hands", 
  "Self- \nisolating" = "self_isolate", 
  "Physical \ndistancing" = "phys_dist"
)
vac_map <- c(
  "Vaccinated or would take vaccine if available" = "vaccine_accept"
)
vac_map_wrap <- c(
  "Vaccinated or would take \nvaccine if available" = "vaccine_accept"
)
vac_map_census <- c(
  'Vaccinated or would "definitely" take vaccine if available' = "vaccine_accept"
)
vac_map_census_wrap <- c(
  'Vaccinated or would "definitely" \ntake vaccine if available' = "vaccine_accept"
)
inp_map <- c(
  "Percent occupied, all patients" = "ip_beds_percent",
  "Percent occupied, COVID patients" = "covid_beds_percent"
)
icu_map <- c(
  "Percent occupied, all patients" = "icu_beds_percent"
)
tevx_map <- c(
  "Estimated, including vaccinations \nwith imputed race/ethnicity" = "bipoc_cum_people_vacc_est",
  "Among vaccinations with \nrace/ethnicity reported to state agencies" = "bipoc_cum_people_vacc_rpt_kff",
  "Among vaccinations with \nrace/ethnicity reported to CDC" = "bipoc_cum_people_vacc_rpt"
)
tevx_map_s <- c(
  "Estimated, including vaccinations \nwith imputed race/ethnicity" = "bipoc_cum_people_vacc_est",
  "Among vaccinations with \nrace/ethnicity reported to state agencies" = "bipoc_cum_people_vacc_rpt"
)
pevx_map <- c(
  "Estimated, including vaccinations \nwith imputed race/ethnicity" = "bipoc_pct_vacc_est",
  "Among vaccinations with \nrace/ethnicity reported to state agencies" = "bipoc_pct_vacc_rpt_kff",
  "Among vaccinations with \nrace/ethnicity reported to CDC" = "bipoc_pct_vacc_rpt"
)
pevx_map_s <- c(
  "Estimated, including vaccinations \nwith imputed race/ethnicity" = "bipoc_pct_vacc_est",
  "Among vaccinations with \nrace/ethnicity reported to state agencies" = "bipoc_pct_vacc_rpt"
)
revx_map <- c(
  "White" = "white_pct_vacc_est",
  "Asian" = "asian_pct_vacc_est",
  "Black" = "black_pct_vacc_est",
  "Hispanic" = "hispanic_pct_vacc_est",
  "American Indian/Alaskan Native" = "ai_an_pct_vacc_est",
  "Native Hawaiian/Pacific Islander" = "nh_pi_pct_vacc_est"
)
beh_metrics <- unname(beh_map)
vac_metrics <- unname(vac_map)

vpeople_map <- c("At least one dose" = "prop_vpeople",
                 "Fully vaccinated" = "prop_vpeople_full",
                 "Booster" = "prop_vpeople_boost")

prob_def_vacc_map <- c("Already vaccinated or will \"definitely\" or \"probably\"\nget a vaccine when available (among all survey respondents)" = "vaccinated_appointment_or_accept",
                       "Will \"definitely\" or \"probably\" get a vaccine when\navailable (among the unvaccinated only)" = "appointment_or_accept_covid_vaccine")

vacc_elig_map <- c("\nNo one or no information\nabout eligibility" = 1, "High risk groups only" = 2, "Everyone" = 3) 


# dictionaries ----------------------------------------------------------
df_region_xwalk <- get_region_xwalk() %>% rename(subregion_1 = country)

umd_vax_acpt_xwalk <- read_csv(here::here("data/umd_vaccine_accept_indicators.csv"),
                               col_types = cols_only(Indicator = col_character(), Description = col_character(),
                                                     Use = col_character())) %>%
  filter(Use != "no") 


df_dashboard <- 
  data.table::fread(here::here("data", "df_dashboard.csv")) %>%
  mutate(date              = as.Date(date,              format = "%Y-%m-%d"),
         recent_behav_date = as.Date(recent_behav_date, format = "%Y-%m-%d")) %>%
  # until we add more data, filter out "unkown" districts, and USA
  # also restrict to date since pandemic began
  filter(subregion_1 != "Unknown",
         subregion_2 != "Unknown",
         subregion_3 != "Unknown",
         # indian state level data includes a category for all of india
         # that we should not present on the dashboard
         subregion_2 != "India",
         date >= pandemic_first_date) %>%
  mutate(prop_vpeople = cum_people_vacc/population) %>%
  mutate(prop_vpeople_full = people_fully_vaccinated/population) %>%
  mutate(prop_vpeople_boost = total_boosters/population) %>%
  mutate(white_pct_vacc_est = if_else(date <= "2021-06-13" & subregion_2 == "Florida", NA_real_, white_pct_vacc_est),         
         black_pct_vacc_est = if_else(date <= "2021-06-13" & subregion_2 == "Florida", NA_real_, black_pct_vacc_est),         
         asian_pct_vacc_est = if_else(date <= "2021-06-13" & subregion_2 == "Florida", NA_real_, asian_pct_vacc_est),         
         hispanic_pct_vacc_est = if_else(date <= "2021-06-13" & subregion_2 == "Florida", NA_real_, hispanic_pct_vacc_est),     
         hispanic_pct_vacc_rpt = if_else(date <= "2021-06-13" & subregion_2 == "Florida", NA_real_, hispanic_pct_vacc_rpt),
         ai_an_pct_vacc_est = if_else(date <= "2021-06-13" & subregion_2 == "Florida", NA_real_, ai_an_pct_vacc_est),
         nh_pi_pct_vacc_est = if_else(date <= "2021-06-13" & subregion_2 == "Florida", NA_real_, nh_pi_pct_vacc_est),
         bipoc_pct_vacc_est = if_else(date <= "2021-06-13" & subregion_2 == "Florida", NA_real_, bipoc_pct_vacc_est),        
         bipoc_pct_vacc_rpt = if_else(date <= "2021-06-13" & subregion_2 == "Florida", NA_real_, bipoc_pct_vacc_rpt),         
         bipoc_cum_people_vacc_est = if_else(date <= "2021-06-13" & subregion_2 == "Florida", NA_real_, bipoc_cum_people_vacc_est),  
         bipoc_cum_people_vacc_rpt = if_else(date <= "2021-06-13" & subregion_2 == "Florida", NA_real_, bipoc_cum_people_vacc_rpt),
         bipoc_pct_of_vacc = if_else(date <= "2021-06-13" & subregion_2 == "Florida", NA_real_, bipoc_pct_of_vacc),
         bipoc_pct_of_pop = if_else(date <= "2021-06-13" & subregion_2 == "Florida", NA_real_, bipoc_pct_of_pop),
         asian_pct_vacc_est = if_else(subregion_2 == "Pennsylvania", NA_real_, asian_pct_vacc_est)) 

df_demo_site <- 
  data.table::fread(here::here("data","df_demo_site.csv")) %>%
  mutate(date              = as.Date(date,              format = "%Y-%m-%d"),
         recent_behav_date = as.Date(recent_behav_date, format = "%Y-%m-%d"))

# assign default UI info based on df characteristics
DATE_RANGE <- seq(min(df_dashboard$date), max(df_dashboard$date), by="days")
source(here::here("src/modules.R"))

# create global list of valid countries in the DF dashboard file
country_list <- unique(df_dashboard$subregion_1)

# identify the list of countries for which subnational data are available
 subnat_countries <- df_dashboard %>% 
   filter(subregion_2 != "TOTAL") %>% 
   filter(subregion_1 == "United States") %>%
   arrange(subregion_1) %>% 
   pull(subregion_1) %>% 
   unique()

# identify all states in the US to use as conditions for subnational
# displays that are restricted to US only
us_states_list <- df_dashboard %>%
  filter(subregion_2 != "TOTAL", subregion_1 == "United States") %>%
  pull(subregion_2) %>%
  unique()

# Select the date to begin with: the most recent date for which AFRICA
# has either test or case data:
default_date <- get_dashboard_data(df_dashboard,
                               region = "Africa",
                               end_date = max(DATE_RANGE),
                               duration = "Since pandemic began") %>%
  filter(!is.na(new_tests) | !is.na(new_cases)) %>%
  filter(date == max(date)) %>%
  pull(date)



plotly_theme <- . %>%
  plotly::config(
    modeBarButtonsToRemove = 
      c('sendDataToCloud',
        'zoom2d',
        'pan2d',
        'select2d',
        'lasso2d',
        'zoomIn2d',
        'zoomOut2d',
        'autoScale2d',
        'hoverClosestCartesian',
        'hoverCompareCartesian',
        'toggleSpikelines'),
    displaylogo = F
  ) %>%
  plotly::layout(
    legend        = list(orientation = "h", x = 0, y = -0.1),
    margin        = list(t = 0, b = 0),
    plot_bgcolor  = "rgba(255,255,255, 0.9)",
    paper_bgcolor = "rgba(0, 0, 0, 0)"
  ) 
