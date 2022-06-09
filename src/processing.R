# fixed parameters for write_data ------------------------------------------------
pop_year <- 2019
# year for US subnational population data
pop_subnat_year <- 2019
df_region_xwalk <- read_xlsx(here::here("data", "geography-country-list.xlsx")) %>%
  rename(region = Region, country = Country) %>%
  rename(subregion_1 = country)
# copying beh_metrics from global construction since it is used in dashboard creation
beh_map <- c(
  "Mask wearing" = "wear_mask",
  "Hand washing" = "wash_hands", 
  "Self-isolating" = "self_isolate", 
  "Physical distancing" = "phys_dist"
)
vac_map <- c(
  "Would take vaccine if available" = "vaccine_accept"
)
beh_metrics <- unname(beh_map)
vac_metrics <- unname(vac_map)

first_vacc_accept_date <- "2021-01-18"
# census skipped on release from the household pulse survey
# dates jump from 03-29 for week 27 to 04-26 for week 28 (skipping 04-12)
skip_vacc_accept_date <- "2021-04-12"
first_behav_wave_date <- "2020-07-19"

# age of eligibility to receive a covid 19 vaccine (in the US)
vacc_elig_age <- 5

# accent_drop -----------------------------------------------------------------
accent_drop_string <- function(x){
  stringi::stri_trans_general(str = x, id = "Latin-ASCII")
}

accent_drop <- function(x, varlist) {
  #'drop accents from characters in string variables in varlist

  result <- x %>% 
    mutate(across(all_of(varlist),
              .fns = accent_drop_string)
           )

  return(result)
}

get_region_population <- function(){
  region_pop <- read_csv(here::here("data", "continent_populations.csv")) %>% 
    rename(display_population = `2010 Population`,
           region = Continent) %>% 
    dplyr::select(region, display_population) %>% 
    mutate(display_population = as.numeric(str_trim(gsub(",","",gsub("[*]","",display_population))))) %>% 
    rbind(get_pop_nat('Latin America & Caribbean') %>% 
            dplyr::select(-iso_code) %>%
            rename(region = country,
                   display_population = population) %>% 
            mutate(region = gsub(' & Caribbean', '', region)))
  
  return(region_pop)
}


# get_jhu_data -----------------------------------------------------------------
get_jhu_nat <- function(countryname){
  #' Returns a dataframe of confirmed cases, deaths, and recovered by country
  #' 
  #' countryname (optional): returns dataframe for a given country
  cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
    clean_jhu_data(., var = "cases")
  
  deaths <-read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
    clean_jhu_data(., var = "deaths")
  
  recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") %>%
    clean_jhu_data(., var = "recovered")
  
  df <- full_join(cases, deaths, by = c("country", "date")) %>%
    full_join(recovered, by =  c("country", "date"))
  
  if (missing(countryname)){
    return(df)
  } else {
    return(df %>% filter(tolower(country) %in% tolower(countryname)))
  }
}

clean_jhu_data <- function(df, var, level = "country"){
  var_new <- paste0("new_", var)
  var <- paste0("cum_", var)
  
  if(level == "country"){
    selected_df <- df %>%
      dplyr::select(-c(`Province/State`, Long, Lat)) %>% 
      dplyr::rename(!!level := `Country/Region`) 
  } else if (level == "state"){
    selected_df <- df %>% 
      dplyr::select(-c(UID, iso2, iso3, code3, FIPS, Admin2, Country_Region, Lat, Long_, Combined_Key)) %>% 
      dplyr::rename(!!level := `Province_State`)
  }
  
  cleaned_df <- selected_df %>%
    group_by(!!ensym(level)) %>% 
    summarise_all(list(sum)) %>% 
    pivot_longer(cols = -c(!!ensym(level)),
                 names_to = "date", 
                 values_to = var) %>%
    filter(date != "Population") %>% 
    mutate(date = mdy(date)) %>%
    arrange(!!ensym(level), date) %>% 
    group_by(!!ensym(level)) %>% 
    mutate(!!var_new := !!ensym(var) - lag(!!ensym(var))) %>%
    ungroup() %>% 
    mutate(!!level := if_else(!!ensym(level) == "US", "United States", !!ensym(level)))
  
  return(cleaned_df)
}

# get_owid_data -----------------------------------------------------------------
get_owid_nat <- function(countryname){
  #' Returns a dataframe of tests conducted by country
  #' 
  #' countryname (optional): returns dataframe for a given country
  
  # Rank our preferred testing variables
  # In some cases multiple will be available for one country
  test_desc_ranking <- data.frame(
    test_desc = c("tests performed","people tested", "samples tested"),
    ranking = c(1,2,3), 
    stringsAsFactors = FALSE
  )
  
  df <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv") %>%
    separate(Entity, c("country","test_desc"), sep = " - ") %>%
    filter(test_desc %in% test_desc_ranking$test_desc) %>%   ## some countries have duplicate measures
    dplyr::select(country, 
                  date = Date, 
                  test_desc,
                  cum_tests  = `Cumulative total`) %>%
    left_join(test_desc_ranking, by = c("test_desc")) %>% 
    arrange(country, date, ranking) %>%  ## sort by test ranking (define above)
    group_by(country, date) %>% 
    slice(1) %>% ## keep one record per country-date combination 
    ungroup() %>%
    create_new_metrics(country) %>% 
    accent_drop(c("country")) %>%
    dplyr::select(-ranking) %>%
    mutate(new_tests = if_else(!(date == "2021-04-28" & country == "Cuba") & 
                                 !(date == "2021-05-24" & country == "Brazil"), 
                               new_tests, NA_real_),
           cum_tests = if_else(!(date == "2021-04-28" & country == "Cuba") & 
                                 !(date == "2021-05-24" & country == "Brazil"),
                               cum_tests, NA_real_))
  
  if (missing(countryname)){
    return(df)
  } else {
    return(df %>% filter(tolower(country) %in% tolower(countryname)))
  }
}

#get_owid_vax_nat -----------------------------------
get_vacc_nat <- function(){
  #create data frame for country-level OWID vaccine measures
  df <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv") %>%
    dplyr::select(location, date, total_vaccinations, people_vaccinated, people_fully_vaccinated, total_boosters, iso_code) %>%
    rename("cum_people_vacc" = "people_vaccinated",
           "cum_total_vacc" = "total_vaccinations",
           "country" = "location") %>%
    create_new_metrics(country) %>%
    accent_drop(c("country"))
  
  return(df)
}

create_vacc_accept_link <- function(week_num){
  return(paste0("https://www2.census.gov/programs-surveys/demo/tables/hhp/2021/wk",week_num,"/health5_week",week_num,".xlsx"))
}

loadWorkbook_url <- function(url) {
  #' Download an excel from from a direct link URL
  # Note: rio::import(url, sheetname) is probably better so we don't have to deal with temp files
  # but we need to use this to get a list of sheetnames for each workbook
  tryCatch({
    
    temp_file <- tempfile(fileext = ".xlsx")
    r <- httr::GET(url, write_disk(temp_file, overwrite = T))
    if (r$status_code==200) {
      return(temp_file)
    } else {
      return(NULL)
    }
    
  },
  warning = function(cond){
    message(paste("Data not available using link:", url))
    return(NULL)
  })
}

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  sheet_list <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X) %>% 
                         mutate(sheet_name = X))
  return(sheet_list)
}

clean_def_vacc_sheet <- function(df){
  #' Clean vaccine acceptance sheets
  #' Each sheet contains unnecessary header rows and multiple lines per column name
  #' This function concatenates these column names and calculates % of people that
  #' haven't gotten a vaccine but definitely will
  df <- df[-c(1,2,3),]
  colrows <- df %>% 
    head(4) %>% 
    dplyr::select(-sheet_name)
  colrows[is.na(colrows)] <- ""
  concat_colnames <- trimws(sapply(colrows, paste, collapse = " "))
  colnames(df)[1:ncol(df)-1] <- concat_colnames
  sheet_df <- df %>% 
    filter(`Select characteristics` == "Total") %>% 
    rename(numer1 = `Received a COVID-19 vaccine No Will definitely get a vaccine`,
           numer2 = `Received a COVID-19 vaccine Yes Total`,
           denom1 = `Total`,
           denom2 = `Received a COVID-19 vaccine Did not report`) %>%
    dplyr::select(sheet_name, numer1, numer2, denom1, denom2) %>% 
    mutate_at(vars(numer1, numer2, denom1, denom2), as.numeric) %>% 
    mutate(vaccine_accept = (numer1 + numer2)/(denom1 - denom2)) %>% 
    dplyr::select(sheet_name, vaccine_accept)
  return(sheet_df)
}

get_pct_def_vaccine <- function(week_num){
  if (week_num >=34) {
    return(data.frame())
  } else {
  link <- create_vacc_accept_link(week_num)
  temp_file <- loadWorkbook_url(link)
  if(is_empty(temp_file)){
    return(data.frame())
  } else {
    sheet_list <- read_excel_allsheets(temp_file)
    final_df <- lapply(sheet_list, clean_def_vacc_sheet) %>% 
      bind_rows() %>% 
      mutate(week_num = week_num)
    return(final_df)

    }
  }
  
}


get_vacc_accept <- function(first_survey_week = 22){
  #' Load percent of total respondents by total US, state, or metro area who did not receive a vaccine, but will definitely get one.
  #' Associated date will be the last date of the survey week
  #' Survey is conducted every two weeks since 1/18. This will pull any surveys that might be available today
  
  end_dates <- seq(ymd(first_vacc_accept_date), today(), by='2 week')
  # pulse survey skips over the period ending 04-12 and week 28 jumps to 04-26
  end_dates <- end_dates[end_dates != lubridate::as_date(skip_vacc_accept_date)]
  weeks <- seq(first_survey_week, first_survey_week+length(end_dates)-1, 1)
  date_lkp <- as.data.frame(list(week_num = weeks, end_date = end_dates))
  
  df <- lapply(weeks, get_pct_def_vaccine) %>% 
    bind_rows() %>% 
    merge(date_lkp, on = "week_num") %>% 
    mutate(lvl = case_when(sheet_name == "US" ~ "national",
                           grepl("Metro",sheet_name) ~ "metro",
                           TRUE ~ "state"))
  
  return(df)
}
  
#get_owid_us_state ----------------
get_vacc_us_state <- function() {
  #create data frame for US state-level OWID vaccine measures
  drop_locations <- c("United States",
                      "Bureau of Prisons",
                      "American Samoa",
                      "Dept of Defense",
                      "Federated States of Micronesia",
                      "Guam",
                      "Indian Health Svc",
                      "Long Term Care",
                      "Marshall Islands",
                      "Northern Mariana Islands",
                      "Republic of Palau",
                      "Veterans Health")
  
  df <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv") %>%
    dplyr::select(location, date, total_vaccinations, people_vaccinated, people_fully_vaccinated, total_boosters) %>%
    filter(!location %in% drop_locations) %>%
    mutate(state = if_else(location == "New York State", "New York", location)) %>%
    dplyr::select(state, date,
                  "cum_people_vacc" = "people_vaccinated",
                  "cum_total_vacc" = "total_vaccinations",
                  people_fully_vaccinated, total_boosters) %>%
    create_new_metrics(state)
  
  return(df)
}

# get_pop_nat -----------------------------------------------------------------
get_pop_nat <- function(countryname){
  #' Returns a dataframe of population by country from the World Bank
  #' 
  #' countryname (optional): returns dataframe for a given country
  
  df <- readxl::read_xls("data/country_populations.xls", sheet = "Data", skip = 3) %>% 
    dplyr::rename(country = `Country Name`, iso_code = 'Country Code') %>%
    dplyr::select(-c(`Indicator Name`, `Indicator Code`)) %>%
    pivot_longer(cols = -c(country, iso_code), 
                 names_to = "year", 
                 values_to = "population") %>%
    mutate(year = as.numeric(year)) %>%
    filter(!is.na(population)) %>%
    group_by(country, iso_code) %>%
    arrange(desc(year)) %>% # get most recent data
    slice(1) %>% 
    ungroup() %>%
    dplyr::select(-c(year)) %>%
    accent_drop(c("country")) %>% 
    mutate(country = case_when(country == "Egypt, Arab Rep." ~ "Egypt",
                               country == "Gambia, The" ~ "Gambia",
                               country == "Cabo Verde" ~ "Cape Verde",
                               country == "Bahamas, The" ~ "Bahamas",
                               country == "St. Lucia" ~ "Saint Lucia",
                               country == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
                               country == "Venezuela, RB" ~ "Venezuela",
                               country == "Swaziland" ~ "Eswatini",
                               TRUE ~ country))
  
  if (missing(countryname)){
    return(df)
  } else {
    return(df %>% filter(tolower(country) %in% tolower(countryname)))
  }
}

get_pop_subnat <- function(countryname){
  df <- read_csv(here::here("data", "subnational_populations.csv")) %>% 
    filter(`Series Name` == "Population, total") %>% 
    rename(population = `2016 [YR2016]`,
           country = Level_attr,
           region = `Country Name`
    ) %>% 
    separate(region, into = c(NA, "region"), sep = ", ") %>% 
    dplyr::select(country, region, population) %>% 
    filter(country == countryname) %>% 
    accent_drop(c("country","region"))
  return(df)
}

get_pop_demosite <- function(filename) {
  # load demonstration site populations (county and city)
  # these were individually populated into a spreadsheet from https://www.census.gov/quickfacts/
  df <- read_xlsx(here::here("data", "County & City population data.xlsx")) %>%
    rename(population = `Population estimates -  July 1, 2019`) %>%
    dplyr::select(-Location, -Source) %>%
    pivot_wider(id_cols = c(city, state), names_from = type, values_from = population, names_prefix = "pop_") %>%
    rename(population = pop_city,
           county_population =pop_county)
}

# get_ccvi_afr -----------------------------------------------------------------
#' Returns a dataframe of covid vulnerability index by country for africa
#' 
#' countryname (optional): returns dataframe for a given country
#' 
#' 
get_ccvi_afr <- function(geo_level, countryname){
  
  if(geo_level == "region"){
    sheet <- 'Vulnerability Index, regions'
  } else if(geo_level == "country"){
    sheet <- 'Vulnerability Index, countries'
  }
  
  df  <- read_xlsx(
    here::here("data", 
               "Surgo Foundation Africa COVID-19 Community Vulnerability Index.xlsx"), 
    sheet = sheet) %>%   
    dplyr::rename(country = NAME_0,
                  rel_cvi = `CCVI compared to continent`)  %>%
    filter(!is.na(rel_cvi), rel_cvi != "NA") %>% 
    mutate(rel_cvi = as.numeric(rel_cvi)) %>% 
    mutate(rel_cvi_desc = "CVI relative to continent") %>%
    accent_drop(c("country"))
  
  if(geo_level == "country"){
    df <- df %>% 
      dplyr::select(country, rel_cvi)
  } else if(geo_level == "region"){
    df <- df %>% 
      rename(subnat = NAME_1) %>% 
      dplyr::select(country, subnat, rel_cvi) %>% 
      accent_drop(c("subnat"))
  }
  
  
  if (missing(countryname)){
    return(df)
  } else {
    return(df %>% filter(tolower(country) %in% tolower(countryname)))
  }
  
}

# get_uhc_nat -----------------------------------------------------------------
#' Returns a dataframe of UHC Service Coverage Index (for 2017) by country
#' 
#' countryname (optional): returns dataframe for a given country
#' 
get_uhc_nat <- function(countryname){
  
  # the first two names of the csv contain title information
  # concatenate them and apply as titles to the rest of file
  names_1 <- read_csv(here::here("data", 
                                 "UHC_service_coverage_index.csv"),
                      n_max = 1)
  names_2 <- read_csv(here::here("data", 
                                 "UHC_service_coverage_index.csv"),
                      n_max = 2,
                      skip = 1)
  
  col_names <- paste(
    str_replace(names(names_1), "_[0-9]", ""), # remove _# suffix applied to duplicated column names
    str_replace(names(names_2), "_[0-9]", "")
  ) 
  
  raw <- read_csv(
    here::here("data", 
               "UHC_service_coverage_index.csv"),
    skip = 2,
    col_names = FALSE)
  
  names(raw) <- col_names
  
  df <- dplyr::select(raw, `X1 Country`, `UHC index of service coverage (SCI) 2017`) %>% # select only the latest available year (not updated frequently)
    dplyr::rename(country = `X1 Country`, uhc_sci_2017 = `UHC index of service coverage (SCI) 2017`) %>%
    accent_drop(c("country")) %>%
    mutate(country = case_when(
      country == "United States of America" ~ "United States",
      TRUE ~ country
    ))
  
  if (missing(countryname)){
    return(df)
  } else {
    return(df %>% filter(tolower(country) %in% tolower(countryname)))
  }
  
}


# get_dtp3_nat -----------------------------------------------------------------
#' Returns a dataframe of DTP3 immunization rates by country and year
#' 
#' countryname (optional): returns dataframe for a given country
#' 
#' web location: https://data.unicef.org/wp-content/uploads/2020/07/Immunization-coverage-by-antigen-country-regional-and-global-trends-WUENIC-2019revision.xlsx
get_dtp3_nat <- function(countryname){
  df <- read_xlsx(here::here("data", "Immunization-coverage-by-antigen-country-regional-and-global-trends-WUENIC-2019revision.xlsx"),
                  sheet = "DTP3") %>%
    dplyr::select(-c("unicef_region", "iso3", "vaccine")) %>%
    pivot_longer(cols = -country, 
                 names_to = "year", 
                 values_to = "dtp3") %>%
    mutate(year = strtoi(year),
           dtp3 = dtp3/100) %>% # string to integer
    filter(year == !!pop_year) %>%
    dplyr::select(c(country, dtp3))
  
  if (missing(countryname)){
    return(df)
  } else {
    return(df %>% filter(tolower(country) %in% tolower(countryname)))
  }
  
}

# get_policy_nat -----------------------------------------------------------------
#' Returns a dataframe of policy indicators by country and year
#' including schools open, restrictions on gatherings, and stay at home orders
#' countryname (optional): returns dataframe for a given country
#' 
get_policy_nat <- function(countryname){
  df <- get_policy_helper(countryname = countryname, jurisdiction = "nat")
}

# policy_level -----------------------------------------------------------------
# subroutine for get_policy_helper. Takes a policy variable, a flag variable
# and levels that correspond to a restriction. Returns 0 if there is no restriction
# 1 if there is a targeted restriction, and 2 if a restriction affects the entire
# geography. NA if the policy variable is missing
policy_level <- function(policy_var, # variable with policy details
                         policy_flag, # variable flagging whether policy is targeted or general
                         valid_levels # levels of policy var that qualify as an active policy
                         ) {
  if_else( is.na(policy_var),
           NA_integer_,
           as.integer(2*as.integer(policy_var %in% valid_levels) - 
                        as.integer(policy_flag %in% c(NA, 0) &
                                     policy_var %in% valid_levels
                        ))
  )
}

# get_policy -----------------------------------------------------------------
#' Returns a dataframe of policy indicators by country/subregion and year
#' including schools open, restrictions on gatherings, and stay at home orders
#' 
#' See the Oxford codebook for details about variables
#' https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md
#' 
#' countryname (optional): returns dataframe for a given country
#' jurisdiction: nat <- create country level file
#' STATE_TOTAL <- create state level file (tested on US)
#' other jurisdictions must be tested separately
#' 
get_policy_helper <- function(countryname, jurisdiction = "nat" # STATE_TOTAL or nat
){
  
  df <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv", 
                 guess_max = 100000) %>%
    mutate(
      # mark policy indicators as 2 if they apply to the entire country
      # mark policy indicators as 1 if they apply only to parts of the country
      school_closing = policy_level(`C1_School closing`, C1_Flag, c(2,3)),
      restr_gath = policy_level(`C4_Restrictions on gatherings`, C4_Flag, c(1,2,3,4)),
      req_mask = policy_level(`H6_Facial Coverings`, H6_Flag, c(2,3,4)),
      sah = policy_level(`C6_Stay at home requirements`, C6_Flag, c(2,3)),
    ) %>%
    mutate(date = ymd(Date)) %>%
    rename(country= CountryName,
           stringency_index = StringencyIndex) # for consistency with other data
  
  df2 <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_vaccines_full.csv", 
                 guess_max = 100000) %>%
    dplyr::select(CountryName,Date,starts_with("V2"),-V2_Notes, 
                  -`V2_Vaccine Availability (summary)`,
                  -`V2B_Vaccine age eligibility/availability age floor (general population summary)`,
                  -`V2C_Vaccine age eligibility/availability age floor (at risk summary)`) %>%
    rowwise() %>%
    mutate(select_sum_v2 = sum(c(`V2_0-4 yrs infants`,
                               `V2_5-15 yrs young people`,
                               `V2_General 16-19 yrs`,
                               `V2_General 20-24 yrs`,
                               `V2_General 25-29 yrs`,
                               `V2_General 30-34 yrs`,
                               `V2_General 35-39 yrs`,
                               `V2_General 40-44 yrs`,
                               `V2_General 45-49 yrs`,
                               `V2_General 50-54 yrs`,
                               `V2_General 55-59 yrs`,
                               `V2_General 60-64 yrs`), na.rm = T)) %>%
    mutate(sum_v2 = sum(across(starts_with("V2")), na.rm = T)) %>%
    ungroup() %>%
    mutate(date = ymd(Date)) %>%
    mutate(vacc_elig = ifelse(select_sum_v2>0,3,NA)) %>% #3= general population eligible; setting all else as NA for now
    group_by(CountryName) %>%
    arrange(date, .by_group=TRUE) %>%
    fill(vacc_elig, .direction = "down") %>% 
    ungroup() %>%
    mutate(vacc_elig = ifelse(is.na(vacc_elig) & sum_v2>0,2, #2= only high-risk or elderly eligible
                              ifelse(is.na(vacc_elig),1,vacc_elig))) %>% #1= no vaccine eligibility or no data about eligibility
    dplyr::select(country= CountryName, date, vacc_elig)
  
  
  stopifnot(sum(is.na(df2$vacc_elig))==0)
  
  df <- full_join(df, df2, by = c("country", "date"))
  
  if (jurisdiction == "nat") {
    df <- filter(df, is.na(RegionName)) %>% # ignore state-level data for now
      dplyr::select(c(country, date, school_closing, restr_gath, req_mask, sah, stringency_index, vacc_elig))
  } else if (jurisdiction == "STATE_TOTAL") { #select by jurisdiction (for subregions of interest, including US states)
    df <- filter(df, Jurisdiction == !!jurisdiction) %>%
      rename(state_abbrev = RegionCode,
             state = RegionName) %>% # for consistency with other data
      dplyr::select(c(country, state, date, school_closing, restr_gath, req_mask, sah, stringency_index, vacc_elig))
  } else {
    # return empty dataset if jurisdiction is not appropriately specified
    df <- NULL
  }
  
  if (missing(countryname)){
    return(df)
  } else {
    return(df %>% filter(tolower(country) %in% tolower(countryname)))
  }
}

# get_behavior_data -----------------------------------------------------------------
get_behavior_data <- function() {
  # try to load the behavioral data from the api
  # the api goes offline intermittently, so this block may fail
  tryCatch(
    {
    # combine behavioral survey data for all waves.
    # waves are 2 weeks apart. Unclear when the final wave will be. Number
    # of waves needs to be updated manually as new data are released
      
    # dynamically identify most recent wave
    url <- "https://covidsurvey.mit.edu/api.html"
    r <- httr::GET(url)
    t <- httr::content(r, as = "text", encoding = "UTF-8")
    waves <- str_extract_all(t, "wave[0-9][0-9]|wave[0-9]")[[1]]
    num_waves <- max(as.numeric(gsub("wave", "", waves)))
    
    # pull data for 'all waves.' This includes "snapshot countries
    # who were only survey for the two week period ending august 3.
    # this dataset will only be used for snapshot countries.
    # "wave" countries will be collected separately in the following step
    snapshot_only <- get_wave_behavior("all", "2020-08-03")
    
    # start all_waves file with wave1 data
    all_waves <- get_wave_behavior("wave1", ymd(first_behav_wave_date))
    for (x in 2:num_waves) {
      all_waves <- suppressWarnings(
                    bind_rows( all_waves, 
                              # add current wave's data to all waves df
                              get_wave_behavior(paste0("wave", x), ymd(first_behav_wave_date) + 14*(x-1) ))
      )
    }
    
    # remove wave countries from snapshot_only df
    wave_countries <- dplyr::distinct(all_waves, country_iso) %>% 
      dplyr::pull(country_iso)
    
    snapshot_only <- filter(snapshot_only, !(country_iso %in% wave_countries))
    
    # read in country name- iso2 crosswalk
    iso_2 <- read_csv(here::here("data", "geography_iso2_list.csv"))
    
    # add snapshot countries to all_waves
    all_waves <- suppressWarnings(bind_rows(all_waves, snapshot_only)) %>%
      # merge with country names
      mutate(country_iso = toupper(country_iso)) %>%
      left_join(iso_2, by = "country_iso") %>%
      dplyr::select(-country_iso) %>%
      rename(country = Country) %>%
      mutate_at(vars(!!beh_metrics, !!vac_metrics),
                ~as.numeric(as.character(.))) %>%
      accent_drop(c("country"))
    
    write_csv(all_waves, "data/df_archive_behave.csv")

    return(all_waves)
  },
  # if the try block above fails, load archived behavioral data
  error = function(cond) {
    message("Here's the original error message:")
    message(cond)
    read_csv(here::here("data","df_archive_behave.csv"))
  },
  warning = function(cond) {
    message("Here's the original warning message:")
    message(cond)
    read_csv(here::here("data","df_archive_behave.csv"))    
  },
  finally = {
    read_csv(here::here("data","df_archive_behave.csv"))    
  }
  
  )
  
}

# get_top3_gvi -----------------------------------------------------------------
get_top3_gvi <- function(var_df, df, metric){
  
  if (metric == "structural") {
    abbr = "barrier"
  } 
  if (metric == "hesitancy") {
    abbr = "hest"
  }
    
  temp <- var_df %>% filter(Use == metric) %>% pull(Indicator)
  ranking <- paste0(c("first_", "second_", "third_"), abbr)
  
  out_df <- df %>% 
    dplyr::select(country, survey_date, all_of(temp)) %>%
    filter(rowSums(is.na(.)) != ncol(.)-2) %>%
    group_by(country, survey_date) %>% 
    pivot_longer(!c(country, survey_date),names_to = "temp2", values_to = "pct") %>%
    arrange(desc(pct), .by_group = TRUE) %>%
    slice_head(n=3) %>% #getting top three
    mutate(ranking= ranking) %>%
    pivot_wider(names_from = ranking, values_from = c(temp2, pct)) %>%
    ungroup() %>%
    rename_with(~ gsub('temp2\\_', '', .x))
  
  return(out_df)
}

# get_vacc_accept -----------------------------------------------------------------
get_gvi <- function(){
  
  #get country list
  path <- "https://covidmap.umd.edu/api/country"
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  umd_countries <- fromJSON(response, flatten = TRUE) %>% data.frame() %>% as_vector() %>% as.character()
  
  #cleaning up country list
  umd_countries <- gsub(" ","_",umd_countries)
  umd_countries <- gsub("Côte_d'Ivoire","Cote_dIvoire",umd_countries)
  
  #pulling in the variables we want
  umd <- read.csv("data/umd_vaccine_accept_indicators.csv", fileEncoding = 'UTF-8-BOM')
  umd <- umd %>%
    filter(!(Use=="no"))
  vars <- umd$Indicator
  
  #pulling the data for all countries for all dates
  pull_global_vaccine_accept <- function(var, country){
    
    today <- gsub("-","",as.character(Sys.Date()))
    
    #build path
    path <- paste0("https://covidmap.umd.edu/api/resources?indicator=", var, "&type=smoothed&country=", country, "&daterange=20210301-", today)
    
    # request data from api
    request <- GET(url = path)
    
    # make sure the content is encoded with 'UTF-8'
    response <- content(request, as = "text", encoding = "UTF-8")
    
    df <- fromJSON(response, flatten = TRUE) 
    
    if (length(df$data)==0) {
      
      paste0("Data for ", country," are not available.")
    } else {
      
      df <- df %>% data.frame()
      
      colnames(df) <- gsub("data.","",colnames(df))
      
      df <- df %>%
        dplyr::select(-(contains("_se")), -(contains("_unw")), -status, -sample_size, -gid_0)
      
      return(df)
      
    }
  }
  
  umd_data <- list()
  
  #pulling the data - this takes a while to run
  for (i in umd_countries) {
    
    print(paste0("Pulling data for ", i))
    list <- rep(i, length(vars))
    z <- map2(vars, list, pull_global_vaccine_accept)
    z <- z[z!=paste0("Data for ", i," are not available.")]
    
    if (length(z)!=0){
      z <- z %>% 
        reduce(full_join, by = c("country","iso_code","survey_date")) %>%
        dplyr::select(country, iso_code, survey_date, everything())
      
      umd_data <- append(umd_data, list(z))
    }
  }
  
  umd_data <- umd_data %>% bind_rows()
  colnames(umd_data) <- gsub("smoothed_pct_","",colnames(umd_data))
  umd_data <- umd_data %>% rename(barrier_reason_side_effects = barrier_reason_sideeffects)
  
  struc_bars <- get_top3_gvi(umd, umd_data, "structural")
  hesitancy <- get_top3_gvi(umd, umd_data, "hesitancy")
  
  #combine with vaccine acceptance data
  umd_data <- umd_data %>%
    dplyr::select(country, survey_date, vaccinated_appointment_or_accept, appointment_or_accept_covid_vaccine, vaccine_acpt = smoothed_vu) %>%
    mutate(appointment_or_accept_covid_vaccine = ifelse(is.na(appointment_or_accept_covid_vaccine), vaccine_acpt, appointment_or_accept_covid_vaccine)) %>%
    dplyr::select(-vaccine_acpt) %>%
    full_join(hesitancy) %>%
    full_join(struc_bars) %>%
    mutate(date=ymd(survey_date)) %>%
    dplyr::select(-survey_date)
  
  return(umd_data)
  
}


# get_wave_behavior -----------------------------------------------------------------
get_wave_behavior <- function(wave, date){
  # select data from one wave from https://covidsurvey.mit.edu/api.html
  # surveys are conducted by wave and one wave can be queried at a time
  # some "snapshot" countries are only surveyed once, and only 
  # can be queried by selecting wave = all
  path <- paste0("http://covidsurvey.mit.edu:5000/query?age=all&wave=", 
                 wave 
                 ,"&gender=all&country=all&signal={vaccine_accept,measures_taken}")
  
  # request data from api
  request <- httr::GET(url = path)
  
  # make sure the content is encoded with 'UTF-8'
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  
  # now we can have a dataframe for use!
  coviddata <- fromJSON(response, flatten = TRUE) %>% data.frame()
  
  df_names <- names(coviddata)
  
  # transpose results, which are loaded as a flat file with one variable per value
  coviddata <- as.data.frame(t(coviddata)) 
  names(coviddata) <- "value"
  # variable names to a column
  coviddata$name <- df_names
  
  coviddata <- coviddata %>%
    # first two digits of variable name are 2 digit country iso code
    mutate(country_iso = substr(name, 1, 2),
           variable = substr(name, 3, length(name))) %>%
    dplyr::select(-name) %>%
    group_by(country_iso) %>%
    # reshape to country level dataset
    spread(variable, value) %>%
    ungroup() %>% 
    mutate(
      date = ymd(!!date),
      # vaccine accept defined as 1 - no - don't know
      # since "I have already been vaccinated" becomes
      # an answer only midway through the survey
      vaccine_accept = 1 -
        as.numeric(as.character(`.vaccine_accept.weighted.No`)) -
        as.numeric(as.character(`.vaccine_accept.weighted.Don.t.know`))
    ) %>% 
    rename(
      wear_mask = `.measures_taken.wearing_a_face_mask_or_covering.weighted.Yes`,
      wash_hands = `.measures_taken.washing_hands.weighted.Yes`,
      self_isolate = `.measures_taken.isolation.weighted.Yes`,
      phys_dist = `.measures_taken.meter_distance.weighted.Yes`
    ) %>%
    dplyr::select(country_iso, date, !!beh_metrics, !!vac_metrics)
  
  return(coviddata)
}

# get_india_data ---------------------------------------------------------------
get_india_pre_data <- function(){
  #' Returns a dataframe of confirmed cases, recovered cases, deaths, and tests
  #' by state and district in India
  #' Test desc draw from "source" on https://www.covid19india.org/
  
  json <- jsonlite::fromJSON("https://api.covid19india.org/v4/min/data-all.min.json")
  df <- map2_dfr(json, names(json), ~clean_india_date(.x, .y))
  
  names(df) <- gsub("total_", "cum_", names(df)) # for consistency with other data
  names(df) <- gsub("deceased", "deaths", names(df)) # for consistency with other data
  names(df) <- gsub("tested", "tests", names(df)) # for consistency with other data
  names(df) <- gsub("confirmed", "cases", names(df)) # for consistency with other data  
  
  state_xwalk <- read_csv("data/india_state_abbr.csv")
  df <- left_join(df, state_xwalk, by = "state_abbr") %>%
    rename(population = meta_population) %>%
    mutate(test_desc = "samples_tested") %>%
    mutate(district = if_else(district == "statewide",
                              "TOTAL",
                              district)) %>%
    mutate(date = lubridate::ymd(date)) %>% 
    arrange(state, district, date) %>% # sort by geography and date to fill cumulative data
    group_by(state, district, date) %>% 
    slice(1) %>% ## de-duplicate same day (TAMIL NADU TOTAL is duplicated on each date e.g.)
    create_new_metrics(state, district) %>% 
    # NOTE: according to covid19india.org, they have stopped collecting testing data 
    # at the district level. In practice, new tests fall to zero for all districts
    # the following identifies the last date for which new_tests has a non-zero value
    # (varies by district) and sets new_tests and cum_tests to missing after that date
    mutate(last_pos_test = if_else(
      (!is.na(new_tests) & new_tests != 0) |
        district == "TOTAL", # make modifications only to district level data 
      # (district != TOTAL)
      1, NA_real_)) %>%
    group_by(state, district) %>%
    tidyr::fill(last_pos_test, .direction = "up") %>%
    ungroup() %>%
    mutate(new_tests = if_else(last_pos_test == 1, new_tests, NA_integer_),
           cum_tests = if_else(last_pos_test == 1, cum_tests, NA_integer_)) %>%
    dplyr::select(c(district, state, date,
                    population,
                    new_cases, cum_cases,
                    new_deaths, cum_deaths,
                    new_recovered, cum_recovered,
                    new_tests, cum_tests, test_desc))
  
  return(df)
}


clean_india_date <- function(x, date){
  df <- map2_dfr(x, names(x), ~clean_state(.x, .y)) %>%
    mutate(date = date)
  return(df)
}

clean_state <- function(x, state_abbr){
  delta <- convert_to_df(x, "delta")
  total <- convert_to_df(x, "total")
  meta <- convert_to_df(x, "meta")
  
  df <- bind_cols(delta, total, meta) %>%
    mutate(district = "statewide")
  
  if ("districts" %in% names(x)) {
    districts <- map2_dfr(x$districts, names(x$districts), ~clean_districts(.x, .y))
  } else{
    districts <- NULL
  }
  
  return(bind_rows(df, districts) %>% mutate(state_abbr = state_abbr))
}

convert_to_df <- function(x, level){
  if (level %in% names(x)){
    df <- as.data.frame(x[level], stringsAsFactors = FALSE) %>% 
      setNames(paste0(level, '_', gsub(paste0(level, "."), "", names(.))))
  } else {
    df <- NULL
  }
  return(df)
}

clean_districts <- function(x, district_name){
  #' pass json json$date$state
  
  delta <- convert_to_df(x, "delta")
  total <- convert_to_df(x, "total")
  meta <- convert_to_df(x, "meta")
  
  df <- bind_cols(delta, total, meta) %>%
    mutate(district = district_name)
  
  return(df)
}

# get india vulnerability -------------------------------------------------------

#get_india_cvi_district <- function() {
  #' https://www.thelancet.com/cms/10.1016/S2214-109X(20)30300-4/attachment/818345a6-4cf7-43ba-92c6-92620f95494c/mmc1.pdf
  #' 
#  raw <- tabulizer::extract_tables("data/india_cvi.pdf", pages = seq(13, 24, 1), method = "stream")
#  df <- map_dfr(raw, ~as.data.frame(., stringsAsFactors = FALSE))
#  names(df) <- c("state", "district", "socioecon_vul", 
#                 "demo_vul", "housing_vul", "healthcare_vul", 
#                 "epi_vul", "overall_vul")
#  
#  df <- df %>% 
#    dplyr::select(state, district, rel_cvi = overall_vul) %>%
#    mutate(rel_cvi = as.numeric(rel_cvi)) %>%
#    filter(!is.na(rel_cvi), rel_cvi != "")
#  
#  # fix up some state names to match 
#  # there are a lot of distict names that don't line up, but not sure we have the
#  # time to look into that right now.
#  df <- df %>%
#    mutate(
#      state = case_when(
#        state == "Kashmir" | state == "J&K" ~ "Jammu and Kashmir", 
#        state == "A&N Islands" ~ "Andaman and Nicobar Islands", 
#        state == "Haveli" ~ "Dadra and Nagar Haveli",
#        state == "Daman&Diu" | state == "Daman & Diu" ~ "Daman and Diu",
#        TRUE ~ state
#      )
#    )
#  
#  write_csv(df, "data/india_district_cvi.csv")
#  return(df)
#}

get_india_cvi_state <- function() {
  url <- "https://www.thelancet.com/action/showFullTableHTML?isHtml=true&tableId=tbl2&pii=S2214-109X%2820%2930300-4"
  raw <- xml2::read_html(url)
  df <- rvest::html_table(raw)[[1]] %>%
    dplyr::select(
      state = "", 
      rel_cvi = "Overall vulnerability"                            
    ) %>%
    mutate(rel_cvi = readr::parse_number(gsub("·", ".", rel_cvi))) 
  
  write_csv(df, "data/india_state_cvi.csv")
  return (df)
}


# get brazil cases, tests, and deaths -------------------------------------------------------

get_brazil_case_test <- function() {
  df <- read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv",
                 col_types = cols(.default = "?", tests = "i", 
                                  recovered = "i", tests_per_100k_inhabitants = "n")) %>%  # guessing for these columns returns na
    filter(state != "TOTAL") %>% # get rid of national level data 
    dplyr::rename(state_abbrev = state) %>% # to match crosswalk
    left_join(read_csv(here::here("data", "brazilian_states.csv")), by = c("state_abbrev")) %>%
    dplyr::select(c(date, state, deaths, totalCases, 
                    recovered, tests)) %>%
    rename(cum_deaths = deaths, 
           cum_cases = totalCases, cum_recovered = recovered, cum_tests = tests) %>%
    arrange(state, date) %>%  ## sort by state and date to calculate cumulative totals
    create_new_metrics(state) %>% 
    accent_drop(c("state")) %>% 
    mutate(state = gsub("MatoGrosso","Mato Grosso", state))
  
  
  return(df)
}

# get brazil policy indicators -------------------------------------------------------
# data description: http://observcovid.miami.edu/methodology/
# Indicators of 0.5 represent non-mandatory/partial policies, indicators of 1 represent
# mandatory policies

get_policy_brz <- function() {
  df <- read_csv("https://raw.githubusercontent.com/lennymd/covidGraphics/main/data/brazil_data_latest.csv") %>%
    filter(state_name != "Nacional") %>%
    # code all non-mandatory/partial policies as 0 and all mandatory geography wide
    # policies as 2 (to be consistent with coding of oxford policy data)
    mutate_at(vars(stay_at_home, sdchool_closure, rest_on_gatherings, use_face_masks),
              ~as.integer(2)*as.integer(. %in% c(1)),
              na.rm = TRUE
    ) %>% 
    rename(sah = stay_at_home, 
           school_closing = sdchool_closure,
           restr_gath = rest_on_gatherings,
           req_mask = use_face_masks) %>%
    dplyr::select(c(state_name, date, sah, 
                    school_closing, restr_gath, req_mask)) %>%
    rename(state = state_name) %>%
    accent_drop(c("state"))
  
  return(df)
}

# get sa cases, tests, recoveries, and deaths ---------------------------------------------------------------
get_sa_case_death <- function(var){
  #' helper file for get_sa_case_death
  #' Returns a dataframe of confirmed cases, recovered cases, deaths, and tests
  #' by province in South Africa
  
  recover <- clean_sa_data("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_recoveries.csv",
                           "recovered")
  
  deaths <- clean_sa_data("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_deaths.csv",
                          "deaths")
  
  cases <- clean_sa_data("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv",
                         "cases")
  
  region_lkp <- read_csv(here::here("data", "south_africa_states.csv"))
  
  df <- full_join(cases, deaths, by = c("subnat", "date")) %>%
    full_join(recover, by = c("subnat", "date")) %>% 
    left_join(region_lkp, by = c("subnat"="state_abbrev")) %>% 
    dplyr::select(-subnat) %>% 
    rename(subnat = state)
  
  return(df)
}


# helper sa cases, tests, recoveries, and deaths ---------------------------------------------------------------
clean_sa_data <- function(file, var){
  #' helper file for get_sa_case_death
  #' Returns a dataframe of confirmed cases, recovered cases, deaths, and tests
  #' by province in South Africa
  
  var_new <- paste0("new_", var)
  var <- paste0("cum_", var)
  
  df <- read_csv(file) %>%
    mutate(date = dmy(date)) %>%
    dplyr::select(-c(YYYYMMDD, UNKNOWN, total, source)) %>%
    pivot_longer(cols = -c(date), 
                 names_to = "subnat", 
                 values_to = var) %>%
    arrange(subnat, date) %>% 
    group_by(subnat) %>%
    tidyr::fill(!!var, .direction = "down") %>% # fill cumulative values (if missing take the last value)
    mutate(!!var_new := !!ensym(var) - lag(!!ensym(var))) %>%
    ungroup() %>%
    accent_drop(c("subnat"))
  
  return(df)
}

get_subnat_vaccine <- function(file, year, source){
  
  df <- read_csv(here::here("data", file)) %>%
    filter(Year == year, `Data Source` == source) %>%
    mutate( dtp3 = as.numeric( 
      trimws( # remove whitespace
        gsub("No data", # remove text from cells
             "",
             gsub(
               "\\s*\\[[^\\)]+\\]", # remove confidence intervals (between brackets) using regex
               "",
               `DTP3 immunization coverage among one-year-olds (%)`
             )
        )
      )
    )/100
    ) %>%
    mutate( subnat = str_to_title(trimws(gsub('[[:digit:]]+', '', `Subnational region`)))) %>%
    dplyr::select(c(subnat, dtp3))
  return(df)
}

# get vaccination data for south africa ---------------------------------------------------------------
get_sa_dtp3 <- function(){
  # pull dtp3 vaccination for south africa. Source does not include all south african provinces
  # source: https://apps.who.int/gho/data/node.main.NODESUBREGimmunization-ZAF?lang=en
  
  df <- get_subnat_vaccine("south_africa_vaccinations_un_gho.csv", year = 2016, source = "DHS") %>% 
    mutate(subnat = ifelse(subnat == "Kwazulu-Natal","KwaZulu-Natal",subnat))
  return(df)
  
}

get_nig_dtp3 <- function(){
  df <- get_subnat_vaccine("nigeria_vaccinations_un_gho.csv", year = 2016, source = "MICS") %>% 
    mutate(subnat = ifelse(subnat == "Fct Abuja","Federal Capital Territory", subnat))
  return(df)
}


clean_nigeria <- function(file, var){
  new_var <- paste0("new_", var)
  cum_var <- paste0("cum_", var)
  
  df <- read_csv(file)
  
  # need to do this before sorting to get lag
  if (var != "cases"){
    df <- df %>% 
      mutate(date = mdy(Date)) %>% 
      dplyr::select(-c(Date))
  } else{
    df <- df %>% 
      rename(date = Date)
  }
  
  df <- df %>% 
    pivot_longer(cols = -c(date), 
                 names_to = "subnat", 
                 values_to = var) %>% 
    rename(!!cum_var := !!ensym(var)) %>% 
    arrange(subnat, date) %>%
    group_by(subnat) %>% 
    tidyr::fill(!!cum_var, .direction = "down") %>% 
    mutate(!!new_var := !!ensym(cum_var) - lag(!!ensym(cum_var))) %>% 
    ungroup()
  
  return(df)
}

get_nigeria_ind <- function(){
  
  # this does not include testing
  # it doesn't look like this has testing either: https://github.com/03balogun/ncdc-covid-data-file-reader
  # https://covng.netlify.app/ I don't see tests
  # # API: https://documenter.getpostman.com/view/6109250/SzmmUup4
  
  files <- list("cases" = "https://raw.githubusercontent.com/Kamparia/nigeria-covid19-data/master/data/csv/ncdc-covid19-states-daily-cases.csv",
                "deaths" = "https://raw.githubusercontent.com/Kamparia/nigeria-covid19-data/master/data/csv/ncdc-covid19-states-daily-deaths.csv",
                "recovered" = "https://raw.githubusercontent.com/Kamparia/nigeria-covid19-data/master/data/csv/ncdc-covid19-states-daily-recovered.csv")
  
  dfs <- c()
  for (var in names(files)){
    df <- clean_nigeria(files[[var]], var)
    dfs <- c(dfs, list(df))
  }
  
  nigerian_ind <- dfs %>% reduce(full_join, by = c("subnat","date")) %>% 
    mutate(subnat = ifelse(subnat == "FCT","Federal Capital Territory", subnat),
           subnat = ifelse(subnat == "Nassarawa","Nasarawa",subnat))
  return(nigerian_ind)
}

get_new_metric <- function(cum_metric){
  cum_metric - lag(cum_metric)
}

create_new_metrics <- function(df, ...){
  #' Returns a dataframe with all new metrics generated from cumulative
  #' 
  #' df
  #' ... list of grouping columns to fill within
  
  df %>% 
    arrange(date) %>% 
    group_by(...) %>% 
    tidyr::fill(-date, .direction = "down") %>% 
    mutate(across(contains("cum"), 
                  .fns = get_new_metric,
                  .names = "new_{col}")) %>% 
    ungroup() %>% 
    rename_with(~ gsub("new_cum","new", .))
}

get_us_state_abbrev <- function(){
  # load state name state abbreviation crosswalk
  us_state_abbrev <- data.frame("state" = datasets::state.name, "state_abbrev" = datasets::state.abb, stringsAsFactors = FALSE)
  us_state_abbrev <- rbind(us_state_abbrev, c("state" = "Puerto Rico", "state_abbrev" = "PR"))
  us_state_abbrev <- rbind(us_state_abbrev, c("state" = "District of Columbia", "state_abbrev" = "DC"))
  return(us_state_abbrev)
}

get_us_case_test_death <- function() {
  # US case, test, death, and recovery data from
  # Covid tracking project api, JHU, and Socrata
  
  us_state_abbrev <- get_us_state_abbrev()
  
  # load data
  
  # cum_tests and new_tests from positive, negative, and inconclusive
  # Positive tests is also used for the US only in calculating TPR
  # Token requires a Socrata or HHS username/password
  # Tequests without a token are subject to lower throttling limits
  # To prevent multiple programmers from having to set up an account and token, I am going to move forward without a token
  # We can revisit this decision if our requests become limited.
  raw <- RSocrata::read.socrata(
    "https://healthdata.gov/resource/j8mb-icvb.json"
  )
  
  # apply create_new_metrics to each file separately, rather than after 
  # joining all dfs together. This leaves the end of each series missing
  # if e.g. the test series ends before the cases series
  tests <- raw %>% 
    dplyr::select(state_name, date, overall_outcome, total_results_reported) %>%
    mutate(date = ymd(date)) %>%
    mutate_at(vars(ends_with("reported")), as.numeric) %>% 
    pivot_wider(
      id_cols = c(state_name, date),
      names_from = overall_outcome,
      values_from = total_results_reported
    ) %>% 
    rowwise() %>%
    mutate(cum_tests = sum(Positive, Negative, Inconclusive, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(state = state_name,
           cum_positive = Positive) %>% 
    dplyr::select(state, date, cum_positive, cum_tests) %>% 
    arrange(state, date) %>%
    group_by(state) %>%
    complete(date = seq.Date(min(date), max(date), by="day")) %>%
    create_new_metrics(state) 
    
  recovered <- read_csv("https://api.covidtracking.com/v1/states/daily.csv") %>%
    rename(state_abbrev = state,
           cum_recovered = recovered) %>% 
    inner_join(us_state_abbrev, by = "state_abbrev") %>% 
    dplyr::select(date, state, cum_recovered) %>%
    mutate(date = lubridate::ymd(date)) %>% 
    arrange(state, date) %>%
    group_by(state) %>%
    complete(date = seq.Date(min(date), max(date), by="day")) %>%
    create_new_metrics(state) 
  
  # cases and deaths
  cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>%
    clean_jhu_data(., var = "cases", level = "state") %>% 
    dplyr::select(-new_cases)%>% 
    arrange(state, date) %>%
    group_by(state) %>%
    complete(date = seq.Date(min(date), max(date), by="day")) %>%
    create_new_metrics(state) 
  
  deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>%
    clean_jhu_data(., var = "deaths", level = "state") %>% 
    dplyr::select(-new_deaths)%>% 
    arrange(state, date) %>%
    group_by(state) %>%
    complete(date = seq.Date(min(date), max(date), by="day")) %>%
    create_new_metrics(state) 
  
  # merge all metrics
  df <- full_join(cases, deaths, by = c("state","date")) %>%
    full_join(tests, by = c("state","date")) %>% 
    full_join(recovered, by = c("state","date")) %>% 
    dplyr::select(-cum_positive)
  
  return(df)
  
}

get_us_policy <- function() {
  df <- get_policy_helper(countryname = "United States", jurisdiction = "STATE_TOTAL") %>% 
    dplyr::mutate(state = case_when(state == "Washington DC" ~ "District of Columbia",
                                    state != "Washington DC" ~ state))
  
  df_pr <- get_policy_helper(countryname = "Puerto Rico") %>%
    mutate(state = "Puerto Rico", country = "United States")
  
  df <- rbind(df, df_pr)
  
  return(df)
}

# get_us_ccvi -----------------------------------------------------------------
#' Returns a dataframe of covid vulnerability index by state for the US
#' original source https://docs.google.com/spreadsheets/d/1qEPuziEpxj-VG11IAZoa5RWEr4GhNoxMn7aBdU76O5k/edit#gid=1536551831
#' from https://precisionforcovid.org
#' Must be downloaded manually, but does not update more often than annually
get_us_ccvi <- function(){
  
  sheet <- 'State CCVI'
  
  df  <- read_xlsx(
    here::here("data", 
               "COVID-19 Community Vulnerability Index (CCVI).xlsx"), 
    sheet = sheet) %>%
    mutate(state = stringr::str_to_title(State)) %>%
    rename(rel_cvi = `CCVI SCORE\nHigher = More Vulnerable`) %>%
    mutate(rel_cvi_desc = "CVI relative to country",
           state = case_when(
             state == "District Of Columbia" ~ "District of Columbia",
             state != "District Of Columbia" ~ state
           )
           ) %>%
    dplyr::select(state, rel_cvi)
  
  return(df)
  
}

get_us_subnat_pop <- function(year) {
  # load subnational census population data for the US
  # latests available data include estimates through 2020
  df <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv") %>%
    # filter out data that are not at the state level 
    dplyr::filter(STATE != "00") %>%
    mutate(population = !!sym(paste0("POPESTIMATE", year))) %>%
    rename(state = NAME) %>%
    dplyr::select(state, population)
  
  return(df)
}


#get US capactiy measure from HeatlhData.gov: 1) covid inpatient capacity, 2) overall inpatient capacity, 3) ICU capacity
#data begins Jan 1, 2020 through present.  First two measures have partial missingness through Mar 31, 2020.  ICU measure
#has partial missingness through July 31 2020

get_uscap_data <- function() {
  
  
  us_state_abbrev <- data.frame("state" = datasets::state.name, "state_abbrev" = datasets::state.abb, stringsAsFactors = FALSE)
  
  
  uscap_df <- read_csv("https://healthdata.gov/api/views/g62h-syeh/rows.csv") %>%
    dplyr::select(
      date,
      "state_abbrev" = "state", 
      "covid_beds_num" = "inpatient_bed_covid_utilization_numerator", 
      "covid_beds_percent" = "inpatient_bed_covid_utilization", 
      "covid_beds_denom" = "inpatient_bed_covid_utilization_denominator",
      "ip_beds_num" = "inpatient_beds_utilization_numerator",
      "ip_beds_percent" = "inpatient_beds_utilization",
      "ip_beds_denom" = "inpatient_beds_utilization_denominator",
      "icu_beds_num" = "adult_icu_bed_utilization_numerator",
      "icu_beds_percent" = "adult_icu_bed_utilization",
      "icu_beds_denom" = "adult_icu_bed_utilization_denominator"
    ) %>%
    left_join(us_state_abbrev) %>%
    mutate(state = case_when(
      state_abbrev == "DC" ~ "District of Columbia",
      state_abbrev == "VI" ~ "Virgin Islands",
      state_abbrev == "PR" ~ "Puerto Rico",
      !is.na(state) ~ state),
      #catch cases where numerator exceeds denominator and recode percents as missing
      ip_beds_num = if_else(ip_beds_num > ip_beds_denom, NA_real_, ip_beds_num),
      ip_beds_denom = if_else(ip_beds_num > ip_beds_denom, NA_real_, ip_beds_denom),
      ip_beds_percent = if_else(ip_beds_num > ip_beds_denom, NA_real_, ip_beds_percent),
      covid_beds_num = if_else(covid_beds_num > covid_beds_denom, NA_real_, covid_beds_num),
      covid_beds_denom = if_else(covid_beds_num > covid_beds_denom, NA_real_, covid_beds_denom),
      covid_beds_percent = if_else(covid_beds_num > covid_beds_denom, NA_real_, covid_beds_percent),
      icu_beds_num = if_else(icu_beds_num > icu_beds_denom, NA_real_, icu_beds_num),
      icu_beds_denom = if_else(icu_beds_num > icu_beds_denom, NA_real_, icu_beds_denom),
      icu_beds_percent = if_else(icu_beds_num > icu_beds_denom, NA_real_, icu_beds_percent),
      date = as.Date(date)
    ) %>%
    mutate_at(vars(contains("_beds_")), .funs = as.numeric)
  
  
  return(uscap_df)
  
}

get_uscap_nat <- function() {
  
  us_state_abbrev <- data.frame("state" = datasets::state.name, "state_abbrev" = datasets::state.abb, stringsAsFactors = FALSE)
  nat_sample <- c(unique(us_state_abbrev$state_abbrev), "PR", "DC")
  
  uscap_nat_hosp <- read_csv("https://healthdata.gov/api/views/g62h-syeh/rows.csv") %>%
    dplyr::select(
      date,
      state,
      "covid_beds_num" = "inpatient_bed_covid_utilization_numerator", 
      "covid_beds_percent" = "inpatient_bed_covid_utilization", 
      "covid_beds_denom" = "inpatient_bed_covid_utilization_denominator",
      "ip_beds_num" = "inpatient_beds_utilization_numerator",
      "ip_beds_percent" = "inpatient_beds_utilization",
      "ip_beds_denom" = "inpatient_beds_utilization_denominator"
    ) %>%
    mutate(
      #catch cases where numerator exceeds denominator and recode denominator = numerator, percents as 100%
      ip_beds_num = if_else(ip_beds_num > ip_beds_denom, NA_real_, ip_beds_num),
      ip_beds_denom = if_else(ip_beds_num > ip_beds_denom, NA_real_, ip_beds_denom),
      ip_beds_percent = if_else(ip_beds_num > ip_beds_denom, NA_real_, ip_beds_percent),
      covid_beds_num = if_else(covid_beds_num > covid_beds_denom, NA_real_, covid_beds_num),
      covid_beds_denom = if_else(covid_beds_num > covid_beds_denom, NA_real_, covid_beds_denom),
      covid_beds_percent = if_else(covid_beds_num > covid_beds_denom, NA_real_, covid_beds_percent),
      date = as.Date(date)) %>%
    filter(date > "2020-03-31", #data is non-missing for full national sample after 2020-03-31
           state %in% nat_sample) %>%
    mutate_at(vars(contains("_beds_")), .funs = as.numeric) %>%
    group_by(date) %>%
    summarize(covid_beds_num = sum(covid_beds_num, na.rm = TRUE),
              covid_beds_denom = sum(covid_beds_denom, na.rm = TRUE),
              ip_beds_num = sum(ip_beds_num, na.rm = TRUE),
              ip_beds_denom = sum(ip_beds_denom, na.rm = TRUE)) %>%
    mutate(covid_beds_percent = covid_beds_num/covid_beds_denom,
           ip_beds_percent = ip_beds_num/ip_beds_denom,
           country = "United States")
  
  uscap_nat_icu <- read_csv("https://healthdata.gov/api/views/g62h-syeh/rows.csv") %>%
    dplyr::select(
      date,
      state,
      "icu_beds_num" = "adult_icu_bed_utilization_numerator",
      "icu_beds_percent" = "adult_icu_bed_utilization",
      "icu_beds_denom" = "adult_icu_bed_utilization_denominator"
    ) %>%
    mutate(icu_beds_num = if_else(icu_beds_num > icu_beds_denom, NA_real_, icu_beds_num),
           icu_beds_denom = if_else(icu_beds_num > icu_beds_denom, NA_real_, icu_beds_denom),
           icu_beds_percent = if_else(icu_beds_num > icu_beds_denom, NA_real_, icu_beds_percent),
           date = as.Date(date)) %>%
    filter(date > "2020-07-31", #data is non-missing for full national sample after 2020-07-31
           state %in% nat_sample) %>%
    mutate_at(vars(contains("_beds_")), .funs = as.numeric) %>%
    group_by(date) %>%
    summarize(icu_beds_num = sum(icu_beds_num, na.rm = TRUE),
              icu_beds_denom = sum(icu_beds_denom, na.rm = TRUE)) %>%
    mutate(icu_beds_percent = icu_beds_num/icu_beds_denom,
           country = "United States")
  
  uscap_nat_df <- uscap_nat_hosp %>%
    left_join(uscap_nat_icu, by = c("date", "country"))
  
  
  return(uscap_nat_df)
  
}

# Save a time series of state level vaccinations by race using KFF data hosted at -------------------------
# https://github.com/KFFData/COVID-19-Data/tree/kff_master/Race%20Ethnicity%20COVID-19%20Data/Vaccines ----
get_us_state_vacc_by_race <- function(df_us_vacc, impute_pct_of_vacc_vars = NULL) {

  
  df_pct_vacc <- join_kff_tables(
                  filelist = get_kff_filelist(search_text = 'Vaccine by RE|Vaccine by RaceEthnicity|5_Percent of Total Population that has Received a COVID-19 Vaccine by Race_Ethnicity'),
                  replace_stub_1 = "X..of.Total.",
                  replace_stub_2 = ".Population.Vaccinated",
                  suffix = "_pct_vacc_rpt") %>%
                dplyr::filter(state != "United States")
  
  df_pct_of_vacc <- join_kff_tables(
                                filelist = get_kff_filelist(search_text = 'COVID-19 Vaccinations by RE|COVID19 Vaccinations by RE|9_Vaccinations by RE|COVID-19 Vaccinations by Race_Ethnicity'),
                                replace_stub_1 = "X..of.Vaccinations.with.",
                                replace_stub_2 ="...of.Vaccinations",
                                suffix = "_pct_of_vacc",
                                in_names = c("Race.Categories.Include.Hispanic.Individuals",
                                              "Location",
                                              "American.Indian.or.Alaska.Native",
                                              "Native.Hawaiian.or.Other.Pacific.Islander"),
                                out_names = c("race_incl_hisp",
                                               "state",
                                               "ai_an",
                                               "nh_pi"))
  
  #need to remove one extra file for df_pct_vacc pulled in for KFF
  stopifnot(nrow(filter(df_pct_vacc, date=="2022-01-10"))==102) #double the data for this date
  df_pct_vacc <- df_pct_vacc %>%
    group_by(state, date) %>%
    slice(1L) %>%
    ungroup()
  # need to remove one extra file for df_pct_of_vacc pulled in for KFF 
  stopifnot(nrow(filter(df_pct_of_vacc, date=="2022-01-10"))==102) #double the data for this date
  df_pct_of_vacc <- df_pct_of_vacc %>%
    group_by(state, date) %>%
    slice(1L) %>%
    ungroup()
  
  #adding checks to make sure these two dfs line up
  stopifnot(sort(unique(df_pct_vacc$date)) == sort(unique(df_pct_of_vacc$date)))
  stopifnot(nrow(df_pct_vacc)==nrow(df_pct_of_vacc))
  
  #check to make sure pct_of_vacc nums combine the way we expect (test #1)
  df_test <- df_pct_of_vacc %>%
    rowwise() %>%
    mutate(sum = case_when(tolower(race_incl_hisp)!="yes" ~ sum(white_pct_of_vacc, 
                                                                black_pct_of_vacc, 
                                                                hispanic_pct_of_vacc,
                                                                asian_pct_of_vacc,
                                                                ai_an_pct_of_vacc,
                                                                nh_pi_pct_of_vacc,
                                                                other_pct_of_vacc, na.rm = TRUE),
                           tolower(race_incl_hisp)=="yes" ~ sum(white_pct_of_vacc, 
                                                                black_pct_of_vacc, 
                                                                #hispanic_pct_of_vacc,
                                                                asian_pct_of_vacc,
                                                                ai_an_pct_of_vacc,
                                                                nh_pi_pct_of_vacc,
                                                                other_pct_of_vacc, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(sum = round(sum,1)) %>%
    filter(!(sum %in% (c(0.0,0.9,1.0,1.1))))
  
  #stopifnot(nrow(df_test)==0) this fails here, Florida dates are the problem (see white rates jump to 70% from 50%) suppressing for now
  fl_issue_dates <- c(as.Date('2021-11-29'), as.Date('2022-01-10'), as.Date('2022-01-31'), as.Date('2022-03-07'))
  stopifnot(nrow(df_test)==length(fl_issue_dates))
  stopifnot(df_test$state=='Florida' & df_test$date %in% fl_issue_dates)
  df_pct_of_vacc <-  df_pct_of_vacc %>% filter(!(date %in% fl_issue_dates & state=="Florida"))
  
  #check to make sure pct_of_vacc nums combine the way we expect (test #2)
  df_test <- df_pct_of_vacc %>%
    rowwise() %>%
    mutate(sum = case_when(tolower(race_incl_hisp)=="yes" ~ sum(white_pct_of_vacc, 
                                                                black_pct_of_vacc, 
                                                                hispanic_pct_of_vacc,
                                                                asian_pct_of_vacc,
                                                                ai_an_pct_of_vacc,
                                                                nh_pi_pct_of_vacc,
                                                                other_pct_of_vacc, na.rm = TRUE))) %>%
    ungroup() %>%
    filter(!is.na(sum)) %>% #removing those with race_incl_hisp ! = yes
    filter(!(is.na(hispanic_pct_of_vacc))) %>% #removing those with missing hispanic (so would NOT expect sum to be greater than 1)
    filter(sum < 1) %>% #identifying those with a low than expected sum
    mutate(sum = round(sum,1)) %>%
    filter(!(sum %in% (c(0.9,1.0)))) #identifying those likely dealing with a rounding error
  
  stopifnot(nrow(df_test)==0)
  
  if (!is.null(impute_pct_of_vacc_vars)) {
    # back fill pct of vacc for race/ethnicities selected
    # backfilled values will be used to construct pct_vacc
    # estimates for ai_an and nh_pi
    df_pct_of_vacc <- df_pct_of_vacc %>% arrange(state, date) %>% 
      group_by(state) %>%
      tidyr::fill(lapply(impute_pct_of_vacc_vars, 
                         function(x) paste0(x, "_pct_of_vacc")) %>%
                    unlist(), 
                  .direction = "up") %>%
      ungroup()
  }
  
  df_pct_of_vacc <- df_pct_of_vacc %>%
    mutate(other_pct_of_vacc_zeros = ifelse(!(is.na(white_pct_of_vacc) & is.na(black_pct_of_vacc)) & is.na(other_pct_of_vacc),
                                            0,other_pct_of_vacc)) %>% #if black and white are non-missing assume other is not missing, but zero
    mutate(other_pct_of_vacc = ifelse(!(is.na(white_pct_of_vacc) & is.na(black_pct_of_vacc)) & is.na(other_pct_of_vacc),
                                            0,other_pct_of_vacc)) %>% #if black and white are non-missing assume other is not missing, but zero
    mutate(known_race_pct_of_vacc_othmiss = known_race_pct_of_vacc - known_race_pct_of_vacc*other_pct_of_vacc_zeros) %>% #consider others to have unknown race
    dplyr::select(-other_pct_of_vacc_zeros)
  
  # 2019 US population by race and state
  # from https://www.kff.org/other/state-indicator/distribution-by-raceethnicity/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
  df_pct_pop_by_race_state <- read_csv(here::here("data", "kff_population_by_race_and_state_2019.csv"),
                                  skip = 2,
                                  na = c("", "NA", "N/A", "<.01")) %>%
   dplyr::select(-c(Footnotes, Total))
  
  # 2019 US population by race and state
  # from census
  # version where respondents must report a single race or "multiple races" category
  df_pop_by_race_state_mutexcl <- get_state_pop_race_cat(pop_year = 2019, num_race_cats = 6, age_cutoff = vacc_elig_age)
    
  # version where respondents are counted if they identify as a race, even if they identify
  # with other races as well.
  df_pop_by_race_state_dupl <- get_state_pop_race_cat(pop_year = 2019, num_race_cats = 5, age_cutoff = vacc_elig_age)
  
  pop_by_race_vars <- names(df_pop_by_race_state_dupl)[names(df_pop_by_race_state_dupl) != "state"]
  
  df_pop_by_race_state_mean <- df_pop_by_race_state_dupl %>%
    full_join(df_pop_by_race_state_mutexcl, by = "state") %>%
    rowwise() %>%
    mutate(white = mean(c(white.x,white.y)),
           ai_an = mean(c(ai_an.x,ai_an.y)),
           asian = mean(c(asian.x,asian.y)),
           black = mean(c(black.x,black.y)),
           hispanic = mean(c(hispanic.x,hispanic.y)),
           nh_pi = mean(c(nh_pi.x,nh_pi.y))) %>%
    ungroup() %>%
    dplyr::select(-ends_with(".x"), -ends_with(".y"), multiple)
  
  # determine rescaling factor to convert XXX_pct_vacc (total population) 
  # to XXX_pct_vacc (eligible population)
  df_tot_pop_by_race_state_mutexcl <- get_state_pop_race_cat(pop_year = 2019, num_race_cats = 6, age_cutoff = 0) %>%
    rename_with(.cols = -c(state), .fn = ~paste0(.x, "_total"))

  df_elig_pop_rescale <- df_tot_pop_by_race_state_mutexcl %>%
    left_join(df_pop_by_race_state_mutexcl, by = "state") %>%
    mutate(
      white_scale = white_total/white,
      black_scale = black_total/black,
      asian_scale = asian_total/asian,
      hispanic_scale = hispanic_total/hispanic
    ) %>%
    dplyr::select(state, ends_with("scale"))
  
  # rescale pct vacc figures so they represent percent of eligible
  # population
  df_pct_vacc <- df_pct_vacc %>% 
    left_join(df_elig_pop_rescale, by = "state") %>%
    mutate(
      white_pct_vacc_rpt = white_pct_vacc_rpt*white_scale,
      black_pct_vacc_rpt = black_pct_vacc_rpt*black_scale,
      asian_pct_vacc_rpt = asian_pct_vacc_rpt*asian_scale,
      hispanic_pct_vacc_rpt = hispanic_pct_vacc_rpt*hispanic_scale
    ) %>%
    dplyr::select(-ends_with("scale"))
  
  # calculate state population (of at least vaccine_elig_age) by adding together
  # all race categories in the mutually exclusive state population df
  df_pop_by_state <-  df_pop_by_race_state_mutexcl %>% 
    rowwise() %>%
    mutate(population = sum(c(white, black, ai_an, asian, nh_pi, multiple))) %>%
    ungroup() %>%
    dplyr::select(state, population)
  
  # calculate population total of American Indian and Alaskan Native/Native Hawaiian and Pacific Islander
  # restrict to states with at least 5 % population belonging to relevant category
  nh_pi_pct_vacc <- construct_pct_vacc(
    re_cat = "nh_pi", 
    re_inname = "Native Hawaiian/Other Pacific Islander",
    df_pop_by_race_state = df_pop_by_race_state_mean,
    df_pct_pop_by_race_state= df_pct_pop_by_race_state,
    df_pct_of_vacc = df_pct_of_vacc,
    df_us_vacc = df_us_vacc)

  ai_an_pct_vacc <- construct_pct_vacc(
    re_cat = "ai_an", 
    re_inname = "American Indian/Alaska Native",
    df_pop_by_race_state = df_pop_by_race_state_mean,
    df_pct_pop_by_race_state = df_pct_pop_by_race_state,
    df_pct_of_vacc = df_pct_of_vacc,
    df_us_vacc = df_us_vacc)
  
  df_bipoc_pop <- df_pop_by_race_state_mutexcl %>%
    full_join(df_pop_by_state, by = "state") %>%
    mutate(bipoc_pop_incl_wh = population - white_nh,
           bipoc_pop_excl_wh = population - white,
           bipoc_pct_pop_incl_wh = 1 - (white_nh/population),
           bipoc_pct_pop_exl_wh = 1 - (white/population)) %>%
    dplyr::select(state, starts_with("bipoc"))
  
  df_bipoc_pct_vax <- df_pct_of_vacc %>% 
    #only include dates for which state vaccinations by race are available using left join
    left_join(df_us_vacc, by = c("state", "date")) %>%
    left_join(df_bipoc_pop, by = c("state")) %>%
    mutate(bipoc_cum_people_vacc_est = (1 - (white_pct_of_vacc/(1-other_pct_of_vacc)))*cum_people_vacc) %>%
    mutate(bipoc_cum_people_vacc_rpt = (1 - white_pct_of_vacc - other_pct_of_vacc)*cum_people_vacc*known_race_pct_of_vacc) %>%
    mutate(bipoc_pct_vacc_est = case_when(tolower(race_incl_hisp)=="yes" ~ bipoc_cum_people_vacc_est/bipoc_pop_excl_wh,
                                          tolower(race_incl_hisp)!="yes" ~ bipoc_cum_people_vacc_est/bipoc_pop_incl_wh)) %>%
    mutate(bipoc_pct_vacc_rpt = case_when(tolower(race_incl_hisp)=="yes" ~ bipoc_cum_people_vacc_rpt/bipoc_pop_excl_wh,
                                          tolower(race_incl_hisp)!="yes" ~ bipoc_cum_people_vacc_rpt/bipoc_pop_incl_wh)) %>%
    mutate(bipoc_pct_of_pop =   case_when(tolower(race_incl_hisp) == "yes" ~ bipoc_pct_pop_exl_wh,
                                          tolower(race_incl_hisp) != "yes" ~ bipoc_pct_pop_incl_wh)) %>%
    mutate(bipoc_pct_of_vacc = 1 - white_pct_of_vacc/(1-other_pct_of_vacc)) %>%
    mutate(bipoc_vacc_disparity = bipoc_pct_of_vacc - bipoc_pct_of_pop)
  
  df_bipoc_pct_vax <- df_bipoc_pct_vax %>%
  #If a state only reports white, black and other for that date, mark BIPOC total people, proportion BIPOC vaccinated, and BIPOC share of vaccinations to date as missing
   mutate_at(vars(starts_with(c('bipoc_cum_people','bipoc_pct_vacc','bipoc_pct_of','bipoc_vacc_disparity'))),
              ~case_when(is.na(hispanic_pct_of_vacc) & 
                           is.na(asian_pct_of_vacc) & 
                           is.na(ai_an_pct_of_vacc) & 
                           is.na(nh_pi_pct_of_vacc) ~ NA_real_,
                         TRUE ~ .))
  
  df_bipoc_pct_vax <- df_bipoc_pct_vax %>%
    arrange(state, date) %>%
    dplyr::select(state, date, starts_with(c("bipoc_pct_vacc", "bipoc_cum_people", "bipoc_pct_of")), bipoc_vacc_disparity)
  
  df <- df_pct_vacc %>% 
    full_join(
      dplyr::select(df_pct_of_vacc, date, state, known_race_pct_of_vacc_othmiss, known_race_pct_of_vacc, known_ethnicity_pct_of_vacc),
      by = c("state", "date")
    ) %>%
    # rescale reported variables to calculate estimated proportions
    mutate(hispanic_pct_vacc_est = case_when(tolower(race_incl_hisp)!="yes" ~ hispanic_pct_vacc_rpt/known_race_pct_of_vacc_othmiss,
                                          tolower(race_incl_hisp)=="yes" ~ hispanic_pct_vacc_rpt/known_ethnicity_pct_of_vacc)) %>%
    
    dplyr::select(-hispanic_pct_vacc_rpt, -race_incl_hisp) %>%
    mutate_at(vars(ends_with("_rpt")), 
              ~./known_race_pct_of_vacc_othmiss
              ) %>%
    rename_with(.fn = ~gsub("_rpt", "_est", .x)) %>%
    # add reported proportions back to df
    full_join(df_pct_vacc, by = c("state", "date")) %>%
    full_join(ai_an_pct_vacc, by = c("state", "date")) %>%
    full_join(nh_pi_pct_vacc, by = c("state", "date")) %>%
    full_join(df_bipoc_pct_vax, by = c("state", "date")) %>%
    dplyr::select(state, date, known_race_pct_of_vacc_othmiss, everything()) %>%
    arrange(state, date)
  
  # check for proportions > 1 and replace instances with NAs
  proportion_vars <- df %>% dplyr::select(ends_with("pct_vacc_est"), ends_with("pct_vacc_rpt")) %>% names()
  for (var in proportion_vars) {
    if(max(df[var], na.rm = TRUE) >= 1 ) {
      warning(paste(var, "contains values greater than 1. Replacing instances with NA"))
    }
  }
  
  #dealing with values over 1
  df <- df %>% 
    
    #if _rpt version is greater than 1 recode _rpt and _est to NA
    mutate(
      
      #_est first
      white_pct_vacc_est = ifelse(white_pct_vacc_rpt>1,NA,white_pct_vacc_est),
      black_pct_vacc_est = ifelse(black_pct_vacc_rpt>1,NA,black_pct_vacc_est),
      asian_pct_vacc_est = ifelse(asian_pct_vacc_rpt>1,NA,asian_pct_vacc_est),
      hispanic_pct_vacc_est = ifelse(hispanic_pct_vacc_rpt>1,NA,hispanic_pct_vacc_est),
      ai_an_pct_vacc_est = ifelse(ai_an_pct_vacc_rpt>1,NA,ai_an_pct_vacc_est),
      nh_pi_pct_vacc_est = ifelse(nh_pi_pct_vacc_rpt>1,NA,nh_pi_pct_vacc_est),
      
      #then _rpt
      white_pct_vacc_rpt = ifelse(white_pct_vacc_rpt>1,NA,white_pct_vacc_rpt),
      black_pct_vacc_rpt = ifelse(black_pct_vacc_rpt>1,NA,black_pct_vacc_rpt),
      asian_pct_vacc_rpt = ifelse(asian_pct_vacc_rpt>1,NA,asian_pct_vacc_rpt),
      hispanic_pct_vacc_rpt = ifelse(hispanic_pct_vacc_rpt>1,NA,hispanic_pct_vacc_rpt),
      ai_an_pct_vacc_rpt = ifelse(ai_an_pct_vacc_rpt>1,NA,ai_an_pct_vacc_rpt),
      nh_pi_pct_vacc_rpt = ifelse(nh_pi_pct_vacc_rpt>1,NA,nh_pi_pct_vacc_rpt)) %>%
    
    #should be no more _rpt greater than one, just need to topcode any _est over 1 to 99
    mutate_at(vars(ends_with("pct_vacc_est")),
                         ~case_when(
                           is.na(.) ~ NA_real_,
                           . > 1 ~ 0.99,
                           TRUE ~ .
                           )
                         )
  
  #hard coding in wisconsin and pa data that looks weird; will remove later
  
  pa_suppress_date <- as.Date("2021-06-21")
  
  df <- df %>%
    mutate(bipoc_pct_vacc_est= 
           ifelse(state=="Wisconsin",NA, #suppressing Wisconsin for all dates b/c looks werid going back in time too
                  ifelse(state=="Pennsylvania" & date>pa_suppress_date, NA, bipoc_pct_vacc_est)),
         bipoc_pct_vacc_rpt= 
           ifelse(state=="Wisconsin",NA, 
                  ifelse(state=="Pennsylvania" & date>pa_suppress_date, NA, bipoc_pct_vacc_rpt)),
         
         bipoc_cum_people_vacc_est= 
           ifelse(state=="Wisconsin",NA,
                  ifelse(state=="Pennsylvania" & date>pa_suppress_date, NA, bipoc_cum_people_vacc_est)),
         bipoc_cum_people_vacc_rpt= 
           ifelse(state=="Wisconsin",NA, 
                  ifelse(state=="Pennsylvania" & date>pa_suppress_date, NA, bipoc_cum_people_vacc_rpt)),
         
         bipoc_pct_of_vacc= 
           ifelse(state=="Wisconsin",NA, 
                  ifelse(state=="Pennsylvania" & date>pa_suppress_date, NA, bipoc_pct_of_vacc)),
         
         hispanic_pct_vacc_est= ifelse(state=="Pennsylvania" & date>pa_suppress_date, NA, hispanic_pct_vacc_est),
         white_pct_vacc_est= ifelse(state=="Pennsylvania" & date>pa_suppress_date, NA, white_pct_vacc_est),
         black_pct_vacc_est= ifelse(state=="Pennsylvania" & date>pa_suppress_date, NA, black_pct_vacc_est))
    
  
  # archive df in case kff stops hosting files
  write_csv(df, here::here("data", "pct_vacc_by_race_state.csv"))
  
  # remove currently unused variables
  df <- dplyr::select(df, -c(asian_pct_vacc_rpt, white_pct_vacc_rpt, black_pct_vacc_rpt, 
                             white_pct_vacc_rpt, ai_an_pct_vacc_rpt, nh_pi_pct_vacc_rpt,
                             race_incl_hisp, known_ethnicity_pct_of_vacc))
  
  return(df)
}

join_kff_tables <- function(
  filelist,
  replace_stub_1, # text to remove from names
  replace_stub_2, # text to remove from names
  suffix,
  in_names =  # variables to rename
    c("Race.Categories.Include.Hispanic.Individuals",
      "Location"),
  out_names = # new names for variables in in_names
    c("race_incl_hisp",
      "state"),
  url = 
    "https://raw.githubusercontent.com/KFFData/COVID-19-Data/kff_master/Race%20Ethnicity%20COVID-19%20Data/Vaccines/"
) {
  for (i in 1:length(filelist)) { 
    file_info <- filelist[[i]] %>% strsplit("_", fixed = TRUE) %>% unlist()
    date <- lubridate::ymd(file_info[1])
    filename <- unlist(filelist[[i]])
    filename <- gsub(" ", '%20', filename)
    filename <- gsub(",", '%2C', filename)
    df_onedate <- get_kff_helper(filename = filename,
                                 replace_stub_1 = replace_stub_1,
                                 replace_stub_2 = replace_stub_2, 
                                 suffix = suffix,
                                 in_names =  in_names,
                                 out_names = out_names,
                                 url = url) %>%
      mutate(date = !!date)
    
    if (i == 1) {
      df <- df_onedate
    } else {
      df <- rbind(df,
                  df_onedate)
    }
  }
  return(df)
  
}

# selects all files from kff github with a given title
get_kff_filelist <- function(search_text, # text to search for files of interest
                              site = # location of files of interest
                               "https://api.github.com/repos/KFFData/COVID-19-Data/git/trees/kff_master?recursive=1", 
                             path = # subfolder containing files of interest
                               'Race Ethnicity COVID-19 Data/Vaccines/'
                             
) {
  req <- httr::GET(site)
  txt <- httr::content(req, as = "text")
  df <- tibble::as_tibble(jsonlite::fromJSON(txt, flatten = T)$tree)
  filelist <- df %>% filter(grepl(search_text, path)) %>% pull(path) %>%
    lapply(function(x) sub(path, "", x))
  
  return(filelist)
}

# load and clean excel file from KFF github page
get_kff_helper <- function(filename, # name of file to load
                           replace_stub_1, # text to remove from names
                           replace_stub_2, # text to remove from names
                           suffix,
                           in_names =  # variables to rename
                             c("Race.Categories.Include.Hispanic.Individuals",
                               "Location"),
                           out_names = # new names for variables in in_names
                             c("race_incl_hisp",
                               "state"),
                           url = 
                             "https://raw.githubusercontent.com/KFFData/COVID-19-Data/kff_master/Race%20Ethnicity%20COVID-19%20Data/Vaccines/") {

  new_names <-data.frame(old_varname=in_names,
                         new_varname=out_names, 
                         stringsAsFactors = F)  
  
  # load list of valid states to filter out non-state rows
  # in particular "United States" row and "Footnotes" row
  state_names <- data.frame("state" = datasets::state.name, stringsAsFactors = FALSE) %>%
    bind_rows(data.frame("state" = c("District of Columbia", "Puerto Rico")))
  
  df <- read.csv(paste0(url, filename)) 
  
  # ensure the first column has the name location 
  # some instances of loading the df result in
  # the word location with random additional characters
  names(df)[1] <- "Location"
  
  df <- df %>% 
    dplyr::select(-one_of("Footnotes")) %>%
    dplyr::select(-ends_with("Ratio"), -starts_with("Percentage.Point")) %>%
    mutate_at( vars(-Location,
                    -Race.Categories.Include.Hispanic.Individuals),
               ~as.numeric(
                 as.character(
                   na_if(na_if(., "NR"), "<.01")
                 )
               )
    ) %>% 
    rename_with(.fn = ~gsub(replace_stub_1, "", .x)) %>%
    rename_with(.fn = ~gsub(replace_stub_2, "", .x)) %>%
    rename(setNames(new_names$old_varname, new_names$new_varname)) %>%
    rename_with(.fn = ~gsub("\\.", "_", .x)) %>%
    mutate(race_incl_hisp = as.character(race_incl_hisp),
           state = as.character(state)) %>%
    rename_with(.cols = -c(state, race_incl_hisp), .fn = ~paste0(.x, suffix)) %>%
    rename_with(.fn = ~tolower(.x)) %>%
    # restrict to rows with valid state names
    mutate(state=trimws(state)) %>% #trim white space first
    inner_join(state_names, by = "state")

  return(df)
}



# construct percent (race/ethnicity) vaccinated based on race/ethnicity percent of vaccinations
# population by race, and total vaccinations
construct_pct_vacc <- function(
  re_cat, 
  re_inname,
  df_pop_by_race_state,
  df_pct_pop_by_race_state,
  df_pct_of_vacc,
  df_us_vacc,
  pop_thresh = 0.05
                              ) {
  
  # this procedure generates estimated percent of population vaccinated
  # assuming vaccinations without reported race/ethnicity had the same
  # distribution as those with reported race/ethnicity
  re_pct_of_vacc <- paste0(re_cat, "_pct_of_vacc")
  re_pct_vacc_est <- paste0(re_cat, "_pct_vacc_est")
  re_pct_vacc_rpt <- paste0(re_cat, "_pct_vacc_rpt")
  
  # calculate population total of American Indian and Alaskan Native/Native Hawaiian and Pacific Islander
  # restrict to states with at least 5 % population belonging to relevant category
  df_re_pop <- df_pct_pop_by_race_state %>% filter(!!ensym(re_inname) >= pop_thresh) %>%
    rename(state = Location) %>%
    left_join(df_pop_by_race_state, by = c("state")) %>%
    mutate(pop_re = !!ensym(re_cat)) %>%
    dplyr::select(state, pop_re)
  
  df_re_pct_vacc <- df_re_pop %>% 
    left_join(df_pct_of_vacc, by = "state") %>%
    left_join(df_us_vacc, by = c("state", "date")) %>%
    dplyr::mutate(!!ensym(re_pct_vacc_est) := (!!ensym(re_pct_of_vacc) * cum_people_vacc)/pop_re) %>%
    dplyr::mutate(!!ensym(re_pct_vacc_rpt) := !!ensym(re_pct_vacc_est) * known_race_pct_of_vacc) %>%
    dplyr::select(state, date, !!ensym(re_pct_vacc_rpt), !!ensym(re_pct_vacc_est))
  
  return(df_re_pct_vacc)
  
}


get_state_pop_race_cat <- function(pop_year, num_race_cats, age_cutoff = 0) {
  # age cutoff is the age above which individuals will be included in the total
  # num_race_cats should either be 5 or 6
  # 5: 5 race categories, include combinations with other races. Sum of race categories
  #      is more than the sum of the population
  # 6: 6 race categories, either [race] alone, or multiple races. Sum of race categories is
  #      equal to the population.
  #see documentation 
  # https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-detail.html#par_textimage_785300169
  #https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/sc-est2019-alldata5.pdf
  #https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/sc-est2019-alldata6.pdf
  
  filename <- paste0("sc-est2019-alldata", num_race_cats, ".csv")
  
  pop_var <- paste0("POPESTIMATE", pop_year)
  
  race_crosswalk <- data.frame(RACE = c(1, 2, 3, 4, 5, 6), race = c("white", "black", "ai_an", "asian", "nh_pi", "multiple"), stringsAsFactors = FALSE)
  
  df_census <- read_csv(here::here("data", filename)) %>%
    # exclude "total" categories to avoid double counting
    dplyr::filter(SEX != 0,
                  ORIGIN != 0,
                  AGE >= age_cutoff)
  
  df_race <- df_census %>%
    dplyr::select(NAME, SEX, ORIGIN, RACE, !!ensym(pop_var)) %>% 
    group_by(NAME, RACE) %>%
    summarize(pop = sum(!!ensym(pop_var))) %>%
    ungroup() %>%
    left_join(race_crosswalk, by = c("RACE")) %>%
    dplyr::rename(state = NAME) %>%
    dplyr::select(state, race, pop) %>%
    group_by(state) %>%
    spread(race, pop) %>%
    ungroup()
  
  df_ethn <- df_census %>%
    dplyr::select(NAME, SEX, ORIGIN, RACE, !!ensym(pop_var)) %>%
    # keep only white and hispanic individuals
    dplyr::filter(RACE == 1 | ORIGIN == 2) %>%
    mutate(race = case_when(
      RACE == 1 & ORIGIN == 1  ~ "white_nh",
      ORIGIN == 2 ~ "hispanic",
      TRUE ~ "error")) %>%
    assertr::verify(race != "error") %>%
    group_by(NAME, race) %>%
    summarize(pop = sum(!!ensym(pop_var))) %>%
    ungroup() %>%
    dplyr::rename(state = NAME) %>%
    dplyr::select(state, race, pop) %>%
    group_by(state) %>%
    spread(race, pop) %>%
    ungroup()
  
  df <- full_join(df_race, df_ethn, by = c("state"))

}
  
# create a time series for US vaccinations by race 
get_us_nat_race_vacc <- function() {
  race_vax <- jsonlite::fromJSON("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_demographics_data")
  
  ts <- read_csv("data/us_nat_vacc_by_race.csv")
  
  df <- race_vax$vaccination_demographics_data %>%
    filter(grepl("Race_eth_", Demographic_category)) %>%
    rename("one_dose" = "Administered_Dose1",
           "full_vacc" = "Series_Complete_Yes") %>%
    mutate(race_c = gsub("Race_eth_", "", Demographic_category),
           race = case_when(
             race_c == "Hispanic" ~ "hispanic",
             race_c == "NHAIAN" ~ "natam",
             race_c == "NHAsian" ~ "asian",
             race_c == "NHBlack" ~ "black",
             race_c == "NHNHOPI" ~ "natpac",
             race_c == "NHWhite" ~ "white",
             race_c == "NHMult_Oth" ~ "other",
             race_c == "unknown" ~ race_c),
           country = "United States",
           date = as.Date(Date)) %>%
    filter(!is.na(race)) %>%
    dplyr::select(race, one_dose, full_vacc, date, country) %>%
    gather(group, count, -c(race, date, country)) %>%
    unite(temp, group, race) %>%
    spread(temp, count)
  
  ts_new <- df %>%
    bind_rows(ts) %>%
    filter(!duplicated(date)) %>%
    # CDC stopped reporting "other" category, forward fill
    mutate(
      full_vacc_other = case_when(
        date == "2022-04-03" ~ filter(ts, date == "2022-03-20") %>% pull(full_vacc_other),
        T ~ full_vacc_other
      ),
      one_dose_other = case_when(
        date == "2022-04-03" ~ filter(ts, date == "2022-03-20") %>% pull(one_dose_other),
        T ~ one_dose_other
      )
    )
  
  write.csv(ts_new, "data/us_nat_vacc_by_race.csv", row.names = FALSE)
  
  ts_out <- ts_new %>%
    mutate(total_vacc = (one_dose_asian + one_dose_black + one_dose_hispanic + one_dose_natpac + one_dose_natam + one_dose_other + one_dose_white + one_dose_unknown),
           known_race_pct_of_vacc_othmiss = (1 - ((one_dose_unknown + one_dose_other)/total_vacc))*100,
           total_full = (full_vacc_asian + full_vacc_black + full_vacc_hispanic + one_dose_natpac + full_vacc_natam + full_vacc_other + full_vacc_white + full_vacc_unknown),
           known_race_pct_of_full = (1 - (full_vacc_unknown/total_full))*100) %>%
    dplyr::select(-c(full_vacc_unknown, one_dose_unknown, total_vacc, total_full))
  
  return(ts_out)
}
# estimate vaccinations by race at the national level based on state data from KFF
get_est_us_nat_vacc_by_race <- function(df_us_vacc) {
  
  # include estimated vaccination rates for ai_an in months before states
  # started reporting data (esp. SD)
  df_us_state_vacc_by_race <- get_us_state_vacc_by_race(df_us_vacc, impute_pct_of_vacc_vars = c("ai_an"))
  df_us_nat_race_vacc <- get_us_nat_race_vacc()
  # 2019 US population by race and state
  # from census
  # version where respondents must report a single race or "multiple races" category
  df_pop_by_race_state_mutexcl <- get_state_pop_race_cat(pop_year = 2019, num_race_cats = 6, age_cutoff = vacc_elig_age)
  
  # calculate state population (of at least vaccine_elig_age) by adding together
  # all race categories in the mutually exclusive state population df
  df_pop_by_state <-  df_pop_by_race_state_mutexcl %>% 
    rowwise() %>%
    mutate(population = sum(c(white, black, ai_an, asian, nh_pi, multiple))) %>%
    ungroup() %>%
    dplyr::select(state, population)
  
  df_bipoc_pop <- df_pop_by_race_state_mutexcl %>%
    full_join(df_pop_by_state, by = "state") %>%
    mutate(bipoc_pop_incl_wh = population - white_nh,
           bipoc_pop_share_incl_wh = (population - white_nh)/population)
  
  # for states that are missing observations from OWID's cumulative vaccinations
  # metric, impute values
  df_us_vacc_dates <- data.frame(date = unique(df_us_vacc$date)) %>% mutate(merge_ind = 1) %>% dplyr::arrange(date)
  df_us_vacc_states <- data.frame(state = unique(df_us_vacc$state)) %>% mutate(merge_ind = 1)
  df_us_vacc_imputed <- df_us_vacc_states %>% full_join(df_us_vacc_dates, by = "merge_ind") %>%
    dplyr::select(-merge_ind) %>%
    left_join(df_us_vacc, by = c("state", "date")) %>%
    dplyr::group_by(state) %>% 
    # impute missing cumulative vaccinations variables
    tidyr::fill(cum_people_vacc, 
                .direction = "down") %>%
    ungroup()
  
  
  # total # bipoc vaccinations. Sum state bipoc vaccinations. For states without
  # race reported, assume bipoc vaccinations are proportionate to the bipoc share of 
  # of population
  df_pct_vacc_est <- df_us_state_vacc_by_race %>%
    
    left_join(df_bipoc_pop, by = c("state")) %>%
    left_join(df_us_vacc_imputed, by = c("state", "date"))
    
  #########################################################
  #START impute missing percent of known vaccinations
  
  df_pct_vacc_est <- df_pct_vacc_est %>%
    #first flagging states always missing bipoc_pct_vacc_est and known_race_pct_of_vacc
    dplyr::group_by(state) %>%
    mutate(allNA= all(is.na(bipoc_pct_vacc_est)) & all(is.na(known_race_pct_of_vacc))) %>%
    mutate(known_allNA = all(is.na(known_race_pct_of_vacc))) %>%
    ungroup() %>%
    mutate(group = known_allNA == FALSE & !is.na(bipoc_pct_vacc_est),
           known_race_pct_of_vacc = ifelse(allNA==TRUE, 0, known_race_pct_of_vacc)) #if if known_race_pct_of vacc and bipoc_pct_vacc_est are always missing, then known_race_pct_of_vacc should be 0
  
  #If known_race_pct_of_vacc appears for at least one date AND bipoc_pct_vacc_est is not missing in that row (group var above), 
  #then fill down first if possible, and then fill up.
  df_pct_vacc_est1 <- df_pct_vacc_est %>%
    
    filter(group==TRUE) %>%
    dplyr::group_by(state) %>%
    arrange(date, .by_group = TRUE) %>%
    tidyr::fill(known_race_pct_of_vacc, 
                .direction = "downup") %>% #fill first down and then up within state
    ungroup() 
  
  df_pct_vacc_est2 <- df_pct_vacc_est %>% filter(group==FALSE)
  
  stopifnot(nrow(df_pct_vacc_est1) + nrow(df_pct_vacc_est2) == nrow(df_pct_vacc_est)) #confirming that we're not losing any rows
  
  #combine back together
  df_pct_vacc_est <- bind_rows(df_pct_vacc_est1,df_pct_vacc_est2)
  
  # if If known_race_pct_of_vacc appears for at least one date AND bipoc_pct_vacc_est is missing in that row, 
  #then calculate a known_race_pct_of_vacc = 
    #(known_race_pct_of_vacc from most recent previous date * cum_people_vacc for the most recent previous date ) / cum_people_vacc for the current date
  df_pct_vacc_est1 <- df_pct_vacc_est %>%
    filter(known_allNA == FALSE) %>% #known_race_pct_of_vacc appears for at least one date
    group_by(state) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(lag_known_race_pct_of_vacc = lag(known_race_pct_of_vacc)) %>% #creating this variable among all rows for now
    tidyr::fill(lag_known_race_pct_of_vacc, 
                .direction = "down") %>% #fill down within state so each NA has the most recent value
    mutate(cum_people_vacc2 = ifelse(is.na(bipoc_pct_vacc_est), NA, cum_people_vacc)) %>% #get most recent cum_people_vacc if bipoc_pct_vacc_est is missing (see fill below)
    tidyr::fill(cum_people_vacc2, 
                .direction = "down") %>%
    ungroup() %>%
    filter(is.na(bipoc_pct_vacc_est) & is.na(known_race_pct_of_vacc)) %>% #known_race_pct_of_vacc and bipoc_pct_vacc_est both missing in the row
    mutate(known_race_pct_of_vacc = (lag_known_race_pct_of_vacc * cum_people_vacc2)/cum_people_vacc) #if lagged variables are missing, the missing carries into known_race_pct_of_vacc
  
  df_pct_vacc_est2 <- df_pct_vacc_est %>% filter(known_allNA == TRUE) 
  df_pct_vacc_est3 <- df_pct_vacc_est %>% filter(known_allNA == FALSE) %>% filter(!(is.na(bipoc_pct_vacc_est) & is.na(known_race_pct_of_vacc)))
  
  stopifnot(nrow(df_pct_vacc_est1) + nrow(df_pct_vacc_est2) + nrow(df_pct_vacc_est3) == nrow(df_pct_vacc_est)) #confirming that we're not losing any rows
  
  #combine back together
  df_pct_vacc_est <- bind_rows(df_pct_vacc_est1,df_pct_vacc_est2,df_pct_vacc_est3)
  
  df_pct_vacc_est <- df_pct_vacc_est %>%
    #If known_race_pct_of_vacc and bipoc_pct_vacc_est are BOTH missing for that row, and there is no previous most recent known_race_pct_of_vacc for a state/date, mark as zero
    mutate(known_race_pct_of_vacc=
             ifelse(is.na(known_race_pct_of_vacc) & is.na(bipoc_pct_vacc_est) & is.na(lag_known_race_pct_of_vacc), 0, known_race_pct_of_vacc)) %>%
    #if When known_race_pct_of_vacc is always missing, but bipoc_pct_vacc_est is non-missing, then impute with unweighted mean across states within date
    group_by(date) %>%
    mutate(known_race_pct_of_vacc=
             ifelse(known_allNA== TRUE & !is.na(bipoc_pct_vacc_est), 
                    mean(known_race_pct_of_vacc,na.rm=TRUE), 
                    known_race_pct_of_vacc)) %>%
    ungroup()
  
  #fixing the one Wisconsin missing value
  df_pct_vacc_est <- df_pct_vacc_est %>%
    mutate(known_race_pct_of_vacc = ifelse(state=="Wisconsin" & is.na(known_race_pct_of_vacc), lag_known_race_pct_of_vacc, known_race_pct_of_vacc))
    
  df_pct_vacc_est <- df_pct_vacc_est %>%
    assertr::verify(!is.na(known_race_pct_of_vacc)) %>% #check that now all missing variables are imputed
    dplyr::select(-allNA,-known_allNA,-group,-lag_known_race_pct_of_vacc, -cum_people_vacc2) #dropping variables used above for imputation
  
  #END impute missing percent of known vaccinations
  #########################################################
  
  #########################################################
  #START impute missing percent of known vaccinations (othmiss version)
  
  df_pct_vacc_est <- df_pct_vacc_est %>%
  
    #first flagging states always missing bipoc_pct_vacc_est and known_race_pct_of_vacc
    dplyr::group_by(state) %>%
      mutate(allNA= all(is.na(bipoc_pct_vacc_est)) & all(is.na(known_race_pct_of_vacc_othmiss))) %>%
      mutate(known_allNA = all(is.na(known_race_pct_of_vacc_othmiss))) %>%
      ungroup() %>%
      mutate(group = known_allNA == FALSE & !is.na(bipoc_pct_vacc_est),
             known_race_pct_of_vacc_othmiss = ifelse(allNA==TRUE, 0, known_race_pct_of_vacc_othmiss)) #if if known_race_pct_of vacc and bipoc_pct_vacc_est are always missing, then known_race_pct_of_vacc_othmiss should be 0
    
  #If known_race_pct_of_vacc_othmiss appears for at least one date AND bipoc_pct_vacc_est is not missing in that row (group var above), 
  #then fill down first if possible, and then fill up.
  df_pct_vacc_est1 <- df_pct_vacc_est %>%
    
    filter(group==TRUE) %>%
    dplyr::group_by(state) %>%
    arrange(date, .by_group = TRUE) %>%
    tidyr::fill(known_race_pct_of_vacc_othmiss, 
                .direction = "downup") %>% #fill first down and then up within state
    ungroup() 
  
  df_pct_vacc_est2 <- df_pct_vacc_est %>% filter(group==FALSE)
  
  stopifnot(nrow(df_pct_vacc_est1) + nrow(df_pct_vacc_est2) == nrow(df_pct_vacc_est)) #confirming that we're not losing any rows
  
  #combine back together
  df_pct_vacc_est <- bind_rows(df_pct_vacc_est1,df_pct_vacc_est2)
  
  # if If known_race_pct_of_vacc_othmiss appears for at least one date AND bipoc_pct_vacc_est is missing in that row, 
  #then calculate a known_race_pct_of_vacc_othmiss = 
  #(known_race_pct_of_vacc_othmiss from most recent previous date * cum_people_vacc for the most recent previous date ) / cum_people_vacc for the current date
  df_pct_vacc_est1 <- df_pct_vacc_est %>%
    filter(known_allNA == FALSE) %>% #known_race_pct_of_vacc_othmiss appears for at least one date
    group_by(state) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(lag_known_race_pct_of_vacc_othmiss = lag(known_race_pct_of_vacc_othmiss)) %>% #creating this variable among all rows for now
    tidyr::fill(lag_known_race_pct_of_vacc_othmiss, 
                .direction = "down") %>% #fill down within state so each NA has the most recent value
    mutate(cum_people_vacc2 = ifelse(is.na(bipoc_pct_vacc_est), NA, cum_people_vacc)) %>% #get most recent cum_people_vacc if bipoc_pct_vacc_est is missing (see fill below)
    tidyr::fill(cum_people_vacc2, 
                .direction = "down") %>%
    ungroup() %>%
    filter(is.na(bipoc_pct_vacc_est) & is.na(known_race_pct_of_vacc_othmiss)) %>% #known_race_pct_of_vacc_othmiss and bipoc_pct_vacc_est both missing in the row
    mutate(known_race_pct_of_vacc_othmiss = (lag_known_race_pct_of_vacc_othmiss * cum_people_vacc2)/cum_people_vacc) #if lagged variables are missing, the missing carries into known_race_pct_of_vacc_othmiss
  
  df_pct_vacc_est2 <- df_pct_vacc_est %>% filter(known_allNA == TRUE) 
  df_pct_vacc_est3 <- df_pct_vacc_est %>% filter(known_allNA == FALSE) %>% filter(!(is.na(bipoc_pct_vacc_est) & is.na(known_race_pct_of_vacc_othmiss)))
  
  stopifnot(nrow(df_pct_vacc_est1) + nrow(df_pct_vacc_est2) + nrow(df_pct_vacc_est3) == nrow(df_pct_vacc_est)) #confirming that we're not losing any rows
  
  #combine back together
  df_pct_vacc_est <- bind_rows(df_pct_vacc_est1,df_pct_vacc_est2,df_pct_vacc_est3)
  
  df_pct_vacc_est <- df_pct_vacc_est %>%
    #If known_race_pct_of_vacc_othmiss and bipoc_pct_vacc_est are BOTH missing for that row, and there is no previous most recent known_race_pct_of_vacc_othmiss for a state/date, mark as zero
    mutate(known_race_pct_of_vacc_othmiss=
             ifelse(is.na(known_race_pct_of_vacc_othmiss) & is.na(bipoc_pct_vacc_est) & is.na(lag_known_race_pct_of_vacc_othmiss), 0, known_race_pct_of_vacc_othmiss)) %>%
    #if When known_race_pct_of_vacc_othmiss is always missing, but bipoc_pct_vacc_est is non-missing, then impute with unweighted mean across states within date
    group_by(date) %>%
    mutate(known_race_pct_of_vacc_othmiss=
             ifelse(known_allNA== TRUE & !is.na(bipoc_pct_vacc_est), 
                    mean(known_race_pct_of_vacc_othmiss,na.rm=TRUE), 
                    known_race_pct_of_vacc_othmiss)) %>%
    ungroup()
  
  #fixing the one Wisconsin missing value
  df_pct_vacc_est <- df_pct_vacc_est %>%
    mutate(known_race_pct_of_vacc_othmiss = ifelse(state=="Wisconsin" & is.na(known_race_pct_of_vacc_othmiss), lag_known_race_pct_of_vacc_othmiss, known_race_pct_of_vacc_othmiss))
  
  df_pct_vacc_est <- df_pct_vacc_est %>%
    assertr::verify(!is.na(known_race_pct_of_vacc_othmiss)) %>% #check that now all missing variables are imputed
    dplyr::select(-allNA,-known_allNA,-group,-lag_known_race_pct_of_vacc_othmiss, -cum_people_vacc2) #dropping variables used above for imputation
  
  #END impute missing percent of known vaccinations
  #########################################################
  
  df_pct_vacc_est <- df_pct_vacc_est %>%
    mutate(bipoc_vacc_full = case_when(
      is.na(bipoc_cum_people_vacc_est) ~ bipoc_pop_share_incl_wh * cum_people_vacc,
      !is.na(bipoc_cum_people_vacc_est) ~ bipoc_cum_people_vacc_est,
      TRUE ~ -1
    )
    ) %>%
    mutate(bipoc_cum_people_vacc_rpt_kff = bipoc_vacc_full*known_race_pct_of_vacc) %>% #recalculating this AFTER the imputation
    mutate(weighted_known_race = cum_people_vacc*known_race_pct_of_vacc_othmiss) %>%
    
    assertr::verify(bipoc_vacc_full != -1| is.na(bipoc_vacc_full)) %>%
    # bipoc_vacc_full should not be missing based on variable construction
    assertr::verify(!is.na(bipoc_vacc_full)) %>%
    assertr::verify(!is.na(bipoc_pop_incl_wh)) %>%
    assertr::verify(!is.na(population)) %>%
    assertr::verify(!is.na(cum_people_vacc)) %>%
    assertr::verify(!is.na(bipoc_cum_people_vacc_rpt_kff) & 
                      (bipoc_cum_people_vacc_rpt_kff != 0 | known_race_pct_of_vacc == 0)) 
  
  #########################################################
  #START impute missing *_pct_vacc_est
  
  race_vars <- c('asian_pct_vacc_est',
                 'black_pct_vacc_est',
                 'hispanic_pct_vacc_est') #note: NOT imputing ai and nh - instead using the weighted average of non-missing states
  
  df_pct_vacc_est <- df_pct_vacc_est %>%
    
    #interpolate vacc by race, which are only available intermittently
    #this fills only between two existing values - leaves lagging/proceeding NAs
    #if data never exist for a state this leaves the NAs as is
    group_by(state) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate_at(vars(all_of(race_vars), white_pct_vacc_est),
              ~ na.approx(., na.rm=FALSE)) %>%
    ungroup() %>%
    
    # only keep if at least 30 states are available for each date - flagging here and then using down below
    group_by(date) %>%
    mutate(   count_white = sum(!is.na(white_pct_vacc_est)),  
              count_black = sum(!is.na(black_pct_vacc_est)),
              count_hispanic = sum(!is.na(hispanic_pct_vacc_est)),
              count_asian = sum(!is.na(asian_pct_vacc_est))) %>%
    ungroup() %>%
    
    #If *pct_vacc_est is missing otherwise assume that each missing value of *pct_vacc_est (non-white) is the same as the 
    #statewide estimated BIPOC vaccination rate for that date
    mutate_at(vars(all_of(race_vars)),
              #~ifelse(is.na(.),bipoc_pct_vacc_est,.)) %>% #not using this because there is missingness
              ~ifelse(is.na(.),bipoc_vacc_full/bipoc_pop_incl_wh,.)) %>% #using this version instead (this mirrors the roll-up calculation below for bipoc_pct_vacc_est)
    
    #If *pct_vacc_est is missing otherwise assume that each missing value of white_pct_vacc_est is the same as the 
    #statewide total vaccinations/total people
    mutate_at(vars(white_pct_vacc_est),
              ~ifelse(is.na(.),cum_people_vacc/population,.)) #population is the eligible population, calculated above
    
    #confirming that there are now no missings for the race_vars
    stopifnot(sum(is.na(df_pct_vacc_est[race_vars]))==0)
    stopifnot(sum(is.na(df_pct_vacc_est$white_pct_vacc_est))==0)
    
    df_pct_vacc_est <- df_pct_vacc_est %>%
    
    #only keep if at least 30 states are available for each date 
    mutate(white_pct_vacc_est = ifelse(count_white<30, NA, white_pct_vacc_est),
           black_pct_vacc_est = ifelse(count_black<30, NA, black_pct_vacc_est),
           hispanic_pct_vacc_est = ifelse(count_hispanic<30, NA, hispanic_pct_vacc_est),
           # for asian keep if at least 25 states. reducing threshold because many states 
           # are missing asian pct vacc estimates
           asian_pct_vacc_est = ifelse(count_asian<25, NA, asian_pct_vacc_est))
  
  #END impute missing *_pct_vacc_est
  #########################################################
  
  df_pct_vacc_est <- df_pct_vacc_est %>%     
    group_by(date) %>%
    summarise(count = sum(!is.na(bipoc_cum_people_vacc_est)),
              bipoc_cum_people_vacc_rpt_kff = sum(bipoc_cum_people_vacc_rpt_kff, na.rm = TRUE), #kff reported total
              bipoc_pop_incl_wh_sum = sum(bipoc_pop_incl_wh), #sum of pop to use in row below
              bipoc_pct_vacc_rpt_kff  = bipoc_cum_people_vacc_rpt_kff/bipoc_pop_incl_wh_sum, #kff pct reported
              known_race_pct_of_vacc_kff = sum(weighted_known_race)/sum(cum_people_vacc), #weighted sum of proportion of vaccinations reporting race via KFF
              bipoc_cum_people_vacc_est = sum(bipoc_vacc_full),
              bipoc_pct_vacc_est  = bipoc_cum_people_vacc_est/bipoc_pop_incl_wh_sum,
              
              white_pct_vacc_est  = weighted.mean(white_pct_vacc_est, white, na.rm = TRUE),
              asian_pct_vacc_est  = weighted.mean(asian_pct_vacc_est, asian, na.rm = TRUE),
              black_pct_vacc_est  = weighted.mean(black_pct_vacc_est, black, na.rm = TRUE),
              hispanic_pct_vacc_est  = weighted.mean(hispanic_pct_vacc_est, hispanic, na.rm = TRUE),
              ai_an_pct_vacc_est  = weighted.mean(ai_an_pct_vacc_est, ai_an, na.rm = TRUE), #this is a weighted mean among non-missing state/date combos
              nh_pi_pct_vacc_est  = weighted.mean(nh_pi_pct_vacc_est, nh_pi, na.rm = TRUE), #this is a weighted mean among non-missing state/date combos
              
              bipoc_pct_of_pop = sum(bipoc_pop_incl_wh)/sum(population),
              bipoc_pct_of_vacc = bipoc_cum_people_vacc_est/sum(cum_people_vacc)) %>%
    
    assertr::assert(within_bounds(0,1),bipoc_pct_vacc_rpt_kff) %>%
    assertr::assert(within_bounds(0,1),known_race_pct_of_vacc_kff) %>%
    
    mutate(country = "United States",
           bipoc_vacc_disparity = bipoc_pct_of_vacc - bipoc_pct_of_pop) %>%
    # only keep state sum if at least 30 states are available
    mutate_at(vars(c(bipoc_cum_people_vacc_est,
                bipoc_cum_people_vacc_rpt_kff,
                bipoc_pct_vacc_est,
                bipoc_pct_vacc_rpt_kff,
                bipoc_pct_of_pop,
                bipoc_pct_of_vacc,
                bipoc_vacc_disparity)), 
             ~case_when(is.na(count) ~ NA_real_, 
                 count < 30 ~ NA_real_, 
                 TRUE ~ .x)) %>%
    dplyr::select(-count)
  
  df_pct_vacc_rpt <-  df_us_nat_race_vacc %>% 
    left_join(df_bipoc_pop %>% 
                dplyr::select(bipoc_pop_incl_wh) %>%
                summarise_all(~sum(.x, na.rm = TRUE)) %>%
                mutate(country = "United States"),
              by = "country") %>%
    rowwise() %>%
    mutate(bipoc_cum_people_vacc_rpt = sum(one_dose_black, 
                                           one_dose_hispanic, 
                                           one_dose_natpac, 
                                           one_dose_natam, 
                                           one_dose_asian)) %>%
    ungroup() %>%
    mutate(bipoc_pct_vacc_rpt = bipoc_cum_people_vacc_rpt/bipoc_pop_incl_wh,
           known_race_pct_of_vacc_othmiss = known_race_pct_of_vacc_othmiss/100) %>%
    dplyr::select(date, country, known_race_pct_of_vacc_othmiss, ends_with("_rpt"))
  
  df_pct_vacc <- full_join(df_pct_vacc_est, df_pct_vacc_rpt, by = c("country", "date")) %>%
    arrange(country, date) %>%
    dplyr::select(country, date, everything())
  
  return(df_pct_vacc)
  
}


# create a time series of US vaccinations for 5 demonstration sites
# using CDC county-vaccination api and data from texas DSH
get_us_county_vacc <- function(filename = "us_county_vax_timeseries.csv", 
                              houston_counties = c("Harris", "Fort Bend", "Montgomery")) {
  
  if (file.exists(here::here("data", filename))) {
    # check whether today's date has already been included in the timeseries
    existing_file <- read_csv(here::here("data", filename)) %>%
      mutate(date = lubridate::as_date(date))
      
    date_check <- lubridate::as_date(Sys.Date()) %in% existing_file$date
  } else {
    date_check <- FALSE
  }
  
  # If today has not been added to the timeseries, load data
  if (date_check == FALSE) {  
  
    county_city_crosswalk <- data.frame(fips = c("24510", #baltimore city
                                                 "17031", #cook county/chicago
                                                 "06001",  #alameda county/oakland
                                                 "34013"  #essex county/newark
                                                 ),
                                        county = c("Baltimore", "Cook", "Alameda", "Essex"),
                                        city = c("Baltimore", "Chicago", "Oakland", "Newark"),
                                        stringsAsFactors = FALSE)
    df_cdc <- jsonlite::fromJSON("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data") %>%
      as.data.frame() %>%
      dplyr::filter(vaccination_county_condensed_data.FIPS %in% c("24510", #baltimore city
                                                                  "17031", #cook county/chicago
                                                                  "06001",  #alameda county/oakland
                                                                  "34013"  #essex county/newark
      ) 
      ) %>%
      rename(fips = vaccination_county_condensed_data.FIPS,
             state = vaccination_county_condensed_data.StateName,
             date = vaccination_county_condensed_data.Date,
             cum_people_fully_vacc = vaccination_county_condensed_data.Series_Complete_Yes) %>%
      dplyr::mutate(date = lubridate::ymd(date)) %>%
      dplyr::left_join(county_city_crosswalk, by = c("fips")) %>%
      dplyr::select(date, state, city, county, cum_people_fully_vacc)
    
    # separately read houston data from texas dshs
    tmp <- tempfile(fileext = ".xlsx")
    httr::GET(
      url = "https://www.dshs.texas.gov/immunize/covid19/COVID-19-Vaccine-Data-by-County.xls",
      httr::write_disk(tmp)
    )
    df_houston <- read_excel(tmp, sheet = "By County") %>%
      dplyr::filter(`County Name` %in% !!houston_counties) %>%
      rename(county = `County Name`,
             cum_vaccinations = `People Fully Vaccinated`) %>%
      dplyr::mutate(cum_people_fully_vacc = as.numeric(cum_vaccinations),
                    state = "Texas",
                    date = Sys.Date(),
                    city = "Houston") %>%
      dplyr::select(date, state, city, county, cum_people_fully_vacc)
    
    df_new <- rbind(df_cdc,
                    df_houston)
    
    if (file.exists(here::here("data", filename))) {
      # determine whether any date-city-county pulled from apis
      # match date-city pairs on the existing file
      # (may occur because the CDC does not update dates
      # until ~8PM ET)
      
      df_exclude <- dplyr::intersect(dplyr::select(existing_file, date, city, county),
                    dplyr::select(df_new, date, city, county)) %>% 
                    mutate(exclude = 1)
      existing_file_excluded <- left_join(existing_file, df_exclude, by = c("date", "city", "county")) %>%
                dplyr::filter(is.na(exclude)) %>%
                dplyr::select(-exclude)
      
      # append new data to existing data
      df <- rbind( existing_file_excluded,
                   df_new
      ) %>%
        assert_rows(col_concat, is_uniq, city, date, county) # assert that rows are uniquely identified by city, county, and date
    } else {
      df <- df_new
    }
    write_csv(df, here::here("data", filename))
    
  } # datecheck = FALSE
  else {
    print(paste(Sys.Date(), "is already included in", filename, ". Loading existing file"))
    df <- existing_file
  } # datecheck = TRUE
  
  return(df)
  
}

#################################################################################
#################################################################################
# SUBROUTINES TO MERGE CLEANED DATAFRAMES
#################################################################################
#################################################################################

# Merge data frames for each indicator from the same geographic level-----------------------------------
indicators_merge <- function(daily_dfs, # list of dataframes
                             annual_dfs, # list of dataframes
                             subregion_1_name = "subregion_1", # name of subregion variable to be renamed 
                             subregion_2_name = "subregion_2", # name of subregion variable to be renamed
                             subregion_3_name = "subregion_3",
                             subregion_1_add = "no", # value of subregion variable to be added to dataframe
                             subregion_2_add = "no",
                             subregion_3_add = "no",
                             country_level = "yes") {
  
  # merge all indicator files for a geography (e.g. countries, India subnational, etc...)
  # if adding a subregion, the same value will be assigned to all data frames
  # if renaming a subregion or date variable, the same original name must be present
  #   on all data frames
  
  df <- region_add(daily_dfs[[1]],
                   subregion_1_add,
                   subregion_2_add,
                   subregion_3_add,
                   subregion_1_name,
                   subregion_2_name,
                   subregion_3_name) 
  
  if (length(daily_dfs) > 1) {
    for (i in 2:length(daily_dfs)) {
      df <- daily_join(x = df, 
                       df_add = daily_dfs[[i]],
                       subregion_1_name = subregion_1_name,
                       subregion_2_name = subregion_2_name,
                       subregion_3_name = subregion_3_name,
                       subregion_1_add = subregion_1_add,
                       subregion_2_add = subregion_2_add,
                       subregion_3_add = subregion_3_add)
    }
  }
  
  if (length(annual_dfs)>0) {
    for (i in 1:length(annual_dfs)) {
      df <- annual_join(df, annual_dfs[[i]],
                        subregion_1_name,
                        subregion_2_name,
                        subregion_3_name,
                        subregion_1_add,
                        subregion_2_add,
                        subregion_3_add
      )
    }
  }
  
  if (country_level == "yes") {
    # link countries to regions if data is at the country level
    df <- left_join(df, df_region_xwalk, by = c("subregion_1")) %>%
      dplyr::select(region, subregion_1, subregion_2, subregion_3, everything()) # re-order columns
    countries_to_drop <- df %>% filter(is.na(region)) %>% pull(subregion_1) %>% unique() %>% sort()
    message(paste0("dropping the following countries: ", knitr::combine_words(countries_to_drop)))
    df <- df %>% filter(!is.na(region)) 
  } else {
    df <- dplyr::select(df, subregion_1, subregion_2, subregion_3, everything()) # re-order columns
  }
  return(df)  
  
}

# Helper function to add and rename subregion variables-----------------------------------
region_add <- function(x,
                       subregion_1_add = "no", # value of subregion variable to be added to dataframe
                       subregion_2_add = "no",
                       subregion_3_add = "no",
                       subregion_1_name = "subregion_1",
                       subregion_2_name = "subregion_2",
                       subregion_3_name = "subregion_3"
) {
  
  # add region variables that are not already present
  if(subregion_1_add != "no") {
    x <- mutate(x, subregion_1 = !!subregion_1_add)
  }
  if(subregion_2_add != "no") {
    x <- mutate(x, subregion_2 = !!subregion_2_add)
  }
  if(subregion_3_add != "no") {
    x <- mutate(x, subregion_3 = !!subregion_3_add)
  }
  
  # if necessary, rename subregion variables
  if((subregion_1_name != "subregion_1") |
     (subregion_2_name != "subregion_2") |
     (subregion_3_name != "subregion_3")) {
    x <- rename(x,
                subregion_1 = !!subregion_1_name,
                subregion_2 = !!subregion_2_name,
                subregion_3 = !!subregion_3_name
    )
  }
  
  return(x)
}


# Merge daily indicators-----------------------------------
daily_join <- function(x, # input dataset
                       df_add, #dataset to merge
                       subregion_1_name = "subregion_1", # name of subregion variable to be renamed 
                       subregion_2_name = "subregion_2", # name of subregion variable to be renamed
                       subregion_3_name = "subregion_3",
                       subregion_1_add = "no", # value of subregion variable to be added to dataframe
                       subregion_2_add = "no",
                       subregion_3_add = "no") {
  
  # input dataframe must have date, subregion_1, and subregion_2 variables
  df_add <- region_add(df_add,
                       subregion_1_add,
                       subregion_2_add,
                       subregion_3_add,
                       subregion_1_name,
                       subregion_2_name,
                       subregion_3_name)
  
  
  df <- full_join(x, 
                  df_add,
                  by = c("subregion_1", "subregion_2", "subregion_3", "date"))
  
  return(df)
  
}

# Merge annual indicators-----------------------------------
annual_join <- function(x, # input dataset
                        df_add, #dataset to merge
                        subregion_1_name = "subregion_1", # name of subregion variable to be renamed 
                        subregion_2_name = "subregion_2", # name of subregion variable to be renamed
                        subregion_3_name = "subregion_3",
                        subregion_1_add = "no", # value of subregion variable to be added to dataframe
                        subregion_2_add = "no",
                        subregion_3_add = "no") {
  
  
  # input dataframe must have subregion_1, and subregion_2 variables
  
  # add region variables that are not already present
  df_add <- region_add(df_add,
                       subregion_1_add,
                       subregion_2_add,
                       subregion_3_add,
                       subregion_1_name,
                       subregion_2_name,
                       subregion_3_name)
  # left join excludes geographies for which we have
  # annual data but no daily data
  df <- left_join(x, 
                  df_add,
                  by = c("subregion_1", "subregion_2", "subregion_3"))
}


replace_jhu <- function(summed, jhu, by = NULL, 
                        country_list = c("United States"),
                        suffix = c(".x", ".y"),
                        join = dplyr::full_join, ...) {
  #' Replaces JHU deaths and recoveries with summed CTP subnational values
  #' for specified countries
  
  joined <- join(summed, jhu, by = by, suffix = suffix, ...)
  
  common_cols <- intersect(names(summed), names(jhu))
  common_cols <- common_cols[!common_cols %in% by]
  
  # finds the first non-missing value
  replaced <- purrr::map_dfc(common_cols, ~ifelse(joined$country %in% country_list,
                                            joined[[paste0(.x, suffix[1])]], 
                                            joined[[paste0(.x, suffix[2])]]
  ))
  names(replaced) <- common_cols
  
  return(dplyr::bind_cols(joined, replaced)[union(names(summed), names(jhu))])
  
}

sum_any_nonull <- function(x){
  #' returns NA if all of series are NA, otherwise sums non-nulls
  ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
}

expand_df_dates <- function(df, mindate, maxdate, fillvars,
                            constant_vars = c("region", "subregion_1", "subregion_2", "subregion_3")) {
  #' fills in all dates between mindate and maxdate to a sparse df. 
  #' fillvars are populated in dates that did not exist on the sparse df.
  
  df_geos <- dplyr::select(df, 
                           !!constant_vars) %>%
    unique()
  
  # create time series from minimum date in data through maximum date in data
  df_dates <- data.frame("date" = seq(from = lubridate::as_date(mindate), to = lubridate::as_date(maxdate), by = "1 day"))
  
  # cartesian merge of country level file and time series
  all_geo_dates <- tidyr::crossing(df_geos, df_dates)
  
  mergelist <- constant_vars %>% append("date")
  
  df_expand <- full_join(all_geo_dates, df, by = mergelist) %>%
    dplyr::arrange(vars(mergelist)) %>%
    dplyr::group_by(!!!syms(constant_vars)) %>%
    # impute missing constant variables
    tidyr::fill(!!fillvars, 
                .direction = "downup") %>%
    ungroup() 
  
  return(df_expand)
  
}

get_LMIC_vacc_pct <- function(){
  
  df_vaccines <- get_vacc_nat()
  df_pop <- get_pop_nat()
  
  #calculating LMIC vaccination rate for region pages
  df_vaccines <- df_vaccines %>% mutate(iso_code= ifelse(iso_code=="OWID_KOS","XKX",iso_code))
  
  LMY_codes <- c("AFG", "AGO", "ALB", "ARG", "ARM", "AZE", "BDI", "BEN", "BFA",
                 "BGD", "BGR", "BIH", "BLR", "BLZ", "BOL", "BRA", "BTN", "BWA", 
                 "CAF", "CHN", "CIV", "CMR", "COD", "COG", "COL", "COM", "CPV", 
                 "CRI", "CUB", "DJI", "DMA", "DOM", "DZA", "ECU", "EGY", "ETH", 
                 "FJI", "GAB", "GEO", "GHA", "GIN", "GMB", "GNB", "GNQ", "GRD",
                 "GTM", "GUY", "HND", "HTI", "IDN", "IND", "IRN", "IRQ", "JAM",
                 "JOR", "KAZ", "KEN", "KGZ", "KHM", "KIR", "LAO", "LBN", "LBR",
                 "LBY", "LCA", "LKA", "LSO", "MAR", "MDA", "MDG", "MDV", "MEX",
                 "MKD", "MLI", "MMR", "MNE", "MNG", "MOZ", "MRT", "MUS", "MWI",
                 "MYS", "NAM", "NER", "NGA", "NIC", "NPL", "PAK", "PAN", "PER",
                 "PHL", "PNG", "PRY", "PSE", "ROU", "RUS", "RWA", "SDN", "SEN",
                 "SLB", "SLE", "SLV", "SOM", "SRB", "SSD", "STP", "SUR", "SWZ",
                 "SYR", "TCD", "TGO", "THA", "TJK", "TKM", "TLS", "TON", "TUN",
                 "TUR", "TUV", "TZA", "UGA", "UKR", "UZB", "VCT", "VNM", "VUT",
                 "WSM", "XKX", "YEM", "ZAF", "ZMB", "ZWE") #XKX = world bank kosovo; OWID_KOS = OWID kosovo
  
  # full join to include country populations that do not have vacc data
  LMICs <- full_join(df_vaccines, df_pop, by = c("iso_code")) %>%
    filter(iso_code %in% LMY_codes) %>%
    dplyr::select(iso_code,cum_people_vacc, population, date) 
  
  # make sure all 132 LMICs are included at least once in dataset
  LMICs_check <- LMICs %>%
    filter(iso_code %in% LMY_codes) %>% pull(iso_code) %>% unique()
  stopifnot(length(LMICs_check)==length(LMY_codes))
  
  LMICs <- LMICs %>%
    # business decision to only display after this date
    filter(date>as.Date("2021-06-09")) %>%
    group_by(date) %>%
    mutate(n_countries = n()) %>%
    # if have data for < 100 LMIC countries for a date, suppress LMIC_vacc_pct
    # thinking: let’s leave those last few dates blank to show that we don’t yet have complete data
    filter(n_countries >= 100) %>%
    ungroup() %>%
    dplyr::select(-n_countries) %>%
    pivot_wider(-cum_people_vacc,  values_from = c("cum_people_vacc", "population"), names_from = iso_code, names_sep  = "__") %>%
    tidyr::fill(-date, .direction = "down") %>%
    pivot_longer(-date,
                 names_to = c(".value", "iso_code"),
                 names_pattern = "(.+)__(.+)"
    ) %>%
    filter(!is.na(cum_people_vacc) | !is.na(population)) %>%
    group_by(date) %>%
    #no missings for pop, China has some missings for cum_people_vacc prior to june 10th 2021
    summarize(LMIC_vacc_pct=sum(cum_people_vacc, na.rm=TRUE)/sum(population)) 
    
  
  return(LMICs)
}

#################################################################################
#################################################################################
# FUNCTIONS TO GET FINAL FORMATTED DATA
#################################################################################
#################################################################################

get_national_data <- function(df_us){
  #' Arguments:
  #' - df_us (dataframe): state-level data to calculate CFR. The output of function get_us_data
  # Daily data --------------------------------------
  jhu <- get_jhu_nat() 
  
  # replace U.S. deaths and recoveries with the sum of the state data (50 states + DC + PR)
  summed <- df_us %>% 
    # exclude county level data from state rollup
    filter(subregion_2 != "Virgin Islands" & subregion_3 == "TOTAL") %>% 
    dplyr::select(date, cum_deaths, cum_recovered, new_recovered, new_deaths, new_positive) %>% 
    group_by(date) %>% 
    summarise_all(~sum_any_nonull(.)) %>% 
    mutate(country = "United States")
  
  # Use summed for US, otherwise use jhu
  df_cases <- replace_jhu(summed, jhu, by = c("country","date"))
  
  df_tests <- get_owid_nat() 
  df_vaccines <- get_vacc_nat()
  df_policy_nat <- get_policy_nat()
  df_behavior <- get_behavior_data()
  df_gvi <- get_gvi()
  df_capacity <- get_uscap_nat()
  df_race_nat <- get_us_nat_race_vacc()
  df_us_vacc <- get_vacc_us_state()
  df_us_nat_vacc_by_race <- get_est_us_nat_vacc_by_race(df_us_vacc)
  
  # select variables to be interpolated after merge
  vacc_by_race_vars <- df_us_nat_vacc_by_race %>% dplyr::select(-country, -date) %>% names()
  
  # Annual data -------------------------------------
  # ccvi at the country level for africa
  df_ccvi_afr_countries <- get_ccvi_afr(geo_level = "country")
  
  # uhc service coverage index at the country level for africa
  df_uhc_nat <- get_uhc_nat()
  
  # dtp3 vaccination rates at the country-year level
  df_dtp3_nat <- get_dtp3_nat()
  
  # population 
  df_pop <- get_pop_nat()
  
  daily_dfs <- c(list(df_cases), list(df_tests), list(df_policy_nat), list(df_behavior), list(df_gvi), list(df_capacity), list(df_vaccines),
                 list(df_us_nat_vacc_by_race)
  )
  annual_dfs <- c(list(df_dtp3_nat), list(df_ccvi_afr_countries), list(df_pop), list(df_uhc_nat))
  daily_dfs <- map(daily_dfs, clean_geo_names)
  annual_dfs <- map(annual_dfs, clean_geo_names)
  
  df_national <- indicators_merge(daily_dfs = daily_dfs, 
                                  annual_dfs = annual_dfs,
                                  subregion_1_name = "country",
                                  subregion_2_add = "TOTAL",
                                  subregion_3_add = "TOTAL") %>%
    rename(sci = uhc_sci_2017) %>% 
    # flag the most recent date that a behavioral survey had been conducted in a country
    mutate(recent_behav_date = if_else(!is.na(wear_mask), date, as.Date(NA_real_))) %>% 
    fill_periodic_surveys(metrics=c(beh_metrics, vac_metrics), date_ind_shortname="behav", geo="subregion_1", first_survey_date=first_behav_wave_date) %>% 
    rename_at(vars(sah, school_closing, restr_gath, req_mask), function(x) paste0(x,"_subregion_1")) %>%
    # interpolate vacc by race, which are only available intermittently
    arrange(subregion_1, subregion_2, subregion_3, date) %>%
    group_by(subregion_1, subregion_2, subregion_3) %>%
    mutate_at( vars(!!vacc_by_race_vars),
               ~ na.approx(., na.rm=FALSE)) %>%
    ungroup()

  return(df_national)
}

get_brazil_data <- function(){
  # Brazil cases, tests deaths
  df_brz_case_test <- get_brazil_case_test()
  # brazilian subnational policies
  df_policy_brz <- get_policy_brz()
  df_pop_brz <- get_pop_subnat("Brazil") %>% 
    dplyr::select(-country) %>% 
    rename(state = region) %>% 
    # TODO: this could go into a clean_region function with accent_drop
    mutate(state = gsub(" Do "," do ",state),
           state = gsub(" De "," de ", state))
  
  daily_dfs <- c(list(df_brz_case_test), list(df_policy_brz))
  annual_dfs <- c(list(df_pop_brz))
  df_brazil <- indicators_merge(daily_dfs = daily_dfs,
                                annual_dfs = annual_dfs,
                                subregion_2_name = "state",
                                subregion_1_add = "Brazil",
                                subregion_3_add = "TOTAL",
                                country_level = "no") %>% 
    mutate(region = "Latin America")
  return(df_brazil)
}

get_india_data <- function(){
  # get Indian case data
  df_india_data <- get_india_pre_data()
  
  # cvi data for india at state and district level
  df_india_state_cvi <- read_csv("data/india_state_cvi.csv")
  df_india_state_cvi$district <- "TOTAL"
  df_india_district_cvi <- read_csv("data/india_district_cvi.csv")
  
  # merge all indian (state-district level data) 
  daily_dfs <- c(list(df_india_data))
  annual_dfs <- c(list(bind_rows(df_india_state_cvi, df_india_district_cvi)))
  df_india <- indicators_merge(daily_dfs = daily_dfs,
                               annual_dfs = annual_dfs,
                               subregion_1_add = "India",
                               subregion_2_name = "state",
                               subregion_3_name = "district",
                               country_level = "no") %>%
    mutate(region = "India") %>%
    dplyr::select(region, everything())
  return(df_india)
}

get_sa_data <- function(){
  # case and death data for south africa
  df_sa_case_death <- get_sa_case_death()
  # dtp3 vaccination data for south africa
  df_sa_dtp3 <- get_sa_dtp3()
  df_ccvi_afr_subnat <- get_ccvi_afr(geo_level = "region")
  df_sa_pop <- get_pop_subnat("South Africa") %>% 
    dplyr::select(-country) %>% 
    rename(subnat = region)
  
  daily_dfs <- c(list(df_sa_case_death))
  annual_dfs <- c(list(df_sa_dtp3),list(df_ccvi_afr_subnat), list(df_sa_pop))
  df_sa <- indicators_merge(daily_dfs = daily_dfs,
                            annual_dfs = annual_dfs,
                            subregion_2_name = "subnat",
                            subregion_1_add = "South Africa",
                            subregion_3_add = "TOTAL",
                            country_level = "no") %>% 
    dplyr::select(-c(country)) %>% 
    mutate(region = "Africa")
  return(df_sa)
}

get_nig_data <- function(){
  # indicators for nigeria
  df_nig_ind <- get_nigeria_ind()
  df_nig_dtp3 <- get_nig_dtp3()
  df_ccvi_afr_subnat <- get_ccvi_afr(geo_level = "region") %>%
    filter(country == "Nigeria")
  df_nig_pop <- get_pop_subnat("Nigeria") %>% 
    dplyr::select(-country) %>% 
    rename(subnat = region) %>% 
    mutate(subnat = case_when(subnat == "Nassarawa" ~ "Nasarawa",
                              subnat == "Abuja" ~ "Federal Capital Territory",
                              TRUE ~ subnat))
  
  # nigeria
  daily_dfs <- c(list(df_nig_ind))
  annual_dfs <- c(list(df_nig_dtp3),list(df_ccvi_afr_subnat), list(df_nig_pop))
  df_nig <- indicators_merge(daily_dfs = daily_dfs,
                             annual_dfs = annual_dfs,
                             subregion_2_name = "subnat",
                             subregion_1_add = "Nigeria",
                             subregion_3_add = "TOTAL",
                             country_level = "no") %>% 
    dplyr::select(-c(country)) %>% 
    mutate(region = "Africa")
  return(df_nig)
}


fill_periodic_surveys <- function(df, metrics, date_ind_shortname, geo, first_survey_date){
  #' @description Fill metrics from surveys that are updated periodically across all dates for a geography
  #' 
  #' @param df dataframe of survey metrics. Must contain all columns listed in metrics parameter and date field.
  #' TODO: add survey dates as a parameter in the function? instead of checking 1 metric for nulls?
  #' @param metrics vector. List of metrics the survey generates to be filled. The first will be used
  #' to check for nulls and determine if this was a survey date or not.
  #' @param date_ind_shortname character. A column will be created with this flagging the most recent survey date
  #' @param geo character. Geography level the survey is at
  #' @param first_survey_date character. First date of the survey in format "YYYY-MM-DD"
  #' 
  #' @return Dataframe with survey metrics filled between start month of survey until most recent date
  
  recent_date_ind <- paste0("recent_", date_ind_shortname, "_date")
  first_date_01 <-  floor_date(ymd(first_survey_date), "month")
  fill_vars <- c(metrics, recent_date_ind)
  null_id <- metrics[1]
  
  filled_df <- df %>% 
    # flag the most recent date that a survey had been conducted in a geography
    mutate(!!recent_date_ind := if_else(!is.na(!!ensym(null_id)), date, as.Date(NA_real_))) %>% 
    group_by(!!ensym(geo)) %>% 
    # fill in survey results so most recent data are available for each geo-date and then backfill
    # survey results to dates prior to the first survey
    tidyr::fill(c(!!fill_vars), .direction = "downup") %>%
    # reset survey results to missing if the date is before the first of the month that the surveys began
    mutate_at(vars(!!metrics),
              ~case_when(
                date < ymd(first_date_01) ~ NA_real_,
                TRUE ~ .),
              na.rm = TRUE
    ) %>%
    mutate(!!recent_date_ind := case_when(
      date < ymd(first_date_01) ~ as.Date(NA_real_),
      TRUE ~ !!ensym(recent_date_ind))) %>%
    ungroup()
  
  return(filled_df)  
}


get_us_data <- function(df_us_vacc_accept){
  
  state_abbrevs <- get_us_state_abbrev()
  
  # indicators for united states
  df_us_case_test_death <- get_us_case_test_death()
  df_us_policy <- get_us_policy()
  df_us_ccvi <- get_us_ccvi()
  df_us_pop <- get_us_subnat_pop(pop_subnat_year) 
  df_us_cap <- get_uscap_data()
  df_us_vacc <- get_vacc_us_state()
  df_us_vacc_by_race <- get_us_state_vacc_by_race(df_us_vacc)
  
  vacc_by_race_vars <- df_us_vacc_by_race %>% dplyr::select(-state, -date) %>% names()
  
  df_us_vacc_accept_clean <- df_us_vacc_accept %>% 
    filter(lvl == "state") %>% 
    merge(state_abbrevs, by.x = "sheet_name", by.y = "state_abbrev") %>% 
    rename(date=end_date) %>% 
    dplyr::select(date, state, vaccine_accept)
  
  # united states
  
  # load list of valid states to filter out non-state rows
  # in particular "United States" row and "Footnotes" row
  state_names <- data.frame("state" = datasets::state.name, stringsAsFactors = FALSE) %>%
    bind_rows(data.frame("state" = c("District of Columbia", "Puerto Rico"))) %>%
    rename(subregion_2 = state) %>%
    dplyr::select(subregion_2)

  daily_dfs <- c(list(df_us_case_test_death), list(df_us_policy), list(df_us_cap), list(df_us_vacc), list(df_us_vacc_accept_clean), list(df_us_vacc_by_race))
  annual_dfs <- c(list(df_us_ccvi), list(df_us_pop))
  df_us <- indicators_merge(daily_dfs = daily_dfs,
                            annual_dfs = annual_dfs,
                            subregion_2_name = "state",
                            subregion_1_add = "United States",
                            subregion_3_add = "TOTAL",
                            country_level = "no") %>% 
    mutate(region = "United States") %>%
    # remove extraneous variables
    dplyr::select(-c(country)) %>%
    arrange(region, subregion_1, subregion_2, subregion_3, date) %>%
    fill_periodic_surveys(metrics = c("vaccine_accept"), date_ind_shortname = "behav", geo = "subregion_2", first_survey_date = first_vacc_accept_date) %>%
    # interpolate vacc by race, which are only available intermittently
    group_by(subregion_1, subregion_2, subregion_3) %>%
    mutate_at( vars(!!vacc_by_race_vars),
      ~ na.approx(., na.rm=FALSE)) %>%
    ungroup() %>%
    # exclude any geographies outside of the 50 states plus DC and PR (those included in the maps)
    right_join(state_names, by = "subregion_2")
  
  return(df_us)
}

get_us_demo_site_data <- function(min_date, max_date) {
  df_us_county_vacc <- get_us_county_vacc() %>% 
    filter(!(county %in% c("Montgomery", "Fort Bend"))) # exclude less relevant counties for Houston
  
  df_pop_demosite <- get_pop_demosite()
  
  df_us_vacc_accept <- get_vacc_accept()

  city_state_crosswalk <- data.frame(msa_city = c("Baltimore", "Chicago", "Houston", "New York", "San Francisco"),
                                     city = c("Baltimore", "Chicago", "Houston", "Newark", "Oakland"),
                                     state = c("Maryland", "Illinois", "Texas", "New Jersey", "California"))
  df_msa_accept <- df_us_vacc_accept %>% 
    filter(lvl == "metro") %>%
    mutate(msa_city = gsub("\\.", " ", gsub("_Metro_Area", "", sheet_name)))
  
  df_msa_accept %>% 
    write_csv('data/msa_vaccine_accept.csv')
  
  df_msa_accept <- df_msa_accept %>% 
    inner_join(city_state_crosswalk, by = "msa_city") %>%
    rename(date = end_date) %>%
    dplyr::select(city, state, date, vaccine_accept)
  
  # united states county level data
  daily_dfs <- c(list(df_us_county_vacc), list(df_msa_accept))
  annual_dfs <- c(list(df_pop_demosite))
  
  df <- indicators_merge(daily_dfs = daily_dfs,
                                   annual_dfs = annual_dfs,
                                   subregion_1_add = "United States",
                                   subregion_2_name = "state",
                                   subregion_3_name = "city",
                                   country_level = "no") %>%
    mutate(region = "United States") %>%
    dplyr::select(region, everything()) 
  
  # ensure that all dates that exist in df_dashboard are populated
  # so the server.R will never try to pull dates that don't exist
  df %>%
    expand_df_dates(min_date, max_date, c("county"),
                    constant_vars = c("region", "subregion_1", "subregion_2", "subregion_3", "population", "county_population")) %>%
    fill_periodic_surveys(metrics = c("vaccine_accept"), date_ind_shortname = "behav", geo = "subregion_3", first_survey_date = first_vacc_accept_date)
  
}


# db_lvl and geography are from user input dropdown
# df is filtered to geograpy and time period
get_policy <- function(df, db_lvl, geography){
  # assumes we don't have policy information at the subregion_3 level
  
  if(db_lvl == "region"){
    
    if(geography == "India"){
      policy_lvl <- "_subregion_1"
    } else {
      stop("No policy for region-level except for India")
    }
    
  } else if(db_lvl == "country"){
    
    policy_lvl <- "_subregion_1"
    
  } else if (db_lvl == "sub-national"){
    policy_lvl <- "_subregion_2"
  }
  
  
  if(policy_lvl == "_subregion_1"){
    policy_df <- df %>% 
      dplyr::select(date, ends_with(policy_lvl)) %>% 
      rename_all(~str_replace_all(., policy_lvl, ''))
  } else if(policy_lvl == "_subregion_2"){
    policy_df <- df %>% 
      dplyr::select(date, matches("_subregion_")) %>% 
      gather(variable, value, -date) %>%  
      # split by the last underscore
      tidyr::extract(variable, into = c("var", "lvl"), "(.*)_(subregion.+)") %>% # split at the word subregion and remove the underscore
      spread(lvl, value) %>% 
      mutate(value = case_when(subregion_1 == 1 ~ subregion_1,
                               subregion_1 %in% c(NA) ~ subregion_2,
                               (subregion_1 == 0) & (!is.na(subregion_2)) ~ subregion_2,
                               TRUE ~ subregion_1)) %>% 
      dplyr::select(date, var, value) %>% 
      spread(var, value)
  }
  
  return(policy_df)
  
}

clean_geo_names <- function(df){
  df %>%
    mutate_at(vars(one_of(c("subregion_1", "country"))),  ~case_when(
      . %in% c("Congo (Brazzaville)", 
               "Congo", 
               "Congo, Rep.") ~ "Republic of Congo",
      . %in% c("Congo (Kinshasa)",  
               "Congo, Dem. Rep.", 
               "Democratic Republic of Congo") ~ "Democratic Republic of the Congo", 
      . %in% c("United Republic of Tanzania") ~ "Tanzania",
      . %in% c("eSwatini") ~ "Eswatini",
      . %in% c("Swaziland") ~ "Eswatini",
      TRUE ~ .)
    )
}

write_data <- function(rolling_days = 7, cfr_rolling_days = 30, resolved_threshold = 250){
  
  one_million <- 1000000
  
  # Subnational data ------------------------------------------------------------
  df_brazil <- get_brazil_data()
  # covid19india.org data source deprecated as of 8-23-2021. Investigate alternative API endpoints from covid19india.org
  # df_india <- get_india_data()
  df_sa <- get_sa_data()
  df_nig <- get_nig_data()
  df_us_vacc_accept <- get_vacc_accept()
  df_us <- get_us_data(df_us_vacc_accept)
  
  #National data --------------------------------------------------------------
  df_national <- get_national_data(df_us)
  
  # Full analytic file ------------------------------------------------------------
  df_subnat <- bind_rows( #df_india,
                         df_brazil,
                         df_sa,
                         df_nig,
                         df_us) %>%
    # this assumes that all observations do not have subregion_3 policy data
    rename_at(vars(sah, school_closing, restr_gath, req_mask), function(x) paste0(x,"_subregion_2")) %>%
    left_join(df_national %>%
                dplyr::select(subregion_1, date, ends_with("_subregion_1")),
              by = c("subregion_1", "date"))
  
  region_variables <- df_national %>% 
    dplyr::select(region, date, starts_with("cum"), starts_with("new"),
                  dtp3, sci, population, stringency_index, wear_mask, wash_hands,
                  self_isolate, phys_dist, vaccine_accept,
                  covid_beds_num, covid_beds_denom, covid_beds_percent,
                  ip_beds_num, ip_beds_denom, ip_beds_percent,
                  icu_beds_num, icu_beds_denom, icu_beds_percent,
                  bipoc_cum_people_vacc_est, bipoc_pct_vacc_est, white_pct_vacc_est, asian_pct_vacc_est,       
                  black_pct_vacc_est, hispanic_pct_vacc_est, ai_an_pct_vacc_est, nh_pi_pct_vacc_est,
                  bipoc_cum_people_vacc_rpt, bipoc_pct_vacc_rpt, known_race_pct_of_vacc_othmiss,
                  bipoc_cum_people_vacc_rpt_kff, bipoc_pct_vacc_rpt_kff,known_race_pct_of_vacc_kff,
                  bipoc_pct_of_pop, bipoc_pct_of_vacc, bipoc_vacc_disparity,
                  people_fully_vaccinated, total_boosters,
                  vaccinated_appointment_or_accept, appointment_or_accept_covid_vaccine) %>% 
    colnames()
  not_rolling_vars <- c("dtp3","sci")
  date_vars <- c("recent_behav_date")
  
  # modify df national so that african and latin america 
  # countries' behavioral measures are all replaced 
  # by the value from the date closes to the snapshot wave
  df_national_mod <- df_national %>% 
    mutate_at(vars("wear_mask", "wash_hands", "self_isolate", "phys_dist", "vaccine_accept"),
              ~case_when(
                (
                  region != "United States" &
                  region != "India" &
                  # 08-03 is the date of the snapshot survey
                  # 08-02 is the date of the wave survey that is closest to the snapshot
                  # dates
                  !(recent_behav_date %in% c(as.Date("2020-08-03"), as.Date("2020-08-02")))
                ) ~ NA_real_,
                # leave time series in place for India and the US, which are wave countries
                # and the only countries in their respective region
                TRUE ~ .),
              na.rm = TRUE
    ) %>%
    mutate(recent_behav_date = case_when(
        region != "United States" &
        region != "India" &
        !(recent_behav_date %in% c(as.Date("2020-08-03"), as.Date("2020-08-02"))) ~ 
          as.Date(NA_real_),
      TRUE ~ recent_behav_date)) %>%     
    dplyr::arrange(region, subregion_1, subregion_2, subregion_3, date) %>%
    group_by(region, subregion_1, subregion_2, subregion_3) %>%
    # fill in survey results so snapshot period survey is populated in all dates after the initial survey
    tidyr::fill(c(wear_mask, wash_hands, self_isolate, phys_dist, vaccine_accept, recent_behav_date), 
                .direction = "down") %>%
    # after survey results are filled "down" backfill survey results to dates prior to the first survey
    tidyr::fill(c(wear_mask, wash_hands, self_isolate, phys_dist, vaccine_accept, recent_behav_date), .direction = "up") %>%
    # fill vaccine results down so region-level rollups account for latest available amounts
    tidyr::fill(c(cum_people_vacc, cum_total_vacc), .direction = "down") %>%
    # reset survey results to missing if the date is before the first of the month that the surveys began (jul 1)
    mutate_at(vars(wear_mask, wash_hands, self_isolate, phys_dist, vaccine_accept),
              ~case_when(
                date < ymd("2020-07-01") ~ NA_real_,
                TRUE ~ .),
              na.rm = TRUE
    ) %>%
    mutate_at(vars(recent_behav_date),
              ~case_when(
      date < ymd("2020-07-01") ~ as.Date(NA_real_),
      TRUE ~ .),
      na.rm = TRUE) %>%
    ungroup()

  # calculate if regional data should be filled forward or not
  df_region_missing <- df_national_mod %>%
    dplyr::select(!!region_variables) %>%
    group_by(region, date) %>%
    summarise_all(list(nmissing = ~sum(!is.na(.)))) %>%
    pivot_longer(
      cols = -c(region, date),
      names_to = c("var")
    ) %>%
    mutate(
      var = gsub("_nmissing", "", var), 
      # as long as region has 5 countries with data on date, create regional roll up
      # always keep US and India
      include = if_else(value < 5 & (!region %in% c("United States", "India")) & (!var %in% not_rolling_vars), FALSE, TRUE)
    ) %>%
    dplyr::select(-value) 
  
  # calculate values to fill forward by subregion
  df_region_rollmean <- df_national_mod %>%
    dplyr::select(subregion_1, !!region_variables, -!!not_rolling_vars) %>%
    arrange(subregion_1, date) %>% # sort before applying rollmean
    group_by(subregion_1) %>%
    mutate_at(vars(-c(subregion_1, date, region)), 
              ~rollapply(., width=rolling_days, FUN=function(x) mean(x, na.rm=TRUE), 
                         align='right', fill = NA_real_)) %>%
    mutate_at(vars(-c(subregion_1, date, region)), 
              ~if_else(is.nan(.), NA_real_, .)) %>%
    group_by(subregion_1) %>%
    arrange(subregion_1, date) %>% # sort before filling
    tidyr::fill(-c(region, subregion_1, date), .direction = "down") %>%
    pivot_longer(
      cols = -c(region, subregion_1, date),
      names_to = "var",
      values_to = "rollmeanval"
    ) %>%
    # replace rollmean value with missing for US and India
    # since these regions are single countries, we don't want
    # to extrapolate values ~ this would cause the region and country views to appear different
    mutate(rollmeanval = 
             case_when(
               region %in% c("United States", "India") ~ NA_real_,
               TRUE ~ rollmeanval
             ))
    
  
  # construct recent behavioral date by region to add to df_region
  # (processing in df region requires that all variables be numeric) 
  df_region_behav_date <- dplyr::select(df_national_mod,
    region, subregion_1, subregion_2, subregion_3, date, recent_behav_date) %>%
    group_by(region, date) %>%
    dplyr::summarise(recent_behav_date = max(recent_behav_date, na.rm = TRUE)
                     ) %>%
    ungroup %>%
    left_join(group_by(df_national_mod,
                       region, date) %>%
                  dplyr::summarise(count = sum(!is.na(recent_behav_date))) %>%
                ungroup(),
                by = c("region", "date")
                ) %>%
    mutate(recent_behav_date = case_when(
            count >= 5 | region %in% c("India", "United States") ~ recent_behav_date,
            TRUE ~ as.Date(NA_real_)
      ) 
    ) %>%
    dplyr::select(-c(count))
 
  # summarize to region-level
  df_region <- df_national_mod %>%
    group_by(region, date) %>%
    dplyr::select(subregion_1, !!region_variables) %>%
    pivot_longer(
      cols = -c(region, subregion_1, date),
      names_to = c("var")
    ) %>%
    left_join(., df_region_missing, by = c("region", "var", "date")) %>%
    left_join(., df_region_rollmean, by = c("region", "subregion_1", "var", "date")) %>%
    mutate(value = if_else(is.na(value) & (!var %in% not_rolling_vars), rollmeanval, value),
           value = if_else(include, value, NA_real_)) %>%
    # pivot wider to weight by population after imputing values
    pivot_wider(
      id_cols = c(region, subregion_1, date),
      names_from = var,
      values_from = value
    ) %>%
    group_by(region, date) %>%
    # calculate region level population to weight annual variables
    # on rollup
    mutate(tot_pop_dtp3 = sum(population*as.numeric(!is.na(dtp3)), na.rm = TRUE),
           tot_pop_sci = sum(population*as.numeric(!is.na(sci)), na.rm = TRUE),
           tot_pop_stringency_index = sum(population*as.numeric(!is.na(stringency_index)), na.rm = TRUE),
           # use wear mask to determine relevant countries for all behavior variables
           tot_pop_behavior = sum(population*as.numeric(!is.na(wear_mask)), na.rm = TRUE),
           # if country's vaccinated_appointment_or_accept / appointment_or_accept_covid_vaccine
           # is missing, do not include country's population in regional denominator
           # for regional roll up. reasoning: vaccine acceptance similar to case counts,
           # so if vaccine acceptance is missing (we can't measure it), it's not 
           # because people don't want vaccines 
           pop_vacc_appt_or_accept = population*as.numeric(!is.na(vaccinated_appointment_or_accept)),
           pop_appt_or_accept_covid_vacc = population*as.numeric(!is.na(appointment_or_accept_covid_vaccine))
    ) %>%
    ungroup() %>%
    mutate( pop_prop_dtp3 = population/tot_pop_dtp3,
            pop_prop_sci = population/tot_pop_sci,
            pop_prop_stringency_index = population/tot_pop_stringency_index,
            pop_prop_behavior= population/tot_pop_behavior,
            dtp3 = dtp3*pop_prop_dtp3,
            sci = sci*pop_prop_sci,
            stringency_index = stringency_index*pop_prop_stringency_index,
            wear_mask = wear_mask*pop_prop_behavior,
            wash_hands = wash_hands*pop_prop_behavior,
            self_isolate = self_isolate*pop_prop_behavior,
            phys_dist = phys_dist*pop_prop_behavior,
            vaccine_accept = vaccine_accept*pop_prop_behavior,
            # vaccinated_appointment_or_accept is a country proportion, make into country count
            vaccinated_appointment_or_accept = vaccinated_appointment_or_accept * population,
            # appointment_or_accept_covid_vaccine is a proportion out of unvaccinated umd survey respondents, make into count
            appointment_or_accept_covid_vaccine = case_when(
              # if cum_people_vacc is missing, treat it like 0 (assume no one is vaccinated)
              is.na(cum_people_vacc) ~ appointment_or_accept_covid_vaccine * population,
              T ~ appointment_or_accept_covid_vaccine * (population - cum_people_vacc)
            )
            ) %>%
    dplyr::select(-starts_with("tot_pop"), -starts_with("pop_prop")) %>%
    pivot_longer(
      cols = -c(region, subregion_1, date),
      names_to = c("var")
    ) %>%
    # remove na rows to eliminate date-variable combinations where no data are
    # available for a region. summarise_all fills zeros if there are no non-na
    # observations. We include this step to avoid the issue
    filter(!is.na(value)) %>%
    dplyr::select(-c(subregion_1)) %>%
    group_by(region, date, var) %>%
    # summarize all before pivot wider to avoid filling zeros for region-dates with
    # no country info
    dplyr::summarise_all(sum, na.rm = TRUE) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = c(region, date),
      names_from = var,
      values_from = value
    ) %>%
    # after summarise_all, vaccinated_appointment_or_accept & tot_pop_vacc_appt_or_accept
    # are summed counts over region, turn vaccinated_appointment_or_accept into proportion
    mutate(vaccinated_appointment_or_accept = vaccinated_appointment_or_accept / pop_vacc_appt_or_accept,
           # appointment_or_accept_covid_vaccine, tot_pop_appt_or_accept_covid_vacc,
           # and cum_people_vacc (any nas are treated as 0) are summed counts over 
           # region turn appointment_or_accept_covid_vaccine into proportion
           appointment_or_accept_covid_vaccine = appointment_or_accept_covid_vaccine / 
             (pop_appt_or_accept_covid_vacc - cum_people_vacc),
           subregion_1 = "TOTAL",
           subregion_2 = "TOTAL",
           subregion_3 = "TOTAL") %>%
    dplyr::select(-c(pop_vacc_appt_or_accept, pop_appt_or_accept_covid_vacc)) %>%
    left_join(get_region_population(), by = "region") %>%
    left_join(df_region_behav_date, by = c("region", "date")) %>%
    # QA check: stringency index between 0 and 100
    # assert returns the data frame if no errors are raised
    assertr::assert(assertr::within_bounds(0, 100), stringency_index) %>%
    left_join(get_LMIC_vacc_pct(), by = "date")
  
  df_dashboard <- bind_rows(df_region,
                            df_national,
                            df_subnat) %>%
    arrange(region, subregion_1, subregion_2, subregion_3, date) %>% 
    group_by(region, subregion_1, subregion_2, subregion_3) %>%
    # only populate moving averages and sums when the current data in not missing
    # this avoids creating moving sums that include fewer than a weeks worth of data 
    # at the end of the series (can unintentionally inflate TPR)
    
    # TODO: mutate(across()) takes a really long time for these for some reason, so leaving as they are.
    mutate(new_cases_mavg = case_when(
           !is.na(new_cases) ~ rollmean(new_cases,rolling_days,align='right', na.rm = TRUE, fill=NA)
            ),
           new_deaths_mavg = case_when(
             !is.na(new_deaths) ~ rollmean(new_deaths,rolling_days,align='right', na.rm = TRUE, fill=NA)
           ),
           new_tests_mavg = case_when(
             !is.na(new_tests) ~ rollmean(new_tests,rolling_days,align='right', na.rm = TRUE, fill=NA)
            ),
           
           # rolling sums
           new_cases_msum = case_when(
             !is.na(new_cases) ~ rollsum(new_cases,rolling_days,align='right', na.rm = TRUE, fill=NA)
           ),
           new_deaths_msum = case_when(
             !is.na(new_deaths) ~ rollsum(new_deaths,rolling_days,align='right', na.rm = TRUE, fill=NA)
           ),
           new_tests_msum = case_when(
             !is.na(new_tests) ~ rollsum(new_tests,rolling_days,align='right', na.rm = TRUE, fill=NA)
           ),
           new_rec_msum = case_when(
             !is.na(new_recovered) ~ rollsum(new_recovered,rolling_days,align='right', na.rm = TRUE, fill=NA)
           ),
           positive_msum = case_when(
             (!is.na(new_positive) ~ rollsum(new_positive,rolling_days, align='right', na.rm = TRUE, fill = NA))
           ),
           
           # cfr rolling sums
           new_deaths_msum_cfr = case_when(
             !is.na(new_deaths) ~ rollsum(new_deaths,cfr_rolling_days,align='right', na.rm = TRUE, fill=NA)
           ),
           new_resolved_msum = case_when(
             (!is.na(new_deaths) & !is.na(new_recovered) ) ~ rollsum(new_deaths + new_recovered, cfr_rolling_days,align = 'right',na.rm=TRUE,fill=NA)
           )
          ) %>% 
    ungroup() %>% 
    mutate(
      # core indicators
      # proportion of tests that are positive
      tpr = ifelse(region == "United States" & subregion_2 != "TOTAL", positive_msum/new_tests_msum, new_cases_msum/new_tests_msum),
      tests_per_mil = (new_tests_mavg * one_million)/population,
      cases_per_mil = (new_cases_mavg * one_million)/population,
      
      # supporting indicators
      cfr = case_when(is.na(new_resolved_msum) ~ NA_real_,
                      new_resolved_msum < resolved_threshold ~ NA_real_,
                      new_resolved_msum >= resolved_threshold ~ new_deaths_msum_cfr/new_resolved_msum),
      deaths_per_mil = (new_deaths_mavg * one_million)/population,
      
      # not dead or recovered cases
      active_cases = cum_cases - cum_deaths - cum_recovered,
      active_cases_per_mil = (active_cases * one_million)/population,
      cum_tpc = cum_tests/cum_cases
    ) %>% 
    dplyr::select(-c(new_resolved_msum, new_deaths_msum_cfr, new_positive, positive_msum)) %>% 
    # populate subregion_2 observations with policy variables from their corresponding subregion 1
    group_by(subregion_1, date) %>%
    mutate(sah_subregion_1 = max(sah_subregion_1, na.rm = TRUE),
           req_mask_subregion_1 = max(req_mask_subregion_1, na.rm = TRUE),
           school_closing_subregion_1 = max(school_closing_subregion_1, na.rm = TRUE),
           restr_gath_subregion_1 = max(restr_gath_subregion_1, na.rm = TRUE)
    ) %>%
    mutate(sah_subregion_1 = if_else(sah_subregion_1 == -Inf, NA_real_, sah_subregion_1),
           req_mask_subregion_1 = if_else(req_mask_subregion_1 == -Inf, NA_real_, req_mask_subregion_1),
           school_closing_subregion_1 = if_else(school_closing_subregion_1 == -Inf, NA_real_, school_closing_subregion_1),
           restr_gath_subregion_1 = if_else(restr_gath_subregion_1 == -Inf, NA_real_, restr_gath_subregion_1)
    ) %>%
    ungroup() %>%
    mutate_if(is.numeric, ~replace(., is.nan(.), NA_real_)) %>% # occurs when 0/0
    mutate_if(is.numeric, ~replace(., is.infinite(.), NA_real_)) %>% # occurs when div by 0
    replace_with_na_at(.vars = c("tpr"),
                       ~(.x < 0 | .x > 1)) %>%
    replace_with_na_at(.vars = c("cfr"),
                       ~(.x < 0 | .x > .1)) %>%
    # reset all US cfr data to missing. Investigation revealed that US recovery data
    # in CTP and JHU was inconsistent and led to unrealistically high CFR values. 
    # we leave the data in the upstream processing but remove it at this stage
    mutate(cfr = 
             case_when(
               region == "United States" ~ NA_real_,
               TRUE ~ cfr
             )
           ) %>%
    dplyr::select(region, subregion_1, subregion_2, subregion_3, date, everything()) 
  
  # remove the last date from the dashboard (the last date is often missing many indicators, so we have opted 
  # not to show it)
  last_date_df <- summarize(df_dashboard, max(date)) %>% pull()
  
  df_dashboard <- filter(df_dashboard, date != last_date_df)
  
  assertthat::assert_that(
    all(df_dashboard %>% 
          group_by(region, subregion_1, subregion_2, subregion_3, date) %>% 
          count() %>% 
          pull(n) == 1
    )
  )
  
  # create time series from minimum date in data through maximum date in data
  df_dates <- data.frame("date" = seq(from = min(df_dashboard$date), to = max(df_dashboard$date), by = "1 day"))
  
  # create geography level dataset with each of the annual indicators
  df_geos <- dplyr::select(df_dashboard, 
                      c(region, subregion_1, subregion_2, subregion_3)) %>%
              unique()
  
  # cartesian merge of country level file and time series
  all_geo_dates <- tidyr::crossing(df_geos, df_dates)
  
  # merge country-day level file with df_dashboard to add in missing rows
  df_dashboard <- full_join(all_geo_dates, df_dashboard, by =c("region", "subregion_1", "subregion_2", "subregion_3", "date")) %>%
    dplyr::arrange(subregion_1, subregion_2, subregion_3, date) %>%
    group_by(subregion_1, subregion_2, subregion_3) %>%
    # impute missing annual variables within the same year
    tidyr::fill(dtp3, population, sci, rel_cvi, display_population , 
                .direction = "downup") %>% # populate cum_tests with the most recent recorded value if missing
    ungroup() 
  
  # TO DO: when we have multiple years of annual data, restrict imputation to be conducted within a single
  # year possibly by replacing 
  #     group_by(subregion_1, subregion_2, subregion_3) %>%
  # with
  #     group_by(subregion_1, subregion_2, subregion_3, year) %>%

  
  if (nrow(df_dashboard) != nrow(all_geo_dates)) {
    stop("some geography-date combinations are not present in all_geo_dates")
  }

  # fill annual indicators up and down to populate date-rows that had previously
  # been missing
  
  df_dashboard %>% 
    write_csv(here::here("data","df_dashboard.csv"))
  
  # TODO: National: new_recovered, new_deaths, new_cases has negatives - from clean_jhu_data
  # # rows in df_national with any new_ variables < 0
  # df_national %>%
  #   filter_at(vars(starts_with("new")),any_vars(. < 0)) %>%
  #   group_by(region, subregion_1, subregion_2) %>%
  #   count()
  # # TODO: new_tests has negatives (only 4 observations) - get_owid_nat, why do we generate our own new_tests?
  # 
  # # TODO: Subnational:
  # df_subnat %>%
  #   filter_at(vars(starts_with("new")),any_vars(. < 0)) %>%
  #   group_by(region, subregion_1, subregion_2) %>%
  #   count()
  # df_subnat %>%
  #   filter_at(vars(starts_with("cum")),any_vars(. < 0)) %>%
  #   group_by(region, subregion_1, subregion_2) %>%
  #   count()
}

write_demo_site_data <- function(min_date, max_date) {
  
  metro_names <- data.frame(subregion_3 = c("Baltimore", "Chicago", "Houston", "Newark", "Oakland"),
                            metro_long = c("Baltimore",
                                           "Chicago/Naperville/Elgin",
                                           "Houston/The Woodlands/Sugar Land",
                                           "New York City/Newark/Jersey City",
                                           "San Francisco/Oakland/Berkeley"))
  
  df_demo_site <- get_us_demo_site_data(min_date, max_date) %>%
    # calculate composite indicators
    mutate(cum_pct_people_fully_vacc = cum_people_fully_vacc/county_population) %>%
    # linearly interpolate indicators that are reported less
    # frequently than daily
    group_by(subregion_1, subregion_2, subregion_3) %>%
    mutate_at( vars(cum_pct_people_fully_vacc),
               ~ na.approx(., na.rm=FALSE)) %>%
    ungroup() %>%
    left_join(metro_names, by = "subregion_3")
    
  
  df_demo_site %>% 
    write_csv(here::here("data","df_demo_site.csv"))
  
  return(df_demo_site)
}



#################################################################################
#################################################################################
