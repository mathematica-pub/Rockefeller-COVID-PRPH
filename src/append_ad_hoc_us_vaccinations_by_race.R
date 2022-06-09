race_vax <- read_csv(here::here("data", "race_ethnicity_of_people_with_at_least_one_dose_administered.csv"))

df_vaccines <- get_vacc_nat() %>%
  filter(country == "United States", date == lubridate::ymd("2021-03-18"))
  
ts <- read_csv("data/us_nat_vacc_by_race.csv")

df <- race_vax %>%
  rename("one_dose" = "Count") %>%
  mutate(race = case_when(
    `Race/Ethnicity` == "Hispanic/Latino" ~ "hispanic",
    `Race/Ethnicity` == "American Indian/Alaska NativeNon-Hispanic" ~ "natam",
    `Race/Ethnicity` == "AsianNon-Hispanic" ~ "asian",
    `Race/Ethnicity` == "Black Non-Hispanic" ~ "black",
    `Race/Ethnicity` == "Native Hawaiian/Other Pacific Islander Non-Hispanic" ~ "natpac",
    `Race/Ethnicity` == "WhiteNon-Hispanic" ~ "white",
    `Race/Ethnicity` == "Multiple/Other Non-Hispanic" ~ "other"
  ),
  date = lubridate::ymd('2021-03-18'),
  country = "United States") %>%
  dplyr::select(race, one_dose,  date, country) %>%
  gather(group, count, -c(race, date, country)) %>%
  unite(temp, group, race) %>%
  spread(temp, count) %>%
  left_join(df_vaccines, by = c("country", "date")) %>%
  mutate(one_dose_unknown = cum_people_vacc - 
           (one_dose_asian + one_dose_black + one_dose_hispanic + 
              one_dose_natam + one_dose_natpac + one_dose_other + 
              one_dose_white)) %>%
  dplyr::select(-starts_with("cum_"), -starts_with("new_"))

ts_new <- df %>%
  bind_rows(ts) %>%
  filter(!duplicated(date)) %>%
  arrange(date)

write.csv(ts_new, "data/us_nat_vacc_by_race.csv", row.names = FALSE)
