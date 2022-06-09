about_text_1 <- "This dashboard tracks Covid-19 case counts, testing and vaccination response, and other contextual indicators in regions where The Rockefeller Foundation and its partners operate. Indicators are pulled or calculated from publicly available sources. For the United States, vaccine equity is a major focus of the dashboard. U.S. vaccination rates by race/ethnicity are calculated or imputed based on underlying data from the Centers for Disease Control and Prevention, the Kaiser Family Foundation, and the U.S. Census Bureau. Detailed source information is linked below. "
about_text_3a <- "For further information, please contact "
about_text_3b <- "Margaret Luo "
about_text_3c <- "at Mathematica."
mailer <- "mailto:mluo@mathematica-mpr.com"

disclaimer <- "Neither Mathematica nor The Rockefeller Foundation or any contributors, collaborators, or contractors guarantee the accuracy, completeness or integrity of the information collected. The information presented has been prepared in good faith on the basis of available data at the date of publication without any independent verification. Further, the information is provided for informational purposes only and is not intended as an endorsement, guidance, recommendations, legal advice, medical advice, or advice for any particular product, program or policy. Any use or interpretation of or reliance on the information for any purpose, is solely and exclusively the responsibility of the recipients of the information. THE INFORMATION IS PROVIDED ''AS IS'' AND NO WARRANTY OF ANY KIND IS GIVEN FOR THE INFORMATION UNDER ANY NATIONAL OR INTERNATIONAL LAW, EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR WARRANTY AGAINST INFRINGEMENT OF INTELLECTUAL PROPERTY. The Rockefeller Foundation, Mathematica, and all contributors, collaborators, and contractors expressly disclaim and assume no responsibility for any losses, damages, claims, or other liabilities, including loss of good will, revenue, profits, or business interruption, or any consequential, special, indirect, incidental, punitive or exemplary loss, including costs of defense or attorneys' fees, arising out of or relating to use of this information. The information is current as of the date of its publication without regard to the date on which you may access this information and is subject to change at any time and for any reason. It is expressly understood that The Rockefeller Foundation, Mathematica, and all contributors, collaborators, and contractors by providing this information, have no obligation to update the information or provide additional support or information to the recipient."
config <- read.config(file = 'versions.config')
version_text <- paste0(" Version ", config$version, " published ", config$publish_date, ".")

tpr_text <- get_indicator_info(indicators_raw, "Test Positivity Rate (TPR)", core = TRUE)
tests_per_mil_text <- get_indicator_info(indicators_raw, "Tests Conducted per Million per Day", core = TRUE)
cases_per_mil_text <- get_indicator_info(indicators_raw, "New Cases per Million per Day", core = TRUE)
vpeople_text <- get_indicator_info(indicators_raw, "Proportion Vaccinated (Full Population, All Ages)", core = TRUE)

demo_vpeople_text <- get_indicator_info(indicators_raw, "Proportion Fully Vaccinated in the County (All Ages)", core = TRUE)
vacc_accept_msa_text <- get_indicator_info(indicators_raw, "Self-Reported Vaccine Acceptance (US State and Demonstration-Site Level)", core = FALSE)
bipoc_vacc_state_text <- get_indicator_info(indicators_raw, "Proportion BIPOC vaccinated (ages 5+, at least one dose), estimated", core = TRUE)
race_state_text <- get_indicator_info(indicators_raw, "Proportion vaccinated by race/ethnicity (ages 5+, at least one dose), estimated", core = TRUE)
tevx_text <- get_indicator_info(indicators_raw, "Total BIPOC Population vaccinated, at least one dose", core = TRUE)
prop_raceeth_text <- get_indicator_info(indicators_raw, "Proportion of Vaccinations with Race/Ethnicity Information Reported", core = TRUE)

burden_of_disease_text <- get_indicator_info(
  indicators_raw, 
  c("Case-Fatality Ratio (CFR)", 
    "COVID-19 Deaths per Million per Day"), 
  core = FALSE
  )

kpi_text_india <- get_indicator_info(
  indicators_raw, 
  c("COVID-19 Vulnerability Index (India)",
    "DTP3 vaccination rate", 
    "Essential Health Services Index (EHSI)"), 
  core = FALSE
)
kpi_text_usa <- get_indicator_info(
  indicators_raw, 
  c("COVID-19 Vulnerability Index (USA)",
    "DTP3 vaccination rate", 
    "Essential Health Services Index (EHSI)"
    ), 
  core = FALSE
)
kpi_text_africa <- get_indicator_info(
  indicators_raw, 
  c("COVID-19 Vulnerability Index (Africa)",
    "DTP3 vaccination rate", 
    "Essential Health Services Index (EHSI)"), 
  core = FALSE
)
kpi_text_other <- get_indicator_info(
  indicators_raw, 
  c("DTP3 vaccination rate", 
    "Essential Health Services Index (EHSI)"), 
  core = FALSE
)

policy_text <-get_indicator_info(
  indicators_raw, 
  c("Stay-at-Home Order", 
    "Restriction on Gatherings", 
    "Masks Required in Public", 
    "Schools Closed"), 
  core = FALSE
)

beh_text_country <- get_indicator_info(
  indicators_raw, 
  c("Self-Reported Preventive Behaviors: Hand Washing", 
    "Self-Reported Preventive Behaviors: Mask wearing", 
    "Self-Reported Preventive Behaviors: Physical distancing", 
    "Self-Reported Preventive Behaviors: Self-Isolating",
    "Self-Reported Vaccine Acceptance (Country level)"), 
  core = FALSE
)

beh_text_us_state <- get_indicator_info(
  indicators_raw, 
  c("Self-Reported Vaccine Acceptance (US State and Demonstration-Site Level)"), 
  core = FALSE
)

evx_text <- get_indicator_info(
  indicators_raw, 
  c("BIPOC share of vaccinations to date, at least one dose"), 
  core = TRUE
)

prop_accept_vacc_text <- get_indicator_info(
  indicators_raw,
  c("Proportion Who Would Accept a Covid-19 Vaccine"),
  core = T
)

vacc_elig_text <- get_indicator_info(
  indicators_raw,
  c("Vaccine Eligibility"),
  core = T
)

vacc_hest_text <- get_indicator_info(
  indicators_raw,
  c("Main Reasons Driving Vaccine Hesitancy"),
  core = T
)

struc_barr_text <- get_indicator_info(
  indicators_raw,
  c("Structural Barriers to Vaccination"),
  core = T
)

lmic_text <- get_indicator_info(
  indicators_raw,
  c("Proportion Vaccinated in All Low- and Middle-Income Countries Globally (All Ages, at Least One Dose)"),
  core = T
)
