shinyServer(function(input, output, session)  {

  # update dropdowns -----------------------------------------------------------
  geo_list <- reactive({
    req(input$level)
    if(tolower(input$level) == "region") {
      list <- dplyr::distinct(df_filt_by_date(), region) %>% rename(names = region) 
    } else if(tolower(input$level) == "country") {
      list <- df_filt_by_date() %>%
        dplyr::distinct(subregion_1)%>% 
        rename(names = subregion_1) 
    } else if(tolower(input$level) == "sub-national") {
      if(!is.null(input$country)){
        df <- filter(df_filt_by_date(), subregion_1 == input$country)
      } else {
        df <- df_filt_by_date()
      }
      list <- df %>% 
        dplyr::distinct(subregion_2) %>% 
        rename(names = subregion_2) 
    } else if(tolower(input$level) == "demonstration site"){
      list <- demo_sites()
    }
    list <- filter(list, names != "TOTAL") %>% arrange(names)
    dplyr::pull(list, names)
  })

  output$geography <- renderUI({
    pickerInput(
      "geography",
      p(if_else(input$level == "Sub-national" & input$country == "India", "State", 
                if_else(input$level == "Demonstration site", "County/City",
                "Geography")), 
        class = "input-label"),
      choices = sort(geo_list()), 
      selected = ifelse(!is.null(input$geography), input$geography, "United States"),
      options = list(`live-search` = TRUE, `dropup-auto` = FALSE)
    )
  })
  
  output$country <- renderUI({
    req(input$level)

    pickerInput(
      "country",
      label = p("Country", class = "input-label"),
      choices = sort(subnat_countries[subnat_countries != "TOTAL"]), 
      selected = ifelse(!is.null(input$country), input$country, subnat_countries[subnat_countries != "TOTAL"][[1]]),
      options = list(`live-search` = TRUE, `dropup-auto` = FALSE)
    )
  })
  
  district_list <- reactive({
    if(is.null(input$country)){
      "TOTAL"
    } else if ((input$level != "Sub-national" | input$country != "India")) {
      "TOTAL"
    } else {
      list <- filter(df_filt_by_date(), 
                     subregion_2 == input$geography,
                     subregion_1 == input$country) %>% 
        dplyr::distinct(subregion_3) %>% 
        rename(names = subregion_3) %>% 
        arrange(names) 
      dplyr::pull(list, names)
    }
  })
  
  output$district <- renderUI({
    pickerInput(
      "district",
      p("District", class = "input-label"),
      selected = "TOTAL",
      choices = sort(district_list()), 
      options = list(`live-search` = TRUE, `dropup-auto` = FALSE))
  })
  
  output$change_over <- renderUI({
    
    pickerInput(
      inputId = "change_over",
      p("Show change over", class = "input-label"),
      choices = TIME_LEVELS,
      selected ="Three months", 
      options = list(`live-search` = TRUE)
    )
  })
  
  
  
  output$downloadPDF <- downloadHandler(
    filename = "COVID-19 Testing Dashboard Data Sources.pdf",
    content = function(file){
      file.copy("www/2022-02-11 data sources description.pdf", file)
    }
  )
  
  # update data based on changing inputs ---------------------------------------  
  
  # KPI and behavior info only: modify info text based on region of selected page
  output$kpi_header <- renderUI({

    req(nrow(df_selected())>0)
    region <- df_selected() %>% dplyr::select(region) %>% unique()

    if (region == "India") {
      kpi_text <- kpi_text_india
    }else if (region == "United States") {
      kpi_text <- kpi_text_usa
    }else if (region == "Africa") {
      kpi_text <- kpi_text_africa
    }else {
      kpi_text <- kpi_text_other
    }
    sectionHeader("Healthcare capacity and equity", info = kpi_text, up = TRUE)

  })
  
  start_date <- reactive({
    req(input$date, input$change_over)
    get_start_date(input$date, input$change_over)
  })
  
  observe({
    req(!start_date() %in% DATE_RANGE)
    shinyalert("Note:", paste0(input$change_over, " of data prior to ", input$date, " not available."), type = "warning")
  })
  
  output$display_date <- renderUI({
    
    req(input$level != "Demonstration site")
    HTML(paste("<h4 style = 'font-size: 16px; z-index:1000; padding:4px;'><b>",
               "Most recent available data as of",
               paste0(gsub(" 0", " ", format(as.Date(input$date), "%b %d")),
                      ","),
               lubridate::year(input$date),
               "</b></h4>"
    ))

  })

  output$dpmtitle <- renderUI({
    req(!is.na(input$geography) & !is.na(input$level))
    geo_title_create(one_country_regions, input$level, input$geography, 
                     "Deaths per million per day (7-day avg.", 
                     " of available country data",
                     end_text = ")")
  })
  output$cfrtitle <- renderUI({
    req(!is.na(input$geography) & !is.na(input$level))
    geo_title_create(one_country_regions, input$level, input$geography, 
                     "Case fatality ratio (30-day avg.", 
                     " of available country data",
                     end_text = ")")
  })
    
  output$cvititle <- renderUI({
    req(!is.na(input$geography) & !is.na(input$level))
    geo_title_create(one_country_regions, input$level, input$geography, 
                     "2020 COVID Vulnerability Index", 
                     " (Average of available countries' annual data)")

  })
  output$ehsititle <- renderUI({
    req(!is.na(input$geography) & !is.na(input$level))
    geo_title_create(one_country_regions, input$level, input$geography, 
                     "2019 Essential Health Services Index", 
                     " (Average of available countries' annual data)")
  })
  output$dtp3title <- renderUI({
    req(!is.na(input$geography) & !is.na(input$level))
    geo_title_create(one_country_regions, input$level, input$geography, 
                     "2019 DTP3 vaccination (%", 
                     ", Average of available countries' annual data",
                     end_text = ")")
  })
  output$tprtitle <- renderText({
    req(!is.na(input$geography) & !is.na(input$level))
    geo_title_create(one_country_regions, input$level, input$geography, 
                     "Test positivity rate (7-day avg.", 
                     " of available country data",
                     end_text = ")")
  })
  output$casetitle <- renderText({
    req(!is.na(input$geography) & !is.na(input$level))
    geo_title_create(one_country_regions, input$level, input$geography, 
                     "New cases per million per day (7-day avg.", 
                     " of available country data",
                     end_text = ")")
  })
  output$testtitle <- renderText({
    req(!is.na(input$geography) & !is.na(input$level))
    geo_title_create(one_country_regions, input$level, input$geography, 
                     "Tests conducted per million per day (7-day avg.", 
                     " of available country data",
                     end_text = ")")
  })
  output$lmictitle <- renderText({
    req(!is.na(input$geography) & !is.na(input$level))
    geo_title_create(one_country_regions, input$level, input$geography, 
                     "Proportion vaccinated in all low- and middle-income countries globally (all ages, at least one dose", 
                     "",
                     end_text = ")")
  })
  output$vpeopletitle <- renderText({
    req(!is.na(input$geography) & !is.na(input$level))
    if (input$geography == "United States" | 
      (input$level == "Sub-national" & input$country == "United States")) {
      text <- "Proportion vaccinated (full population, all ages"
      }
    else {
      text <- "Proportion vaccinated (all ages"
    }
    geo_title_create(one_country_regions, input$level, input$geography, 
                     text, 
                     ", assumes 0 vaccinations for countries with no reported data",
                     end_text = ")")
  })
  output$bipoc_pct_vacc_esttitle <- renderText({
    req(!is.na(input$geography) & !is.na(input$level))
    geo_title_create(one_country_regions, input$level, input$geography, 
                     "Proportion BIPOC vaccinated (ages 5+, at least one dose), estimated")
  })
  output$demo_vpeopletitle <- renderText({
    req(!is.na(input$geography) & !is.na(input$level))
    req(df_demo_selected())
    if (unique(df_demo_selected()$county) == "Baltimore") geo_label <- "city"
    else geo_label <- "county"
    geo_title_create(one_country_regions, input$level, input$geography, 
                     paste0("Proportion fully vaccinated in ", unique(df_demo_selected()$county) , " ", geo_label, " (full population, all ages, at least one dose)"),
                     end_text = "")
  })
  
  output$bipoc_vacc_state_title <- renderText({
    req(input$level, input$geography, demo_state())
    geo_title_create(one_country_regions, input$level, input$geography, 
                     paste0("Proportion of BIPOC population vaccinated in ", demo_state() , " (ages 5+): county data not available"),
                     end_text = "")
  })
  
  output$race_state_title <- renderText({
    req(input$level, input$geography, demo_state())
    geo_title_create(one_country_regions, input$level, input$geography, 
                     paste0("Proportion vaccinated in ", demo_state() , " by race/ethnicity (ages 5+), estimated: county data not available"),
                     end_text = "")
    
  })
  
  output$vacc_accept_msa_title <- renderText({
    req(input$level, input$geography, demo_state(), df_demo_selected())
    shiny::validate(
      need(nrow(df_demo_selected()) > 0, 
          "")
    )
    metro_long <- unique(df_demo_selected()$metro_long)
    
    # show time series title ending with "over time" if there
    # are multiple data points or if there are no valid data points
    # to display
    if (line_bool_demo() | nrow(df_demo_valid_dates()) == 0) {
      # time series
      text <- get_line_bar_title(TRUE, NULL, vac_map_census, input$level, input$geography, metro_long = metro_long)
    } else{
      # bar chart
      df_last_day <- get_last_day_data(df_demo_valid_dates())
      text <- get_line_bar_title(FALSE, df_last_day, vac_map_census, input$level, input$geography, metro_long = metro_long)
    }
    
    text
    
  })
  
  output$inptitle <- renderUI({
    shiny::validate(
      need(nrow(df_selected() %>% filter(!is.na(tpr))) > 0, 
           "")
    )
    p("Inpatient beds occupied",
      style = sub_chart_title_style)
  })
  output$icutitle <- renderUI({
    shiny::validate(
      need(nrow(df_selected() %>% filter(!is.na(tpr))) > 0, 
           "")
    )
    p("ICU beds occupied",
      style = sub_chart_title_style)
  })
  
  vax_elig_title <- reactive({
    req(input$level, input$geography)
    geo_title_create(one_country_regions, input$level, input$geography, 
                     "Proportion who would accept a Covid-19 vaccine", 
                     " (of available country data)")

  })

  output$vaxacctitle <- renderText({
    req(!is.na(input$geography) & !is.na(input$level))
    vax_elig_title()
  })
  output$vaxacctitleregional <- renderText({
    req(!is.na(input$geography) & !is.na(input$level))
    vax_elig_title()
  })

  output$vaxeligtitle <- renderText({
    req(!is.na(input$geography) & !is.na(input$level))
    paste0(geo_title_create(one_country_regions, input$level, input$geography, 
                     "Vaccine eligibility", 
                     " (of available country data)"),
          " as of ", vax_elig_date())

  })
  output$vaxhesttitle <- renderText({
    req(!is.na(input$geography) & !is.na(input$level))
    geo_title_create(one_country_regions, input$level, input$geography, 
                     "Main reasons driving vaccine hesitancy 
                     (reported by unvaccinated people who are not \"definitely\" planning to get vaccinated", 
                     ", of available country data",
                     end_text = ")")
  })
  output$strucbartitle <- renderText({
    req(!is.na(input$geography) & !is.na(input$level))
    geo_title_create(one_country_regions, input$level, input$geography, 
                     "Structural barriers to vaccination (reported by the vaccinated and those trying to get vaccinated", 
                     ", of available country data",
                     end_text = ")")
  })
  
  # update data based on changing inputs ---------------------------------------
  
  df_filt_by_date <- reactive({
    req(input$date, input$change_over)
    filter_dashboard_dates(df_dashboard, input$date, input$change_over)
  })
  
  df_demo_filt_by_date <- reactive({
    req(input$date, input$change_over)
    filter_dashboard_dates(df_demo_site, input$date, input$change_over)
  })
  
  demo_sites <- reactive({
    req(df_demo_filt_by_date())
    df_demo_filt_by_date() %>% 
      dplyr::distinct(subregion_3) %>% 
      rename(names = subregion_3)
  })
  
  df_demo_selected <- reactive({
    req(input$level, input$geography, demo_sites(),
        input$level == "Demonstration site",
        input$geography %in% (demo_sites() %>% pull())
        )
    df_demo_filt_by_date() %>% filter(subregion_3 == input$geography)
  })
  
  demo_state <- reactive({
    req(df_demo_selected())
    df_demo_selected() %>% dplyr::select(subregion_2) %>% unique() %>% pull()
  })
  
  df_selected <- reactive({
    req(input$level, input$geography)
    if(input$level == "Demonstration site"){
      req(df_demo_selected())
    }
    
    get_df_selected(input$level, input$geography, input$country, 
                            input$district, df_filt_by_date(), demo_state())
  }) 
  
  df_valid_dates <- reactive({
    req(df_selected())
    survey_conducted <- df_selected() %>% 
      filter(!is.na(recent_behav_date)) %>% 
      dplyr::select(all_of(vac_metrics), all_of(beh_metrics), recent_behav_date) %>% 
      unique()
    filtered <- survey_conducted %>% 
      filter(recent_behav_date >= start_date(), recent_behav_date <= input$date)
    if(nrow(filtered) == 0){
      get_last_day_data(survey_conducted)
    } else {
      filtered
    }
  })
  
  df_demo_valid_dates <- reactive({
    req(df_demo_selected())
    survey_conducted <- df_demo_selected() %>%
      filter(!is.na(recent_behav_date)) %>%
      dplyr::select(vaccine_accept, recent_behav_date) %>%
      unique()
    filtered <- survey_conducted %>%
      filter(recent_behav_date >= start_date(), recent_behav_date <= input$date)
    if(nrow(filtered) == 0){
      get_last_day_data(survey_conducted)
    } else {
      filtered
    }
  })
  
  line_bool <- reactive({
    req(df_valid_dates(), input$level, input$geography)
    get_line_bool(df_valid_dates(), input$level, input$geography)
  })
  
  line_bool_demo <- reactive({
    req(df_demo_valid_dates(), input$level, input$geography)
    get_line_bool(df_demo_valid_dates(), input$level, input$geography)
  })
  
  # reactive values ------------------------------------------------------------
  test_threshold1 <- reactive({
    # determine the threshold for a "good" level of tests
    # based on the level of cases in the selected date and geography. 
    val <- df_selected() %>% filter(date == input$date) %>% pull(cases_per_mil)
    max(val*test_threshold_case_multiplier, test_threshold_minimum, na.rm = TRUE)
  })
  
  test_high_threshold <- reactive({
    # determine the threshold for a "very bad" level of 
    # tests based on the level of cases in the selected date
    # and geography, uses the threshold defined in the global
    val <- df_selected() %>% filter(date == input$date) %>% pull(cases_per_mil)
    max(val*test_threshold_multiplier_low, test_threshold_minimum, na.rm = TRUE)
  })
  
  test_threshold2 <- reactive({
    #adding a value in the middle to add a new category
    mean(test_threshold1(),test_high_threshold())
  })

  # dynamic text ---------------------------------------------------------------
  
  output$display_geo <- renderText({
    # display the geography, unless a specific district is selected
     # in which case display the district
    if(input$level == "Demonstration site"){
      paste(input$geography, demo_state(), sep=", ")
    } else if (is.null(input$district)) {
        input$geography
    } else if (input$district == "TOTAL") {
       input$geography
    } else {
      input$district
    }
     
  })
  output$demo_display_geo <- renderText({
    # display the geography, unless a specific district is selected
    # in which case display the district
    if(input$level == "Demonstration site"){
      paste(input$geography, demo_state(), sep=", ")
    } else if (is.null(input$district)) {
      input$geography
    } else if (input$district == "TOTAL") {
      input$geography
    } else {
      input$district
    }
    
  })
  
  output$display_pop <- renderText({ 
    if(input$level == "Demonstration site"){
      req(nrow(df_demo_selected()) > 0)
      pop_data <- df_demo_selected() %>% 
        mutate(display_population = population)
    } else {
      req(nrow(df_selected()) > 0)
      pop_data <- df_selected() 
    }
    pop <- pop_data %>% 
      filter(date == input$date) %>% 
      pull(if_else((input$level == "Region" & !(input$geography %in% one_country_regions)), 
                   display_population, 
                   population)) 
    
    shiny::validate(need(!is.na(pop), "") # display nothing if we are missing population for the geography
    )

    paste("Population", format(round(pop/100000)/10, big.mark=",", scientific = FALSE), "million")
  })
  output$demo_display_pop <- renderText({ 
    if(input$level == "Demonstration site"){
      req(nrow(df_demo_selected()) > 0)
      pop_data <- df_demo_selected() %>% 
        mutate(display_population = population)
    } else {
      req(nrow(df_selected()) > 0)
      pop_data <- df_selected() 
    }
    pop <- pop_data %>% 
      filter(date == input$date) %>% 
      pull(if_else((input$level == "Region" & !(input$geography %in% one_country_regions)), 
                   display_population, 
                   population)) 
    
    shiny::validate(need(!is.na(pop), "") # display nothing if we are missing population for the geography
    )
    
    paste("Population", format(round(pop/100000)/10, big.mark=",", scientific = FALSE), "million")
  })

  output$as_of <- renderText({
    "As of"
  })

  output$sah_display_date <- renderText( {
    req(nrow(df_recent_sah()) > 0)
    display_date(input$date, df_recent_sah()$date[1])
  })
  output$restr_gath_display_date <- renderText( {
    req(nrow(df_recent_restr_gath()) > 0)
    display_date(input$date, df_recent_restr_gath()$date[1])
  })
  output$school_closing_display_date <- renderText( {
    req(nrow(df_recent_school_closing()) > 0)
    display_date(input$date, df_recent_school_closing()$date[1])
  })
  output$req_mask_display_date <- renderText( {
    req(nrow(df_recent_req_mask()) > 0)
    display_date(input$date, df_recent_req_mask()$date[1])
  })

  output$display_change <- renderText({
    req(input$change_over)
    if (input$change_over != "Since pandemic began") {
      paste("Change over", tolower(input$change_over))
    } else {
      paste("Change", tolower(input$change_over))
    } 
    
  })

  output$recent_date <- renderText({
    req(df_selected(), input$date)
    # if no data, display today's date
    if(nrow(filter(df_selected(), !is.na(deaths_per_mil))) == 0) 
      display_date(today, input$date)
    # else display recent date with valid data
    else get_nmiss_date(df_selected(), deaths_per_mil, today)
  })

  output$deaths_val <- renderText({
    req(nrow(df_selected()) > 0)
    format(
      get_nmiss_val(df_selected(), deaths_per_mil),
      big.mark=",", digits =2, scientific = FALSE
    )
      
  })
  output$cfr_pct <- renderText({
    shiny::validate(
      need(nrow(df_selected() %>% filter(!is.na(cfr))) > 0, 
           paste("Insufficient CFR data for", input$geography, "in this time period."))
    )
    
    get_nmiss_trend(df_selected(), cfr)
  })
  
  output$cfr_val <- renderText({
    shiny::validate(
      need(nrow(df_selected() %>% filter(!is.na(cfr))) > 0, 
           paste("Insufficient CFR data for", input$geography, "in this time period."))
    )
    
    df <- filter(df_selected(),(as.numeric(date - ymd(input$date)) %% 7) == 0)
    scales::percent(get_nmiss_val(df, cfr), accuracy = .1)

  })
  
  output$deaths_pct <- renderText({
    req(nrow(df_selected()) > 0)
    paste(get_nmiss_trend(df_selected(), deaths_per_mil))
  })
  
  # policy indicators ----------------------------------------------------------
  
  df_recent_sah <- reactive({
    req(nrow(df_selected()) > 0, input$date, input$level, input$geography)
    get_recent_policy(df_selected(), df_dashboard, "sah", input$date, input$level, input$geography)
  })
  output$sah <- renderUI({
    req(input$level, input$geography, df_recent_sah(), input$level != "Demonstration site")
    get_policy_icon(df_recent_sah(), "sah", df_recent_sah()$date[1], input$level, input$geography)
  })
  
  
  df_recent_restr_gath <- reactive({
    req(nrow(df_selected()) > 0, input$date, input$level, input$geography)
    get_recent_policy(df_selected(), df_dashboard, "restr_gath", input$date, input$level, input$geography)
  })
  output$restr_gath <- renderUI({
    req(input$level, input$geography, df_recent_restr_gath(), input$level != "Demonstration site")
    get_policy_icon(df_recent_restr_gath(), "restr_gath", df_recent_restr_gath()$date[1], input$level, input$geography)
  })
  
  df_recent_school_closing <- reactive({
    req(nrow(df_selected()) > 0 , input$date, input$level, input$geography)
    get_recent_policy(df_selected(), df_dashboard, "school_closing", input$date, input$level, input$geography)
  })
  output$school_closing <- renderUI({
    req(input$level, input$geography, df_recent_school_closing(), input$level != "Demonstration site")
    get_policy_icon(df_recent_school_closing(), "school_closing", df_recent_school_closing()$date[1], input$level, input$geography)
  })
  
  df_recent_req_mask <- reactive({
    req(nrow(df_selected()) > 0, input$date, input$level, input$geography)
    get_recent_policy(df_selected(), df_dashboard, "req_mask", input$date, input$level, input$geography)
  })
  output$req_mask <- renderUI({
    req(input$level, input$geography, df_recent_req_mask(), input$level != "Demonstration site")
    get_policy_icon(df_recent_req_mask(), "req_mask", df_recent_req_mask()$date[1], input$level, input$geography)
  })
    
  # figures --------------------------------------------------------------------
  duration_dates <- reactive({
    case_when(
      input$change_over == "Two weeks"                  ~ weeks(2), 
      input$change_over == "One month"                  ~ months(1), 
      input$change_over == "Three months"               ~ months(3), 
      input$change_over == "Six months"                 ~ months(6),
      input$change_over == "Twelve months"              ~ months(12),
      input$change_over == "Eighteen months"            ~ months(18),
      input$change_over == "Since the pandemic began"   ~ as.period(ymd(input$date) - ymd("2020-03-01"))
    )
  })
  
  output$death_trend <- renderPlotly({
    shiny::validate(
      need(nrow(df_selected() %>% filter(!is.na(deaths_per_mil))) > 0, 
           paste("Insufficient death data for", input$geography, "in this time period."))
    )
    trend_only(df_selected(),
               deaths_per_mil, 
               date_range = c(ymd(input$date) %m-% duration_dates(), ymd(input$date))
               ) 
  })
      
  output$cfr_trend <- renderPlotly({
    
    df_weekly <- filter(df_selected(),
                        # limit to data every 7 days from the end date
                        (as.numeric(date - ymd(input$date)) %% 7) == 0 )
    
    shiny::validate(
      need(nrow(df_weekly %>% filter(!is.na(cfr))) > 1, 
           paste("Insufficient CFR data for", input$geography, "in this time period."))
    )
    trend_only(df_weekly,
               cfr,
               date_range = c(ymd(input$date) %m-% duration_dates(), ymd(input$date)),
               trend_length = 1, 
               tooltip_acc = 0.1, 
               percent = TRUE)
  })
  
  output$tpr_fig <- renderPlotly({
    shiny::validate(
      need(nrow(df_selected() %>% filter(!is.na(tpr))) > 0, 
           paste("Insufficient TPR data for", input$geography, "in this time period."))
    )
    
    line_graph(df_selected(),
               "tpr",
               tpr_threshold1,
               duration = input$change_over,
               label = "Percent",
               acc = .1,
               label_opt = "percent",
               tooltip_acc = .1)
  })
  
  output$tests_fig <- renderPlotly({
    shiny::validate(
      need(nrow(df_selected() %>% filter(!is.na(tests_per_mil))) > 0, 
           paste("Insufficient test data for", input$geography, "in this time period."))
    )
    line_graph(df_selected(),
               "tests_per_mil",
               test_threshold1(),
               duration = input$change_over,
               label = "Tests",
               acc = 1,
               tooltip_acc = 0, 
               label_opt = "numeric",
               trim_target = TRUE)
  })
  
  output$cases_fig <- renderPlotly({
    
    shiny::validate(
      need(nrow(df_selected() %>% filter(!is.na(cases_per_mil))) > 0, 
           paste("Insufficient case data for", input$geography, "in this time period."))
    )

    line_graph(df_selected(),
               "cases_per_mil",
               cases_threshold1,
               duration = input$change_over,
               label = "Cases",
               acc = .1,
               tooltip_acc = 0, 
               label_opt = "numeric",
               target_label_down = FALSE)
  })
  
  output$lmic_fig <- renderPlotly({
    
    shiny::validate(
      need(nrow(df_selected() %>% filter(!is.na(LMIC_vacc_pct))) > 0, 
           paste("Insufficient case data for", input$geography, "in this time period."))
    )
    
    line_graph(df_selected(),
               "LMIC_vacc_pct",
               threshold=vpeople_display_threshold,
               duration = input$change_over,
               label = "Percent",
               acc = .1,
               tooltip_acc = 1, 
               label_opt = "percent")
  })
  
  output$prop_vacc_county_figure <- renderPlotly({
    # vpeople_fig
    req(df_demo_selected())
    create_multi_time_series(df_demo_selected(), start_date(), input$date, 
                             c("all ages" = "cum_pct_people_fully_vacc"), 
                             input$change_over, 
                             c("black"),
                             y_lab = "Proportion fully vaccinated",
                             showlegend = FALSE)
  })
  output$prop_vacc_county_err <- renderText({
    req(df_demo_selected())
    paste("Insufficient vaccination data for", input$geography, "in this time period.")
  })
  observe({
    req(df_demo_selected())
    validate_mod(name = "prop_vacc_county",
                 condition =
                   (nrow(df_demo_selected() %>% filter(!is.na(cum_pct_people_fully_vacc)) > 0))
    )
  })
  
  output$vacc_accept_msa_figure <- renderPlotly({
    # vac_figure
    req(df_demo_valid_dates(), start_date(),
        nrow(df_demo_valid_dates() %>% dplyr::filter(!is.na(vaccine_accept))) > 0)

    behServer(df_demo_valid_dates(), line_bool_demo(), start_date(), input$date, 
              vac_map_census, vac_map_census_wrap,
              input$change_over, c("black"), input$level, input$geography,
              y_lab = "Percentage of\nunvaccinated respondents", wrap = TRUE,
              legend_pos = list(orientation = "h", y = -0.1),
              target = vaccine_accept_threshold)
  })
  output$vacc_accept_msa_err <- renderText({
    req(df_demo_valid_dates())
    paste("Insufficient vaccine acceptance data for", input$geography, "in this time period.")
  })
  observe({
    req(df_demo_valid_dates(), start_date())
    validate_mod(name = "vacc_accept_msa",
                 condition =
                   (nrow(df_demo_valid_dates()) > 0))
  })
  
  
  output$vpeople_fig <- renderPlotly({
    propVaccServer(df_selected(), vpeople_map,
                   input$geography, input$change_over, threshold = vpeople_display_threshold, 
                   colors = c('black', dark_grey, light_grey), 
                   booster_fully_vac_threshold = booster_fully_vac_threshold)
    
  })
  output$bipoc_pct_vacc_est_fig <- renderPlotly({
    propVaccServer(df_selected(), "bipoc_pct_vacc_est", input$geography, input$change_over, 
    threshold = vpeople_display_threshold)
    
  })

  vax_acc_plot <- reactive({
    req((nrow(df_selected() %>% filter(!is.na(vaccinated_appointment_or_accept))) > 0) |
             (nrow(df_selected() %>% filter(!is.na(appointment_or_accept_covid_vaccine))) > 0),
        input$change_over)

    line_graph(df_selected(),
            prop_metric = prob_def_vacc_map,
            threshold = vacc_acc_threshold,
            label = "Percent",
            duration = input$change_over,
            acc = .1,
            label_opt = "percent",
            tooltip_acc = .1, 
            show_threshold = TRUE,
            colors = c("black", dark_grey))
    
  })
  output$vax_acc_fig <- renderPlotly({
    # need either "vaccinated_appointment_or_accept" or "appointment_or_accept_covid_vaccine" 
    # to have at least 1 row of data
    shiny::validate(
      need((nrow(df_selected() %>% filter(!is.na(vaccinated_appointment_or_accept))) > 0) |
             (nrow(df_selected() %>% filter(!is.na(appointment_or_accept_covid_vaccine))) > 0), 
           paste("Insufficient survey data for", input$geography, "in this time period."))
    )
    
    vax_acc_plot()
  })

  output$vax_acc_fig_regional <- renderPlotly({
    # need either "vaccinated_appointment_or_accept" or "appointment_or_accept_covid_vaccine" 
    # to have at least 1 row of data
    shiny::validate(
      need((nrow(df_selected() %>% filter(!is.na(vaccinated_appointment_or_accept))) > 0) |
             (nrow(df_selected() %>% filter(!is.na(appointment_or_accept_covid_vaccine))) > 0), 
           paste("Insufficient survey data for", input$geography, "in this time period."))
    )
    
    vax_acc_plot()
  })
  
  # map ------------------------------------------------------------------------
  
  output$map <- renderLeaflet({
    req(input$level, input$date, input$geography)
    req(nrow(df_selected()) > 0)
    req(test_high_threshold(), test_threshold1())
    
    if(input$level == "Sub-national"){
      req(input$country)
      req(input$district)
    }
    
    if(input$level == "Demonstration site"){
      geo <- demo_state()
      lvl <- "Sub-national"
      country <- unique(df_selected()$subregion_1)
      site_name <- input$geography
    } else {
      geo <- input$geography
      lvl <- input$level
      country <- input$country
      site_name <- NULL
    }
      
    map_geo(lvl, geo, country, input$district,
            input$date, df_selected(), test_high_threshold(), test_threshold1(), test_threshold2(), site_name)
   })
  
  output$demo_map <- renderLeaflet({
    req(input$level, input$date, input$geography)
    req(nrow(df_selected()) > 0)
    req(test_high_threshold(), test_threshold1())
    
    if(input$level == "Sub-national"){
      req(input$country)
      req(input$district)
    }
    
    if(input$level == "Demonstration site"){
      geo <- demo_state()
      lvl <- "Sub-national"
      country <- unique(df_selected()$subregion_1)
      site_name <- input$geography
    } else {
      geo <- input$geography
      lvl <- input$level
      country <- input$country
      site_name <- NULL
    }
    
    map_geo(lvl, geo, country, input$district,
            input$date, df_selected(), test_high_threshold(), test_threshold1(), test_threshold2(), site_name)
  })

  
  observe({
    req(nrow(df_selected()) != 0, input$level != "Demonstration site", input$geography != 'United States',
        !(input$level == 'Sub-national' & input$country == 'United States'))
    val <- filter(df_selected(), date == input$date)$rel_cvi*100
    kpiServer(id = "cvi", indicator_value = val,
              format = ".1f", tick_format = ".0f", 
              rev = "false", min = 0,  max = 100)
  })
    
  observe({
    req(nrow(df_selected()) != 0, input$level != "Demonstration site", input$geography != 'United States',
        !(input$level == 'Sub-national' & input$country == 'United States'))
    val <- filter(df_selected(), date == input$date)$sci
    kpiServer(id = "ehsi", indicator_value = val, 
              format = ".0f", tick_format = ".0f",
              rev = "true", min = 0,  max = 100) # rev = true implies lower = worse
  })
  
  observe({
    req(nrow(df_selected()) != 0, input$level != "Demonstration site", input$geography != 'United States',
        !(input$level == 'Sub-national' & input$country == 'United States'))
    val <- filter(df_selected(), date == input$date)$dtp3*100
    kpiServer(id = "dtp3", indicator_value = val, 
              format = ".0f", tick_format = ".0f", 
              rev = "true", min = 0,  max = 100) # rev = true implies lower = worse
  })
  
  observe({
    req(nrow(df_selected()) != 0, input$level != "Demonstration site")
    val <- filter(df_selected(), date == input$date)$stringency_index
    kpiServer(id = "polstr", indicator_value = val, 
              format = ".0f", tick_format = ".0f", 
              rev = "false", min = 0,  max = 100)
  })

  # update the map legend
  observeEvent(c(input$level, input$map_groups, input$date, input$geography), {
     map <- leafletProxy("map") %>% clearControls()
     if (any(input$map_groups %in% "Proportion BIPOC vaccinated (ages 5+, at least one dose), estimated")) {
       map <- map %>% 
         addLegend(data = map,
                   colors = c(red, orange, yellow, navy, light_grey),
                   labels = c(paste0("Less than ", scales::percent(bipoc_vacc_pct_threshold1, digits = 0)),
                              paste0(scales::percent(bipoc_vacc_pct_threshold1, digits = 0), " to ", scales::percent(bipoc_vacc_pct_threshold2-.01, digits = 0)),
                              paste0(scales::percent(bipoc_vacc_pct_threshold2, digits = 0), " to ", scales::percent(bipoc_vacc_pct_threshold3-.01, digits = 0)),
                              paste0("Greater than or equal to ", scales::percent(bipoc_vacc_pct_threshold3, digits = 0)),
                              "No data"),
                   group = "Proportion BIPOC vaccinated (ages 5+, at least one dose), estimated",
                   position = "topleft")
     }
     if (any(input$map_groups %in% "New cases per million per day (7-day avg.)")) {
        map <- map %>% 
            addLegend(data = map,
                      colors = c(navy, yellow, orange, red, light_grey),
                      labels = c(paste0("Less than ",format(cases_threshold1, big.mark=",", digits =0, scientific = FALSE)),
                                 paste0(format(cases_threshold1, big.mark=",", digits =0, scientific = FALSE), " to ", format(cases_threshold2-1, big.mark=",", digits =0, scientific = FALSE)),
                                 paste0(format(cases_threshold2, big.mark=",", digits =0, scientific = FALSE), " to ", format(cases_high_threshold-1, big.mark=",", digits =0, scientific = FALSE)),
                                 paste0("Greater than or equal to ", format(cases_high_threshold, big.mark=",", digits =0, scientific = FALSE)),
                                 "No data"),
                      group = "New cases per million per day (7-day avg.)",
                      position = "topleft")
      }
      if (any(input$map_groups %in% "Tests conducted per million per day (7-day avg.)")) {
        map <- map %>% 
          addLegend(data = map,
                    colors = c(navy, yellow, orange, red, light_grey),
                    labels = c("Sufficient given case counts",
                               "Marginal given case counts",
                               "Low given case counts",
                               "Very low given case counts",
                               "No data"),
                    group = "Tests conducted per million per day (7-day avg.)",
                    position = "topleft")
      }
      if(any(input$map_groups %in% "Test positivity rate (7-day avg.)")) {
        map <- map %>%
          addLegend(data = map,
                    colors = c(navy, yellow, orange, red, light_grey),
                    labels = c(paste0("Less than ", scales::percent(tpr_threshold1, accuracy = 0.1)),
                               paste0(scales::percent(tpr_threshold1, accuracy = 0.1), " to ", scales::percent(tpr_threshold2-0.001, accuracy = 0.1)),
                               paste0(scales::percent(tpr_threshold2, accuracy = 0.1), " to ", scales::percent(tpr_high_threshold-0.001, accuracy = 0.1)),
                               paste0("Greater than or equal to ", scales::percent(tpr_high_threshold, accuracy = 0.1)),
                               "No data"),
                    group = "Test positivity rate (7-day avg.)",
                    position = "topleft")
      }
     if(any(input$map_groups %in% "Proportion vaccinated (full population, all ages, at least one dose)")) {
       map <- map %>%
         addLegend(data = map,
                   colors = c(red, orange, yellow, navy, light_grey),
                   labels = c(paste0("Less than ", scales::percent(vpeople_threshold1, digits = 0)),
                              paste0(scales::percent(vpeople_threshold1, digits = 0), " to ", scales::percent(vpeople_threshold2-.01, digits = 0)),
                              paste0(scales::percent(vpeople_threshold2, digits = 0), " to ", scales::percent(vpeople_high_threshold-.01, digits = 0)),
                              paste0("Greater than or equal to ", scales::percent(vpeople_high_threshold, digits = 0)),
                              "No data"),
                   group = "Proportion vaccinated (full population, all ages, at least one dose)",
                   position = "topleft")
     }
  })
  
  # modify button text based on level
  observeEvent(c(input$level, input$country, input$district), {
    
    if (input$level == "Country") {
      return_label <- "Return to region"
    } else if(input$level == "Sub-national"){
      req(input$country, input$district)
      if(input$country != "India" | input$district == "TOTAL"){
        return_label <- "Return to country"
      } else {
        return_label <- "Return to state"
      }
    } else {
      return_label <- "Return to state"
    }
    updateActionButton(session = session, inputId = "return",
                       label = return_label)
    updateActionButton(session = session, inputId = "demo_return",
                       label = return_label)
    
  })
  
  # update level input on map clicks
  observeEvent(input$map_shape_click, {
    req(input$level != "Demonstration site")
    click <- input$map_shape_click  # store clicked geography

    if (! gsub("[0-9]", "", click$id) == ""){
      id <- gsub("[0-9]", "", click$id)
      
      
      # construct valid_click, which 
      # confirms that clicked geography exists in the dataset
      if (input$level == "Region") {
        
        valid_click <- (id %in% country_list)
        
      } else if (input$level == "Country" & input$geography %in% subnat_countries) {
        # identify the list of subnational geographies for which data are available
        subnat_opts <- df_dashboard %>% 
          filter(subregion_2 != "TOTAL" & subregion_1 == input$geography) %>% 
          arrange(subregion_2) %>% 
          pull(subregion_2) %>% 
          unique()
        # confirms that clicked geography exists in the dataset
        valid_click <- (id %in% subnat_opts | id == input$geography)
      } else if (input$level == "Country") {
        valid_click <- TRUE
      } else if (input$level == "Sub-national") {
        if (is.null(input$geography)) {
          valid_click <- FALSE
        } else if ((input$country != "India") | (input$district == "TOTAL")) {
          # standard subnational page: user can select a new state or province by clicking
          valid_click <- (id %in% geo_list())
        } else if (is.null(input$district)) {
          valid_click <- FALSE
        } else {
          valid_click <- (id %in% district_list())
        }
        
      }
    }
    
    # unless valid_click is true, do nothing
    if (valid_click == TRUE) { 
      if (input$level == "Region") {
        new_level <- "Country"
        new_geography <- id
        new_action_label <- "Return to region"
        
      } else if (input$level == "Country") {
  
        # only set new levels for countries with sub-national data available
        # (note, skip this step if id == input$geography, meaning sub-national
        # data were not available to display)
        if (input$geography %in% subnat_countries & id != input$geography) {
          new_level <- "Sub-national"
          # set new_geography to NULL, dashboard will select default subregion
          new_geography <- id
          new_action_label <- "Return to country"
          new_country <- input$geography
        } else if (input$geography %in% subnat_countries & id == input$geography) {
          new_level <- "Sub-national"
          # set new_geography to NULL, dashboard will select default subregion
          new_geography <- NULL
          new_action_label <- "Return to country"
          new_country <- input$geography
        } else {
          new_level <- NULL
          new_geography <- NULL
          new_action_label <- NULL
          new_country <- NULL
        }
        
      } else if(input$level == "Sub-national") {
      if (is.null(input$geography)) {
        new_level <- NULL
        new_geography <- NULL
        new_action_label <- NULL
      } else if (input$country != "India") {
        # standard subnational page: user can select a new state or province by clicking
        new_level <- "Sub-national"
        new_geography <- id
        new_action_label <- "Return to country"
      } else if (is.null(input$district)) {
        new_level <- NULL
        new_geography <- NULL
        new_action_label <- NULL
      } else if (input$district == "TOTAL") {
        new_level <- "Sub-national"
        new_geography <- id
        new_action_label <- "Return to country"
      } else {
        new_level <- "Sub-national"
        # do not update geography when changing districts within the same state
        new_geography <- NULL
        new_district <- id
        new_action_label <- "Return to state"
      }
      
    }
    
    # do nothing if new_level is null
    if ((is.null(new_level)== FALSE)) {
      
      # update selections based on click and context
      
      if (is.null(new_action_label)== FALSE) {
        updateActionButton(session = session, 
                           inputId = "return", 
                           label = new_action_label)
      }
      updatePickerInput(session = session,
                        inputId = "level",
                        selected = new_level)
      if (is.null(new_geography)==FALSE) {
        updatePickerInput(session = session,
                          choices = new_geography,
                          inputId = "geography",
                          selected = new_geography)
      }
      # update the country when jumping from country to sub-national
      if (input$level=="Country") {
        # if new_country is null (for countries w/o subnational data) do nothing
        if (!is.null(new_country)) {
          updatePickerInput(session = session,
                            choices = new_country,
                            inputId = "country",
                            selected = new_country)
        }
      }
      # update the district when a district is currently selected
      if ((input$level == "Sub-national")) {
        if ((input$country == "India")) {
          if (!is.null(input$district)) {
            if ((input$district != "TOTAL")) {
              updatePickerInput(session = session,
                                choices = district_list(),
                                inputId = "district",
                                selected = new_district)
            }
          } 
        }
      }
    }
    
  }
    
  })
  
  # return to higher level when user clicks return button
  observeEvent(input$return, {

    if (input$level == "Country") {
      
      # want to change the level and geography pickers
      new_level <- "Region"
      new_geography <- unique(df_selected()$region)
      
    } else if(input$level == "Demonstration site"){
      new_level <- "Sub-national"
      new_geography <- demo_state()
      new_district <- "TOTAL"
      
    } else if(input$level == "Sub-national") {
      if (is.null(input$geography)) {
        new_level <- NULL
        new_geography <- NULL
      } else if (input$country != "India") {
        new_level <- "Country"
        new_geography <- unique(df_selected()$subregion_1)
      } else if (is.null(input$district)) {
        new_level <- NULL
        new_geography <- NULL
      } else if (input$district == "TOTAL") {
        new_level <- "Country"
        new_geography <- unique(df_selected()$subregion_1)
        new_district <- "TOTAL"
      } else {
        new_level <- "Sub-national"
        new_geography <- unique(df_selected()$subregion_2)
        # stay on subregion page, but return to state total
        new_district <- "TOTAL"
      }
      
    }

    # do nothing if new_level/geography are null
    if ((!is.null(new_level)) & (!is.null(new_geography))) {

      # update the district when a district is currently selected
      if (!is.null(input$district) & !is.null(input$country)) {
        if ((input$country == "India") & (input$level == "Sub-national") & (input$district != "TOTAL")) {
          updatePickerInput(session = session,
                            choices = new_district,
                            inputId = "district",
                            selected = new_district)
        }
      }


      updatePickerInput(session = session,
                        inputId = "level",
                        selected = new_level)
      if(input$level == "Demonstration site"){
        updatePickerInput(session = session,
                          inputId = "country",
                          selected = "United States")
      }
      updatePickerInput(session = session,
                        choices = new_geography,
                        inputId = "geography",
                        selected = new_geography)

     

    
    }

  })
  
  # return to higher level when user clicks return button
  observeEvent(input$demo_return, {
    
    if (input$level == "Country") {
      
      # want to change the level and geography pickers
      new_level <- "Region"
      new_geography <- unique(df_selected()$region)
      
    } else if(input$level == "Demonstration site"){
      new_level <- "Sub-national"
      new_geography <- demo_state()
      new_district <- "TOTAL"
      
    } else if(input$level == "Sub-national") {
      if (is.null(input$geography)) {
        new_level <- NULL
        new_geography <- NULL
      } else if (input$country != "India") {
        new_level <- "Country"
        new_geography <- unique(df_selected()$subregion_1)
      } else if (is.null(input$district)) {
        new_level <- NULL
        new_geography <- NULL
      } else if (input$district == "TOTAL") {
        new_level <- "Country"
        new_geography <- unique(df_selected()$subregion_1)
        new_district <- "TOTAL"
      } else {
        new_level <- "Sub-national"
        new_geography <- unique(df_selected()$subregion_2)
        # stay on subregion page, but return to state total
        new_district <- "TOTAL"
      }
      
    }
    
    # do nothing if new_level/geography are null
    if ((!is.null(new_level)) & (!is.null(new_geography))) {
      
      # update the district when a district is currently selected
      if (!is.null(input$district) & !is.null(input$country)) {
        if ((input$country == "India") & (input$level == "Sub-national") & (input$district != "TOTAL")) {
          updatePickerInput(session = session,
                            choices = new_district,
                            inputId = "district",
                            selected = new_district)
        }
      }
      
      
      updatePickerInput(session = session,
                        inputId = "level",
                        selected = new_level)
      if(input$level == "Demonstration site"){
        updatePickerInput(session = session,
                          inputId = "country",
                          selected = "United States")
      }
      updatePickerInput(session = session,
                        choices = new_geography,
                        inputId = "geography",
                        selected = new_geography)
      
      
      
      
    }
    
  })
  
  output$inp_figure <- renderPlotly({
    shiny::validate(
      need(nrow(df_selected() %>% filter(!is.na(ip_beds_percent) | !is.na(covid_beds_percent))) > 0 , 
           paste("Insufficient inpatient hospitalization data for", input$geography, "in this time period."))
    )
    
    create_multi_time_series(df_selected(), start_date(), input$date, inp_map, input$change_over, c("black", dark_grey))
  })
  
  output$icu_figure <- renderPlotly({
    shiny::validate(
      need(nrow(df_selected() %>% filter(!is.na(icu_beds_percent))) > 0, 
           paste("Insufficient ICU data for", input$geography, "in this time period."))
    )
    
    create_multi_time_series(df_selected(), start_date(), input$date, icu_map, input$change_over, c("black"))
  })
  
  output$tevx_figure <- renderPlotly({
    req(nrow(df_selected() %>% filter(!is.na(bipoc_cum_people_vacc_est) | !is.na(bipoc_cum_people_vacc_rpt))) > 0)
    
    if (input$level %in% c("Country", "Region")) {
      create_multi_time_series(df_selected(), start_date(), input$date, tevx_map, input$change_over, c(light_green, orange, "black"), 
                               percent = FALSE, target = tevx_threshold, y_lims = c(0, tevx_threshold), y_lab = "People vaccinated", 
                               legend_pos = list(orientation = "h", y = -0.1),
                               legend_order = names(tevx_map))  
    } else {
      
      # determine eligible BIPOC population used to calculate BIPOC pct of vaccinations
      # includes Hispanic individuals if Hispanic individuals are not reported separately from race categories
      bipoc_pop_display <- df_selected() %>% 
        mutate(bipoc_pop_display = bipoc_cum_people_vacc_est/bipoc_pct_vacc_est) %>%
        filter(!is.na(bipoc_pop_display)) %>%
        arrange(subregion_1, subregion_2, subregion_3, date) %>%
        filter(row_number() == n()) %>%
        pull(bipoc_pop_display)
      
      # target will match 70% target for BIPOC percent vaccinated figure
      target_threshold <- 0.7 * bipoc_pop_display
      
      create_multi_time_series(df_selected(), start_date(), input$date, tevx_map_s, input$change_over, c(light_green, "orange"), 
                               percent = FALSE, target = target_threshold, y_lims = c(0, target_threshold), y_lab = "People vaccinated", 
                               legend_pos = list(orientation = "h", y = -0.1),
                               legend_order = names(tevx_map_s)) 
    }

  })
  output$tevx_err <- renderText({
    req(input$geography)
    paste("Insufficient vaccination data for", input$geography, "in this time period.")
  })
  observe({
    req(df_selected())
    if (nrow(df_selected() %>% filter(!is.na(bipoc_cum_people_vacc_est) | !is.na(bipoc_cum_people_vacc_rpt))) > 0) {
      shinyjs::show("tevx_figure_parent")
      shinyjs::hide("tevx_err_parent")
    } else {
      shinyjs::hide("tevx_figure_parent")
      shinyjs::show("tevx_err_parent")
    }
  })
  
  output$demo_pevx_figure <- renderPlotly({
    pevxFigure(df_selected(), start_date(), input$geography, input$date, input$change_over)
  })
  
  output$demo_pevx_err <- renderText({
    req(input$geography)
    paste("Insufficient vaccination data for", input$geography, "in this time period.")
  })
  observe({
    req(df_selected())
    validate_mod(name = "demo_pevx",
                 condition =
                   (nrow(df_selected() %>% filter(!is.na(bipoc_pct_vacc_est) | !is.na(bipoc_pct_vacc_rpt))) > 0))
  })
    
  output$pevx_figure <- renderPlotly({
    
    if (input$level %in% c("Country", "Region")) {
      pevxFigure(df_selected(), start_date(), input$geography, input$date, input$change_over)
    } else {
      pevxFigure_s(df_selected(), start_date(), input$geography, input$date, input$change_over)
    }
  })
  output$pevx_err <- renderText({
    req(input$geography)
    paste("Insufficient vaccination data for", input$geography, "in this time period.")
  })
  observe({
    req(df_selected())
    if (nrow(df_selected() %>% filter(!is.na(bipoc_pct_vacc_est) | !is.na(bipoc_pct_vacc_rpt))) > 0) {
      shinyjs::show("pevx_figure_parent")
      shinyjs::hide("pevx_err_parent")
    } else {
      shinyjs::hide("pevx_figure_parent")
      shinyjs::show("pevx_err_parent")
    }
  })
  
  
  output$demo_revx_figure <- renderPlotly({
    req(df_selected())
    revxFigure(df_selected(), input$geography, start_date(), input$date, input$change_over)
  })
  output$demo_revx_err <- renderText({
    req(input$geography)
    paste("Insufficient vaccination data for", input$geography, "in this time period.")
  })
  observe({
    req(df_selected())
    validate_mod(name = "demo_revx",
                 condition =
                   (nrow(df_selected() %>%
                           filter(!is.na(white_pct_vacc_est) | !is.na(asian_pct_vacc_est) |
                                    !is.na(black_pct_vacc_est) | !is.na(hispanic_pct_vacc_est) |
                                    !is.na(ai_an_pct_vacc_est) | !is.na(nh_pi_pct_vacc_est))) > 0))
  })
  
  output$revx_figure <- renderPlotly({
    revxFigure(df_selected(), input$geography, start_date(), input$date, input$change_over)
  })
  output$revx_err <- renderText({
    req(input$geography)
    paste("Insufficient vaccination data for", input$geography, "in this time period.")
  })
  observe({
    req(df_selected())
    if (nrow(df_selected() %>% 
             filter(!is.na(white_pct_vacc_est) | !is.na(asian_pct_vacc_est) |
                    !is.na(black_pct_vacc_est) | !is.na(hispanic_pct_vacc_est) |
                    !is.na(ai_an_pct_vacc_est) | !is.na(nh_pi_pct_vacc_est))) > 0) {
      shinyjs::show("revx_figure_parent")
      shinyjs::hide("revx_err_parent")
    } else {
      shinyjs::hide("revx_figure_parent")
      shinyjs::show("revx_err_parent")
    }
  })
  
  output$pvwkrfiguretitle <- renderText({
    req(!is.na(input$geography) & !is.na(input$level))
    if (input$geography == "United States") {
      text <- "Proportion of vaccinations with race/ethnicity information reported to CDC"
    }
    else {
      text <- "Proportion of vaccinations with race/ethnicity information reported to state agencies"
    }
   
  })
  
  observe({
    req(nrow(df_selected()) != 0, input$level != "Demonstration site")
    val <- df_selected() %>%
      filter(!is.na(known_race_pct_of_vacc_othmiss)) %>%
      arrange(subregion_1, subregion_2, subregion_3, date) %>%
      filter(row_number() == n()) %>%
      pull(known_race_pct_of_vacc_othmiss)
    kpiServer(id = "pvwkr_figure", indicator_value = val,
              format = ".0%", tick_format = ".0%",
              rev = "true", min = 0,  max = 1, r = 12) # rev = true implies lower = worse
  })

  output$pvwkr_err <- renderText({
    req(input$geography)
    paste("Insufficient vaccination data for", input$geography, "in this time period.")
  })
  
  observe({
    req(df_selected())
    if (nrow(df_selected() %>% filter(!is.na(known_race_pct_of_vacc_othmiss))) > 0) {
      shinyjs::show("pvwkr_figure_parent")
      shinyjs::hide("pvwkr_err_parent")
    } else {
      shinyjs::hide("pvwkr_figure_parent")
      shinyjs::show("pvwkr_err_parent")
    }
  })
  
  observe({
    req(nrow(df_selected()) != 0, input$level != "Demonstration site")
    val <- df_selected() %>%
      filter(!is.na(known_race_pct_of_vacc_kff)) %>%
      arrange(subregion_1, subregion_2, subregion_3, date) %>%
      filter(row_number() == n()) %>%
      pull(known_race_pct_of_vacc_kff)
    kpiServer(id = "pvwkr_figure_kff", indicator_value = val,
              format = ".0%", tick_format = ".0%",
              rev = "true", min = 0,  max = 1, r = 12) # rev = true implies lower = worse
  })
  
  output$pvwkr_err_kff <- renderText({
    req(input$geography)
    paste("Insufficient vaccination data for", input$geography, "in this time period.")
  })
  
  observe({
    req(df_selected())
    if (nrow(df_selected() %>% filter(!is.na(known_race_pct_of_vacc_kff))) > 0) {
      shinyjs::show("pvwkr_figure_parent_kff")
      shinyjs::hide("pvwkr_err_parent_kff")
    } else {
      shinyjs::hide("pvwkr_figure_parent_kff")
      shinyjs::show("pvwkr_err_parent_kff")
    }
  })
  
  output$disp_figure <- renderPlotly({
    req(df_selected())
    if (nrow(df_selected() %>% 
             filter(!is.na(bipoc_pct_of_vacc) & !is.na(bipoc_pct_of_pop))) > 0) {
    bipoc_graph(df_selected(), label = "Percent", duration = input$change_over, acc = 1, label_opt = "percent", limits = NULL, tooltip_acc = .1)
    }
  })
  output$disp_err <- renderText({
    req(input$geography)
    paste("Insufficient vaccination data for", input$geography, "in this time period.")
  })
  observe({
    req(df_selected())
    if (nrow(df_selected() %>% 
             filter(!is.na(bipoc_pct_of_vacc) & !is.na(bipoc_pct_of_pop))) > 0) {
      shinyjs::show("disp_figure_parent")
      shinyjs::hide("disp_err_parent")
    } else {
      shinyjs::hide("disp_figure_parent")
      shinyjs::show("disp_err_parent")
    }
  })
  
  output$info_panel <- renderUI({
    div(style = "display:table;table-layout:fixed",
        div(style = "display:table-cell; width:50%;", 
            textOutput("display_geo") %>% 
              tagAppendAttributes(class = "h4", style= 'font-size:22px;text-transform:uppercase;'), 
            textOutput("display_pop") %>% 
              tagAppendAttributes(class = "h4", style= 'font-size:22px;text-transform:uppercase;')
        ), 
        div(style = "display:table-cell;vertical-align:middle",
            conditionalPanel(# hide cvi for subnational pages
              condition = "input.level != 'Region'",
              actionButton("return", "Return to higher level",
                           style = "width:50%; font-size:140%;
                                                      position:absolute; right:5px;
                                                      padding:10px;"))
        )
    )
  })
  output$date_panel <- renderUI({
    absolutePanel(id="controls",
                  style="z-index:1000;",
                  class = "panel panel-default",
                  right = 0,
                  draggable = TRUE,
                  htmlOutput("display_date")
    )
  })
  output$demo_info_panel <- renderUI({
    div(style = "display:table;table-layout:fixed",
        div(style = "display:table-cell; width:50%;", 
            textOutput("demo_display_geo") %>% 
              tagAppendAttributes(class = "h4", style= 'font-size:22px;text-transform:uppercase;'), 
            textOutput("demo_display_pop") %>% 
              tagAppendAttributes(class = "h4", style= 'font-size:22px;text-transform:uppercase;')
        ), 
        div(style = "display:table-cell;vertical-align:middle",
            conditionalPanel(# hide cvi for subnational pages
              condition = "input.level != 'Region'",
              actionButton("demo_return", "Return to higher level",
                           style = "width:50%; font-size:140%;
                                                      position:absolute; right:5px;
                                                      padding:10px;"))
        )
    )
  })
  output$demo_date_panel <- renderUI({
    absolutePanel(id="controls",
                  style="z-index:1000;",
                  class = "panel panel-default",
                  right = 0,
                  draggable = TRUE
    )
  })

  output$dpm_row <- renderUI({
    trend_row("dpmtitle", "death_trend",
                              "deaths_val",
                              "deaths_pct")
  })

  output$cfr_row <- renderUI({
    # if don't have available data, don't show anything
    df_weekly <- filter(df_selected(),
                        # limit to data every 7 days from the end date
                        (as.numeric(date - ymd(input$date)) %% 7) == 0 )
    shiny::validate(
      need(nrow(df_weekly %>% filter(!is.na(cfr))) > 1, 
           "")
    )

    trend_row("cfrtitle", "cfr_trend", 
                              "cfr_val",
                              "cfr_pct")
  })
  
  output$policy_matrix <- renderUI({
    
    req(df_recent_sah(), df_recent_req_mask(), df_recent_restr_gath(), df_recent_school_closing())
    
    policy_dfs <- list(df_recent_sah(), df_recent_req_mask(), df_recent_school_closing(), df_recent_restr_gath())
    #select distinct valid dates for display
    display_dates <- lapply(policy_dfs, pol_date_select, level = input$level) %>% unlist() %>% as.Date() %>% unique()
    
    # if display dates is length 0 (all dates are missing) replace display_dates with today's date
    if (length(display_dates[!is.na(display_dates)])==0) display_dates <- as.Date(input$date)

    # all policies have the same most recent date (or there are no valid policies)
    if (length(display_dates[!is.na(display_dates)]) %in% c(1)) {
      fluidRow(
        fluidRow(
          column(7),
          column(2, p(display_date(today, display_dates[!is.na(display_dates)], prefix = "As of ")) %>% 
                   tagAppendAttributes(
                     class = "p",
                     style= 'font-size:16px;text-align:center;text-decoration:underline;font-weight: 600'
                   ))
        ),
        policy_row("Stay-at-home order", "sah"),
        policy_row("Restrictions on gatherings", "restr_gath"),
        policy_row("Masks required in public", "req_mask"),
        policy_row("Schools closed", "school_closing")
      )
    }
    # some policies were recorded more recently than others
    else {
      fluidRow(
        fluidRow(
          column(9),
          column(2, textOutput("as_of") %>% 
                   tagAppendAttributes(
                     class = "p",
                     style= 'font-size:16px;text-align:center;text-decoration:underline;font-weight: 600'
                   ))
        ),
        policy_row_date("Stay-at-home order", "sah", "sah_display_date"),
        policy_row_date("Restrictions on gatherings", "restr_gath", "restr_gath_display_date"),
        policy_row_date("Masks required in public", "req_mask", "req_mask_display_date"),
        policy_row_date("Schools closed", "school_closing", "school_closing_display_date")
      )
    }
  })

  # top three structural barriers ----------------------------------------------
  df_top_three_barrier <- reactive({
    req(nrow(df_selected()) > 0, input$date, input$level, input$geography)
    get_top_three_hest_struct(df_selected(), df_dashboard, "barrier",
      input$date, input$level, input$geography, input$country, input$district)
  })

  top_three_barrier_date <- reactive({
    req(df_top_three_barrier(), input$date)
    get_vax_acpt_survey_date(df_top_three_barrier(), input$date, today)
  })

  output$top_three_barrier <- renderUI({
    req(df_top_three_barrier(), input$geography, top_three_barrier_date())
    
    shiny::validate(
      need(nrow(df_top_three_barrier()) == 1, 
           paste("Insufficient survey data for", input$geography, "in this time period."))
    )
    
    top_three_survey_resp(df_top_three_barrier(), "barrier", top_three_barrier_date())
  })
  
  # top three vaccine hesitancy ------------------------------------------------
  df_top_three_hest <- reactive({
    req(nrow(df_selected()) > 0, input$date, input$level, input$geography)
    get_top_three_hest_struct(df_selected(), df_dashboard, "hest",
      input$date, input$level, input$geography, input$country, input$district)
  })

  top_three_hest_date <- reactive({
    req(df_top_three_hest(), input$date)
    get_vax_acpt_survey_date(df_top_three_hest(), input$date, today)
  })

  output$top_three_hest <- renderUI({
    req(df_top_three_hest(), input$geography, top_three_hest_date())

    shiny::validate(
      need(nrow(df_top_three_hest()) == 1, 
            paste("Insufficient survey data for", input$geography, "in this time period."))
    )
           
    top_three_survey_resp(df_top_three_hest(), "hest", top_three_hest_date())
  })

  # vaccine eligibility progress bar -------------------------------------------
  # vaccine eligibility will always be as of the date selected in input because 
  # the variable should always be filled 
  act_vacc_elig <- reactive({
    req(df_selected(), input$date)
    df_selected()[df_selected()$date == input$date, ]$vacc_elig
  })

  vax_elig_date <- reactive({
    req(input$date)
    display_date(today, input$date) 
  })

  output$vacc_elig_progress_bar <- renderPlotly({
    shiny::validate(
      need(!is.na(act_vacc_elig()), 
           paste("Insufficient survey data for", input$geography, "in this time period."))
    )
    make_vacc_elig_horiz_bar(act_vacc_elig(), vacc_elig_map)
  })
  
  output$dash_title <- renderUI({
    text <- "COVID-19 Response Dashboard"
    
    h1(text, style = "text-transform: uppercase;")
    
  })
  
  observeEvent(input$switchtab, {
    updateTabItems(session, "sidebar", "indicators")
  })
  
  observeEvent(input$sidebar, {
    shinyjs::runjs("window.scrollTo(0, 50)")
  })
  
  observe({
    if (input$sidebar == "dashboard") {
      addClass(selector = "body", class = "sidebar-collapse")
      removeClass(selector = "body", class = "control-sidebar-open")
    } else {
      removeClass(selector = "body", class = "sidebar-collapse")
      addClass(selector = "body", class = "control-sidebar-open")
    }
  })

})
