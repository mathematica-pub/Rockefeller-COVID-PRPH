get_map <- function(geo_level,
                    name,
                    district = NULL,
                    demo_site_state = NULL) {
  
  # create a map for the geography of interest
  # Region: create a map of the continent (or countries) with country as the lowest geographic resolution
  # Subregion 1 (country): create a map of the country with state/province as the lowest geographic resolution
  # Subregion 2 (state/province): create a map of the country containing the state/province, with state/province
  #         as the lowest level of geographic resolution
  # Subregion 3 (district): create a map of the state containing the district, with state as the lowest
  #         level of geographic resolution
  #
  # Note: current version of maps were created using raster and the map_create.R program
  
  # arguments:
  #' geo_level - region, country, sub-national, or district
  #' name - the name of the region or country of interest
  #' district - name of the district in the country of interest

  if (geo_level=="country") {
    map <- geo_sf_lvl0 %>% dplyr::filter(NAME_0==c(name))
  }
  else if (geo_level=="sub-national") {
    map <- geo_sf_lvl1 %>% dplyr::filter(NAME_0==c(name))
  }
  else if (geo_level=="district") {
    map <- geo_sf_lvl2 %>% dplyr::filter(NAME_0==c(name))
    state_name <- dplyr::filter(map, NAME_2==c(district)) %>% pull(NAME_1) 
    map <- dplyr::filter(map, NAME_1==c(state_name))
  }
  else if (geo_level=="demo_site") {
    map <- geo_sf_lvl1 %>% dplyr::filter(NAME_0==c(name) & NAME_1==c(demo_site_state))
  }
  else {
    map <- geo_sf_lvl0 %>% dplyr::filter(REGION==c(name))
  }
  
  if (name=="United States" & geo_level!="region") {
    map <- left_join(map, geo_sf_lvl3, by = c("NAME_1" = "state"))
  }
  
  return(map)
  
}

add_map_rectangle <- function(map, condition)  {
  if (condition) {
    map <- map %>%
      addRectangles(
        lng1=-137.036705, lat1=31.072695,
        lng2=-117.085529, lat2=18.562118,
        fillColor = "transparent",
        color = "gray",
        opacity = 1
      )
  }
  
  return(map)
}


# Plot a heat map in leaflet-----------------------------------
plot_map <- function(geo_level,
                     name, 
                     district = NULL,
                     data,
                     merge_name_x, #name in map file that defines geographies
                     merge_name_y, #name in data file that defines geographies
                     vbipoc_levels, #cutoffs for good/bad levels of BIPOC proportion vacc
                     tpr_levels, # cutoffs for good/bad levels of tpr
                     case_levels, # cutoffs for good/bad levels of cases per million
                     test_levels, # cutoffs for good/bad levels of tests per million
                     vpeople_levels, #cutoffs for low/mid/high levels of proportion pop vaccinated
                     vbipoc_var, #name in dataset that defines BIPOC proportion vacc
                     case_var, # name in dataset that defines cases
                     test_var, # name in dataset that defines tests 
                     tpr_var, # name in dataset that defines tpr
                     vpeople_var, # name in dataset that defines cumulative people vaccinated
                     format = "comma", # how should labels be formatted
                     demo_site_state = NULL #name to determine marker placement for US map
) {
  
  # Testing maps
  map <- get_map(geo_level = geo_level, 
                 name = name,
                 district = district)

  if (nrow(map) == 0) {return (NULL)}
  
  map <- left_join(map  %>% mutate(lhs := !!sym(merge_name_x)),
                   data %>% mutate(rhs := !!sym(merge_name_y), display = TRUE),
                   by = c("lhs" = "rhs"))
  
  map <- 
    map %>%
    rename(geo_name    = !!ensym(merge_name_x), # consistent name variable
           vbipoc_var  = !!ensym(vbipoc_var), 
           case_var    = !!ensym(case_var),
           test_var    = !!ensym(test_var),
           tpr_var     = !!ensym(tpr_var),
           vpeople_var = !!ensym(vpeople_var)) %>% 
    mutate(
      vbipoc_factor = case_when(
        is.na(display) ~ -1, 
        is.na(vbipoc_var) & display ~ NA_real_,
        TRUE ~ 1 + (vbipoc_var >= vbipoc_levels[1]) + (vbipoc_var>= vbipoc_levels[2]) + (vbipoc_var > vbipoc_levels[3])
      ), 
      new_cases_factor = case_when(
        is.na(display) ~ -1, 
        is.na(case_var) & display ~ NA_real_,
        TRUE ~ 1 + (case_var >= case_levels[1]) + (case_var >= case_levels[2]) + (case_var > case_levels[3])
      ), 
      new_tests_factor = case_when(
        is.na(display) ~ -1, 
        is.na(test_var) & display ~ NA_real_,
        TRUE ~ 1 + (test_var >= test_levels[1]) + (test_var >= test_levels[2]) + (test_var > test_levels[3])
      ), 
      tpr_factor = case_when(
        is.na(display) ~ -1, 
        is.na(tpr_var) & display ~ NA_real_,
        TRUE ~ 1 + (tpr_var >= tpr_levels[1]) + (tpr_var >= tpr_levels[2]) + (tpr_var > tpr_levels[3])
      ),
      vpeople_factor = case_when(
        is.na(display) ~ -1, 
        is.na(vpeople_var) & display ~ NA_real_,
        TRUE ~ 1 + (vpeople_var >= vpeople_levels[1]) + (vpeople_var >= vpeople_levels[2]) + (vpeople_var > vpeople_levels[3])
      ),
      # add numbers to end of country ids to avoid issue with LayerIds in leaflet map
      geo_name_vbipoc  = geo_name,
      geo_name_cases   = paste0(geo_name, 2), 
      geo_name_tests   = paste0(geo_name, 3), 
      geo_name_tpr     = paste0(geo_name, 4),
      geo_name_vpeople = paste0(geo_name, 5)
    )
  
  pal_vbipoc <- leaflet::colorFactor(palette = c(navy, yellow, orange, red, "#FFFFFF"), levels = c(-1,1,2,3,4), ordered = TRUE, reverse = TRUE, na.color = light_grey)
  pal_cases <- leaflet::colorFactor(palette = c("#FFFFFF", navy, yellow, orange, red), levels = c(-1,1,2,3,4), ordered = TRUE, na.color = light_grey)
  # note: reverse = true for tests, where lower is WORSE
  pal_tests <- leaflet::colorFactor(palette = c(navy, yellow, orange, red, "#FFFFFF"), levels = c(-1,1,2,3,4), ordered = TRUE, reverse = TRUE, na.color = light_grey)
  pal_tpr <- leaflet::colorFactor(palette = c("#FFFFFF",navy, yellow, orange, red), levels = c(-1,1,2,3,4), ordered = TRUE, na.color = light_grey)
  pal_vpeople <- leaflet::colorFactor(palette = c(navy, yellow, orange, red, "#FFFFFF"), levels = c(-1,1,2,3,4), ordered = TRUE, reverse = TRUE, na.color = light_grey)
  
  highlights <- leaflet::highlightOptions(
    weight = 3,
    color = "#000000", 
    dashArray = "", 
    bringToFront = TRUE)
  
  label_opts <- leaflet::labelOptions(
    style = list("font-weight" = "normal", 
                 padding = "3px 8px", 
                 "font-size" = "12px", 
                 "font-family" = "Montserrat"),
    direction = "auto")
  
  label_opts_marker <- leaflet::labelOptions(
    style = list("font-weight" = "bold", 
                 padding = "3px 8px", 
                 "font-size" = "12px", 
                 "font-family" = "Montserrat"),
    direction = "auto")
  
  #list states that should have demo-site markers when individually selected
  demo_site_states <- unique(map$geo_name[!is.na(map$site)])
  
  # set format on hover labels
  map_label_format_comma <- function(var) { 
    sprintf("<b>%s</b><br/>%s </sup>", 
            map$geo_name, 
            scales::comma(var, accuracy = .01)) %>% 
      lapply(htmltools::HTML)
  }
  map_label_format_percent <- function(var) { 
    sprintf("<b>%s</b><br/>%s </sup>", 
            map$geo_name, 
            scales::percent(var, accuracy = .01)) %>% 
      lapply(htmltools::HTML)
  }
  
  labels_vbipoc  <- map_label_format_percent(map$vbipoc_var)
  labels_cases   <- map_label_format_comma(map$case_var)
  labels_tests   <- map_label_format_comma(map$test_var)
  labels_tpr     <- map_label_format_percent(map$tpr_var)
  labels_vpeople <- map_label_format_percent(map$vpeople_var)
  
  l <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
    addProviderTiles("CartoDB.PositronNoLabels", 
                     options = providerTileOptions(opacity = 0.4)) %>%
    addMapPane("New cases per million per day (7-day avg.)", zIndex = 410) %>%
    addMapPane("New Tests conducted per million per day (7-day avg.)", zIndex = 420) %>%
    addMapPane("Test positivity rate (7-day avg.)", zIndex = 430) %>%
    addMapPane("Proportion vaccinated (full population, all ages, at least one dose)", zIndex = 440) %>%
    add_map_rectangle((name == "United States")) %>% # add a rectangle below polygons when processing the US
    addPolygons(data = map, 
                layerId = map$geo_name_cases,
                group = "New cases per million per day (7-day avg.)", 
                color = "#444444",
                weight = 1.5,
                dashArray = "",
                fillOpacity = ~if_else(!is.na(display), 0.9, 0),
                fillColor = ~pal_cases(new_cases_factor), 
                highlight = highlights, 
                labelOptions = label_opts,
                label = labels_cases) %>%
    addPolygons(data = map, 
                layerId = map$geo_name_tests,
                group = "Tests conducted per million per day (7-day avg.)", 
                color = "#444444",
                weight = 1.5,
                dashArray = "",
                fillOpacity = ~if_else(!is.na(display), 0.9, 0),
                fillColor = ~pal_tests(new_tests_factor), 
                highlight = highlights, 
                labelOptions = label_opts,
                label = labels_tests) %>%
    addPolygons(data = map, 
                layerId = map$geo_name_tpr,
                group = "Test positivity rate (7-day avg.)", 
                color = "#444444",
                weight = 1.5,
                dashArray = "",
                fillOpacity = ~if_else(!is.na(display), 0.9, 0),
                fillColor = ~pal_tpr(tpr_factor), 
                highlight = highlights, 
                labelOptions = label_opts,
                label = labels_tpr) %>%
    addPolygons(data = map,
                layerId = map$geo_name_vpeople,
                group = "Proportion vaccinated (full population, all ages, at least one dose)", 
                color = "#444444",
                weight = 1.5,
                dashArray = "",
                fillOpacity = ~if_else(!is.na(display), 0.9, 0),
                fillColor = ~pal_vpeople(vpeople_factor), 
                highlight = highlights, 
                labelOptions = label_opts,
                label = labels_vpeople)
  
  if(name == "United States") {
    l <- l %>%
      addMapPane("Proportion BIPOC vaccinated (ages 5+, at least one dose), estimated", zIndex = 400) %>% 
      addPolygons(data = map,
                  layerId = map$geo_name_vbipoc,
                  group = "Proportion BIPOC vaccinated (ages 5+, at least one dose), estimated", 
                  color = "#444444",
                  weight = 1.5,
                  dashArray = "",
                  fillOpacity =  ~if_else(!is.na(display), 0.9, 0),
                  fillColor = ~pal_vbipoc(vbipoc_factor), 
                  highlight = highlights, 
                  labelOptions = label_opts,
                  label = labels_vbipoc) %>%
      addLayersControl(
        baseGroups  = c("Proportion BIPOC vaccinated (ages 5+, at least one dose), estimated", "Proportion vaccinated (full population, all ages, at least one dose)", "Test positivity rate (7-day avg.)", "Tests conducted per million per day (7-day avg.)", "New cases per million per day (7-day avg.)"),
        options = layersControlOptions(position = "bottomright", collapsed = FALSE),
      )
      
  } else {
    l <- l %>%
      addLayersControl(
        baseGroups  = c("Proportion vaccinated (full population, all ages, at least one dose)", "Test positivity rate (7-day avg.)", "Tests conducted per million per day (7-day avg.)", "New cases per million per day (7-day avg.)"),
        options = layersControlOptions(position = "bottomright", collapsed = FALSE),
      )
  }
  
  if(name == "United States" & geo_level == "sub-national" & demo_site_state == "United States") {
    
   map_sites <- map %>% dplyr::filter(!is.na(lat) & !is.na(lon))  
    
   l <- l %>%
     addAwesomeMarkers(layerId = map_sites$site,
                       map_sites$lon, 
                       map_sites$lat, 
                       icon = awesomeIcons(icon = 'circle',
                                           library = "fa",
                                           markerColor = "lightgray",
                                           iconColor = '#000000'),
                       label = map_sites$site,
                       labelOptions = label_opts_marker,
                       popup = paste0("Click to open the ",map_sites$site," dashboard maintained by ", map_sites$gov,"<br> <a href=",map_sites$link,
                                      " target='_blank' rel='noopener noreferrer'>",map_sites$link,"</a>"))
   
  }
  
  if(name == "United States" & geo_level == "sub-national" & demo_site_state %in% demo_site_states) {
    
    map_site <- map %>% dplyr::filter(!is.na(lat) & !is.na(lon) & geo_name==c(demo_site_state))
    
    l <- l %>%
      addAwesomeMarkers(layerId = map_site$site,
                        map_site$lon, 
                        map_site$lat, 
                        icon = awesomeIcons(icon = 'circle',
                                            library = "fa",
                                            markerColor = 'lightgray',
                                            iconColor = '#000000'),
                        label = map_site$site,
                        labelOptions = label_opts_marker,
                        popup = paste0("Click to open the ",map_site$site," dashboard maintained by ", map_site$gov,"<br> <a href=",map_site$link,
                                       " target='_blank' rel='noopener noreferrer'>",map_site$link,"</a>"))
    
  }

  return(l)
  
}

plot_demo_map <- function(geo_level,
                          name, 
                          demo_site_state,
                          site_name) {
  # Testing maps
  map <- get_map(geo_level = geo_level, 
                 name = name,
                 demo_site_state = demo_site_state)
  
  if (nrow(map) == 0) {return (NULL)}
  
  label_opts <- leaflet::labelOptions(
    style = list("font-weight" = "bold", 
                 padding = "3px 8px", 
                 "font-size" = "12px", 
                 "font-family" = "Montserrat"),
    direction = "auto")
  
  leaflet(data = map, 
          options = leafletOptions(zoomControl = TRUE)) %>%
    addPolygons(color = dark_grey,
                weight = 2) %>%
    addAwesomeMarkers(~lon, 
                      ~lat, 
                      icon = awesomeIcons(icon = 'circle',
                                          library = "fa",
                                          markerColor = 'black',
                                          iconColor = '#FFFFFF'),
                      label = ~site,
                      labelOptions = label_opts) %>%
    fitBounds(lng1 = ~min_lon,
              lng2 = ~max_lon,
              lat1 = ~min_lat,
              lat2 = ~max_lat) %>%
    addProviderTiles("CartoDB.PositronNoLabels",
                     options = providerTileOptions(opacity = 0.2)) 
}


map_geo <- function(lvl, geo, country, district, date, df_selected, thresh_high, thresh2, thresh1, site_name){
  # default value for any_data (used to determine which map to display)
  any_data <- TRUE
  
  # for region maps pull all countries in the region
  if (lvl == "Region") {
    df_last_day <- filter(df_dashboard, 
                          region == geo,
                          subregion_1 != "TOTAL",
                          subregion_2 == "TOTAL") %>%
      select_map_last_day("subregion_1", date)
    
  # for countries with sub-national data, pull the sub-national values 
  } else if (lvl == "Country" & geo %in% subnat_countries) {
    
    df_last_day <- filter(df_dashboard, 
                          subregion_1 == geo,
                          subregion_2 != "TOTAL",
                          subregion_3 == "TOTAL") %>%
      select_map_last_day("subregion_2", date)
    
    #determine whether any of the mapped indicators have non-missing
    #values in the most recent 7 day period
    any_data <- any(
      c(
        any(!is.na(df_last_day$bipoc_pct_vacc_est)),
        any(!is.na(df_last_day$cases_per_mil)),
        any(!is.na(df_last_day$tests_per_mil)),
        any(!is.na(df_last_day$tpr)),
        any(!is.na(df_last_day$prop_vpeople))
      )
    )
    
    #if all mapped values are missing in the recent period, display
    #the country level map instead of sub-national
    if (any_data == FALSE) {
      df_last_day <- select_map_last_day(df_selected, "subregion_2", date) 
    }
    
  } else { # for other maps pull from the selected data
    
    df_dates <- data.frame("date" = seq(from = min(df_selected$date), to = ymd(date), by = "1 day"))
    
    df_selected_datefill <- full_join(df_selected, df_dates, by = "date") %>% 
      # make sure to populate geographic data. use fill since there is only one 
      # geography in df_selected
      tidyr::fill(c(region, subregion_1, subregion_2, subregion_3), .direction = "down")
    
    # any geo level can be used as the argument to select_map_last_day() since there is
    # only one geography in the dataset
    df_last_day <- df_selected_datefill %>% select_map_last_day("subregion_3", date)
  }
  
  if (tolower(lvl) != "sub-national" | (district == "TOTAL" & is.null(site_name))){ # 
    # reset level to sub-national for countries with subnational data
    if (lvl == "Country" & geo %in% subnat_countries & any_data == TRUE) {
      country <- geo
      level <- "Sub-national"
    } else {
      country <- country
      level <- lvl
    }
    
    name_name_list = list( "Region" = geo,
                           "Country" = geo,
                           "Sub-national" = country)
    name_x_list = list( "Region" = "NAME_0",
                        "Country" = "NAME_0",
                        "Sub-national" = "NAME_1")
    name_y_list = list( "Region" = "subregion_1",
                        "Country" = "subregion_1",
                        "Sub-national" = "subregion_2")
    
    plot_map(geo_level = tolower(level),
             name = name_name_list[[level]],
             data = df_last_day,
             merge_name_x = name_x_list[[level]],
             merge_name_y = name_y_list[[level]],
             vbipoc_levels = c(bipoc_vacc_pct_threshold1, bipoc_vacc_pct_threshold2, bipoc_vacc_pct_threshold3),
             case_levels = c(cases_threshold1, cases_threshold2, cases_high_threshold),
             # note the order of the threshold for tpr is reversed, since lower tests are WORSE
             test_levels = c(thresh_high, thresh2, thresh1),
             tpr_levels = c(tpr_threshold1, tpr_threshold2, tpr_high_threshold),
             vpeople_levels = c(vpeople_threshold1, vpeople_threshold2, vpeople_high_threshold),
             vbipoc_var = bipoc_pct_vacc_est,
             case_var = cases_per_mil,
             test_var = tests_per_mil,
             tpr_var = tpr,
             vpeople_var = prop_vpeople,
             demo_site_state = geo)
    
  } else if (!is.null(site_name)) {

    plot_demo_map(geo_level = "demo_site",
                  name = country,
                  demo_site_state = geo)
    
  } else { # TODO: modification to plot indian district data # subnational; district != "TOTAL"
    
    name_x_list = list( "Region" = "NAME_0",
                        "Country" = "NAME_0",
                        "Sub-national" = "NAME_2")
    name_y_list = list( "Region" = "subregion_1",
                        "Country" = "subregion_1",
                        "Sub-national" = "subregion_3")
    
    plot_map(geo_level = "district",
             name = country,
             district = district,
             data = df_last_day,
             merge_name_x = name_x_list[[lvl]],
             merge_name_y = name_y_list[[lvl]],
             vbipoc_levels = c(bipoc_vacc_pct_threshold1, bipoc_vacc_pct_threshold2, bipoc_vacc_pct_threshold3),
             case_levels = c(cases_threshold1, cases_threshold2, cases_high_threshold),
             # note the order of the threshold for tpr is reversed, since lower tests are WORSE
             test_levels = c(thresh_high, thresh2, thresh1),
             tpr_levels = c(tpr_threshold1, tpr_threshold2, tpr_high_threshold),
             vpeople_levels = c(vpeople_threshold1, vpeople_threshold2, vpeople_high_threshold),
             vbipoc_var = bipoc_pct_vacc_est,
             case_var = cases_per_mil,
             test_var = tests_per_mil,
             tpr_var = tpr,
             vpeople_var = prop_vpeople)
    
  }
}
