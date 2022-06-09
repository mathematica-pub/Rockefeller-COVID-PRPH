# get crosswalk between countries and regions ----------------------------------
get_region_xwalk <- function() {
  df <- read_xlsx(here::here("data", "geography-country-list.xlsx")) %>%
        rename(region = Region, country = Country)
  
}

#################################################################################
#################################################################################
# DATA PROCESSING FOR SERVER SIDE
#################################################################################
#################################################################################

# Select data based on user inputs-----------------------------------
get_start_date <- function(end_date, duration){
  end_date <- ymd(end_date)
  
  start_date <- case_when(
    duration == "Since pandemic began" ~ min(DATE_RANGE),
    duration == "Eighteen months" ~ end_date %m-% months(18),
    duration == "Twelve months" ~ end_date %m-% months(12),
    duration == "Six months" ~ end_date %m-% months(6), 
    duration == "Three months" ~ end_date %m-% months(3), 
    duration == "One month" ~ end_date %m-% months(1), 
    duration == "Two weeks" ~ end_date - 13)
  
  return(start_date)
}


filter_dashboard_dates <- function(df, end_date, duration){
  end_date <- ymd(end_date)
  start_date <- get_start_date(end_date, duration)
  
  df <- filter(df, date <=  !!end_date, date >= !!start_date)
  
  return(df)
}

get_df_selected <- function(input_level, input_geography, input_country, 
                            input_district, df, demo_state = NA) {
  #' input_level (str): input$level 
  #' input_geography (str): input$geography
  #' input_country (str): input$country
  #' input_district (str): input$district
  #' df (tibble): df_filt_by_date() if creating df_selected(); df_dashboard if 
  #' creating subset for historical dataframe for specific indicators
  #' demo_state (tibble): demo_state() if creating df_selected(); otherwise don't use

  if(tolower(input_level) == "region") {
    out_df <- df %>% filter(region == input_geography, subregion_1 == "TOTAL")
  } else if(tolower(input_level) == "country") {
    out_df <- df %>% filter(subregion_1 == input_geography, subregion_2 == "TOTAL")
  } else if((tolower(input_level) == "sub-national") & !is.null(input_district) & !is.null(input_country)) {
    out_df <- df %>% filter(subregion_1 == input_country,
                                        subregion_2 == input_geography, 
                                        subregion_3 == input_district)
  } else if((tolower(input_level) == "sub-national") & (is.null(input_district) | is.null(input_country))) {
    # handling interim cases when district is NULL (when district resets)
    out_df <- df %>% filter(subregion_2 == input_geography, subregion_3 == "TOTAL")
  } else if((tolower(input_level) == "demonstration site") & !is.na(demo_state)){
    out_df <- df %>% filter(subregion_1 == "United States",
                                        subregion_2 == demo_state, subregion_3 == "TOTAL")
  }
  out_df
  }

filter_dashboard_geo <- function(df, 
                                 region = NA_character_,
                                 subregion_1 = "TOTAL", 
                                 subregion_2 = "TOTAL",
                                 subregion_3 = "TOTAL") {
  if (!is.na(region)) {
    df <- filter(df, 
                 region == !!region,
                 subregion_1 == !!subregion_1,
                 subregion_2 == !! subregion_2,
                 subregion_3 == !! subregion_3)
  } else if (subregion_1 != "TOTAL") {
    df <- filter(df, 
                 subregion_1 == !!subregion_1,
                 subregion_2 == !! subregion_2,
                 subregion_3 == !! subregion_3)  
  } else if (subregion_2 != "TOTAL") {
    df <- filter(df, 
                 subregion_2 == !! subregion_2,
                 (subregion_3 == !! subregion_3 | is.na(subregion_3)))
  }
  return(df)
}

get_dashboard_data <- function(x,
                               region = NA_character_,
                               subregion_1 = "TOTAL", # default subregions to TOTAL (region-level)
                               subregion_2 = "TOTAL",
                               subregion_3 = "TOTAL",
                               end_date,
                               duration) {
  
  df <- filter_dashboard_dates(x, end_date, duration)
  df <- filter_dashboard_geo(df, region, subregion_1, subregion_2, subregion_3)
  return(df)
}

# Select data for regional heatmaps-----------------------------------
get_region_country_data <- function(x,
                               region,
                               end_date) {
  
  end_date <- ymd(end_date)

  df <- filter(x, 
               region == !!region,
               subregion_1 != "TOTAL", #don't select total region data
               subregion_2 == "TOTAL", # select only the whole country
               date ==  !!end_date # only select last day
  )
  
  return(df)
  
  
}

# return TRUE if an indicator has available data on the last day of a window, FALSE o/w
# requires dataframe with a single geography (already filtered)
annual_na_check <- function(df, indicator) {
  
  selected <- arrange(df, date) %>%
    slice(n()) # take last row after sorting by date
  
  result <- !is.na(selected[1, indicator])[1,1]
  
  return(result)
}

# return TRUE if an indicator has available data for at least two days of a window, FALSE o/w
# requires dataframe with a single geography (already filtered)
daily_na_check <- function(df, indicator) {
  
  no_na <- drop_na(df, !!indicator) 
  
  return(nrow(no_na) >= 2)
  
}


# return indicator title to display depending one current 
# selected geography
geo_title_create <- function(one_country_regions, level, geography, title_text, region_text, end_text = "") {
  if (level == "Region" & !(geography %in% one_country_regions) ) {
    text <- paste0(title_text, region_text, end_text)
  }else {
    text <- paste0(title_text, end_text)
  }
  return(text)
}

get_last_day_data <- function(df_valid){
  df_last_day <- df_valid %>% 
    arrange(desc(recent_behav_date)) %>% 
    slice(1)
  return(df_last_day)
}


# Select most recent data from last 60 days-----------------------------------
select_map_last_day <- function(df, geo_level, select_date, lookback = 60) {
  #only selects tests_per_mil, cases_per_mil, tpr, and people vaccinated for maps
  #input dataframe must be restricted to one geographic level (e.g.
  #country or region)
  filter(df, 
         date >= ymd(!!select_date) - !!lookback # data must exist within the last week
         ) %>%
    arrange(!!ensym(geo_level), date) %>%
    group_by(!!ensym(geo_level)) %>%
    tidyr::fill(c(bipoc_pct_vacc_est,
                  tests_per_mil,
                  cases_per_mil,
                  tpr,
                  prop_vpeople), .direction = "down") %>% # populate cum_tests with the most recent recorded value if missing
    ungroup() %>%
    filter(date == !!select_date)
}


#################################################################################
#################################################################################
# SUBROUTINES FOR MAPPING
#################################################################################
#################################################################################

get_map <- function(geo_level,
                      name,
                      district = NULL) {
  
  # create a map for the geography of interest
  # Region: create a map of the continent (or countries) with country as the lowest geographic resolution
  # Subregion 1 (country): create a map fo the country with state/province as the lowest geographic resolution
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

  map <- readOGR(dsn = here::here("data", "shapefiles"), layer = paste0(geo_level , "_", name))

  if(geo_level == "district") { 
    state_name <- filter(map@data, NAME_2 == district) %>% pull(NAME_1) 
    map <- subset(map, NAME_1 == state_name)
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
                     tpr_levels, # cutoffs for good/bad levels of tpr
                     case_levels, # cutoffs for good/bad levels of cases per million
                     test_levels, # cutoffs for good/bad levels of tests per million
                     vpeople_levels, #cutoffs for low/mid/high levels of proportion pop vaccinated
                     case_var, # name in dataset that defines cases
                     test_var, # name in dataset that defines tests 
                     tpr_var, # name in dataset that defines tpr
                     vpeople_var, # name in dataset that defines cumulative people vaccinated
                     format = "comma" # how should labels be formatted
                     ) {
  
  # Testing maps
  map <- get_map(geo_level = geo_level, 
                 name = name,
                 district = district)
  if (nrow(map) == 0) {return (NULL)}
  map <- sp::merge(map, data %>% mutate(display = TRUE), 
                   by.x = merge_name_x, by.y = merge_name_y)
  
  map@data <- map@data %>%
    rename(geo_name = !!ensym(merge_name_x), # consistent name variable
           case_var = !!ensym(case_var),
           test_var = !!ensym(test_var),
           tpr_var = !!ensym(tpr_var),
           vpeople_var = !!ensym(vpeople_var)) %>% 
    mutate(
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
      geo_name_cases = geo_name, 
      geo_name_tests = paste0(geo_name, 2), 
      geo_name_tpr = paste0(geo_name, 3),
      geo_name_vpeople = paste0(geo_name, 4)
    )
  
  pal_cases <- leaflet::colorFactor(palette = c("#FFFFFF", navy, yellow, red), levels = c(-1,1,2,3), ordered = TRUE, na.color = light_grey)
  # note: reverse = true for tests, where lower is WORSE
  pal_tests <- leaflet::colorFactor(palette = c(navy, yellow, red, "#FFFFFF"), levels = c(-1,1,2,3), ordered = TRUE, reverse = TRUE, na.color = light_grey)
  pal_tpr <- leaflet::colorFactor(palette = c("#FFFFFF",navy, yellow, red), levels = c(-1,1,2,3), ordered = TRUE, na.color = light_grey)
  pal_vpeople <- leaflet::colorFactor(palette = c(navy, yellow, red, "#FFFFFF"), levels = c(-1,1,2,3), ordered = TRUE, reverse = TRUE, na.color = light_grey)

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
  

  # set format on hover labels
     map_label_format_comma <- function(var) { 
      sprintf("<b>%s</b><br/>%s </sup>", 
              map@data$geo_name, 
              scales::comma(var, accuracy = .01)) %>% 
        lapply(htmltools::HTML)
     }
    map_label_format_percent <- function(var) { 
      sprintf("<b>%s</b><br/>%s </sup>", 
              map@data$geo_name, 
              scales::percent(var, accuracy = .01)) %>% 
        lapply(htmltools::HTML)
    }

  labels_cases <- map_label_format_comma(map$case_var)
  labels_tests <- map_label_format_comma(map$test_var)
  labels_tpr <- map_label_format_percent(map$tpr_var)
  labels_vpeople <- map_label_format_percent(map$vpeople_var)
  
  leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
    addProviderTiles("CartoDB.PositronNoLabels", 
                     options = providerTileOptions(opacity = 0.4)) %>%
    addMapPane("New cases per million per day (7-day avg.)", zIndex = 400) %>%
    addMapPane("Tests conducted per million per day (7-day avg.)", zIndex = 410) %>%
    addMapPane("Test positivity rate (7-day avg.)", zIndex = 420) %>%
    addMapPane("Proportion vaccinated (full population, all ages, at least one dose)", zIndex = 430) %>%
    add_map_rectangle((name == "United States")) %>% # add a rectangle below polygons when processing the US
    addPolygons(data = map, 
                layerId = map@data$geo_name_cases,
                group = "New cases per million per day (7-day avg.)", 
                color = "#444444",
                weight = 2,
                dashArray = "",
                fillOpacity =  ~if_else(!is.na(display), 1, 0),
                fillColor = ~pal_cases(new_cases_factor), 
                highlight = highlights, 
                labelOptions = label_opts,
                label = labels_cases) %>%
    addPolygons(data = map, 
                layerId = map@data$geo_name_tests,
                group = "Tests conducted per million per day (7-day avg.)", 
                color = "#444444",
                weight = 2,
                dashArray = "",
                fillOpacity =  ~if_else(!is.na(display), 1, 0),
                fillColor = ~pal_tests(new_tests_factor), 
                highlight = highlights, 
                labelOptions = label_opts,
                label = labels_tests) %>%
    addPolygons(data = map, 
                layerId = map@data$geo_name_tpr,
                group = "Test positivity rate (7-day avg.)", 
                color = "#444444",
                weight = 2,
                dashArray = "",
                fillOpacity =  ~if_else(!is.na(display), 1, 0),
                fillColor = ~pal_tpr(tpr_factor), 
                highlight = highlights, 
                labelOptions = label_opts,
                label = labels_tpr) %>%
    addPolygons(data = map,
                layerId = map@data$geo_name_vpeople,
                group = "Proportion vaccinated (full population, all ages, at least one dose)", 
                color = "#444444",
                weight = 2,
                dashArray = "",
                fillOpacity =  ~if_else(!is.na(display), 1, 0),
                fillColor = ~pal_vpeople(vpeople_factor), 
                highlight = highlights, 
                labelOptions = label_opts,
                label = labels_vpeople) %>%
    addLayersControl(
      baseGroups  = c("Test positivity rate (7-day avg.)", "Tests conducted per million per day (7-day avg.)", "New cases per million per day (7-day avg.)", "Proportion vaccinated (full population, all ages, at least one dose)"),
      options = layersControlOptions(position = "bottomright", collapsed = FALSE),
    )
    
}

#################################################################################
#################################################################################
# FUNCTIONS TO PLOT DATA
#################################################################################
#################################################################################

# define function for arrow chart -------------------------------------------
trend_only <- function(data, # dataset
                       series, # variable to plot
                       date_range, 
                       trend_length = 3, # number of days to use when calculating the direction of the arrow
                       tooltip_acc = 2, 
                       percent = FALSE
                       ){
  
  data <- mutate(data, series = !!ensym(series)) 

  #create dynamic lower and upper y axis
  low_y_lim <- if_else(min(data$series) == 0 , -1.2*max(data$series), 0.8*min(data$series))
  hi_y_lim <- 1.2*max(data$series)
  
  # determine last date with non-missing data, calculate trend through that date. Plot the figure 
  # on the full axis, regardless of missing data
  data <- filter(data, !is.na(series))
  
  #determine number of days included in trend length (may be more than trend length if weekly)
  trend_start_day <- data$date[nrow(data) - trend_length]
  trend_length_days <- as.integer(max(data$date) - trend_start_day)
  
  #calculate the direction of the trend over the last X days
  values_trend <- (filter(data, date == max(date))$series - filter(data, date == trend_start_day)$series)/trend_length_days
  arrow_x <- max(data$date)
  arrow_y <- filter(data, date == max(date))$series
  
  if (percent) data <- mutate(data, text = paste0(date, "\n", scales::percent(series, accuracy = tooltip_acc)))  
  else data <- mutate(data, text = paste0(date, "\n", round(series, tooltip_acc)))    
 
  g <- ggplot() +
    geom_line(
      data = data, 
      aes(x=date, y=series, text=text, group=1), 
      size=0.8, color="black"
      ) +  
    theme_bw() +
    theme(
      axis.line = element_line(color = "black",size=1),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_blank()
      ) + 
    scale_x_date(limits = c(date_range[1]-1, date_range[2]+5)) + 
    scale_y_continuous(limits=c(low_y_lim, hi_y_lim)) 

  arrow_length <- suppressMessages(lubridate::interval(floor_date(min(data$date), "month"), ceiling_date(max(data$date), "month")) %/% months(1))

  gg <- ggplotly(g, tooltip="text") %>%
    add_annotations(
      text = "", 
      x = as.numeric(arrow_x) + 1*arrow_length,
      y = arrow_y + values_trend,
      xref = "x",
      yref = "y",
      axref = "x",
      ayref = "y",
      showarrow = TRUE,
      arrowhead=1,
      arrowsize=1,
      arrowwidth=3, 
      ax = as.numeric(arrow_x),
      ay = arrow_y, 
      inherit = FALSE, 
      arrowcolor = "#808080"
    ) %>%
    plotly_theme() %>%
    plotly::config(displayModeBar = FALSE)

  
}


# define function for line graph -------------------------------------------
line_graph <- function(df_selected,
                       prop_metric,
                       threshold,
                       label,
                       duration,
                       acc = 1,
                       label_opt = "numeric",
                       limits = NULL, 
                       tooltip_acc = 2,
                       show_threshold = TRUE,
                       trim_target = FALSE,
                       target_label_down = TRUE,
                       colors = NULL
                       ){
  
  #' df_selected (tibble): time series to plot
  #' prop_metric (str or list): values to plot
  #' threshold to identify on the y axis
  #' label for y axis
  #' label options (numeric or percent)
  #' limits for y axis
  #' colors (list): colors for proportion metrics if there is more than 1 metric
  
  if (length(prop_metric) == 1) {
    data <- mutate(df_selected, series = !!ensym(prop_metric), metric = prop_metric) %>% 
    arrange(date) %>% 
      dplyr::select(date, series, metric) 
  } else if (length(prop_metric) > 1) {
    data <- df_selected %>%
      dplyr::select(date, all_of(prop_metric)) %>%
      pivot_longer(-date, names_to = "metric", values_to = "series") %>%
      # this means that the prop_metric list needs to come the order of highest prop to lowest prop
      mutate(metric = factor(metric, levels = names(prop_metric)))
  }
  
  # assign y axis limits if none are specified
  if (is.null(limits)) {
    lower_limit <- min(0,threshold, min(data$series, na.rm = TRUE)) * .95
    upper_limit <- max(threshold, max(data$series, na.rm = TRUE)) * 1.05
    limits <- c(lower_limit, upper_limit)
  }
  
  if (label_opt == "percent"){
    labels <- scales::percent_format(accuracy = acc)
    data <- mutate(data, text = paste0(date, "\n", scales::percent(series, tooltip_acc)))
  } else {
    labels <- scales::comma_format(accuracy = acc)
    data <- mutate(data, text = paste0(date, "\n", round(series, tooltip_acc)))
  }
  
  # set date breaks depending on duration of figure
  axis_info <- format_time_axis_labels(data$date[1], data$date[length(data$date)], duration)
  axis_breaks <- axis_info$breaks
  axis_labels <- axis_info$labels
  
  # create plot
  # if there is only 1 date of data, draw a point for the date, or else point wont 
  # show up with geom_line()
  n_dates_nonmissing <- data %>% filter(!is.na(series)) %>% pull(date) %>% n_distinct()
  if (n_dates_nonmissing == 1) {
    p <- ggplot(data) +
      geom_point(aes(x=date, y=series, text = text, group = metric, color = metric), size = 1.2)
  } else if (n_dates_nonmissing > 1) {
  p <- ggplot(data) +
    geom_line(aes(x=date, y=series, text = text, group = metric, color = metric), size = 1.2) 
  }
  p <- p + 
    xlab("") + 
    theme_bw(base_family ="Montserrat") +
    theme(# adjust the margins so y axis labels display
      plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm"),
      plot.title = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.ticks = element_line(size = .5),
      axis.ticks.length = unit(.2, "cm"),
      axis.line = element_line(color = "black",size=1)
    ) +
    scale_x_date(limits = c(data$date[1], data$date[length(data$date) + 10]), # extend limit of figure so date will be visible
                 breaks = axis_breaks,
                 labels = axis_labels) +
    scale_y_continuous(name=label, limits=limits, labels = labels)+  
    coord_cartesian(clip = "off")
  
  if (show_threshold == TRUE & trim_target == FALSE) {
    p <- p +  
      geom_hline(yintercept=threshold, linetype="dashed", color = "red", size = 0.3)
      
  }
  
  if (length(prop_metric) > 1) {
    p <- p +
      scale_color_manual(values = colors) 
  } else {
    p <- p +
      scale_color_manual(values = "black") +
      theme(legend.position = "none")
  }
  
  if (show_threshold == TRUE & trim_target == TRUE) {
    p <- p +  
      geom_segment(aes(x=(max(date)-30),xend=max(date),y=threshold, yend=threshold), linetype="dashed", color = "red", size = 0.3)
  }
  
  gg <- ggplotly(p, tooltip="text") %>% 
    plotly_theme() %>%
    plotly::config(displayModeBar = FALSE) %>%
    layout(legend = list(y = -.2, orientation = 'h', bgcolor = 'rgba(0,0,0,0)'))
  
  if (show_threshold) {
    m <- threshold/(max(threshold, max(data$series, na.rm = TRUE)) * 1.05)
    
    if (target_label_down == TRUE){
      gg <- gg %>% 
        layout(annotations = 
                 list(x = 1, y = m, text = "Target", 
                      showarrow = F, xref='paper', yref='paper',
                      yshift=-8,
                      font=list(size=10, color="red",family="Montserrat")))
      
    }
    
    if (target_label_down == FALSE){
      gg <- gg %>% 
        layout(annotations = 
                 list(x = 1, y = m, text = "Target", 
                      showarrow = F, xref='paper', yref='paper',
                      yshift=8,
                      font=list(size=10, color="red",family="Montserrat"))) 
      
    }
  }
  
  return(gg)
}



# determine the threshold for the testing indicator-------------------------------
test_threshold <- function(data,
                           test_threshold_case_multiplier,
                           test_threshold_minimum,
                           select_date,
                           case_var) {
  
  # calculate testing threshold which should be a multiple of the number
  # of active cases
  
  value <- filter(data, date == !!select_date) %>%
    pull(!!ensym(case_var))
  
  if (is.na(value)) {
    # return the minimum threshold value
    result <- test_threshold_minimum
  }
  else {
    # return the larger of the minimum threshold value or the dynamically 
    # calculated threshold
    result <- max(value*test_threshold_case_multiplier, test_threshold_minimum)
  }
  
  return(result)
  
}

# return the most recent non-missing value from a series -------------------------
get_nmiss_val <- function(data, series) {
  
  result <- filter(data, !is.na(!!ensym(series))) %>%
    filter(date == max(date)) %>%
    pull(!!ensym(series))
  
  return(result)
  
}

# return the date of recent non-missing value from a series -------------------------
get_nmiss_date <- function(data, series, today) {
  
  result <- filter(data, !is.na(!!ensym(series))) %>%
    filter(date == max(date)) %>%
    pull(date)
  
  result <- display_date(today, result)
  
  return(result)
  
}

# calculate the trend from the beginning of a series through the most recent non-missing value -

get_nmiss_trend <- function(data, series, trend_length = 3, decimals = 2){
  data <- mutate(data, series = !!ensym(series)) %>%
    arrange(date) # ensure data are in chronological order
  
  #create index variable
  data$rownum <- 1:nrow(data)
  
  # determine last date with non-missing data, calculate trend through that date. Plot the figure 
  # on the full axis, regardless of missing data
  last_nonmiss_index <- filter(data, !is.na(series)) %>%
    filter(date == max(date)) %>%
    pull(rownum)
  
  #calculate the direction of the trend over the last X days
  values_trend <- (data$series[last_nonmiss_index] - data$series[1])/data$series[1]
  
  # print the absolute value of the result preceded by a plus if positive and a minus
  # if negative
  # set NA if the denominator is 0 or missing
  if ((data$series[1] == 0) | is.na(data$series[1])) {
    result <- paste("N/A")
  }
  else if (values_trend <0 ) {
    result <- paste0("- ", 
                     round(100*(abs(values_trend)), digits = decimals), 
                     "%")
  }
  else {
    result <- paste0("+ ", 
                     round(100*(abs(values_trend)), digits = decimals), 
                           "%")
  }
  
  return(result)
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
      mutate(value = ifelse(!is.na(subregion_1) & subregion_1==2, subregion_1, # if policy applies to whole country, use country level
                     ifelse(is.na(subregion_1), subregion_2, # if no data on country level policy, use subnat level
                     ifelse(subregion_1 %in% c(0,1) & !is.na(subregion_2), subregion_2, NA)))) %>% # if no country level policy, or country level policy is not general, use subnat level
      dplyr::select(date, var, value) %>% 
      spread(var, value)
  }
  
  return(policy_df)
  
}

# Print a check when a policy is active and a cross when the policy is inactive ----
get_policy_icon <- function(data, series, date, level, geography){
  # pull latest date of policy variable from dataset
  if(level == "Region") {# get policy does not work with region level
    indicator <- NA
  } else {
    indicator <- get_policy(data, tolower(level), geography) %>%
      filter(date == !!date)
    if (nrow(indicator) == 0) { 
      indicator <- NA_character_ 
    } else {
      indicator <- indicator %>% pull(series)
    }
  }

  if(is.na(indicator)) {
    result <- HTML("<p style='text-align:center;vertical-align:middle;font-size:16px;'> No data </h4>")
  } else if(indicator %in% c("1", "2")) {
    result <- HTML("<div style='text-align:center;font-size:28px;'><i class='fa fa-check fa-sm' style='vertical-align: top'></i></div>")
  } else if (indicator == "0") {
    result <- HTML("<div style='text-align:center; font-size:28px;'><i class='fa fa-minus fa-sm' style='vertical-align: top'></i></div>")
  } else {
    result <- HTML("<p style='text-align:center;vertical-align:middle;font-size: 16px;'> No data </h4>")
  }

  return(result)
    
}

display_date <- function(curr_date, disp_date, prefix = "") {
  if (year(disp_date) == year(curr_date)){    
    paste0(prefix, gsub(" 0", " ",paste(format(ymd(disp_date), "%b %d"))))
  }
  else {
    paste0(prefix, gsub(" 0", " ",paste(format(ymd(disp_date), "%b %d, %Y"))))
  }
}

polfilter <- function(df, level, policy_var) {
  policy_var_1 <- paste0(policy_var, "_subregion_1")
  policy_var_2 <- paste0(policy_var, "_subregion_2")
  if (level == "Sub-national") {
    df %>% filter(
      !!ensym(policy_var_1) == 2 | !is.na(!!ensym(policy_var_2))
    )
  }
  else {
    df %>% filter(!is.na(!!ensym(policy_var_1)))
  }
}

# select most recent non-missing policy indicator from df_selected. If
# none is available in the selected window, select the most recent
# non-missing policy indicator from the full df dashboard
get_recent_policy <- function(df_selected, df_dashboard, policy_var, date, level, geography) {

  if (nrow(df_selected %>% polfilter(level, policy_var)) > 0) {
    df_valid_pol <- df_selected %>% 
      polfilter(level, policy_var) %>%
      arrange(subregion_1, subregion_2, subregion_3, date) 
  }
  else {
    # select records from the relevant geography in df_dashboard
    df_geo <- df_selected %>% 
      dplyr::select(region, subregion_1, subregion_2, subregion_3) %>%
      unique() %>%
      left_join(df_dashboard,
                by = c("region", "subregion_1", "subregion_2", "subregion_3")) %>%
      # exclude dates after the selected date window
      filter(date <= !!date) %>%
      # only consider dates within 90 days of the selected date
      filter(date >= lubridate::ymd(!!date) - ddays(90))
    
    if (nrow(df_geo %>% polfilter(level, policy_var)) > 0 ) {
      df_valid_pol <- df_geo %>%
        polfilter(level, policy_var) %>%
        arrange(subregion_1, subregion_2, subregion_3, date) 

    }
    else {
      # keep the existing data and just report "no data" for the current date
      df_valid_pol <- df_geo %>%
        arrange(subregion_1, subregion_2, subregion_3, date) 

    }
  }

  df_valid_pol <- df_valid_pol %>%
    dplyr::select(date, region, starts_with("subregion"), starts_with(policy_var)) 

  df_valid_pol <- df_valid_pol %>% 
    slice(tail(row_number(), 1))
  
  # for sub-region, if date with most recent available data consists of
  # 1 or 0 for subregion_1 (no national policy)
  # NA for subregion_2
  # reset the date to the selected date. Display will show "No Data" as of 'date'
  if (level == "Sub-national") {
    
    policy_var_1 <- paste0(policy_var, "_subregion_1")
    policy_var_2 <- paste0(policy_var, "_subregion_2")
    
    df_valid_pol <- df_valid_pol %>%
      mutate(
        date = 
        case_when(
          !!ensym(policy_var_1) %in% c(0, 1) & is.na(!!ensym(policy_var_2)) ~ lubridate::ymd(!!date),
          TRUE ~ date
        )
      )
  }
  
  return(df_valid_pol)
  
}

# format indicators text -------------------------------------------------------
format_indicator <- function(indicator, core = TRUE) {
  if (core) { target_li <- tags$li(tags$strong("Targets: "), HTML(indicator$target)) }
  def_li <- tags$li(tags$strong("Definition: "), HTML(indicator$definition))
  calc_li <- tags$li(tags$strong("Calculation: "), HTML(indicator$calculation))
  imp_li <- tags$li(tags$strong("Importance: "), HTML(indicator$importance))
  
  if (core){
    lis <- tagList(tags$ul(def_li, calc_li, imp_li, target_li))
  } else {
    lis <- tagList(tags$ul(def_li, calc_li, imp_li))
  }
  
  return(tagList(tags$p(indicator$indicator, class = "subheading"), lis, tags$br()))
}

build_indicator_html <- function(df, core, corelist = NULL) {
  names(df) <- tolower(gsub(" ", "_", names(df)))
  if (is.null(corelist)) {
    indicators_html <- map(seq(1, nrow(df)), ~format_indicator(slice(df, .x), core = core))
  }
  else {
    # show target only for selected indicators
    indicators_html <- map(seq(1, nrow(df)), ~format_indicator(slice(df, .x), core = (slice(df, .x)$indicator %in% corelist)))
  }
  return(map(indicators_html, ~tagList(.)))
}

# create behaviors bar chart----------------------------------------------------
count_na <- function(x) sum(is.na(x)) 

remove_all_na_rows <- function(df, metrics){
  df_valid <- df %>%
    select(date, !!metrics) %>% 
    mutate(num_na = apply(., 1, count_na),
           # can do this because date is never null
           all_beh_na = num_na == length(metrics)) %>% 
    filter(!all_beh_na)
  return(df_valid)
}

title_as_of <- function(df_last_day, title){
  as_of <- df_last_day %>% 
    dplyr::select(recent_behav_date) %>%
    mutate(month = months(recent_behav_date),
           year = format(recent_behav_date, "%Y"))
  paste(title, "as of", as_of$month, as_of$year)
}

get_line_bar_title <- function(line_bool_beh, df_last_day, var_map, geo_level, geo, metro_long = NULL){
  if("vaccine_accept" %in% var_map){
    title <- "Self-reported vaccine acceptance"
  } else {
    title <- "Self-reported preventive behaviors"
  }
  
  if(geo_level == "Demonstration site") {
    title <- paste(title, "in", metro_long, "metro area")
  }
  
  if(line_bool_beh){
    title <- paste(title, "over time")
  } else {
    title <- title_as_of(df_last_day, title)
  }
  
  if ((geo_level == "Sub-national" & geo %in% us_states_list)) {
    title <- paste(title, "(ages 18+)")
  }
  if(geo_level == "Demonstration site") {
    title <- paste(title, "(ages 18+, all races/ethnicities)")
  }

  if (geo_level == "Region" & !(geo %in% c("India", "United States"))) {
    paste(title, "(Average of available country data)")
  } else {
    title
  }
  
}

create_100p_bar_plot <- function(df_last_day, var_map, geo_level, geo, bar_color = dark_grey, y_lab = "Percentage of respondents", legend_pos = NULL, target = NULL){
  #' creates a bar chart from 0-100% on the y-axis
  #' df = a dataframe of one row (i.e. one geography/date)
  #' var_map = a named list of variable labels and variable names in the df to plot
  
  assertthat::assert_that(nrow(df_last_day) == 1) 
  
  # optionally set style for target 
  if (is.null(target)) {
    hline <- NULL
  } else {
    hline <- geom_hline(yintercept=target, linetype="dashed", color = "red", size = 0.3)
  }
  
  plot_df <- df_last_day %>% 
    dplyr::select(all_of(var_map)) %>% 
    pivot_longer(
      cols = everything(), 
      names_to = "var", 
      values_to = "val"
    ) %>% 
    mutate(text = paste0(var, "\n", scales::percent(val, accuracy = .1)))
  
  scale <- length(unique(plot_df$var))
  p <- plot_df %>%
    ggplot() +
    geom_bar(
      aes(x = var, y = val, 
          text = text),
      fill = bar_color, 
      color = bar_color, 
      stat = "identity",
      width = 0.15*scale,
      position = position_dodge()
    ) + 
    hline
  
  style_beh_plots(p, title = NULL, y_lab = y_lab) %>%
    layout(legend = legend_pos)
}

format_axis <- function(axis_breaks, start_date, end_date) {
  if (year(as.Date(start_date)) == year(as.Date(end_date))) {
    format(axis_breaks, "%b-%d")
  } else {
    format(axis_breaks, "%b %d, %Y")
  }
}

create_beh_time_series <- function(df_valid_dates, start_date, end_date, var_map, duration, color_list = c(red, navy, yellow, light_grey), geo_level, geo, y_lab = "Percentage of respondents", legend_pos = NULL, target = NULL){

  
  # optionally set style for target 
  if (is.null(target)) {
    hline <- NULL
  } else {
    hline <- geom_hline(yintercept=target, linetype="dashed", color = "red", size = 0.3)
  }
  
  # set date breaks depending on duration of figure
  axis_info <- format_time_axis_labels(start_date, end_date, duration)
  axis_breaks <- axis_info$breaks
  axis_labels <- axis_info$labels

  plot_df <- df_valid_dates %>% 
    dplyr::select(recent_behav_date, all_of(var_map)) %>% 
    pivot_longer(
      -recent_behav_date, 
      names_to = "var", 
      values_to = "val"
    ) %>% 
    mutate(month = months(recent_behav_date),
           year = format(recent_behav_date, "%Y"),
           text = paste0(var, ": ", scales::percent(val, accuracy = .1), " as of\n",
                         paste(month, year)))
  
  p <- plot_df %>%
    ggplot(aes(x = recent_behav_date, y = val, color = var,
               # need group for text to work
               text = text, group = 1)) +
    geom_line(size = 1) +
    geom_point() +
    scale_color_manual(values = color_list) +
    scale_x_date(limits = c(as.Date(start_date),as.Date(end_date)),
                 breaks = axis_breaks,
                 labels = axis_labels) + 
    hline
  
  style_beh_plots(p, title = NULL, y_lab = y_lab) %>%
    layout(legend = legend_pos)

}

style_beh_plots <- function(plot, title, y_lab = "Percentage of respondents", y_lims = c(0,1),threshold=.82){
  title <- paste0("<span style='font-size:16px;text-align:center;text-decoration:underline;font-weight: 600'>",title,"</span>")
  
  p <- plot +
    labs(y = y_lab, x = "") +
    scale_y_continuous(labels = scales::percent, limits = y_lims) +
    theme_minimal2(base_family = "Zilla Slab") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.line = element_line(color = "black",size=1),
          legend.title = element_blank()) 

  plotly::ggplotly(p, tooltip = "text") %>% 
    layout(annotations = 
             list(x = 1, y = threshold, text = "Target", 
                  showarrow = F, xref='paper', yref='paper', 
                  font=list(size=10, color="red",family="Montserrat"))) %>%
    plotly_theme()
}

millions_label <- function(n) {
  
  labels <- if_else(n < 100000, paste(format(round(n/1000)*1000, big.mark=",", scientific = FALSE)),
                    paste(round(n/1000000, 1), "million"))
    
  return(labels)
}
tooltip_style_count <- function(n) {
  if_else(n < 100000, paste(format(round(n/1000)*1000, big.mark=",", scientific = FALSE)),
          format(round(round(n/1000000, 1)*1000000), big.mark = ",", scientific = FALSE))

}

style_millions_plots <- function(plot, title, y_lab, y_lims, threshold=0.93) {
  p <- plot +
    labs(y = y_lab, x = "") +
    scale_y_continuous(labels = millions_label, limits = y_lims) +
    theme_minimal2(base_family = "Zilla Slab") +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.line = element_line(color = "black",size=1),
          legend.title = element_blank()) 
   
  plotly::ggplotly(p, tooltip = "text") %>% 
    layout(annotations = 
             list(x = 1, y = threshold, text = "Target", 
                  showarrow = F, xref='paper', yref='paper', 
                  font=list(size=10, color="red",family="Montserrat"))) %>%
    plotly_theme() 
}

# create plots for percent inpatient beds and icu beds occupied
# in general: daily plots with potentially multiple series on one graph
create_multi_time_series <- function(df_valid_dates, start_date, end_date, var_map, duration, color_list = c(red, navy, yellow, light_grey), percent = TRUE, tooltip_acc = .1, 
                                     target = NULL, y_lims = c(0,1), y_lab = "Percent occupied", legend_pos = NULL, legend_order = NULL,
                                     showlegend = TRUE){

  # set date breaks depending on duration of figure
  axis_info <- format_time_axis_labels(start_date, end_date, duration)
  axis_breaks <- axis_info$breaks
  axis_labels <- axis_info$labels
  
  # optionally set style for target 
  if (is.null(target)) {
    hline <- NULL
  } else {
    hline <- geom_hline(yintercept=target, linetype="dashed", color = "red", size = 0.3)
  }

  
  plot_df <- df_valid_dates %>% 
    dplyr::select(date, all_of(var_map)) %>% 
    pivot_longer(
      -date, 
      names_to = "var", 
      values_to = "val"
    )
  
  if (is.null(legend_order)) {
  # sort plot_df so that series with the highest values are first in the legend
    plot_df_order <- plot_df %>% 
      dplyr::select(-date) %>% group_by(var) %>% summarise(val = max(val, na.rm = TRUE)) %>%
      arrange(desc(val))
    levels <- as.vector(plot_df_order$var)
    plot_df$var <- factor(plot_df$var, levels = levels)
  }
  else {
    plot_df$var <- factor(plot_df$var, levels = legend_order)
  }
  


  
   
    if (percent) plot_df <- mutate(plot_df, text = paste0(date, "\n", scales::percent(val, accuracy = tooltip_acc)))  
    else plot_df <- mutate(plot_df, text = paste0(date, "\n", tooltip_style_count(val))) 
  
  p <- plot_df %>%
    ggplot(aes(x = date, y = val, color = var,
               # need group for text to work
               text = text, group = 1)) +
    geom_line(size = 1) +
    scale_color_manual(values = color_list) +
    scale_x_date(limits = c(as.Date(start_date),as.Date(end_date)),
                 breaks = axis_breaks,
                 labels = axis_labels) +
    hline 
  
  # determine limit for y axis (max of ylim or highest value in plot_df)
  y_lims[2] <- max(y_lims[2], max(plot_df$val, na.rm = TRUE))
  
  m <- target/(max(target, max(plot_df$val, na.rm = TRUE)) * 1.05)

  if (percent){ 
    p <- style_beh_plots(p, title = NULL, y_lab = y_lab, y_lims = y_lims, threshold = m)
    }
  else {
    p <- style_millions_plots(p, title = NULL, y_lab = y_lab, y_lims = y_lims, threshold = m)
  }
  
  p %>%
    layout(legend = legend_pos, showlegend = showlegend)
}

format_time_axis_labels <- function(start_date, end_date, duration) {
  # set date breaks depending on duration of figure
  if (duration %in% c("Two weeks", "One month")) {
    # short durations don't have intermediate ticks
    axis_breaks <- c(as.Date(start_date), as.Date(end_date))
    axis_labels <- format_axis(axis_breaks, start_date, end_date)
  } else {
    axis_breaks <- seq(from = as.Date(start_date), to = as.Date(end_date), by = "1 month")
    if (axis_breaks[length(axis_breaks)]!= as.Date(end_date)) {
      # ensure the axis breaks go through the last day in the data
      append(axis_breaks, as.Date(end_date))
    }
    # don't display any dates but the first and last on the axis (only tick marks)
    axis_labels <- format_axis(axis_breaks, start_date, end_date)
    if (length(axis_labels)>1) {
      # ensure there are no errors in this step
      axis_labels[2:(length(axis_labels)-1)] <- ""
    }
  }
  
  out_list <- list("labels" = axis_labels, "breaks" = axis_breaks)
  
  return(out_list)
}


get_indicator_info <- function(indicator_df, indicators, core){
  if (any(! tolower(indicators) %in% tolower(indicator_df$Indicator))){
    warning("WARNING: couldn't find some indicators in indicator_df provided.")
  }
  
  ind <- filter(indicator_df, tolower(Indicator) %in% tolower(indicators))
  
  if (nrow(ind) == 0){ # additional safeguard against accidental errors from wrong indicatory spelling
    text <- ""
  } else {
    if (core){
      text <- rowwise(ind) %>% 
        mutate(
          Definition = gsub("<a ", "<a class=info-button-link ", Definition),
          Target = gsub("<a ", "<a class=info-button-link ", Target), 
          # some "core" indicators don't have a target
          text = if_else(is.na(Target), Definition,
                         paste0(Definition, "<br><br><b>Target: </b>", Target))
          )
    } else {
      text <- rowwise(ind) %>% 
        mutate(text = paste0("<b>", Indicator, ": </b>", Definition)) 
    }
    text <- text %>% 
      ungroup() %>%
      summarize(text = str_c(text, collapse = "<br><br>")) %>%
      pull(text)
  }
  return(text)
}

# function to determine which policy variables to use to determine
# the display date
pol_date_select <- function(df, level) {
  df <- df %>% dplyr::select(-c(starts_with("subregion")))
  
  polvar1 <- df %>% dplyr::select(ends_with("subregion_1")) %>% names()
  polvar2 <- df %>% dplyr::select(ends_with("subregion_2")) %>% names()
  
  # at the subnational level, only consider policies where there was 
  # a local restriction or a total national restriction
  if (level == "Sub-national") {
    date <- df %>% mutate(date = case_when(
      !!ensym(polvar1) == 2 | !is.na(!!ensym(polvar2)) ~ date,
      TRUE ~ as.Date(NA)
    )) %>%
      dplyr::select(date) %>%
      pull()
  }
  else {
    date <- df %>% mutate(date = case_when(
      !is.na(!!ensym(polvar1)) ~ date,
      TRUE ~ as.Date(NA)
    )) %>%
      dplyr::select(date) %>%
      pull()
  }
}

#BIPOC disparity graph
bipoc_graph <- function(df_selected,
                        label,
                        duration,
                        acc = 1,
                        label_opt = "percent",
                        limits = NULL, 
                        tooltip_acc = 1
){
  
  #select data
  data <- df_selected %>% 
    arrange(date) %>% 
    dplyr::select(date, bipoc_pct_of_vacc, bipoc_pct_of_pop, bipoc_vacc_disparity) 
  
  #get last population threshold amount
  pop_thresh_last <- data %>%
    filter(!is.na(bipoc_pct_of_pop)) %>%
    filter(date == max(date)) %>%
    pull(bipoc_pct_of_pop)
  
  #get range of dates selected
  date_min <- data %>%
    filter(date == min(date)) %>%
    pull(date)
  
  date_max <- data %>%
    filter(date == max(date)) %>%
    pull(date)
  
  date_range <- date_max - date_min
  
  #set text box position based on dates
  rect_x0 <- date_min + (date_range*.6)
  rect_x1 <- date_max
  
  
  if(date_range < 15) {
    rect_x1 <- date_min + date_range*.9
  } 
  
  #set arrow positions based on dates and data
  disp_x <- filter(data, !is.na(bipoc_pct_of_vacc)) %>%
    filter(date == max(date)) %>%
    pull(date)
  
  disp_y <- filter(data, !is.na(bipoc_pct_of_vacc)) %>%
    filter(date == max(date)) %>%
    pull(bipoc_pct_of_vacc)
  
  
  # assign y axis limits if none are specified
  if (is.null(limits)) {
    upper_limit <- max(pop_thresh_last, max(data$bipoc_pct_of_vacc, na.rm = TRUE), na.rm = TRUE) * 1.05
    lower_limit <- upper_limit - .3
    if(lower_limit < 0) {
      lower_limit <- 0
    }
    limits <- c(lower_limit, upper_limit)
  }
  
  y_range <- upper_limit - lower_limit
  
  
  if (label_opt == "percent"){
    labels <- scales::percent_format(accuracy = acc)
    data <- mutate(data, text = paste0(date, "\n", scales::percent(bipoc_pct_of_vacc, tooltip_acc)))
  } else {
    labels <- scales::comma_format(accuracy = acc)
    data <- mutate(data, text = paste0(date, "\n", round(bipoc_pct_of_vacc, tooltip_acc)))
  }
  
  # set date breaks depending on duration of figure
  axis_info <- format_time_axis_labels(data$date[1], data$date[length(data$date)], duration)
  axis_breaks <- axis_info$breaks
  axis_labels <- axis_info$labels
  
  # create text for text box
  vacc_disparity <- data %>%
    filter(!is.na(bipoc_pct_of_vacc)) %>%
    filter(date == max(date)) %>%
    pull(bipoc_vacc_disparity)*-1
  
  if(vacc_disparity < 0) {
    disp_lab1 <- paste(round(-100*vacc_disparity, 1), "percentage points") 
    disp_lab2 <- "above parity."
  } else {
    disp_lab1 <- paste(round(100*vacc_disparity, 1), "percentage points")
    disp_lab2 <- "below parity."
  }
  
  #create colors for text box
  if(vacc_disparity < .02) {
    disp_color <- navy
  }
  if(vacc_disparity >= .02 & vacc_disparity <= .05) {
    disp_color <- yellow
  }
  if(vacc_disparity > .05) {
    disp_color <- red
  }
  
  #format disparity data for tooltip label
  data <- data %>% 
    mutate(bipoc_vacc_disparity = scales::percent(bipoc_vacc_disparity, .1)) %>%
    rename("Disparity" = "bipoc_vacc_disparity")
  
  #create separate pop threshold lines if bipoc_pct_pop changes in date range needed
  nonm_vacc <- data %>%
    filter(!is.na(bipoc_pct_of_vacc)) %>%
    filter(date == min(date)) %>%
    pull(date)
  
  cutoff_strtdate <- filter(data, !is.na(bipoc_pct_of_pop)) %>%
    filter(date == min(date)) %>%
    pull(date)
  
  hisp_cutoff <- data %>%
    arrange(desc(date)) %>%
    mutate(pop_adjustment = if_else(lead(bipoc_pct_of_pop) - lag(bipoc_pct_of_pop) != 0, 1, 0)) %>%
    filter(pop_adjustment == 1)
  
  if (nrow(hisp_cutoff)>0) {
    hisp_cutoff <-
      hisp_cutoff %>%
      filter(date == max(date)) %>%
      pull(date)
  } else {
    hisp_cutoff <- NA
  }
  
  if(!is.na(hisp_cutoff[1]) & hisp_cutoff[1] > nonm_vacc) {
    
    dash_data <- data %>%
      filter(date > hisp_cutoff + 1,
             !is.na(bipoc_pct_of_vacc)) 
    
  } else {
    
    dash_data <- data %>%
      filter(!is.na(bipoc_pct_of_vacc)) 
    
  }
  
  # create plot
  p <- ggplot(data) +
    geom_line(aes(x = date, y = bipoc_pct_of_vacc, text = text, label = `Disparity`, group = 1, color = "BIPOC share of vaccinations"), size = 1.2)  +
    geom_line(aes(x = date, y = bipoc_pct_of_pop, group = 1, color = "BIPOC share of population (5+)"), size = 0.3, linetype = "dashed", data = dash_data) +
    geom_rect(aes(NULL, NULL, xmin = rect_x0, xmax = rect_x1, group = 1),
              ymin = lower_limit, ymax = lower_limit+(y_range*.2), fill = disp_color, color = "black") +
    geom_text(aes(x = date_min + (date_range*.8), y = lower_limit + (y_range*.13)), label = disp_lab1, color = "white", size = 3.3) +
    geom_text(aes(x = date_min + (date_range*.8), y = lower_limit + (y_range*.07)), label = disp_lab2, color = "white", size = 3.3) +
    scale_color_manual(
      "",
      breaks = c("BIPOC share of vaccinations", "BIPOC share of population (5+)"),
      values = c("BIPOC share of vaccinations" = 'black', "BIPOC share of population (5+)" = 'red')) +
    xlab("") +
    scale_x_date(limits = c(data$date[1], data$date[length(data$date) + 10]), # extend limit of figure so date will be visible
                 breaks = axis_breaks,
                 labels = axis_labels) +
    scale_y_continuous(name = label, limits = limits, labels = labels)+  
    coord_cartesian(clip = "off") +
    theme_minimal2(base_family = "Zilla Slab") +
    theme(panel.background = element_rect(color = "white")) +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.line = element_line(color = "black",size=1),
          legend.title = element_blank()) 
  
  #threshold <- max(data$bipoc_pct_of_pop)
  #threshold <- max(data$bipoc_pct_of_pop)
  
  #m <- threshold/(max(threshold, max(upper_limit, na.rm = TRUE)) * 1.05)
  
  pp <- ggplotly(p, tooltip= c("text", "label")) %>% 
    layout(annotations = 
             list(x = 0.85, y = pop_thresh_last, text = "Target", 
                  showarrow = F, xref='paper', yref='y',
                  yshift=-10,
                  font=list(size=10, color="red",family="Montserrat"))) %>%
    plotly_theme() %>%
    plotly::config(displayModeBar = FALSE)
  
  if (abs(vacc_disparity)/y_range < .1) {
    
    pp <- pp %>%
      add_annotations(
        text = "", 
        x = as.numeric(disp_x),
        y = pop_thresh_last,
        xref = "x",
        yref = "y",
        axref = "x",
        ayref = "y",
        showarrow = TRUE,
        arrowhead = 0,
        arrowsize = 1,
        arrowwidth=2,
        ax = as.numeric(disp_x),
        ay = disp_y, 
        inherit = FALSE, 
        arrowcolor = "red"
      ) 
    
  } else {
    
    pp <- pp %>%  
      add_annotations(
        text = "", 
        x = as.numeric(disp_x),
        y = pop_thresh_last,
        xref = "x",
        yref = "y",
        axref = "x",
        ayref = "y",
        showarrow = TRUE,
        arrowhead = 1,
        arrowsize = 1,
        arrowwidth=2, 
        ax = as.numeric(disp_x),
        ay = disp_y, 
        inherit = FALSE, 
        arrowcolor = "red"
      ) %>%
      add_annotations(
        text = "", 
        x = as.numeric(disp_x),
        y = disp_y,
        xref = "x",
        yref = "y",
        axref = "x",
        ayref = "y",
        showarrow = TRUE,
        arrowhead = 1,
        arrowsize = 1,
        arrowwidth=2, 
        ax = as.numeric(disp_x),
        ay = pop_thresh_last, 
        inherit = FALSE, 
        arrowcolor = "red"
      )
  }
  
  pp$x$data[[3]]$hoverinfo <- "none"
  pp$x$data[[4]]$hoverinfo <- "none"
  pp$x$data[[5]]$hoverinfo <- "none"
  
  return(pp)
}

# top three vaccination hesitancy or structural barriers 
get_top_three_hest_struct <- function(df_selected, df_dashboard, metric, input_date, level, geography,
                                      country, district) {
  #' df_selected (tibble): df_selected()
  #' df_dashboard (tibble): full data to find previous available data
  #' metric (str): "barrier" or "hest"
  #' input_date (date): input$date
  #' level (str): input$level
  #' geography(str): input$geography
  #' country (str): input$country
  #' district (str): input$district

  metric_col <- paste0("first_", metric)

  # if df_selected does not have any data for metric, use df_dashboard to get historic data
  if (nrow(df_selected %>% filter(date == input_date & !is.na(!!ensym(metric_col)))) > 0) {
    df_vax <- df_selected %>% 
      filter(date == !!input_date) 
  }
  else {
    # select records from the relevant geography in df_dashboard
    df_historic <- get_df_selected(input_level = level, input_geography = geography, 
                                  input_country = country, input_district = districy,
                                  df = df_dashboard)
    
    # limit data to specific date range
    df_historic <- df_historic %>%
      # exclude dates after the selected date window
      filter(date <= !!input_date) %>%
      # only consider dates within 120 days of the selected date 
      # (see ticket rocovid-166 for reference)
      filter(date >= lubridate::ymd(!!input_date) - ddays(120))
    
    # if the metric we want is not missing for some rows, take most recent 
    # available row of data
    if (nrow(df_historic %>% filter(!is.na(!!ensym(metric_col)))) > 0 ) {
      df_vax <- df_historic %>%
        filter(!is.na(!!ensym(metric_col))) %>%
        arrange(date) %>%
        # get last row
        slice_tail()
    }
    # if the metric we want is missing for all rows, create empty tibble
    else {
      # make tibble with 0 rows to pass into function to make output
      df_vax <- tibble(date = numeric(), region = character(),
                       first_hest = character(), pct_first_hest = numeric(),
                       first_barrier = character(), pct_first_barrier = numeric())
    }
  }
  
  df_vax <- df_vax %>%
    dplyr::select(date, region, starts_with("subregion"), ends_with(metric)) 
  
  assertthat::assert_that((nrow(df_vax) == 1) | (nrow(df_vax) == 0)) 

  return(df_vax) 
}

get_vax_acpt_str <- function(indicator) {
  #' indicator (str): vaccine structural barrier or hesitancy
  
  return(umd_vax_acpt_xwalk[umd_vax_acpt_xwalk$Indicator == indicator, ]$Description)
}


make_vacc_elig_horiz_bar <- function(act_vacc_elig, vacc_elig_map) {
  #' create vaccine eligibility progress bar
  #' act_vacc_elig (int): number from 1 to 3
  #' vacc_elig_map (list): maps group to integer
  
  bar_width <- 0.3
  outline_width <- 2
  # only show the label for the level of vaccine eligibilty
  bottom_labels <- names(vacc_elig_map)[act_vacc_elig]
  spacing <- c(33, 66, 100)
  label_spacing <- c(spacing[1]/2, spacing[2]/2, spacing[3]/2)
  label_spacing <- label_spacing[act_vacc_elig]
  annotation_color <- c("#000000", "#000000", "#FFFFFF")
  annotation_color <- annotation_color[act_vacc_elig]
  
  vacc_elig_df <- tibble(elig_groups = c('no_one', 'high_risk', 'everyone'),
                        vals = spacing
  ) %>%
    pivot_wider(names_from = elig_groups, values_from = vals) %>%
    mutate(a = "vacc_elig")
  
  fig <- plot_ly(vacc_elig_df, type = 'bar', orientation = 'h') 
  if (act_vacc_elig == 1) {
    fig <- fig %>%
      add_trace(x = ~no_one, y = ~a,
                width = bar_width,
                marker = list(color = red,
                              line = list(color = "#000000",
                                          width = outline_width))) 
  } else if (act_vacc_elig == 2) {
    fig <- fig %>%
      add_trace(x = ~high_risk, y = ~a,
                width = bar_width,
                marker = list(color = yellow,
                              line = list(color = "#000000",
                                          width = outline_width)))
    
  } else if (act_vacc_elig == 3) {
    fig <- fig %>% 
      add_trace(x = ~everyone, y = ~a,
                width = bar_width,
                marker = list(color = navy, 
                              line = list(color = "#000000",
                                          width = outline_width)))
  }
  
  fig %>% plotly_theme() %>%
    layout(barmode = 'stack',
                 paper_bgcolor = bg_color, plot_bgcolor = bg_color,
                 xaxis = list(title = "",
                              range=c(0,100),
                              showgrid = F,
                              showline = F,
                              showticklabels = FALSE,
                              zeroline = FALSE),
                 yaxis = list(title = "",
                              showgrid = FALSE,
                              showline = FALSE,
                              showticklabels = FALSE,
                              zeroline = FALSE),
                 showlegend = F) %>%
    # labeling vax eligibility groups
    add_annotations(xref = 'x', yref = 'paper',
                    x = label_spacing,
                    y = .5,
                    text = bottom_labels,
                    font = list(family = 'Montserrat', size = 12,
                                color = annotation_color), 
                    showarrow = F) %>%
    style(hoverinfo = 'none') %>%
    plotly::config(displayModeBar = FALSE)
}
