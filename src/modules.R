# fig titles and info buttons 
infoButton <- function(info, up = FALSE, width = "350px"){
  div(class = "fig-info", 
      dropdownButton(
        HTML(info),
        circle = TRUE,
        up = up, 
        right = TRUE,
        size = "xs",
        width = width,
        icon = icon("info"),
        status = 'warning')
  )
}

figTitle <- function(title, info = NA, up = FALSE, font_size = 16, width = "350px") { 
  #' up (bool): Display the dropdown menu above.
  if (is.na(info)){
    div(class = "fig-title-div",
        uiOutput(title)
        )
  } else {
    div(class = "fig-title-div", 
        span(textOutput(title, inline = T) %>% 
          tagAppendAttributes(style = paste0("font-size:", font_size, "px;")), infoButton(info, up, width)))
  }
}
altFigTitle <- function(title, info = NA, up = FALSE, width = "350px") { 
  if (is.na(info)){
    div(class = "alt-fig-title-div",
        p(title,
          style = sub_chart_title_style)
    )
  } else {
    div(class = "alt-fig-title-div", 
        span(p(title,
          style = sub_chart_title_style),
        infoButton(info, up, width)))
  }
}

sectionHeader <- function(title, info = NA, up = FALSE) { 
  if (is.na(info)){
    div(class = "section-div", h3(title, class = "section-text"))
  } else {
    div(class = "section-div", h3(title, class = "section-text"), infoButton(info, up))
  }
}

kpiUI <- function(title, id){
  ns <- NS(id)
  fluidRow(
    column(1),
    column(3, htmlOutput(title)),
    column(8, id = paste0(title, "_ui"),
           r2d3::d3Output(ns("kpi"), height = "50px", width = "100%"))
  )
}

kpiUI_notitle <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12, id = paste0(id, "_ui"),
           r2d3::d3Output(ns("kpi"), height = "50px", width = "100%"))
  )
}

kpiServer <- function(id, 
                      indicator_value, 
                      format,
                      tick_format, 
                      rev, 
                      min, 
                      max,
                      r = 8
                      ){
  callModule(
    id = id, function(input, output, session){
      output$kpi <- r2d3::renderD3({
        r2d3::r2d3(
          data = data.frame(value = indicator_value),
          css = "www/styles.css", 
          script = "www/js/kpi.js",
          options(r2d3.shadow = FALSE), 
          options = list( 
            format = format,
            tick_format = tick_format, 
            rev = rev, 
            min = min, 
            max = max, 
            name = id, 
            r = r,
            visible = if_else(is.na(indicator_value), "false", "true")
          )
        )
      })
    }
  )
 
}

# vaccine acceptance panel modules ---------------------------------------------
get_vax_acpt_survey_date <- function(df, input_date, today) {
  #' gets the date of displayed vaccine acceptance survey data 
  #' df (tibble): df_top_three_barrier() or df_top_three_hest()
  #' input_date (date): input$date
  #' today (date) : today from global.R

  if (nrow(df) == 1) {
    dt <- df$date
  } else if (nrow(df) == 0) {
    dt <- input_date
  }
  return(display_date(today, dt, prefix = "As of "))
}

# creates top three table with percentages
top_three_survey_resp <- function(df, metric, data_date) {
  #' df (tibble): df_dashboard df filtered based on input level and geography
  #' metric (str): 'barrier' or 'hest'
  #' data_date (str): display_date in which data was pulled
  
  item_names <- paste0(c("first", "second", "third"), "_", metric)
  item_perc_names <- paste0("pct_", item_names)

  column(12,
    fluidRow(column(8),
             column(2, p(data_date) %>% 
                        tagAppendAttributes(
                          class = "p",
                          style= 'font-size:16px;text-align:center;text-decoration:underline;font-weight: 600'
                        ))
    ),
    create_survey_row(get_vax_acpt_str(pull(df, item_names[1])),
                      pull(df, item_perc_names[1]), rank = 1),
    create_survey_row(get_vax_acpt_str(pull(df, item_names[2])),
                      pull(df, item_perc_names[2]), rank = 2),
    create_survey_row(get_vax_acpt_str(pull(df, item_names[3])),
                      pull(df, item_perc_names[3]), rank = 3)

  )
}

create_survey_row <- function(label, perc, rank, acc = 1) {
  #' label (str): survey response item
  #' item (numeric): survey response percentage
  #' rank (int): 1, 2, or 3 for rank of response
  #' acc (numeric): accuracy for scales::percent
  fluidRow(
    column(1),
    column(7, paste0(rank, ". ", label)),
    column(2, align = "center", scales::percent(perc, accuracy = acc))
  )
}

# plot trends over time next to current values and percent changes
trend_row <- function(title, id, val, pct){
  fluidRow(
    column(1),
    column(3,
           uiOutput(title)
    ), 
    column(2, 
           textOutput(val) %>%
             tagAppendAttributes(
               class = "p", 
               style= 'font-size:16px;text-align:center;'
             )
    ),
    column(6, plotlyOutput(id, height = "120px", width = "auto")
    )

  )
}

# create column names for figures that show trends over time
trend_header <- function(recent_date, duration) {
  fluidRow(
    column(4),
    column(2, 
          textOutput(recent_date) %>%
            tagAppendAttributes(
              class = "p", 
              style= 'font-size:16px;text-align:center;text-decoration:underline;font-weight: 600;'
              )
    ),
    column(6, 
           textOutput(duration) %>%
             tagAppendAttributes(
               class = "p", 
               style= 'font-size:16px;text-align:center;text-decoration:underline;font-weight: 600;'
             )
    )
    
  )
}

policy_row <- function(title, val) {
  conditionalPanel(
    condition = "input.level != 'Region'",
    fluidRow(
      column(6, p(title, style="text-align:right; font-size:16px;")),
      column(1),
      column(2, htmlOutput(val, stle = "text-align:center;"))
    )
  )

  
}
policy_row_date <- function(title, val, date_val) {
  conditionalPanel(
    condition = "input.level != 'Region'",
    fluidRow(
      column(6, p(title, style="text-align:right; font-size:16px;")),
      column(1),
      column(2, htmlOutput(val, stle = "text-align:center;")),
      column(2, textOutput(date_val) %>% 
               tagAppendAttributes(
                 class = "p",
                 style= 'font-size:16px;text-align:center;font-weight: 600')
      )
    )
  )
  
  
}

# # testing only
# df_filt_by_date <- filter_dashboard_dates(df_dashboard, "2020-12-01", "Since pandemic began")
# df_selected <- df_filt_by_date %>% filter(subregion_1 == "Brazil", subregion_2 == 'TOTAL')
# df_selected <- df_filt_by_date %>% filter(region == "Africa", subregion_1 == 'TOTAL')

get_line_bool <- function(df_valid_dates, level, geography){
  # how many unique values are there?
  # if there are more than 1 for any metric OR level = region, then make a line graph
  # else make a bar graph
  has_data <- nrow(df_valid_dates) > 1
  line_bool <-  (has_data & ((level != "Region") | geography %in% c("India","United States")))
  
  return(line_bool)
}

behServer <- function(df_valid_dates, line_bool, start_date, end_date, var_map, var_map_wrap, duration, color_list = c(red, navy, yellow, light_grey), geo_level, geo, y_lab = "Percentage of respondents", wrap=FALSE, legend_pos = NULL, target = NULL){
  #' creates a line chart if there is time series data for the given geography
  #' or a bar chart if there is not, using data from the most recent valid data
  #' 
  #' df_selected = full dataaset, because we need recent_behav_date
  #' df_valid_dates = all valid rows of data from df_selected (have at least 1 non-null of the metrics in question)
  #' line_bool = boolean determining whether or not to display a time series plot
  #' start_date = of time period selected
  #' end_date = of time period selected
  #' var_map = a named list of variable labels and variable names in the df to plot
  
  if (line_bool) {
    
    if(wrap){
      create_beh_time_series(df_valid_dates, start_date, end_date, var_map_wrap, duration, color_list, geo_level, geo, y_lab = y_lab, legend_pos = legend_pos, target = target)
    } else {
      create_beh_time_series(df_valid_dates, start_date, end_date, var_map, duration, color_list, geo_level, geo, y_lab = y_lab, legend_pos = legend_pos, target = target)
    }
    
  } else {
    
    df_last_day <- get_last_day_data(df_valid_dates)
    # this shows all behavioral metrics because in our data, they are either all missing or all valid
    create_100p_bar_plot(df_last_day, var_map_wrap, geo_level, geo, y_lab = y_lab, legend_pos = legend_pos, target = target) 
  }
}

propVaccServer <- function(df_selected, prop_metric, geo, change_over, 
                          threshold = vpeople_display_threshold, show_threshold = TRUE, 
                          trim_target = FALSE, colors = NULL,
                          booster_fully_vac_threshold = NULL){
  #' df_selected (tibble): df_dashboard df filtered based on input level and geography
  #' prop_metric (str or list): str if one metric; list of strings of metrics, in order of highest to lowest prop, to plot
  #' geo (str): input geography
  #' change_over (str): input change_over
  #' threshold (numeric): proportion for threshold (visualized as horizontal dashed red line) 
  #'   default to vpeople_display_threshold
  #' show_threshold (bool): whether or not display threshold line
  #'   default to TRUE
  #' trim_target (bool): ????
  #' colors (list): colors for proportion metrics if there is more than 1 metric
  #' booster_fully_vac_threshold (numeric): threshold to show booster proportions 
  #'                                        depending on full vaccinated proportions
  req(df_selected)
  
  # allow for multiple `prop_metrics` so can draw multiple lines on one line graph
  suff_data_check <- c()
  for (metric in prop_metric) {
    chk <- nrow(df_selected %>% filter(!is.na(!!ensym(metric)))) > 0 
    suff_data_check <- append(suff_data_check, chk)
  }
  
  # need any `prop_metrics` to have at least 1 row of data
  shiny::validate(
    need(any(suff_data_check) , 
         paste("Insufficient vaccination data for", geo, "in this time period."))
  )
  
  # keep list of which proportions did not have sufficient data
  insuff_prop <- prop_metric[!suff_data_check]
  
  # fix y axes. Lower limit is always 0. Upper limit is between .3 and 1
  # adjusts depending on max value of data and threshold
  upper_limit <- (df_selected %>% summarize_at(all_of(prop_metric), max, na.rm = TRUE)) *1.05
  upper_limit <- min(max(upper_limit, .3, threshold), 1)
  limits <- c(0, upper_limit)

  if ("prop_vpeople_boost" %in% prop_metric) {
    if (is.null(booster_fully_vac_threshold)) {
      # some error handling if forget to put in argument
      booster_fully_vac_threshold <- 0.15
    }
    df_selected <- df_selected %>%
      mutate(prop_vpeople_boost = if_else(prop_vpeople_full < booster_fully_vac_threshold, 
             NA_real_, prop_vpeople_boost)) 
  }

  if (length(prop_metric) > 1 & is.null(colors)) {
    print("making colors function")
    color_f <- colorRampPalette(c("black", light_grey))
    colors <- color_f(length(prop_metric))
  }
  
  line_graph(df_selected,
             prop_metric,
             threshold,
             duration = change_over,
             label = "Percent",
             acc = .1,
             label_opt = "percent",
             tooltip_acc = .1,
             show_threshold = show_threshold,
             trim_target = trim_target,
             limits = limits,
             colors = colors
  )
}

pevxFigure <- function(df_selected, start_date, geo, date, change_over){
  req(df_selected, start_date)
  shiny::validate(
    need(nrow(df_selected %>% filter(!is.na(bipoc_pct_vacc_est) | !is.na(bipoc_pct_vacc_rpt))) > 0, 
         paste("Insufficient vaccination data for", geo, "in this time period."))
  )
  
  
  create_multi_time_series(df_selected, start_date, date, pevx_map, change_over, c(light_green, orange, "black"), 
                           percent = TRUE, y_lims = c(0, 0.7), y_lab = "Proportion vaccinated", 
                           target = vpeople_display_threshold,
                           legend_pos = list(orientation = "h", y = -0.1),
                           legend_order = names(pevx_map))  
}

pevxFigure_s <- function(df_selected, start_date, geo, date, change_over){
  req(df_selected, start_date)
  shiny::validate(
    need(nrow(df_selected %>% filter(!is.na(bipoc_pct_vacc_est) | !is.na(bipoc_pct_vacc_rpt))) > 0, 
         paste("Insufficient vaccination data for", geo, "in this time period."))
  )
  
  
  create_multi_time_series(df_selected, start_date, date, pevx_map_s, change_over, c(light_green, orange), 
                           percent = TRUE, y_lims = c(0, 0.7), y_lab = "Proportion vaccinated", 
                           target = vpeople_display_threshold,
                           legend_pos = list(orientation = "h", y = -0.2),
                           legend_order = names(pevx_map_s))  
}

revxFigure <- function(df_selected, geo, start_date, date, change_over){
  req(df_selected, start_date)
  shiny::validate(
    need(nrow(df_selected %>% filter(!is.na(white_pct_vacc_est) | !is.na(asian_pct_vacc_est) |
                                         !is.na(black_pct_vacc_est) | !is.na(hispanic_pct_vacc_est) |
                                         !is.na(ai_an_pct_vacc_est) | !is.na(nh_pi_pct_vacc_est))) > 0, 
         paste("Insufficient vaccination data for", geo, "in this time period."))
  )
  
  # exclude variables with all missing values from the plot
  color_list <- c("white_pct_vacc_est" = orange, 
                  "asian_pct_vacc_est" = light_grey, 
                  "black_pct_vacc_est" = light_green, 
                  "hispanic_pct_vacc_est" = electric_teal, 
                  "ai_an_pct_vacc_est" = gold,
                  "nh_pi_pct_vacc_est" = navy)
  new_revx_map <- revx_map
  varlist <- c("white_pct_vacc_est", "asian_pct_vacc_est", "black_pct_vacc_est", 
               "hispanic_pct_vacc_est", "ai_an_pct_vacc_est", "nh_pi_pct_vacc_est")
  for (var in varlist) {
    if (nrow(df_selected %>% filter(!is.na(!!ensym(var)))) == 0 ) {
      new_revx_map <- new_revx_map[new_revx_map != var]
      color_list <- color_list[names(color_list) != var]
    }
  }
  
  color_list <- unname(color_list)
  legend_order <- names(new_revx_map)
  create_multi_time_series(df_selected, start_date, date, new_revx_map, change_over, 
                           color_list, 
                           percent = TRUE, y_lims = c(0, 0.7), y_lab = "Proportion vaccinated", 
                           target = vpeople_display_threshold,
                           legend_pos = list(orientation = "h", y = -0.1),
                           legend_order = legend_order
  )  
  
}

validate_mod <- function(name, condition) {
    if (condition) {
      shinyjs::show(paste0(name, "_figure_parent"))
      shinyjs::hide(paste0(name, "_err_parent"))
    } else {
      shinyjs::hide(paste0(name, "_figure_parent"))
      shinyjs::show(paste0(name, "_err_parent"))
    }

}

validate_modOutput <- function(name, fig_height, err_height, width = "100%") {
  fluidRow(
    tags$div(
      id = paste0(name, "_figure_parent"),
      style = paste0("height: ", fig_height, "px;"),
      plotlyOutput(paste0(name, "_figure"), height = paste0(fig_height, "px"), width = width)),
    tags$div(
      id = paste0(name, "_err_parent"),
      class = "shiny-output-error-validation",
      style = paste0("height: ", err_height, "px;"),
      textOutput(paste0(name, "_err")))
  )
}