shinyUI(dashboardPage(

  title = "Rockefeller",
  dashboardHeader(
    title = uiOutput("dash_title"),
    tags$li(a(href = "https://www.rockefellerfoundation.org/", 
              img(src="RF white logo for dark backgrounds crop.png", 
                  title = "rockefellerfoundation.org", 
                  height = "50px"),
              style = "padding-top:10px; padding-bottom:10px;"), 
            class = "dropdown"),
    tags$li(a(href = "https://mathematica.org", 
              img(src="MathematicaLogo_White.png", 
                  title = "Mathematica.org", 
                  height = "50px"),
              style = "padding-top:10px; padding-bottom:10px;"), 
            class = "dropdown"),
    titleWidth = "550px"),
  
  dashboardSidebar(
    width = 350,
    useShinyalert(),
    collapsed = TRUE,
    br(), 
    sidebarMenu(id = "sidebar",
                menuItem(p("Dashboard", class = "menu-item", style = "font-family:'Montserrat'"), 
                         tabName = "dashboard", 
                         icon = icon('dashboard', class = "menu-item")),
                 div(conditionalPanel(condition = "input.sidebar == 'dashboard'", style = "padding-left: 15px;", 
                )),
                menuItem(p("Indicator definitions", class = "menu-item", style = "font-family:'Montserrat'"), 
                         tabName = "indicators", 
                         icon = icon('file', class = "menu-item")) 
    )
  ),

  dashboardBody(
    tags$head(includeCSS("www/styles.css")),
    tags$head(includeHTML(("google-analytics.html"))),
    tags$style(".fa-check {color:#4d4d4d}"),
    tags$style(".fa-minus {color:#4d4d4d}"),
    shinyjs::useShinyjs(),
    tabItems(
      tabItem(tabName = "dashboard", 
              
              fluidRow(
                p("About", class = "subheading", style = "margin:10px 0 2px 5px;"), 
                hr(),
                div(tags$p(about_text_1, class = "about-text"), style = "word-break:break-word; font-size: 16px;"),
                div(p("Options", class = "subheading", style = "margin:10px 0 2px 5px;")), 
                hr(),
                column(1),
                column(1,conditionalPanel(
                  condition = "input.level == 'Sub-national'",
                  uiOutput("country")
                )),
                column(2,
                 pickerInput(
                   inputId = "level",
                   p("Dashboard level", class = "input-label"),
                   selected = "Country",
                   choices = DASH_LEVELS,
                   options = list(`live-search` = TRUE))),
                column(2,uiOutput("geography"),style = "z-index:900;"),
                column(2,
                       sliderTextInput(
                         "date",
                         p("Date", class = "input-label"),
                         choices = DATE_RANGE,
                         selected = default_date,
                         grid = FALSE,
                         # hide_min_max = TRUE,
                         width = "95%"
                       )),
                 column(2,uiOutput("change_over")),
                 column(1),
                 column(1,conditionalPanel(
                    condition = "input.country == 'India' & input.level == 'Sub-national'",
                    uiOutput("district")
                  )),
                style = "background-color:#E0D4B5;"),
              fluidRow(
                # Line graphs
                conditionalPanel(
                  condition = "input.level != 'Demonstration site'",
                  column(6,
                         uiOutput("info_panel"),
                         h4(""),
                         uiOutput("date_panel"),
                         leafletOutput("map", height = 750),
                         style = "z-index:500;"),
                  column(1),
                  column(5,
                       conditionalPanel(
                        condition = "input.geography == 'United States' | (input.level == 'Sub-national' & input.country == 'United States')",
                        figTitle(title = "bipoc_pct_vacc_esttitle", info = bipoc_vacc_state_text),
                        plotlyOutput(outputId = "bipoc_pct_vacc_est_fig", height = "175px", width = "95%")
                       ),
                       figTitle(title = "vpeopletitle", info = vpeople_text),
                       plotlyOutput(outputId = "vpeople_fig", height = "190px", width = "95%"),
                       figTitle(title ="tprtitle", info = tpr_text), 
                       plotlyOutput(outputId = "tpr_fig", height = "175px", width = "95%"),
                       figTitle(title ="testtitle", info = tests_per_mil_text),
                       plotlyOutput(outputId = "tests_fig", height = "175px", width = "95%"),
                       figTitle(title ="casetitle", info = cases_per_mil_text),
                       plotlyOutput(outputId = "cases_fig", height = "175px", width = "95%"),
                       conditionalPanel(
                         condition = "input.level == 'Region' & input.geography != 'United States'",
                         figTitle(title ="lmictitle", info = lmic_text), 
                         plotlyOutput(outputId = "lmic_fig", height = "175px", width = "95%"))
                  )
                ),
                conditionalPanel(
                  condition = "input.level == 'Demonstration site'",
                  column(3,
                         uiOutput("demo_info_panel"),
                         h4(""),
                         uiOutput("demo_date_panel"),
                         leafletOutput("demo_map", height = 250)),
                  column(9,
                         figTitle(title = "demo_vpeopletitle", info = demo_vpeople_text, font_size = 24),
                         validate_modOutput(name = "prop_vacc_county", fig_height = 225, err_height = 40, width = "80%"),
                         div(style = "height: 15px; background-color:'transparent';"),
                         figTitle(title = "bipoc_vacc_state_title", info = bipoc_vacc_state_text, font_size = 24),
                         validate_modOutput(name = "demo_pevx", fig_height = 225, err_height = 40, width = "80%"),
                         div(style = "height: 15px; background-color:'transparent';"),
                         figTitle(title = "race_state_title", info = race_state_text, font_size = 24),
                         validate_modOutput(name = "demo_revx", fig_height = 225, err_height = 40, width = "80%"),
                         div(style = "height: 15px; background-color:'transparent';"),
                         figTitle(title = "vacc_accept_msa_title", info = vacc_accept_msa_text, font_size = 24),
                         validate_modOutput(name = "vacc_accept_msa", fig_height = 225, err_height = 40, width = "80%")
                       )
                )
              ),
              # Vaccine acceptance panel for countries except US
              conditionalPanel(
                condition = "input.level != 'Demonstration site' & input.level != 'Region' & input.geography != 'United States' & !(input.level == 'Sub-national' & input.country == 'United States')",
                fluidRow(
                  column(12,
                         sectionHeader("Vaccine acceptance"),
                         column(6,
                                figTitle("vaxacctitle", info = prop_accept_vacc_text),
                                plotlyOutput(outputId = "vax_acc_fig",
                                             height = "200px", width = "95%"),
                                br(),
                                figTitle("vaxeligtitle", info = vacc_elig_text, width = "225px"),
                                plotlyOutput(outputId = "vacc_elig_progress_bar",
                                             height = "130px", width = "95%")
                         ),
                         column(6,
                                fluidRow(
                                  column(1, icon("frown", "fa-3x")),
                                  column(11, figTitle("vaxhesttitle", info = vacc_hest_text))
                                ),
                                fluidRow(
                                  uiOutput("top_three_hest")
                                ),
                                br(),
                                br(),
                                fluidRow(
                                  column(1, icon("calendar-check", "fa-3x")),
                                  column(11, figTitle("strucbartitle", info = struc_barr_text))
                                ),
                                fluidRow(
                                  uiOutput("top_three_barrier")
                                ),
                                br()
                         )
                  )
                  
                )
              ),
              # Vaccine acceptance for regions, except US
              conditionalPanel(
                condition = "input.level == 'Region' & input.geography != 'United States'",
                fluidRow(
                  column(12,
                         sectionHeader("Vaccine acceptance"),
                         column(6,
                                figTitle("vaxacctitleregional", info = prop_accept_vacc_text),
                                plotlyOutput(outputId = "vax_acc_fig_regional",
                                             height = "200px", width = "95%")
                         )
                  )
                )
              ),
              
              # Trend graphs
              conditionalPanel(
                condition = "input.level != 'Demonstration site' & input.geography != 'United States' & !(input.level == 'Sub-national' & input.country == 'United States')",
                fluidRow(
                  column(6,
                    sectionHeader("COVID-19 burden of disease", info = burden_of_disease_text),
                    trend_header("recent_date", "display_change"),
                    uiOutput("dpm_row"),
                    uiOutput("cfr_row")
                  ),
                  # Policy indicators
                  column(6,
                         conditionalPanel(
                           sectionHeader("Policies enacted", info = policy_text, up = TRUE),
                           condition = "input.level != 'Region'",
                           uiOutput("policy_matrix")
                         ),
                         
                         # ,
                         # conditionalPanel(
                         #   condition = "input.level == 'Region'",
                         #   kpiUI("Stringency Index", id = "polstr")
                         # )
                  )
              )),
              
              conditionalPanel(
                condition = "input.geography == 'United States' | (input.country == 'United States' & input.level == 'Sub-national')",
                fluidRow(
                  div(class = "section-div-slim", h3("Vaccine Equity", class = "section-text")),
                  column(6, 
                         altFigTitle("Total BIPOC population vaccinated, at least one dose",
                                     info = tevx_text),
                         tags$div(
                           id = "tevx_figure_parent",
                           style = "height: 400px; width: 97.5%",
                           plotlyOutput("tevx_figure")),
                         tags$div(
                           id = "tevx_err_parent",
                           class = "shiny-output-error-validation",
                           style = "height: 80px;",
                           textOutput("tevx_err"))),
                  column(6, 
                         altFigTitle("Proportion BIPOC vaccinated (ages 5+, at least one dose)",
                                     info = bipoc_vacc_state_text),
                         tags$div(
                           id = "pevx_figure_parent",
                           style = "height: 400px; width: 97.5%",
                           plotlyOutput("pevx_figure")),
                         tags$div(
                           id = "pevx_err_parent",
                           class = "shiny-output-error-validation",
                           style = "height: 80px;",
                           textOutput("pevx_err"))),
                  style = "padding-bottom: 25px;"
                ),
                fluidRow(
                  column(6,
                         altFigTitle("BIPOC share of vaccinations to date, at least one dose",
                                     info = evx_text),
                         tags$div(
                           id = "disp_figure_parent",
                           style = "height: 400px; width: 97.5%",
                           plotlyOutput("disp_figure")),
                         tags$div(
                           id = "disp_err_parent",
                           class = "shiny-output-error-validation",
                           style = "height: 80px;",
                           textOutput("disp_err"))
                  ),
                  column(6,
                         altFigTitle("Proportion vaccinated by race/ethnicity (ages 5+, at least one dose), estimated",
                                     info = race_state_text),
                         tags$div(
                           id = "revx_figure_parent",
                           style = "height: 400px; width: 97.5%",
                           plotlyOutput("revx_figure")),
                         tags$div(
                           id = "revx_err_parent",
                           class = "shiny-output-error-validation",
                           style = "height: 80px;",
                           textOutput("revx_err"))),
                  style = "padding-bottom: 50px;"
                ),
                fluidRow(
                  column(6,
                         altFigTitle(title = textOutput("pvwkrfiguretitle", inline = T),
                                     info = prop_raceeth_text),
                         tags$div(
                           id = "pvwkr_figure_parent",
                           style = "height: 100px;",
                           div(style = "height: 30px;"),
                           kpiUI_notitle(id = "pvwkr_figure")),
                         tags$div(
                           id = "pvwkr_err_parent",
                           class = "shiny-output-error-validation",
                           style = "height: 80px;",
                           textOutput("pvwkr_err")),
                         div(style = "height: 100px; background-color:'transparent';")
                  ),
                  column(6,
                         conditionalPanel(
                           condition = "input.geography == 'United States'",
                         altFigTitle("Proportion of vaccinations with race/ethnicity information reported to state agencies",
                                     info = prop_raceeth_text),
                          tags$div(
                            id = "pvwkr_figure_parent_kff",
                            style = "height: 100px;",
                            div(style = "height: 30px;"),
                            kpiUI_notitle(id = "pvwkr_figure_kff")),
                          tags$div(
                            id = "pvwkr_err_parent_kff",
                            class = "shiny-output-error-validation",
                            style = "height: 80px;",
                            textOutput("pvwkr_err_kff"))),
                  div(style = "height: 100px; background-color:'transparent';")
                  ),
                )
              ),
              conditionalPanel(
                condition = "input.level != 'Demonstration site' & input.geography != 'United States' & !(input.level == 'Sub-national' & input.country == 'United States')",
                fluidRow(
                  # Annual indicators
                  div(class = "section-div-slim", htmlOutput("kpi_header")),
                  column(6, style = "padding-left:0",
                         conditionalPanel(# hide cvi for subnational pages
                           condition = "input.level != 'Region' & !(input.geography == 'India' & input.level=='Country')",
                           kpiUI("cvititle", id = "cvi"),
                           div(style = "height: 50px;")
                         ),
                         kpiUI("ehsititle", id = "ehsi"),
                         conditionalPanel(# hide dtp3 for subnational pages
                           condition = "input.level != 'Sub-national'",
                           div(style = "height: 50px;"),
                           kpiUI("dtp3title", id = "dtp3")
                         )
                  ),
                  conditionalPanel(
                    condition = "input.geography == 'United States' | (input.country == 'United States' & input.level == 'Sub-national')",
                    column(6, 
                           uiOutput("inptitle"),
                           plotlyOutput("inp_figure", height = "220px"),
                           uiOutput("icutitle"),
                           plotlyOutput("icu_figure", height = "220px")
                           )
                  )
                )
           ),
           
           fluidRow(
             div(style = "height: 15px; background-color:'transparent';"),
             h2("MORE INFORMATION"),
             
             div(HTML(
               paste("<p class = about-text>",
                     about_text_3a,
                     "<a style='color:#6666FF; text-decoration:underline;' href=",
                     mailer,
                     ">",
                     about_text_3b,
                     " </a>", 
                     about_text_3c,
                     version_text,
                     "</p>")), 
               style = "word-break:break-word; font-size: 16px;"),
             
             column(3, 
                      actionButton("switchtab","Indicator definitions",
                                   icon = icon('file'),
                                   class = "shiny-download-link",
                                   style = "background-color:#E0D4B5; border-color: #E0D4B5")),
             column(4,tags$li(class = "menu-item", 
                     downloadLink(
                       "downloadPDF", 
                       tagList(icon('database', class = 'menu-item'), 
                               span(p("Download data source details", 
                                      class = "shiny-download-link", 
                                      style = "font-family:'Montserrat'"))) 
                     ))),
             style = "background-color:#E0D4B5;"
           ),
           
           fluidRow(
             div(style = "height: 15px; background-color:'transparent';"),
             h2("DISCLAIMER AND LIMITATION OF LIABILITY"),
             h4(disclaimer, style = "margin-left: 5px;"),
             style = "background-color:#E0D4B5;"
             )
           ),
          tabItem(tabName = "indicators", 
                  div(style = "background-color:white;padding:10px;border:1px solid #000000;", 
                    h2("Core indicators", style = "font-size:20px; text-align:center;"), 
                    tagList(core_indicators)
                    ), 
                  div(
                    h2("Supporting indicators", style = "font-size:20px; text-align:center;"),
                    tagList(sup_indicators))
                  )
  
  ) # tab items close
) # dashboard body close

) # dashboard page close
) # shinyui close


