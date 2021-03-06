library(magrittr)

ui <- function(request){
dashboardPage(
    dashboardHeader(title = "Corona status"
                    #tags$li(class = "dropdown", bookmarkButton(label = "", icon = icon("bookmark")))
                    ),
    dashboardSidebar(
      sidebarMenu(id = "sidebarMenu",
                  menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
                  menuItem("About", tabName = "tab_about", icon = icon("info")),
                  tags$a("Bookmark current page:"),
                  bookmarkButton(label = "", icon = icon("bookmark"))
      ),
      collapsed = TRUE
    ),
    # Dashboard Body ------------------------------------------------------------
    dashboardBody(
      # js used to detect browser information -----------------------------------
      tags$head(tags$script('
                              var browserInfo = ["", "", "", ""];
                              $(document).on("shiny:connected", function(e) {
                              browserInfo[0] = navigator.appVersion;
                              browserInfo[1] = navigator.platform;
                              browserInfo[2] = navigator.userAgent;
                              browserInfo[3] = navigator.appCodeName;
                              Shiny.onInputChange("browserInfo", browserInfo);
                              });
                              ')),
      tags$head(tags$style(
        type="text/css",
        "#animated_map img {max-width: 95%; max-height: 600px; width: auto; height: auto}"
      )),
      tags$head(tags$style(
        type="text/css",
        "#map_R img {max-width: 95%; max-height: 600px; width: auto; height: auto}"
      )),
      tags$head(tags$style(
        type="text/css",
        "#map_cases_7_day img {max-width: 95%; max-height: 600px; width: auto; height: auto}"
      )),
      tags$head(tags$style(
        type="text/css",
        "#animated_map_cases_7_day img {max-width: 95%; max-height: 600px; width: auto; height: auto}"
      )),
      
      tabItems(
        # Tab: tab_dashboard ----------------------------------------------------
        tabItem(
          tabName = "tab_dashboard",
          
          # First row ----
          fluidRow(
            # Reproduction rate ----
            tabBox(
              title = "Reproduction rate",
              id = "reproduction_rate",
              side = "right",
              tabPanel("Hospitalisations",
                       withSpinner(plotlyOutput("estimated_R", height = "140%"),
                                   type = 4
                       )),
              tabPanel("Cases",
                       withSpinner(plotlyOutput("estimated_R_cases", height = "140%"),
                                   type = 4
                       )),
              tabPanel("By region (cases)",
                       withSpinner(plotlyOutput("estimated_R_region", height = "140%"),
                                   type = 4)),
              width = 6,
              selected = "Cases"
            ),
            # Cases per 100000 last 7 days ----
            tabBox(
              title = "Cases per 100,000 last 7 days",
              id = "cases_rate",
              side = "right",
              tabPanel(list.dirs("data/zip_file", full.names = FALSE)[-1] %>% as.Date() %>% max(),
                       withSpinner(imageOutput("map_cases_7_day", height = "auto"),
                                   type = 4)),
              tabPanel("Animated",
                       withSpinner(
                         imageOutput("animated_map_cases_7_day", height = "auto"),
                         type = 4)),
              selected = "Animated",
              width = 6
            )
          ),
          
          # Second row ----
          fluidRow(
            # Map with reproduction rate ----
            tabBox(
              title = "Reproduction rate map",
              id = "map_box",
              side = "right",
              tabPanel(list.dirs("data/zip_file", full.names = FALSE)[-1] %>% as.Date() %>% max(),
                       withSpinner(
                         imageOutput("map_R", height = "auto"),
                         type = 4)),
              tabPanel("Animated",
                       withSpinner(
                         imageOutput("animated_map", height = "auto"),
                         type = 4)),
              width = 6
            ),
            # World ----
            tabBox(
              title = "World situation",
              id = "world_siuation",
              side = "right",
              # Confirmed cases
              tabPanel("Cases",
                       withSpinner(plotlyOutput("confirmedCases", height = "140%"),
                                   type = 4)
                       
              ),
              # Confirmed deaths
              tabPanel("Deaths",
                       withSpinner(plotlyOutput("confirmedDeaths", height = "140%"),
                                   type = 4)
                       
              ),
              # Deaths corrected for onset
              tabPanel("Controlling for onset",
                       withSpinner(plotlyOutput("onsetDeaths", height = "140%"),
                                   type = 4)
                       
              ),
              tags$hr(),
              # Controls
                materialSwitch(
                  inputId = "addPredictions",
                  label = "Predictions", 
                  status = "primary",
                  right = TRUE
                ),
                materialSwitch(
                  inputId = "addConfidence",
                  label = "Confidence intervals", 
                  status = "primary",
                  right = TRUE
                ),
              sliderInput("days",
                              label = "Days",
                              min = 1,
                              max = 100,
                              value = 10),
              selectizeInput(inputId = "input_countries",
                             label = "Countries:",
                             choices = read_fst("data/plotData.fst", as.data.table = TRUE)$`Country/Region` %>% unique() %>% as.vector(),
                             multiple = TRUE,
                             selected = "Denmark", width = "70%"),
              
              width = 6
            )
            
          ),
          
        # Third row ----
        fluidRow(
          # New daily cases ----
          tabBox(
            title = "New daily cases DK (last 4 months)",
            id = "newcases",
            side = "right",
            # Plot
            tabPanel("National",
                     withSpinner(plotlyOutput("new_cases", height = "140%"),
                                 type = 4)
                     #tags$hr(),
                      # Controls
                     #dateRangeInput(
                    #   "new_cases_date_range",
                    #   label = "Date range:",
                    #   start = lubridate::today()-lubridate::days(30*3),
                    #   end = lubridate::today(),
                    #   min = as.Date("2020-03-02"),
                    #   max = lubridate::today()
                    # ),
            ),
            tabPanel("Municipality",
                     withSpinner(plotlyOutput("new_cases_municipality", height = "140%"),
                                 type = 4),
                     tags$hr(),
                     selectizeInput("selected_municipality",
                                    label = "Municipality",
                                    choices = readRDS("data/municipalities_dk.RDS"),
                                    selected = "Odense")
            ),
                     
            width = 12,
            selected = "National"
          )
        ),
        # Table ----
        fluidRow(
          tabBox(
            title = "Latest incidence by municipality",
            id = "incidence",
            side = "right",
            # Table
            tabPanel("Incidence",
                     withSpinner(DT::dataTableOutput("incidence_table"),
                                 type = 4)
            ),
            
            width = 12,
            selected = "Incidence"
          )
          
        )
        ), # Close tab_dashboard
        # Tab: tab_about -----------------------------------------------------
        tabItem(
          "tab_about",
          fluidRow(
            # About the dashboard
            box(
              title = "About this Dashboard",
              width = "6 col-lg-4",
              tags$p(
                class = "text-center",
                tags$a(
                  href = "https://www.r-project.org",
                  target = "_blank",
                  tags$img(class = "image-responsive",
                           src = "Rlogo.svg",
                           style = "max-width: 150px;"
                  )
                ),
                tags$a(
                  href = "https://rstudio.com",
                  target = "_blank",
                  tags$img(class = "image-responsive",
                           src = "RStudio.svg",
                           style = "max-width: 150px; margin-left: 2em;"
                  )
                ),
                tags$a(
                  href = "https://ggplot2.tidyverse.org/",
                  target = "_blank",
                  tags$img(class = "image-responsive",
                           src = "ggplot2.svg",
                           style = "max-width: 150px; margin-left: 2em;"
                  )
                )
              ),
              tags$p(
                "This dashboard was built in ",
                tags$a(href = "https://r-project.org", target = "_blank", "R"),
                "and", tags$a(href = "https://rstudio.com", target = "_blank", "RStudio"), "with",
                tags$strong("shiny,"),
                tags$strong("shinydashboard,"),
                "the", tags$strong("tidyverse,"),
                "and many more packages."
                
              ),
              tags$p(
                "The dashboard presents data gathered from ",
                tags$a(href = "https://github.com/CSSEGISandData/COVID-19", target = "_blank", "CSSE at Johns Hopkins University"),
                "and ", tags$a(href = "https://www.ssi.dk/aktuelt/sygdomsudbrud/coronavirus/covid-19-i-danmark-epidemiologisk-overvaagningsrapport", target = "_blank", "SSI"),
                "."
              ),
              tags$p("The sourcecode is accessible ", tags$a(href = "https://github.com/fink42/coronaDashboard", taget = "_blank", "here"), ".")
              )
          )
        )
        
        ) # Close tabItems
      ) # Close dashboardBody
        
) # Close dashboardPage
}
