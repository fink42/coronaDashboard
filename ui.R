library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(shinycssloaders)

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
        "#map img {max-width: 95%; max-height: 600px; width: auto; height: auto}"
      )),
      
      tabItems(
        # Tab: tab_dashboard ----------------------------------------------------
        tabItem(
          tabName = "tab_dashboard",
          
          # Plots ----
          fluidRow(
            tabBox(title = "",
                   id = "tabset1",
                   # Confirmed cases
                   tabPanel("Cases",
                            withSpinner(plotlyOutput("confirmedCases", height = "140%"),
                              type = 4
                            )
                            
                   ),
                   # Confirmed deaths
                   tabPanel("Deaths",
                            withSpinner(plotlyOutput("confirmedDeaths", height = "140%"),
                                        type = 4
                            )
                            
                   ),
                   # Deaths corrected for onset
                   tabPanel("Controlling for onset",
                            withSpinner(plotlyOutput("onsetDeaths", height = "140%"),
                                        type = 4
                            )
                            
                   ),
                   # Estimating R
                   tabPanel("Reproduction rate",
                            withSpinner(plotlyOutput("estimated_R", height = "140%"),
                                        type = 4
                            ),
                            withSpinner(plotlyOutput("estimated_R_cases", height = "140%"),
                                        type = 4
                            )
                            
                   ),
                   # Estimating R - Region
                   tabPanel("Reproduction rate by region",
                            withSpinner(plotlyOutput("estimated_R_region", height = "140%"),
                                        type = 4)
                   ),
                   # Map estimated R
                   tabPanel("Map",
                            withSpinner(
                              plotOutput("map_R", height = "600px"),
                              type = 4)
                            
                   ),
                   # Map estimated R
                   tabPanel("Map (animated)",
                            #plotOutput("map_R", height = "600px")
                            withSpinner(
                              imageOutput("animated_map", height = '600px'),
                              type = 4)
                            
                   ),
                   width = 12
            )
          ),
          # Controls ----
          conditionalPanel(condition = "input.tabset1 == 'Cases' || input.tabset1 == 'Deaths' || input.tabset1 == 'Controlling for onset'",
                           fluidRow(
                             # Add predictions
                             box(title = "Controls",
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
                                                choices = c("Austria", "Belgium", "Denmark", "France", "Germany", "Greece", "Italy", "Korea, South", "Norway", "Russia", "Spain", "Sweden", "United Kingdom", "US"),
                                                multiple = TRUE,
                                                selected = "Denmark", width = "70%"),
                                 width = 4
                             )
                           )
          )
        ),
        # About - tab_about -----------------------------------------------------
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
