server <- function(input, output, session) {
  message(packageVersion("shiny"))
  message(Sys.info()["user"])
  # Functions ----
    # Function to log visit
    logVisit <- function(application, platform = NULL){
      mydb <- dbConnect(MySQL(), user = sql_settings$user, password=sql_settings$password, dbname='shiny', host=sql_settings$host, port = sql_settings$port)
      
      # Run insert query
      dbGetQuery(mydb, paste0("INSERT INTO visit_stats (application, platform) VALUES ('", application, "', '", platform, "')"))
      
      # Disconnect
      dbDisconnect(mydb)
    }
  

  # Log visit
  observeEvent(input$browserInfo[2], {
    if(Sys.info()["user"] != "nicolai"){
      logVisit(application = "coronaDashboard", platform = input$browserInfo[2])
      print(input$browserInfo)
    }
    
  }, once = TRUE)
  
    
    reactive_data <- reactiveFileReader(intervalMillis = 30*60*1000,
                                        session = NULL,
                                        filePath = "data/plotData.fst",
                                        readFunc = read_fst,
                                        as.data.table = TRUE)
    dataFunction2 <- reactive({
      data <- reactive_data()
      data[Type == "Prediction", prediction_n := 1:.N, by = c("Country/Region", "variable")]
      data[is.na(prediction_n), prediction_n := 0]
      data
    })
    
    # Cases plot ----
    output$confirmedCases <- renderPlotly({
        #plotData <- dataFunction()
        plotData <- dataFunction2()
        
        # Control predictions
        if(input$addPredictions == FALSE){
            plotData <- plotData[Type == "Actual" & `Country/Region` %in% input$input_countries]
        } else {
          plotData <- plotData[(prediction_n_cases <= input$days | is.na(prediction_n_cases)) & `Country/Region` %in% input$input_countries]
        }
        
        # With or without confidence bans
        if(input$addConfidence == FALSE | input$addPredictions == FALSE){
          casesPlot <- plot_ly(plotData[variable == "Cases per 100k"],
                               x = ~Date, y = ~value, color = ~`Country/Region`, linetype = ~Type,
                               colors = "Paired",
                               type = 'scatter',
                               mode = 'lines',
                               text = ~paste("Country:", `Country/Region`,
                                             "<br>Cases:", round(value,2),
                                             "<br>Date:", Date),
                               hoverinfo = "text") %>%
            layout(yaxis = list(
              title = "Cases per 100k",
              fixedrange = TRUE),
              xaxis = list(fixedrange = TRUE)
            ) %>%
            config(displayModeBar = FALSE)
          casesPlot
        } else {
          plot_gg <- ggplot(plotData[variable == "Cases per 100k"],
                            aes(x = Date,
                                y = value,
                                linetype = Type,
                                color = `Country/Region`)) +
            geom_line() +
            geom_ribbon(aes(ymin = Cases_lower,
                            ymax = Cases_upper,
                            fill = `Country/Region`),
                        alpha = .3,
                        color = NA) +
            theme_minimal() +
            scale_color_brewer(palette="Paired") +
            scale_fill_brewer(palette="Paired")
          
          
          print(ggplotly(plot_gg,
                         tooltip = c("x", "y", "colour"))%>%
                  layout(yaxis = list(
                    title = "Cases per 100k",
                    fixedrange = FALSE),
                    xaxis = list(fixedrange = TRUE)
                  ) %>%
                  config(displayModeBar = FALSE)
          )
        }
        
    })
    
    # Deaths plot ----
    output$confirmedDeaths <- renderPlotly({
        plotData <- dataFunction2()
        
        # Control predictions
        if(input$addPredictions == FALSE){
          plotData <- plotData[Type == "Actual" & `Country/Region` %in% input$input_countries]
        } else {
          plotData <- plotData[(prediction_n_deaths <= input$days | is.na(prediction_n_deaths)) & `Country/Region` %in% input$input_countries]
        }
        
        # With or without confidence bans
        if(input$addConfidence == FALSE | input$addPredictions == FALSE){
          deathPlot <- plot_ly(plotData[variable == "Deaths per 100k"],
                               x = ~Date, y = ~value, color = ~`Country/Region`, linetype = ~Type,
                               colors = "Paired",
                               type = 'scatter',
                               mode = 'lines',
                               text = ~paste("Country:", `Country/Region`,
                                             "<br>Deaths:", round(value,2),
                                             "<br>Date:", Date),
                               hoverinfo = "text") %>%
            layout(yaxis = list(title = "Deaths per 100k",
                                fixedrange = FALSE),
                   xaxis = list(fixedrange = TRUE)) %>%
            config(displayModeBar = FALSE)
          deathPlot
        } else {
          plot_gg <- ggplot(plotData[variable == "Deaths per 100k"],
                            aes(x = Date,
                                y = value,
                                linetype = Type,
                                color = `Country/Region`)) +
            geom_line() +
            geom_ribbon(aes(ymin = Deaths_lower,
                            ymax = Deaths_upper,
                            fill = `Country/Region`),
                        alpha = .3,
                        color = NA) +
            theme_minimal() +
            scale_color_brewer(palette="Paired") +
            scale_fill_brewer(palette="Paired")
          
          print(ggplotly(plot_gg,
                         tooltip = c("x", "y", "colour"))%>%
                  layout(yaxis = list(
                    title = "Deaths per 100k",
                    fixedrange = TRUE),
                    xaxis = list(fixedrange = TRUE)
                  ) %>%
                  config(displayModeBar = FALSE)
          )
        }
        
    })

    
    # Deaths plot corrected for onset ----
    output$onsetDeaths <- renderPlotly({
        plotData <- dataFunction2()
        
        # Control predictions
        if(input$addPredictions == FALSE){
          plotData <- plotData[Type == "Actual" & `Country/Region` %in% input$input_countries]
        } else {
          plotData <- plotData[(prediction_n_deaths <= input$days | is.na(prediction_n_deaths)) & `Country/Region` %in% input$input_countries]
        }
        
        plot <- plot_ly(plotData[variable == "Deaths per 100k"],
                             x = ~Day_death, y = ~value, color = ~`Country/Region`, linetype = ~Type,
                             colors = "Paired",
                             type = 'scatter',
                             mode = 'lines',
                             text = ~paste("Country:", `Country/Region`,
                                           "<br>Deaths:", round(value,2),
                                           "<br>Date:", Date),
                             hoverinfo = "text") %>%
            layout(
                yaxis = list(title = "Deaths per 100k",
                             fixedrange = FALSE),
                xaxis = list(title = "Days since 0.2 deaths per 100k",
                             fixedrange = TRUE)
            ) %>%
            config(displayModeBar = FALSE)
        plot
        
    })
    
    # Plot with estimated R (based on hospitalization)----
    output$estimated_R <- renderPlotly({
      plot <- readRDS("data/reproduction_rate.RDS")
      plot
    })
    
    # Plot with estimated R (based on confirmed cases)----
    output$estimated_R_cases <- renderPlotly({
      plot <- readRDS("data/reproduction_rate_cases.RDS")
      plot
    })
    
    # Plot with estimated R - by region (based on hospitalization)----
    output$estimated_R_region <- renderPlotly({
      plot <- readRDS("data/reproduction_rate_region.RDS")
      plot
    }) 
    
    # Map with estimated R per municipality ----
    output$map_R <- renderImage({
      list(src = "data/map.png",
           contentType = "image/png")
    }, deleteFile = FALSE)
    
    # Map with cases per 100,000 last 7 days ----
    output$map_cases_7_day <- renderImage({
      list(src = "data/map_confirmed_cases_7_day.png",
           contentType = "image/png")
    }, deleteFile = FALSE)
    
    # Animated map ----
    output$animated_map <- renderImage({
      
      list(src = "data/mapAnimation.gif",
           contentType = 'image/gif')
      
    }, deleteFile = FALSE)
    
    # Animated map cases per 100,000 last 7 days ----
    output$animated_map_cases_7_day <- renderImage({
      
      list(src = "data/mapAnimation_cases_7_day.gif",
           contentType = 'image/gif')
      
    }, deleteFile = FALSE)
    
}

