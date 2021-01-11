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
    
    # AutoUpdate function every 30 min
    autoUpdate <- reactiveTimer(1000*60*30)
    
    latestDataDate <- reactive({
      autoUpdate()
      list.dirs("data/zip_file", full.names = FALSE)[-1] %>% as.Date() %>% max()
    })
    
    dataFunction3 <- reactive({
      autoUpdate()
      latest <- latestDataDate()
      
      # Load cases per municipality
      latest <- list.dirs("data/zip_file", full.names = FALSE)[-1] %>% as.Date() %>% max()
      data <- fread(paste0(data_path, "zip_file/", latest, "/Municipality_cases_time_series.csv"), dec = ",")
      setnames(data, "date_sample", "date")
      data <- melt(data, id.vars = "date", variable.name = "Kommune", value.name = "Positive")
      data[, new_cases_7d := frollsum(Positive, n = 7, fill = NA), by = "Kommune"]
      
      # Merge with befolkningsdata
      bef <- fread(paste0(data_path, "/zip_file/2020-11-17/Municipality_test_pos.csv"), dec = ",")
      bef[`Kommune_(navn)` == "København", `Kommune_(navn)` := "Copenhagen"]
      bef[,Befolkningstal := as.numeric(str_remove(Befolkningstal, "[.]"))]
      data <- merge(data, bef[, .(`Kommune_(navn)`, Befolkningstal)], by.x = "Kommune", by.y = "Kommune_(navn)", all.x = TRUE)
      
      # Merge with number of tests
      tests <- fread(paste0(data_path, "zip_file/", latest, "/Municipality_tested_persons_time_series.csv"), dec = ",")
      setnames(tests, "PrDate_adjusted", "date")
      tests <- melt(tests, id.vars = "date", variable.name = "Kommune", value.name = "Tested")
      data <- merge(data, tests, by.x = c("date", "Kommune"), by.y = c("date", "Kommune"))
      
      # 7 day average
      data[, `7_day_average` := frollmean(Positive, n = 7), by = "Kommune"]
      data[, `7_day_average_percent` := new_cases_7d/frollsum(Tested, n = 7)*100, by = "Kommune"]
      
      # 7 day Incidense
      data[, new_cases_incidense_7d := new_cases_7d/(Befolkningstal/100000)]
      
      # Return
      data
      
    })
    
    # Define path for data storage
    data_path <- "data/"
    
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
      autoUpdate()
      plot <- readRDS("data/reproduction_rate.RDS")
      plot
    })
    
    # Plot with estimated R (based on confirmed cases)----
    output$estimated_R_cases <- renderPlotly({
      autoUpdate()
      plot <- readRDS("data/reproduction_rate_cases.RDS")
      plot
    })
    
    # Plot with estimated R - by region (based on hospitalization)----
    output$estimated_R_region <- renderPlotly({
      autoUpdate()
      plot <- readRDS("data/reproduction_rate_region.RDS")
      plot
    }) 
    
    # Map with estimated R per municipality ----
    output$map_R <- renderImage({
      autoUpdate()
      list(src = "data/map.png",
           contentType = "image/png")
    }, deleteFile = FALSE)
    
    # Map with cases per 100,000 last 7 days ----
    output$map_cases_7_day <- renderImage({
      autoUpdate()
      list(src = "data/map_confirmed_cases_7_day.png",
           contentType = "image/png")
    }, deleteFile = FALSE)
    
    # Animated map ----
    output$animated_map <- renderImage({
      autoUpdate()
      list(src = "data/mapAnimation.gif",
           contentType = 'image/gif')
      
    }, deleteFile = FALSE)
    
    # Animated map cases per 100,000 last 7 days ----
    output$animated_map_cases_7_day <- renderImage({
      autoUpdate()
      list(src = "data/mapAnimation_cases_7_day.gif",
           contentType = 'image/gif')
      
    }, deleteFile = FALSE)
    
    # Plot with new daily cases ----
    output$new_cases <- renderPlotly({
      latest <- latestDataDate()
      data <- dataFunction3()
      
      # Collapse by date
      data <- data[, .(Positive = sum(Positive),
                       `Befolkningstal` = sum(Befolkningstal),
                        Tested = sum(Tested)),
                   by = "date"]
      # 7 day average
      setkey(data, "date")
      data[, new_cases_7d := frollsum(Positive, n = 7, fill = NA)]
      data[, `7_day_average` := frollmean(Positive, n = 7)]
      data[, `7_day_average_percent` := new_cases_7d/frollsum(Tested, n = 7)*100]
      
      # Plot
        p <- plot_ly(data = data[date >= lubridate::today()-lubridate::days(30*6) & date <= max(date)-2, ],
                     x = ~date,
                     y = ~Positive,
                     type = "bar",
                     showlegend = FALSE,
                     name = 'New daily cases',
                     text = ~paste("Date:", date,
                                   "<br>New cases:", Positive,
                                   "<br>7 day average:", round(`7_day_average`, 1),
                                   "<br>7 day positive rate:", round(`7_day_average_percent`, 2), "%"),
                     hoverinfo = "text") %>%
          add_lines(x = ~date,
                    y = ~`7_day_average`,
                    showlegend = FALSE,
                    name = "7 day average") %>%
          add_lines(x = ~date,
                    y = ~`7_day_average_percent`,
                    showlegend = FALSE,
                    name = "7 day positive rate",
                    yaxis = "y2") %>%
          layout(#title = "New daily cases",
            xaxis = list(title = 'Date',
                         fixedrange = TRUE),
            yaxis = list(title = 'Cases',
                         fixedrange = TRUE),
            yaxis2= list(title = "Positive rate",
                         fixedrange = TRUE,
                         overlaying = "y",
                         side = "right",
                         showgrid = FALSE,
                         tickformat = "%",
                         dtick = 0.25,
                         autotick = FALSE),
            annotations = 
              list(x = 0, y = 0, text = paste0("Updated: ", latest), 
                   showarrow = F, xref='paper', yref='paper', 
                   xanchor='left', yanchor='auto', xshift=-5, yshift=-56,
                   font=list(size=10))) %>%
          config(displayModeBar = FALSE)
        p
      
    })
    
    # Plot with daily cases by municipality ----
    output$new_cases_municipality <- renderPlotly({
      latest <- latestDataDate()
      data <- dataFunction3()
      
      p <- plot_ly(data = data[date >= lubridate::today()-lubridate::days(30*6) & date <= max(date)-2 & Kommune == input$selected_municipality, ],
                   x = ~date,
                   y = ~Positive,
                   type = "bar",
                   showlegend = FALSE,
                   name = 'New daily cases',
                   text = ~paste("Date:", date,
                                 "<br>New cases:", Positive,
                                 "<br>7 day average:", round(`7_day_average`, 1),
                                 "<br>7 day positive rate:", round(`7_day_average_percent`, 2), "%"),
                   hoverinfo = "text") %>%
        add_lines(x = ~date,
                  y = ~`7_day_average`,
                  showlegend = FALSE,
                  name = "7 day average") %>%
        add_lines(x = ~date,
                  y = ~`7_day_average_percent`,
                  showlegend = FALSE,
                  name = "7 day positive rate",
                  yaxis = "y2") %>%
        layout(title = input$selected_municipality,
               xaxis = list(title = 'Date',
                            fixedrange = TRUE),
               yaxis = list(title = 'New cases',
                            fixedrange = TRUE),
               yaxis2= list(title = "Positive rate",
                            fixedrange = TRUE,
                            overlaying = "y",
                            side = "right",
                            showgrid = FALSE,
                            tickformat = "%"),
               annotations = 
                 list(x = 0, y = 0, text = paste0("Updated: ", latest), 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='left', yanchor='auto', xshift=-5, yshift=-56,
                      font=list(size=10))) %>%
        config(displayModeBar = FALSE)
      p
    })
    
    # Table with incidence by municipality ----
    output$incidence_table <- DT::renderDataTable({
      
      latest <- latestDataDate()
      data <- dataFunction3()
      
      # Output required rows and columns
      data <- data[date == max(date), .("Municipality" = Kommune, 
                                        "New cases (7 days)" = new_cases_7d,
                                        "Incidence (7 days)" = round(new_cases_incidense_7d,2),
                                        "Percent positive (7 days)" = round(`7_day_average_percent`, 2))]
      setorder(data, -"Incidence (7 days)")
      data
      
    })
    
}

