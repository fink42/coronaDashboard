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
      autoUpdate()
      # Load data
        latest <- list.dirs("data/zip_file", full.names = FALSE)[-1] %>% as.Date() %>% max()
        data <- fread(paste0("data/zip_file/", latest, "/Test_pos_over_time.csv"), dec = ",")
      # Clean data
        data[, NewPositive := as.numeric(str_remove(NewPositive, "[.]"))]
        data <- data[1:(nrow(data)-2), .("date" = as.Date(Date), NewPositive)]
      
      # Drop early observations
        data <- data[date >= data[NewPositive > 1, min(date)], ]
      
        #data <- data[date >= input$new_cases_date_range[1] & date <= input$new_cases_date_range[2], ]
        
      # Plot
        p <- plot_ly(data = data[date >= lubridate::today()-lubridate::days(30*4), ],
                     x = ~date,
                     y = ~NewPositive,
                     type = "bar",
                     showlegend = FALSE,
                     name = 'New daily cases',
                     text = ~paste("Date:", date,
                                   "<br>New cases:", NewPositive),
                     hoverinfo = "text") %>%
          layout(#title = "New daily cases",
                 xaxis = list(title = 'Date',
                              fixedrange = TRUE),
                 yaxis = list(title = 'Cases',
                              fixedrange = TRUE),
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
      autoUpdate()
      # Load cases per municipality
      res <- mclapply(list.dirs(paste0(data_path, "zip_file"), full.names = FALSE)[-1], function(date){
        temp <- fread(paste0(data_path, "zip_file/", date, "/Municipality_test_pos.csv"), dec = ",")
        temp[, Antal_testede := as.numeric(str_remove(Antal_testede, "[.]"))]
        temp[, `Antal_bekræftede_COVID-19` := as.numeric(str_remove(`Antal_bekræftede_COVID-19`, "[<.]"))]
        temp[, Befolkningstal := as.numeric(str_remove(Befolkningstal, "[.]"))]
        temp[, `Kumulativ_incidens_(per_100000)` := as.numeric(str_remove(`Kumulativ_incidens_(per_100000)`, "[<.]"))]
        temp[, date := date]
      }, mc.cores = 2)
      
      data <- rbindlist(res)
      setkey(data, Kommune_(id), date)
      data <- data[, .(`Kommune_(id)`, `Kommune_(navn)`, date, `Antal_bekræftede_COVID-19`, Befolkningstal)]
      data[, date := as.Date(date)]
      
      # Impute missing
      helper <- expand.grid(seq.Date(data[, min(date)], data[, max(date)], by = "day"), data[, unique(`Kommune_(navn)`)]) %>% data.table()
      names(helper) <- c("date", "Kommune_(navn)")
      data[, imputed := FALSE]
      data <- merge(helper, data, by = c("date", "Kommune_(navn)"), all.x = TRUE)
      data[, count_na := sum(is.na(`Antal_bekræftede_COVID-19`))/.N, by = "Kommune_(navn)"]
      data <- data[count_na < .5]
      data[, count_na := NULL]
      data[is.na(imputed), imputed := TRUE]
      
      
      data[, `Antal_bekræftede_COVID-19` := round(imputeTS::na_interpolation(`Antal_bekræftede_COVID-19`)), by = "Kommune_(navn)"]
      data[, `Kommune_(id)` := median(`Kommune_(id)`, na.rm = TRUE), by = "Kommune_(navn)"]
      
      data[, new_cases := `Antal_bekræftede_COVID-19`- shift(`Antal_bekræftede_COVID-19`, n = 1, type = "lag"), by = "Kommune_(id)"]
      data[new_cases < 0, new_cases := 0]
      data <- data[, .(`Kommune_(id)`, `Kommune_(navn)`, date, new_cases, Befolkningstal, imputed)]
      
      # Plot
      latest <- list.dirs("data/zip_file", full.names = FALSE)[-1] %>% as.Date() %>% max() # Used in the annotation
      p <- plot_ly(data = data[date >= lubridate::today()-lubridate::days(30*4) & `Kommune_(navn)` == input$selected_municipality, ],
                   x = ~date,
                   y = ~new_cases,
                   color = ~imputed,
                   type = "bar",
                   showlegend = FALSE,
                   name = 'New daily cases',
                   text = ~paste("Date:", date,
                                 "<br>New cases:", new_cases,
                                 "<br>Municipality:", `Kommune_(navn)`,
                                 "<br>Imputed:", imputed),
                   hoverinfo = "text") %>%
        layout(title = input$selected_municipality,
          xaxis = list(title = 'Date',
                       fixedrange = TRUE),
          yaxis = list(title = 'New cases',
                       fixedrange = TRUE),
          annotations = 
            list(x = 0, y = 0, text = paste0("Updated: ", latest), 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='left', yanchor='auto', xshift=-5, yshift=-56,
                 font=list(size=10))) %>%
        config(displayModeBar = FALSE)
      p
    })
    
}

