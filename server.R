library(drc)
library(data.table)
library(fst)
library(plotly)
library(RMySQL, quietly = TRUE)
library(parallel)
library(stringr)
library(EpiEstim)
library(parallel)


server <- function(input, output, session) {
  message(packageVersion("shiny"))
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
    logVisit(application = "coronaDashboard", platform = input$browserInfo[2])
    print(input$browserInfo)
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
      # Load data
        latest <- list.dirs("data/zip_file", full.names = FALSE)[-1] %>% as.Date() %>% max()
        data <- fread(paste0("data/zip_file/", latest, "/Newly_admitted_over_time.csv"))
        # Drop early observations
          data[, Dato := as.Date(Dato)]
          data <- data[Dato >= data[Total > 1, min(Dato)], ]
        # Estimate R
          mean_si <- 4.7
          std_si <- mean_si/2
          
          res <- estimate_R(incid = data$Total,
                            method = "parametric_si",
                            config = make_config(list(
                              mean_si = mean_si,
                              std_si = std_si)))
          res$dates <- data$Dato
        
        # Plot
          setDT(res$R)
          plotdata <- res$R
          plotdata[, date :=  res$dates[8:length(res$dates)]]
          plotdata <- plotdata[date <= max(date)-4] # Remove 4 latest observations
          plot <- plot_ly(data = plotdata,
                          x = ~date,
                          y = ~`Quantile.0.975(R)`,
                          type = 'scatter',
                          mode = 'lines',
                          line = list(color = 'transparent'),
                          showlegend = FALSE,
                          name = 'Upper',
                          hoverinfo = 'skip')
          plot <- plot %>% add_trace(x = ~date,
                                     y = ~`Quantile.0.025(R)`,
                                     type = 'scatter',
                                     mode = 'lines',
                                     fill = 'tonexty',
                                     fillcolor = 'rgba(28, 32, 255,0.3)',
                                     line = list(color = 'transparent'),
                                     showlegend = FALSE,
                                     name = 'Lower',
                                     hoverinfo = 'skip')
          plot <- plot %>% add_trace(x = ~date,
                                     y = ~`Mean(R)`,
                                     customdata = ~`Quantile.0.975(R)`,
                                     text = ~`Quantile.0.025(R)`,
                                     type = 'scatter',
                                     mode = 'lines',
                                     line = list(color = 'rgb(28, 32, 255)'),
                                     hovertemplate = paste('Date: %{x}',
                                                           '<br>R: %{y:.2f} [%{text:.2f}:%{customdata:.2f}]'),
                                     name = 'Predicted R')
          plot <- plot %>% add_lines(x = ~date,
                                     y = 1,
                                     type = 'scatter',
                                     mode = 'lines',
                                     line = list(color = 'black', dash = 'dot'),
                                     hoverinfo = 'skip')
          plot <- plot %>% layout(title = "Reproduction rate estimated using hospitalisations",
                                  xaxis = list(title = 'Date',
                                               fixedrange = TRUE),
                                  yaxis = list(title = 'R',
                                               fixedrange = TRUE,
                                               range = c(0, 5.6))) %>%
            config(displayModeBar = FALSE)
          plot
    })
    
    # Plot with estimated R (based on confirmed cases)----
    output$estimated_R_cases <- renderPlotly({
      # Load data
      latest <- list.dirs("data/zip_file", full.names = FALSE)[-1] %>% as.Date() %>% max()
      data <- fread(paste0("data/zip_file/", latest, "/Test_pos_over_time.csv"), dec = ",")
        # Clean data
        data[, NewPositive := as.numeric(str_remove(NewPositive, "[.]"))]
        data <- data[1:(nrow(data)-2), .("date" = as.Date(Date), NewPositive)]
        
      # Drop early observations
      data <- data[date >= data[NewPositive > 1, min(date)], ]
      # Estimate R
      mean_si <- 4.7
      std_si <- mean_si/2
      
      res <- estimate_R(incid = data$NewPositive,
                        method = "parametric_si",
                        config = make_config(list(
                          mean_si = mean_si,
                          std_si = std_si)))
      res$dates <- data$date
      
      # Plot
      setDT(res$R)
      plotdata <- res$R
      plotdata[, date :=  res$dates[8:length(res$dates)]]
      plotdata <- plotdata[date <= max(date)-4] # Remove 4 latest observations
      plot <- plot_ly(data = plotdata,
                      x = ~date,
                      y = ~`Quantile.0.975(R)`,
                      type = 'scatter',
                      mode = 'lines',
                      line = list(color = 'transparent'),
                      showlegend = FALSE,
                      name = 'Upper',
                      hoverinfo = 'skip')
      plot <- plot %>% add_trace(x = ~date,
                                 y = ~`Quantile.0.025(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 fill = 'tonexty',
                                 fillcolor = 'rgba(28, 32, 255,0.3)',
                                 line = list(color = 'transparent'),
                                 showlegend = FALSE,
                                 name = 'Lower',
                                 hoverinfo = 'skip')
      plot <- plot %>% add_trace(x = ~date,
                                 y = ~`Mean(R)`,
                                 customdata = ~`Quantile.0.975(R)`,
                                 text = ~`Quantile.0.025(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'rgb(28, 32, 255)'),
                                 hovertemplate = paste('Date: %{x}',
                                                       '<br>R: %{y:.2f} [%{text:.2f}:%{customdata:.2f}]'),
                                 name = 'Predicted R')
      plot <- plot %>% add_lines(x = ~date,
                                 y = 1,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'black', dash = 'dot'),
                                 hoverinfo = 'skip')
      plot <- plot %>% layout(title = "Reproduction rate estimated using confirmed cases",
                              xaxis = list(title = 'Date',
                                           fixedrange = TRUE),
                              yaxis = list(title = 'R',
                                           fixedrange = TRUE,
                                           range = c(0, 5.6))) %>%
        config(displayModeBar = FALSE)
      plot
    })
    
    # Plot with estimated R - by region (based on hospitalization)----
    output$estimated_R_region <- renderPlotly({
      # Load data
        latest <- list.dirs("data/zip_file", full.names = FALSE)[-1] %>% as.Date() %>% max()
        data <- fread(paste0("data/zip_file/", latest, "/Newly_admitted_over_time.csv"))
        data[, Dato := as.Date(Dato)]
        data <- data[Dato >= data[Total > 1, min(Dato)], ] # Drops early observations where # cases is very low
      # Reshape
        data <- melt(data, id.vars = "Dato", variable.name = "Region", value.name = "Cases")
      
      # Estimate R
        mean_si <- 4.7
        std_si <- mean_si/2
        
        res <- lapply(data[, unique(Region)], function(region){
          res <- estimate_R(incid = data[Region == region, Cases],
                            method = "parametric_si",
                            config = make_config(list(
                              mean_si = mean_si,
                              std_si = std_si)))
          setDT(res$R)
          res$R[, Region := region]
        })
        
        plotData <- rbindlist(res)
        
      # Merge with date
        helper <- data.table("t_end" = seq(plotData[, min(t_end, na.rm = TRUE)], plotData[, max(t_end, na.rm = TRUE)]))
        helper[, date := seq(data[, as.Date(max(Dato)) - plotData[, max(t_end, na.rm = TRUE) - min(t_end, na.rm = TRUE)]],
                             data[, as.Date(max(Dato))], by = "day")]
        plotData <- merge(plotData, helper, by = "t_end")
      
      # Plot
      plotData <- plotData[date <= max(date)-4] # Remove 4 latest observations
      #plotData <- plotData[date >= "2020-04-01"]
      
      plot <- plot_ly(data = plotData[Region == "Total"],
                      x = ~date,
                      y = ~`Quantile.0.975(R)`,
                      type = 'scatter',
                      mode = 'lines',
                      line = list(color = 'transparent'),
                      showlegend = FALSE,
                      name = 'Upper',
                      hoverinfo = 'skip')
      plot <- plot %>% add_trace(x = ~date,
                                 y = ~`Quantile.0.025(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 fill = 'tonexty',
                                 fillcolor = 'rgba(0, 0, 0, 0.3)',
                                 line = list(color = 'transparent'),
                                 showlegend = FALSE,
                                 name = 'Lower',
                                 hoverinfo = 'skip')
      plot <- plot %>% add_trace(x = ~date,
                                 y = ~`Mean(R)`,
                                 customdata = ~`Quantile.0.975(R)`,
                                 text = ~`Quantile.0.025(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'rgb(0, 0, 0)'),
                                 hovertemplate = paste('Date: %{x}',
                                                       '<br>R: %{y:.2f} [%{text:.2f}:%{customdata:.2f}]'),
                                 name = 'Total')
      # Hovedstaden
      plot <- plot %>% add_trace(data = plotData[Region == "Hovedstaden"],
                                 x = ~date,
                                 y = ~`Quantile.0.975(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'transparent'),
                                 showlegend = FALSE,
                                 name = 'Upper',
                                 hoverinfo = 'skip')
      plot <- plot %>% add_trace(x = ~date,
                                 y = ~`Quantile.0.025(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 fill = 'tonexty',
                                 fillcolor = 'rgba(223, 83, 107,0.3)',
                                 line = list(color = 'transparent'),
                                 showlegend = FALSE,
                                 name = 'Lower',
                                 hoverinfo = 'skip')
      plot <- plot %>% add_trace(x = ~date,
                                 y = ~`Mean(R)`,
                                 customdata = ~`Quantile.0.975(R)`,
                                 text = ~`Quantile.0.025(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'rgb(223, 83, 107)'),
                                 hovertemplate = paste('Date: %{x}',
                                                       '<br>R: %{y:.2f} [%{text:.2f}:%{customdata:.2f}]'),
                                 name = 'Hovedstaden')
      # Sjælland
      plot <- plot %>% add_trace(data = plotData[Region == "Sjælland"],
                                 x = ~date,
                                 y = ~`Quantile.0.975(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'transparent'),
                                 showlegend = FALSE,
                                 name = 'Upper',
                                 hoverinfo = 'skip')
      plot <- plot %>% add_trace(x = ~date,
                                 y = ~`Quantile.0.025(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 fill = 'tonexty',
                                 fillcolor = 'rgba(97, 208, 79,0.3)',
                                 line = list(color = 'transparent'),
                                 showlegend = FALSE,
                                 name = 'Lower',
                                 hoverinfo = 'skip')
      plot <- plot %>% add_trace(x = ~date,
                                 y = ~`Mean(R)`,
                                 customdata = ~`Quantile.0.975(R)`,
                                 text = ~`Quantile.0.025(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'rgb(97, 208, 79)'),
                                 hovertemplate = paste('Date: %{x}',
                                                       '<br>R: %{y:.2f} [%{text:.2f}:%{customdata:.2f}]'),
                                 name = 'Sjælland')
      # Syddanmark
      plot <- plot %>% add_trace(data = plotData[Region == "Syddanmark"],
                                 x = ~date,
                                 y = ~`Quantile.0.975(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'transparent'),
                                 showlegend = FALSE,
                                 name = 'Upper',
                                 hoverinfo = 'skip')
      plot <- plot %>% add_trace(x = ~date,
                                 y = ~`Quantile.0.025(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 fill = 'tonexty',
                                 fillcolor = 'rgba(34, 151, 230,0.3)',
                                 line = list(color = 'transparent'),
                                 showlegend = FALSE,
                                 name = 'Lower',
                                 hoverinfo = 'skip')
      plot <- plot %>% add_trace(x = ~date,
                                 y = ~`Mean(R)`,
                                 customdata = ~`Quantile.0.975(R)`,
                                 text = ~`Quantile.0.025(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'rgb(34, 151, 230)'),
                                 hovertemplate = paste('Date: %{x}',
                                                       '<br>R: %{y:.2f} [%{text:.2f}:%{customdata:.2f}]'),
                                 name = 'Syddanmark')
      # Midtjylland 
      plot <- plot %>% add_trace(data = plotData[Region == "Midtjylland"],
                                 x = ~date,
                                 y = ~`Quantile.0.975(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'transparent'),
                                 showlegend = FALSE,
                                 name = 'Upper',
                                 hoverinfo = 'skip')
      plot <- plot %>% add_trace(x = ~date,
                                 y = ~`Quantile.0.025(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 fill = 'tonexty',
                                 fillcolor = 'rgba(205, 11, 188,0.3)',
                                 line = list(color = 'transparent'),
                                 showlegend = FALSE,
                                 name = 'Lower',
                                 hoverinfo = 'skip')
      plot <- plot %>% add_trace(x = ~date,
                                 y = ~`Mean(R)`,
                                 customdata = ~`Quantile.0.975(R)`,
                                 text = ~`Quantile.0.025(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'rgb(205, 11, 188)'),
                                 hovertemplate = paste('Date: %{x}',
                                                       '<br>R: %{y:.2f} [%{text:.2f}:%{customdata:.2f}]'),
                                 name = 'Midtjylland')
      # Nordjylland
      plot <- plot %>% add_trace(data = plotData[Region == "Nordjylland"],
                                 x = ~date,
                                 y = ~`Quantile.0.975(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'transparent'),
                                 showlegend = FALSE,
                                 name = 'Upper',
                                 hoverinfo = 'skip')
      plot <- plot %>% add_trace(x = ~date,
                                 y = ~`Quantile.0.025(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 fill = 'tonexty',
                                 fillcolor = 'rgba(245, 199, 16,0.3)',
                                 line = list(color = 'transparent'),
                                 showlegend = FALSE,
                                 name = 'Lower',
                                 hoverinfo = 'skip')
      plot <- plot %>% add_trace(x = ~date,
                                 y = ~`Mean(R)`,
                                 customdata = ~`Quantile.0.975(R)`,
                                 text = ~`Quantile.0.025(R)`,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'rgb(245, 199, 16)'),
                                 hovertemplate = paste('Date: %{x}',
                                                       '<br>R: %{y:.2f} [%{text:.2f}:%{customdata:.2f}]'),
                                 name = 'Nordjylland')
      
      plot <- plot %>% add_lines(x = ~date,
                                 y = 1,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'black', dash = 'dot'),
                                 hoverinfo = 'skip')
      plot <- plot %>% layout(xaxis = list(title = 'Date',
                                           fixedrange = TRUE),
                              yaxis = list(title = 'R',
                                           fixedrange = TRUE,
                                           range = c(0, 6))) %>%
        config(displayModeBar = FALSE)
      plot
      
    })
    
    # Map with estimated R per municipality ----
    output$map_R <- renderPlot({
      # Load cases per municipality
      res <- list()
      for (date in list.dirs("data/zip_file", full.names = FALSE)[-1]){
        temp <- fread(paste0("data/zip_file/", date, "/Municipality_test_pos.csv"), dec = ",")
        temp[, Antal_testede := as.numeric(str_remove(Antal_testede, "[.]"))]
        temp[, `Antal_bekræftede_COVID-19` := as.numeric(str_remove(`Antal_bekræftede_COVID-19`, "[<.]"))]
        temp[, Befolkningstal := as.numeric(str_remove(Befolkningstal, "[.]"))]
        temp[, `Kumulativ_incidens_(per_100000)` := as.numeric(str_remove(`Kumulativ_incidens_(per_100000)`, "[<.]"))]
        temp[, date := date]
        
        res[[date]] <- temp
      }
      
      data <- rbindlist(res)
      setkey(data, Kommune_(id), date)
      data[, new_cases := `Antal_bekræftede_COVID-19`- shift(`Antal_bekræftede_COVID-19`, n = 1, type = "lag"), by = "Kommune_(id)"]
      data[new_cases < 0, new_cases := 0]
      data <- data[, .(`Kommune_(id)`, `Kommune_(navn)`, date, new_cases)]
      
      data[!is.na(new_cases), new_cases := round(zoo::rollmean(new_cases, k = 3, fill = NA, align = "right")), by = `Kommune_(id)`]
      
      # Estimate R
      mean_si <- 4.7
      std_si <- mean_si/2
      delta_days <- 7
      
      estimates <- mclapply(data[, unique(`Kommune_(id)`)], function(municipality){
        if (data[`Kommune_(id)` == municipality & !is.na(new_cases), max(new_cases)] > 1){
          N <- length(data[`Kommune_(id)` == municipality & !is.na(new_cases), new_cases])
          est <- estimate_R(incid = data[`Kommune_(id)` == municipality & !is.na(new_cases), new_cases],
                            method = "parametric_si",
                            config = make_config(list(
                              mean_si = mean_si,
                              std_si = std_si,
                              t_start = seq(2, (N-delta_days+1)),
                              t_end = seq(delta_days+1, N))))
          setDT(est$R)
          est$R[, municipality := municipality]
        }
      })
      
      plotData <- rbindlist(estimates)
      plotData <- merge(plotData, data[date == "2020-05-11", .(`Kommune_(id)`, `Kommune_(navn)`)],
                        by.x = "municipality",
                        by.y = "Kommune_(id)")
      setnames(plotData, "Kommune_(navn)", "Kommune")
      plotData[, Kommune := as.factor(`Kommune`)]
      
      # Map Plot
      library(mapDK)
      library(ggpolypath)
      mapPlotData <- mapDK(data = plotData[t_end == max(t_end)], #  & `Mean(R)` < 4
                           values = "Mean(R)",
                           id = "Kommune",
                           detail = "municipal",
                           show_missing = TRUE)$data
      setDT(mapPlotData)
      #mapPlotData[is.na(values), values := 0]
      mapPlotData[values >=3, values := 3] # Truncated values above 3
      
      ggplot(mapPlotData) +
        geom_polypath(aes(long, lat, group = group, fill = values, color = "")) +
        geom_path(aes(long, lat, group = group), size = .2) +
        scale_fill_gradient2(low = "#00c853",  mid = "#fbc02d" ,high = "#d32f2f", midpoint = 1.5, na.value = "#9e9e9e", limits = c(0,3), name = "Reproduction rate (R)", aesthetics = "fill") +
        theme_void() +
        scale_color_manual(values = NA) +
        guides(colour=guide_legend("Too few cases", override.aes=list(color="#9e9e9e", fill = "#9e9e9e"))) +
        coord_fixed(ratio = 1.85) +
        theme(legend.position = c(.87, .7)) +
        ggtitle("Reproduction rate per municipality estimated using confirmed cases")
    },
    height = 600,
    width = 800,
    res = 96)
    
    # Animated map ----
    output$animated_map <- renderImage({
      
      list(src = "data/mapAnimation.gif",
           contentType = 'image/gif')
      
    }, deleteFile = FALSE)
    
}

