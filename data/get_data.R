library(data.table)
library(fst)
library(magrittr)
library(parallel)
library(drc)
library(stringr)
library(EpiEstim)
library(mapDK)
library(ggpolypath)
library(plotly)
library(gganimate)


# Define path for data storage
data_path <- "/home/nicolai/forslevdata-docker/shinyapps/corona/data/"

# Prepare plotdata ----
if(lubridate::hour(lubridate::with_tz(Sys.time(), tzone = "CEST")) == 5){
  # Data gets update at 4:50 and 4:55
  
# Load data from github

cases <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
               fill = TRUE)

deaths <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                fill = TRUE)

# Save as .fst
write_fst(cases, paste0(data_path, "cases.fst"))
write_fst(deaths, paste0(data_path, "deaths.fst"))
  
start_time <- proc.time() # Just used for timing

# Sum by country
cases <- cases[, lapply(.SD, sum), by = "Country/Region", .SDcols = grep("/20", names(cases))]
deaths <- deaths[, lapply(.SD, sum), by = "Country/Region", .SDcols = grep("/20", names(deaths))]

# Combine
data <- merge(melt(cases, id.vars = c("Country/Region"), variable.name = "Date", value.name = "Cases"),
              melt(deaths, id.vars = c("Country/Region"), variable.name = "Date", value.name = "Deaths"),
              by = c("Country/Region", "Date"),
              all = TRUE)

data[, Date := as.Date(Date, format = "%m/%d/%y")]
data[, `Country/Region` := as.factor(`Country/Region`)]

# Add predictions
data[, Type := "Actual"]

# Weight by population
# Merge with population
pop <- data.table("Austria"         =   8822000,
                  "Belgium"         =  11400000,
                  "Brazil"          = 209500000,
                  "Denmark"         =   5603000,
                  "France"          =  66990000,
                  "Germany"         =  82790000,
                  "Greece"          =  10720000,
                  "Italy"           =  60480000,
                  "Japan"           = 126500000,
                  "Korea, South"    =  51640000,
                  "Mexico"          = 126200000,
                  "Norway"          =   5368000,
                  "Peru"            =  31990000,
                  "Russia"          = 144500000,
                  "Spain"           =  46660000,
                  "Sweden"          =  10120000,
                  "Switzerland"     =   8570000,
                  "United Kingdom"  =  66440000,
                  "US"              = 327200000
                  ) %>%
  melt(variable.name = "Country/Region", value.name = "pop", measure.vars = names(.))

data <- merge(data, pop, by = "Country/Region")

data[, `Cases per 100k` := Cases/(pop/100000)]
data[, `Deaths per 100k` := Deaths/(pop/100000)]

# Define day with more than 0.2 cases by country
data[`Cases per 100k` > 0.2, Day_cases := difftime(Date, min(Date), units = "days") %>% as.numeric(), by = "Country/Region"]
# Loop over countries
predictionDays <- 100
predictions_cases <- mclapply(data[, unique(`Country/Region`)], function(country, days = predictionDays){
  # Estimate model
  model <- drm(Cases ~ Day_cases,
               data = data[`Country/Region` == country & !is.na(Day_cases)],
               fct = LL.4())
  
  # Find latest obs.
  latestObs <- data[`Country/Region` == country, max(Day_cases, na.rm = TRUE)]
  
  # Make prediction x days ahead
  prediction <- data.table("Country/Region" = country,
                           "Day_cases" = seq(from = 0, to = latestObs+1 + days),
                           "Type" = "Prediction")
  prediction[, Date := data[`Country/Region` == country & `Cases per 100k` > .2, min(Date, na.rm = TRUE)] + Day_cases]
  prediction[, prediction_n_cases := Day_cases-latestObs]
  prediction <- cbind(prediction, predict(model, prediction, interval = "prediction"))
  setnames(prediction, "Prediction", "Cases")
  setnames(prediction, "Lower", "Cases_lower")
  setnames(prediction, "Upper", "Cases_upper")
  # Return data
  return(prediction)
}, mc.cores = 2)

# Define day with more than 10 deaths by country
data[`Deaths per 100k` > .2, Day_death := difftime(Date, min(Date), units = "days") %>% as.numeric(), by = "Country/Region"]
# Loop over countries
predictions_deaths <- mclapply(data[, unique(`Country/Region`)], function(country, days = predictionDays){
  # Estimate model
  model <- drm(Deaths ~ Day_death,
               data = data[`Country/Region` == country & !is.na(Day_death)],
               fct = LL.4())
  
  # Find latest obs.
  latestObs <- data[`Country/Region` == country, max(Day_death, na.rm = TRUE)]
  
  # Make prediction x days ahead
  prediction <- data.table("Country/Region" = country,
                           "Day_death" = seq(from = 0, to = latestObs+1 + days),
                           "Type" = "Prediction")
  prediction[, Date := data[`Country/Region` == country & `Deaths per 100k` > .2, min(Date, na.rm = TRUE)] + Day_death]
  prediction <- cbind(prediction, predict(model, prediction, interval = "prediction"))
  prediction[, prediction_n_deaths := Day_death-latestObs]
  setnames(prediction, "Prediction", "Deaths")
  setnames(prediction, "Lower", "Deaths_lower")
  setnames(prediction, "Upper", "Deaths_upper")
  
  # Return data
  return(prediction)
}, mc.cores = 2)

# Merge all predictions
predictions <- merge(rbindlist(predictions_cases),
                     rbindlist(predictions_deaths),
                     by = c("Country/Region", "Date", "Type"),
                     all = TRUE)
# Append to data
data <- rbindlist(list(data,
                       predictions),
                  fill = TRUE)

# Merge with population
data[, pop := max(pop, na.rm = TRUE), by = "Country/Region"]

data[, `Cases per 100k` := Cases/(pop/100000)]
data[, Cases_lower := Cases_lower/(pop/100000)]
data[, Cases_upper := Cases_upper/(pop/100000)]

data[, `Deaths per 100k` := Deaths/(pop/100000)]
data[, Deaths_lower := Deaths_lower/(pop/100000)]
data[, Deaths_upper := Deaths_upper/(pop/100000)]

# Reshape
data <- melt(data,
             id.vars = c("Country/Region", "Date", "Day_cases", "Day_death", "Type","Cases_upper", "Cases_lower","Deaths_upper", "Deaths_lower", "prediction_n_cases", "prediction_n_deaths"),
             measure.vars = c("Cases per 100k", "Deaths per 100k"))
# Save
write.fst(data[Date > "2020-03-01", ], paste0(data_path, "plotData.fst"), compress = 50)

end_time <- proc.time()


# Save time it takes the script to run
new <- data.table("date" = Sys.time(),
                  "user"    = end_time[1]-start_time[1],
                  "system"  = end_time[2]-start_time[2],
                  "elapsed" = end_time[3]-start_time[3])
new <- rbindlist(list(read_fst(paste0(data_path, "timing.fst"), as.data.table = TRUE),
                      new))
write_fst(new, paste0(data_path, "timing.fst"))

## Map of incidense in US ----
  # Load latest data
  data <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv",
               fill = TRUE)
  data2 <- fread(paste0("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports_us/", format(lubridate::today()-1, "%m-%d-%Y"), ".csv"))
  data2[, inhabitants_in_100k := People_Tested/Testing_Rate]
  
  data <- melt(data,
               id.vars = c("Province_State", "Admin2"),
               measure.vars = grep("/20", names(data), value = TRUE),
               variable.name = "Date",
               value.name = "Cases")
  data <- data[, sum(Cases, na.rm = TRUE), by = c("Province_State", "Date")]
  setnames(data, "V1", "Cases")
  data[, Date := as.Date(Date, format = "%m/%d/%Y")]
  
  # Merge with inhabitants
  data <- merge(data, data2[, .(Province_State, inhabitants_in_100k)], by = "Province_State")
  
  # Generate new cases
  data[, new_cases := Cases-shift(Cases, n = 1, type = "lag"), by = "Province_State"]
  data[, new_cases_100k := new_cases/inhabitants_in_100k]
  
  # Average last 7 days
  data[, average_new_cases_100k_7d := round(zoo::rollmean(new_cases_100k, k = 7, fill = NA, align = "right")), by = "Province_State"]
  
  # Load US map
  us_map <- read_fst(paste0(data_path, "us_states_coord.fst"), as.data.table = TRUE)
  plot_data <- merge(data, us_map, by.x = "Province_State" , by.y = "full", allow.cartesian = TRUE)
  
  # Generate plot
  plot <- ggplot(plot_data[Date == max(Date)]) +
    geom_polypath(aes(x, y, group = group, fill = average_new_cases_100k_7d)) +
    geom_path(aes(x, y, group = group), size = .2) +
    scale_fill_gradient2(low = "#00c853",  mid = "#fbc02d" ,high = "#d32f2f", midpoint = 20, na.value = "#9e9e9e", name = "Cases per. 100,000", aesthetics = "fill") +
    theme_void() +
    coord_fixed(ratio = 1.35) +
    ggtitle("Confirmed daily cases per 100,000 during last 7 days")
  
  dpi <- 196
  ggsave(paste0(data_path, "us_map_confirmed_cases_7_day.png"),
         height = 1200/dpi,
         width = 1600/dpi,
         dpi = dpi)
  rm(plot)
  gc()

}

## Zip-file from SSI ----
page <- "https://www.ssi.dk/sygdomme-beredskab-og-forskning/sygdomsovervaagning/c/covid19-overvaagning"
html <- paste(readLines(page))
link <- tryCatch({grep("data-epidemiologisk", tolower(html), value = TRUE)  %>%
    str_match_all("<a href=\"(.*?)\"") %>% .[[1]] %>% .[1,2]}, error = function(e){return(e)})
link <- str_remove(link, "\t")

if(!as.character(lubridate::today()) %in% list.dirs(paste0(data_path,"zip_file"), full.names = FALSE) & grepl(format(lubridate::today(), "%d%m%Y"), link)){
  # Create folder
  dir.create(paste0(data_path, "zip_file/", lubridate::today()))
  # Download zip-file
  temp_file <- tempfile()
  download.file(paste0(link, ".zip"), temp_file)
  unzip(temp_file, exdir = paste0(data_path, "zip_file/", lubridate::today()))
  
  ## Generate plot with reproduction rate ----
  {
    # Load data
    latest <- list.dirs(paste0(data_path, "zip_file"), full.names = FALSE)[-1] %>% as.Date() %>% max()
    data <- fread(paste0(paste0(data_path, "zip_file/"), latest, "/Newly_admitted_over_time.csv"))
    # Drop early observations
    data[, Dato := as.Date(Dato)]
    data <- data[Dato >= data[Total > 1, min(Dato)], ]
    # ggplot(data, aes(x = Dato, y = Total)) + geom_bar(stat = "identity") + geom_line(data = data[, .(Dato, "Total" =zoo::rollmean(Total, k = 7, fill = NA, align = "right"))], color = "red")
    
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
    plotdata <- plotdata[date >= Sys.Date()-90] # Only plot last 3 months
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
                                         range = c(0, 3))) %>%
      config(displayModeBar = FALSE)
    
    # Save
    saveRDS(plot, paste0(data_path, "reproduction_rate.RDS"))
  }
  
  ## Generate plot with reproduction rate using cases ----
  {
    # Load data
    latest <- list.dirs(paste0(data_path, "zip_file"), full.names = FALSE)[-1] %>% as.Date() %>% max()
    data <- fread(paste0(data_path, "zip_file/", latest, "/Test_pos_over_time.csv"), dec = ",")
    # Clean data
    data[, NewPositive := as.numeric(str_remove(NewPositive, "[.]"))]
    data <- data[1:(nrow(data)-2), .("date" = as.Date(Date), NewPositive)]
    
    # Drop early observations
    data <- data[date >= data[NewPositive > 1, min(date)], ]
    # ggplot(data, aes(x = date, y = NewPositive)) + geom_bar(stat = "identity") + geom_line(data = data[, .(date, "NewPositive" =zoo::rollmean(NewPositive, k = 7, fill = NA, align = "right"))], color = "red")
    
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
    plotdata <- plotdata[date >= Sys.Date()-90] # Only plot last 3 months
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
                                         range = c(0, 3))) %>%
      config(displayModeBar = FALSE)
    
    # Save
    saveRDS(plot, paste0(data_path, "reproduction_rate_cases.RDS"))
  }
  
  ## Generate plot with reproduction rate using hospitalisations (by region) ----
  {
    # Load data
    latest <- list.dirs(paste0(data_path, "zip_file"), full.names = FALSE)[-1] %>% as.Date() %>% max()
    data <- fread(paste0(data_path, "zip_file/", latest, "/Newly_admitted_over_time.csv"))
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
    plotdata <- plotdata[date >= Sys.Date()-90] # Only plot last 3 months
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
    plot <- plot %>% layout(title = "Reproduction rate estimated using hospitalisations",
                            xaxis = list(title = 'Date',
                                         fixedrange = TRUE),
                            yaxis = list(title = 'R',
                                         fixedrange = TRUE,
                                         range = c(0, 6)),
                            legend = list(orientation = "h",
                                          xanchor = "center",
                                          x = 0.5)) %>%
      config(displayModeBar = FALSE)
    # Save
    saveRDS(plot, paste0(data_path, "reproduction_rate_region.RDS"))
  }
  
  ## Generate map - reproduction rate----
  {
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
    data <- merge(helper, data, by = c("date", "Kommune_(navn)"), all.x = TRUE)
    data[, count_na := sum(is.na(`Antal_bekræftede_COVID-19`))/.N, by = "Kommune_(navn)"]
    data <- data[count_na < .5]
    data[, count_na := NULL]
    library(imputeTS)
    data[, `Antal_bekræftede_COVID-19` := round(na_interpolation(`Antal_bekræftede_COVID-19`)), by = "Kommune_(navn)"]
    data[, `Kommune_(id)` := median(`Kommune_(id)`, na.rm = TRUE), by = "Kommune_(navn)"]
    
    data[, new_cases := `Antal_bekræftede_COVID-19`- shift(`Antal_bekræftede_COVID-19`, n = 1, type = "lag"), by = "Kommune_(id)"]
    data[new_cases < 0, new_cases := 0]
    data <- data[, .(`Kommune_(id)`, `Kommune_(navn)`, date, new_cases, Befolkningstal)]
    
    data[!is.na(new_cases), new_cases := round(zoo::rollmean(new_cases, k = 5, fill = NA, align = "right")), by = `Kommune_(id)`]
    
    # Estimate R
    mean_si <- 4.7
    std_si <- mean_si/2
    delta_days <- 7
    
    
    estimates <- list()
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
        est$dates <- data[`Kommune_(id)` == municipality & !is.na(new_cases), date]
        
        setDT(est$R)
        est$R[, municipality := municipality]
        
      }
    })
    plotData <- rbindlist(estimates)
    
    # Merge with municipality names
    plotData <- merge(plotData, data[date == "2020-05-11", .(`Kommune_(id)`, `Kommune_(navn)`)],
                      by.x = "municipality",
                      by.y = "Kommune_(id)")
    setnames(plotData, "Kommune_(navn)", "Kommune")
    plotData[, Kommune := as.factor(`Kommune`)]
    
    # Try to correct for municipalities with no new cases
    plotData[`Std(R)`/`Mean(R)` > 0.8 & `Quantile.0.025(R)` < 0.3, `Mean(R)` := NA]
    
    # Map Plot
    mapPlotData <- mapDK(data = plotData[t_end == max(t_end)], #  & `Mean(R)` < 4
                         values = "Mean(R)",
                         id = "Kommune",
                         detail = "municipal",
                         show_missing = TRUE)$data
    setDT(mapPlotData)
    #mapPlotData[is.na(values), values := 0]
    mapPlotData[values >=3, values := 3] # Truncated values above 3
    
    plot <- ggplot(mapPlotData) +
      geom_polypath(aes(long, lat, group = group, fill = values, color = "")) +
      geom_path(aes(long, lat, group = group), size = .2) +
      scale_fill_gradient2(low = "#00c853",  mid = "#fbc02d" ,high = "#d32f2f", midpoint = 1.5, na.value = "#9e9e9e", limits = c(0,3), name = "Reproduction rate (R)", aesthetics = "fill") +
      theme_void() +
      scale_color_manual(values = NA) +
      guides(colour=guide_legend("Too few cases", override.aes=list(color="#9e9e9e", fill = "#9e9e9e"))) +
      coord_fixed(ratio = 1.85) +
      theme(legend.position = c(.87, .7)) +
      ggtitle(paste0("Reproduction rate per municipality estimated using confirmed cases. \nDate: ",data[, max(date)]))
    
    dpi <- 196
    ggsave(paste0(data_path, "map.png"),
           height = 1200/dpi,
           width = 1600/dpi,
           dpi = dpi)
  }
  
  ## Generate animate map ----
  {
    # Map Plot data
    mapPlotData <- vector(mode = "list", length = 30) # Only plot the last 30 days
    
    mapPlotData <- mclapply(seq(from = plotData[, max(t_end)-29], to = plotData[, max(t_end)]), function(N){
      temp <- mapDK(data = plotData[t_end == N],
                    values = "Mean(R)",
                    id = "Kommune",
                    detail = "municipal",
                    show_missing = TRUE)$data
      setDT(temp)
      temp[, t_end := N]
      temp
    })
    
    mapPlotData <- rbindlist(mapPlotData)
    
    # Merge with date
    helper <- data.table("t_end" = seq(mapPlotData[, min(t_end, na.rm = TRUE)], mapPlotData[, max(t_end, na.rm = TRUE)]))
    helper[, date := seq(data[, as.Date(max(date)) - mapPlotData[, max(t_end, na.rm = TRUE) - min(t_end, na.rm = TRUE)]],
                         data[, as.Date(max(date))], by = "day")]
    mapPlotData <- merge(mapPlotData, helper, by = "t_end")
    
    mapPlotData[values >=3, values := 3] # Truncated values above 3
    
    # Animation
    library(gganimate)
    mapPlotData <- mapPlotData[, .(long, lat, values, group, date)]
    gc()
    anim <- ggplot(mapPlotData) +
      geom_polypath(aes(long, lat, group = group, fill = values, color = "")) +
      geom_path(aes(long, lat, group = group), size = .2) +
      scale_fill_gradient2(low = "#00c853",  mid = "#fbc02d" ,high = "#d32f2f", midpoint = 1.5, na.value = "#9e9e9e", limits = c(0,3), name = "Reproduction rate (R)", aesthetics = "fill") +
      theme_void() +
      scale_color_manual(values = NA) +
      guides(colour=guide_legend("Too few cases", override.aes=list(color="#9e9e9e", fill = "#9e9e9e"))) +
      coord_fixed(ratio = 1.85) +
      theme(legend.position = c(.87, .7)) +
      transition_time(date) +
      ggtitle('Date: {frame_time}. Progress: {frame}%')
    
    animation <- animate(anim,
                         height = 1200,
                         width = 1600,
                         res = 192,
                         nframes = 150,
                         fps = 10,
                         end_pause = 50)
    
    anim_save(paste0(data_path, "mapAnimation.gif"),
              animation = animation)
    rm(anim, animation)
  }
  
  ## Generate map - cases per 100.000 last 7 days ----
  {
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
    data <- merge(helper, data, by = c("date", "Kommune_(navn)"), all.x = TRUE)
    data[, count_na := sum(is.na(`Antal_bekræftede_COVID-19`))/.N, by = "Kommune_(navn)"]
    data <- data[count_na < .5]
    data[, count_na := NULL]
    library(imputeTS)
    data[, `Antal_bekræftede_COVID-19` := round(na_interpolation(`Antal_bekræftede_COVID-19`)), by = "Kommune_(navn)"]
    data[, `Kommune_(id)` := median(`Kommune_(id)`, na.rm = TRUE), by = "Kommune_(navn)"]
    
    data[, new_cases := `Antal_bekræftede_COVID-19`- shift(`Antal_bekræftede_COVID-19`, n = 1, type = "lag"), by = "Kommune_(id)"]
    data[new_cases < 0, new_cases := 0]
    data <- data[, .(`Kommune_(id)`, `Kommune_(navn)`, date, new_cases, Befolkningstal)]
    
    data[!is.na(new_cases), new_cases := round(zoo::rollmean(new_cases, k = 7, fill = NA, align = "right")), by = `Kommune_(id)`]
    
      # Add new cases incidense
      data[, Befolkningstal := median(Befolkningstal, na.rm = TRUE), by = "Kommune_(id)"]
      data[, new_cases_incidense := new_cases/(Befolkningstal/100000)]
      
      # Generate mapPlotData
      mapPlotData <- mapDK(data = data[!is.na(new_cases_incidense) & date == max(date),], 
                           values = "new_cases_incidense",
                           id = "Kommune_(navn)",
                           detail = "municipal",
                           show_missing = TRUE)$data
      setDT(mapPlotData)
      
      # Define truncating value
      truncating_value <- data[date >= max(date)-29 & new_cases_incidense > 0, quantile(new_cases_incidense, probs = .95)] %>%
        round()
      
      mapPlotData[values >= truncating_value, values := truncating_value] # Truncated values above truncating value
      
      plot <- ggplot(mapPlotData) +
        geom_polypath(aes(long, lat, group = group, fill = values)) +
        geom_path(aes(long, lat, group = group), size = .2) +
        scale_fill_gradient2(low = "#00c853",  mid = "#fbc02d" ,high = "#d32f2f", midpoint = truncating_value/2, na.value = "#9e9e9e", limits = c(0,truncating_value), name = paste0("Cases per 100,000\nTruncated at ", truncating_value), aesthetics = "fill") +
        theme_void() +
        coord_fixed(ratio = 1.85) +
        theme(legend.position = c(.87, .7)) +
        ggtitle(paste0("Confirmed daily cases per 100,000 during last 7 days \n Date: ", data[, max(date)]))
      
      dpi <- 196
      ggsave(paste0(data_path, "map_confirmed_cases_7_day.png"),
             height = 1200/dpi,
             width = 1600/dpi,
             dpi = dpi)
      rm(plot)
      gc()
  }
  
  ## Generate animate map ----
  {
    # Map Plot data
    mapPlotData <- vector(mode = "list", length = 30) # Only plot the last 30 days
    
    mapPlotData <- mclapply(seq(from = data[!is.na(new_cases_incidense), max(date)-29], to = data[!is.na(new_cases_incidense), max(date)], by = "days"), function(i){
      temp <- mapDK(data = data[date == i,], 
                    values = "new_cases_incidense",
                    id = "Kommune_(navn)",
                    detail = "municipal",
                    show_missing = TRUE)$data
      setDT(temp)
      temp[, date := i]
      temp
    })
    
    mapPlotData <- rbindlist(mapPlotData)
    
    # Define truncating value
    truncating_value <- data[date >= max(date)-29 & new_cases_incidense > 0, quantile(new_cases_incidense, probs = .95)] %>%
      round()
    
    mapPlotData[values >= truncating_value, values := truncating_value]
    
    # Animation
    mapPlotData <- mapPlotData[, .(long, lat, values, group, date)]
    gc()
    anim <- ggplot(mapPlotData) +
      geom_polypath(aes(long, lat, group = group, fill = values)) +
      geom_path(aes(long, lat, group = group), size = .2) +
      scale_fill_gradient2(low = "#00c853",  mid = "#fbc02d" ,high = "#d32f2f", midpoint = truncating_value/2, na.value = "#9e9e9e", limits = c(0,truncating_value), name = paste0("Cases per 100,000\nTruncated at ", truncating_value), aesthetics = "fill") +
      theme_void() +
      coord_fixed(ratio = 1.85) +
      theme(legend.position = c(.87, .7)) +
      transition_time(date) +
      ggtitle("Confirmed daily cases per 100,000 during the last 7 days \n Date: {frame_time}. Progress: {frame}%")
    
    animation <- animate(anim,
                         height = 1200,
                         width = 1600,
                         res = 192,
                         nframes = 150,
                         fps = 10,
                         end_pause = 50)
    
    anim_save(paste0(data_path, "mapAnimation_cases_7_day.gif"),
              animation = animation)
  }
  
  rm(list = ls())
  gc()
}


