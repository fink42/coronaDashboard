enableBookmarking(store = "server")

# Settings ----
# SQL settings
sql_settings <- list(
  # SQL username
  user = "root",
  # SQL password
  password = "secretPassword42",
  # SQL host
  host = '172.17.0.1',
  # SQL port
  port = 3308
)

library(drc)
library(data.table)
library(fst)
library(plotly)
library(RMySQL, quietly = TRUE)
library(parallel)
library(stringr)
library(EpiEstim)
library(parallel)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(shinycssloaders)
library(imputeTS)