library(RMySQL)
library(DBI)
library(e1071)
library(naivebayes)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(lubridate)

source("DBConnect.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)



