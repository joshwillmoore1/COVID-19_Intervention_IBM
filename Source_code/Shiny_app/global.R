library(shiny)
library(ggplot2)
library(ggthemes)
library(numbers)
library(htmltools)
library(writexl)
library(htmltools)
library(bslib)
library(shinyWidgets)
library(fontawesome)
library(shinyBS)



light <- bs_theme(version = 4,
                  bg = "#fff", 
                  fg = "#000", 
                  primary = "#B43225",
                  secondary = "#E7A69F",
                  success ="#62C462",
                  info="#5BC0DE",
                  warning = "#F89406",
                  danger = "#EE5F5B",
                  "progress-bar-bg" = "#B43225",
                  base_font = font_google("Overpass"))

dark <- bs_theme(version = 4,
                 bg = "#272B30", 
                 fg = "#F5F7F9", 
                 primary = "#A55BF5",
                 secondary = "#6F5092",
                 success ="#62C462",
                 info="#5BC0DE",
                 warning = "#F89406",
                 danger = "#EE5F5B",
                 "progress-bar-bg" = "#F5F7F9",
                 base_font = font_google("Overpass"))

NoS <- 500; #number of simulations
R_numbers <- read.csv(url("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=transmissionRateMax&metric=transmissionRateMin&format=csv"))
DataCases <- read.csv(url("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=newCasesByPublishDate&metric=newTestsByPublishDate&format=csv"))

sevenDayRollingPrev<-mean((DataCases$newCasesByPublishDate[1:7]/DataCases$newTestsByPublishDate[1:7])*100, na.rm = TRUE)
DeloymentTime <- format(Sys.time())

source("ui.R")
source("server.R")
