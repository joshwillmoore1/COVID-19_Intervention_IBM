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

totalInf_help_text = paste0("This figure displays the average cumulative infections of 500 simulations expressed as a percentage of the total population.",
                           " The shaded regions of the plot designate the 95% confidence intervals about the mean.")

activeInf_help_text = paste0("This figure displays the average number of infections of 500 simulations expressed as a percentage of the total population.",
                            " The shaded regions of the plot designate the 95% confidence intervals about the mean.")

IsolationInf_help_text = paste0("This figure displays the average number of isolating agents of 500 simulations expressed as a percentage of the total population.",
                             " The shaded regions of the plot designate the 95% confidence intervals about the mean.")

RatioInf_help_text = paste0("This figure displays the total cumulative infections against the cumulative number of absent days from education for all 500 simulations.",
                            " Each grey circle is the result of a single simulation."," The blue circle highlights the mean of the scatter data.")

simIDhelpText = paste0("Keep track of your simulations by naming them!")


#information panel
ModelInfoText <- p("This application uses a stochastic agent-based model to simulate infection transmission of COVID-19 coupled with various non-pharmaceutical interventions in educational settings.",
                   " The employed model allows for the agents to be grouped together to simulate the effect of close-range contact on transmission and various isolation policies, for example, the subgroups can be considered as tables groups in a classroom or class members within a school of agents.",
                   " In addition, the software allows for control of agent mixing and testing frequencies over a characteristic week. A flow diagram of the model subroutines is shown below and for more details on the agent-based infection transmission model, see the reference text which can be found", 
                   a("here.",href = "https://www.medrxiv.org/content/10.1101/2021.03.08.21253122v1") )

DataInfoText <- p("The data used to update the parameters is sourced from the official UK Government COVID-19 database, which can be found ",
                  a("here.",href = "https://coronavirus.data.gov.uk/details/download"),
                  " On start-up, this software retrieves the most up-to-date information for the transmission rates, positive tests and administered tests in Wales from the database.",
                  " The background prevalence is estimated as the a severn day average of the percentage of postive cases in total daily tests administered.",
                  " As the transmission data is updated biweekly, the R number is estimated using the most recent data entries only.")

ResultsInfoText <- p("The results shown in the plots, summary table and exported files represent the average of 500 simulations using the user-specified parameters. The total infections, active infections and active isolations figures are expressed as a percentage of the total population, with the shaded regions of the plots designate the 95% confidence intervals about the mean.",
                     " The 'Export results' button saves the parameter values and simulation data from all simualtions displayed in the summary results table to a downloadable .xlsx file to allow the user to conduct further analysis using excel.")

DisclaimerText <- p("Any results extracted from this applet are purely theoretical and should not be used solely in any decision making process. We offer no warranty, nor guarantee for the performance, nor fitness of purpose of the software. The applet should be used in conjunction with a trained virologist’s expertise and the reference text which can be found" ,
                    a("here.",href = "https://www.medrxiv.org/content/10.1101/2021.03.08.21253122v1"),
                    " The source code can be found",a("here",href = "https://github.com/joshwillmoore1/COVID-19_Intervention_IBM"), 
                    " as is offered under the MIT License. By using the software you are explicitly accepting these terms of service.")

sevenDayRollingPrev<-mean((DataCases$newCasesByPublishDate[1:7]/DataCases$newTestsByPublishDate[1:7])*100, na.rm = TRUE)
DeloymentTime <- format(Sys.time())

source("ui.R")
source("server.R")
