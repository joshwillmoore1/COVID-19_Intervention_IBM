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
                           " The shaded regions represent the 95% confidence intervals about the mean.")

activeInf_help_text = paste0("This figure displays the average number of infections of 500 simulations expressed as a percentage of the total population.",
                            " The shaded regions represent the 95% confidence intervals about the mean.")

IsolationInf_help_text = paste0("This figure displays the average number of isolating agents of 500 simulations expressed as a percentage of the total population.",
                             " The shaded regions represent the 95% confidence intervals about the mean.")

RatioInf_help_text = paste0("This figure displays the total cumulative infections against the cumulative number of absent days from education for all 500 simulations.",
                            " Each circle is the result of a single simulation."," The cross highlights the mean of the scatter data.")


#information panel
ModelInfoText <- p("This application uses a stochastic agent-based model to simulate infection transmission of COVID-19 coupled with various non-pharmaceutical interventions in educational settings.",
                   " The employed model allows for the agents to be grouped together to simulate the effect of close-range contact on transmission and various isolation policies. For example, the subgroups can be considered as tables groups in a classroom, or classes within an entire school.",
                   " In addition, the software allows for control of agent mixing and testing frequencies over a characteristic week. A flow diagram of the model subroutines is shown below and for more details on the agent-based infection transmission model, see the ", 
                   a("reference text.",href = "https://www.medrxiv.org/content/10.1101/2021.03.08.21253122v1") )

DataInfoText <- p("The data used to update the parameters is sourced from the ",
                  a("official UK Government COVID-19 database.",href = "https://coronavirus.data.gov.uk/details/download"),
                  " On start-up, this software retrieves the most up-to-date information for the transmission rates, positive tests and administered tests in Wales from the database.",
                  " The background prevalence is estimated as the a seven day average of the percentage of postive cases in total daily tests administered.",
                  " As the transmission data is updated biweekly, the R number is estimated using the most recent data entries only.")

ResultsInfoText <- p("The results shown in the plots, summary table and exported files represent the average of 500 simulations using the user-specified parameters. The total infections, active infections and active isolations figures are expressed as a percentage of the total population, with the shaded regions of the plots representing the 95% confidence intervals about the mean.",
                     " The 'Export results' button saves the parameter values and simulation data from all simulations displayed in the summary results table to a downloadable .xlsx file, which will allow the user to conduct further analysis using excel.")

DisclaimerText <- p("Any results extracted from this applet are purely theoretical and should not be used solely in any decision making process. We offer no warranty, nor guarantee for the performance, nor fitness of purpose of the software. The applet should be used in conjunction with a trained virologist’s expertise and the reference text which can be found" ,
                    a("here.",href = "https://www.medrxiv.org/content/10.1101/2021.03.08.21253122v1"),
                    " The source code can be found",a("here",href = "https://github.com/joshwillmoore1/COVID-19_Intervention_IBM"), 
                    " as is offered under the MIT License. By using the software you are explicitly accepting these terms of service.")

#control panel tips
simIDhelpText = paste0("Keep track of your simulations by naming them.")


#Infection panel tips
InfoTotAgentsText = paste0("The number of individuals within the population, e.g. the total number of students in the classroom.")
InfoSubgroupText = paste0("The number of individuals in a close-contact bubble within the population, e.g. table groups within a classroom. Note all subgroups are the same size.")
InfoBackPrevText = paste0("The background prevalance will determine the initial number of infections in the population.",
                          " If 'infections from wider population' is turned on, this is the probability of a susceptible becoming infected at the end of every day due to infection sources outside of the school environment.")
InfoRnumText = paste0("The basic reproductive number is the number of infections expected from an infected agent.")
InfoSymptText = paste0("Once infected, each agent has a probability of presenting symptoms (symptomatic), or not presenting symptoms (asymptomatic) as they become infectious.")
InfoCurrentDataText = paste0("This data is sourced from the official UK Government COVID-19 database and uses comfirmed cases only, as a result the numbers may be larger than in reality.")
InfoRecoverText = paste0("Recovered agents are able to return to the population 10 days after isolation begins. Note that any recovered agent are assumed to be immune from further infection.")
InfoPrevBoolText = paste0("At the end of each day, any susceptible agent has the probability of being infected from interactions outside of the population using the background prevalence.")
InfoImmuneText = paste0("Immune agents cannot be infected however still may recieve a false positive result from testing. Immunity may be due to vaccination or prior infection.")

#Intervention panel tips
InfoFalseNegText = paste0("Every test administered to an agent has the probability of giving a false negative result. A higher proportion of false negatives means that the tests miss more infected agents.")
InfoFalsePosText = paste0("Every test administered to an agent has the probability of giving a false positive result. A higher proportion of false positives means that the tests cause more healthy agents to isolate.")
InfoComplianceText = paste0(" All agents have the capacity to ignore an isolation order. This paramater controls the probability of accepting the isolation policy imposed.")
InfoIsoSymText = paste0("If agents are proactive towards prevention of transmission, they will isolate themselves (and their close-contact group) on initial signs of symptoms.")
InfoVentilationText = paste0("If agent are in a well-ventilated location, the probability of transmission to agents outside of the close-contact group is reduced by 50%. A well-ventilated area also yields a 5-fold decrease in R.")
InfoMasksText = paste0("Agents wearing masks reduces the R number by half.")

#Mixing and testing panel tips
InfoMixAndTestText = paste0("Mixing days define when agents have the capacity to transmit the infection within the population.",
                            " Testing days define when testing occurs. Tests are administered to all non-isolating agents in the morning before any agent interactions.")
InfoWeeksText = paste0("Length of simulated time. Noted that all parameters are fixed over this period.")
InfoDelayText = paste0("The testing days prescribed above will not begin until an agent with symptoms is found.")

sevenDayRollingPrev<-mean((DataCases$newCasesByPublishDate[1:7]/DataCases$newTestsByPublishDate[1:7])*100, na.rm = TRUE)
DeloymentTime <- format(Sys.time())

source("ui.R")
source("server.R")
