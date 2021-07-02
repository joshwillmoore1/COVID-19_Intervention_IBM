library(htmltools)
library(shiny)
library(bslib)
library(numbers)
library(shinyWidgets)
library(fontawesome)
library(shinyBS)
library(shinyjs)
library(shinyalert)


shinyUI(fluidPage(
  theme = light,
  useShinyalert(),
  useShinyjs(),
  
  #for google tracking
  tags$head(HTML(
    "<script async src='https://www.googletagmanager.com/gtag/js?id=G-HMEHLW3KFP'></script>
     <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());

        gtag('config', 'G-HMEHLW3KFP');
     </script>"
  )),

  
  
  fluidRow(
    column(9,  style = 'padding:1.5%;',
           fluidRow(
             h3("    "),
             
             conditionalPanel( condition = "input.dark_mode == 0",
             
             titlePanel(title = div(img(
               src = 'UpdatedLogoSmall.png',height = round(344 * 0.4),width = round(344 * 0.4)),
               "COVID-19 Intervention Simulator", style = "float:left; font-size:130%"), 
               shiny::tags$head(shiny::tags$link(rel = "shortcut icon",
                                                 type="image/vnd.microsoft.icon",
                                                 href="https://www.noaa.gov/sites/all/themes/custom/noaa/favicon.ico"),
                                                 tags$title("COVID-19 Intervention Simulator")))
             
             ),
             
             conditionalPanel( condition = "input.dark_mode == 1",
                               
                               titlePanel(title = div(img(
                                 src = 'UpdatedLogoSmallDM.png',height = round(344 * 0.4),width = round(344 * 0.4)),
                                 "COVID-19 Intervention Simulator", style = "float:left; font-size:130%"), 
                                 shiny::tags$head(shiny::tags$link(rel = "shortcut icon",
                                                                   type="image/vnd.microsoft.icon",
                                                                   href="https://www.noaa.gov/sites/all/themes/custom/noaa/favicon.ico"),
                                                  tags$title("COVID-19 Intervention Simulator")))      
             
             
           ))),
    
    #column for icon bar
    column( 3, align = "right", style = 'padding:1%;',
      
      fluidRow(
        
        column(6,p("     ")),
        
        column(2,align = "right",style = 'padding:0%;',
               
               actionButton("GitButton",label=NULL,icon = icon("github",style = "font-size: 20px"), 
                            style = "padding:1%;background: #66000000;border: 0px;height: 30px",
                            onclick ="window.open('https://github.com/joshwillmoore1/COVID-19_Intervention_IBM', '_blank')")
               
               ),
        
        column(2,align = "center",style = 'padding:0%;',
               
                actionButton("HelpButton",label=NULL,icon = icon("envelope",style = "font-size: 20px")
                             , style = "padding:0%;background: #66000000;border: 0px;height: 30px")
                
               
        ),
        
        
        column(1,align = "center",style = 'padding:0%',
               
      shiny::div( class = "custom-control custom-switch",align = "right",
        tags$input( id = "dark_mode",  type = "checkbox", class = "custom-control-input",
          onclick = HTML( "Shiny.setInputValue('dark_mode', document.getElementById('dark_mode').value);" )),
        tags$label(tags$p(fa("moon", fill = NULL)), `for` = "dark_mode", class = "custom-control-label"),
        
      )),
      
      column(1, style = 'padding:0%',p(" "))
      
      
      
    
    
    )
    
    )
  ),
  
  
  #information panel
  fluidRow(column(12,style='padding:0px;',
    
    wellPanel( style = "border-radius: 0px 0px 0px 0px",
    bsCollapse(id = "InformationPanel",
               bsCollapsePanel(
                 tags$p(strong("Information"),fa("chevron-down", fill = NULL)), 
                 h4("The model"),
                 p("This application uses a stochastic agent-based model to simulate infection transmission of COVID-19 coupled with various non-pharmaceutical interventions in educational settings.",
                   " The employed model allows for the agents to be grouped together to simulate the effect of close-range contact on transmission and various isolation policies, for example, the subgroups can be considered as tables groups in a classroom or class members within a school of agents.",
                   " In addition, the software allows for control of agent mixing and testing frequencies over a characteristic week. A flow diagram of the model subroutines is shown below and for more details on the agent-based infection transmission model, see the reference text which can be found", 
                   a("here.",href = "https://www.medrxiv.org/content/10.1101/2021.03.08.21253122v1") ),
                 column(12,align = "center",
                        img(src = "model_flowchart_nw.png",width = "60%",height = "auto")
                        ),
                 h4("Data"),
                 p("The data used to update the parameters is sourced from the official UK Government COVID-19 database, which can be found ",
                   a("here.",href = "https://coronavirus.data.gov.uk/details/download"),
                   " On start-up, this software retrieves the most up-to-date information for the transmission rates, positive tests and administered tests in Wales from the database.",
                   " The background prevalence is estimated as the a severn day average of the percentage of postive cases in total daily tests administered.",
                    " As the transmission data is updated biweekly, the R number is estimated using the most recent data entries only."),
                 
                 h4("Results"),
                 p("The results shown in the plots, summary table and exported files represent the average of 500 simulations using the user-specified parameters. The total infections, active infections and active isolations figures are expressed as a percentage of the total population, with the shaded regions of the plots designate the 95% confidence intervals about the mean.",
                   " The 'Export results' button saves the current parameter values and simulation data to a downloadable .xlsx file to allow the user to conduct further analysis using excel.")
                 )))
  )),
  
  
  
  
  # Collapse panels for parameters
  fluidRow(column( 4,style='padding:0px;',
    
    wellPanel( style = "border-radius: 0px 0px 0px 0px",
        bsCollapsePanel(
         tags$p(strong("Infection Parameters"), fa("chevron-down", fill = NULL)), 
          
            sliderInput( "NtotSlider", "Total number of agents:", min = 10,
              max = 300,  value = 30, step = 5),
            
            sliderInput("SubgroupSlider",  "Subgroup size:",  min = 1,
              max = 30, value = 5 ),
            
            sliderInput( "PrevSlider", "Background prevalence (%):", min = 0,
              max = 15,   value = 1,  step = 0.5 ),
            
            sliderInput("Rlider","R number:", min = 0,
              max = 3, value = 1, step = 0.1),
            
            sliderInput( "SymptSlider", "Probability of symptomatic infections (%):",
              min = 0,  max = 100, value = 30 ),
         
            h6(" "),
         
         shiny::div( class = "custom-control custom-switch",
           tags$input( id = "UseDataRCheck",  type = "checkbox", class = "custom-control-input",
             onclick = HTML( "Shiny.setInputValue('UseDataRCheck', document.getElementById('UseDataRCheck').value);" )),
           tags$label( p("Use current ", a("infection data", href = "https://coronavirus.data.gov.uk/details/download"), "in Wales"),
             `for` = "UseDataRCheck",
             class = "custom-control-label")),
         
            
            shiny::div(class = "custom-control custom-switch",
              tags$input(id = "RecoveryCheck", type = "checkbox", class = "custom-control-input", checked = TRUE,
              onclick = HTML("Shiny.setInputValue('RecoveryCheck', document.getElementById('RecoveryCheck').value);")),
              tags$label( "Agents recover from infection",  `for` = "RecoveryCheck", class = "custom-control-label"  ) ),
            
            
            h6(" "),
            
            shiny::div( class = "custom-control custom-switch", 
            tags$input(id = "PrevBoolCheck",type = "checkbox",class = "custom-control-input",
            onclick = HTML( "Shiny.setInputValue('PrevBoolCheck', document.getElementById('PrevBoolCheck').value);")),
              
            tags$label("Include infections from wider population",`for` = "PrevBoolCheck",class = "custom-control-label"))
            
          
        )),
      
      #intervention panel collapse
      wellPanel( style = "border-radius: 0px 0px 0px 0px",
                 
      bsCollapsePanel(
        tags$p(strong("Intervention Parameters"), fa("chevron-down", fill = NULL)),
  
          
          sliderInput( "FalseNegSlider", "Probability of false negative test (%):",
            min = 0, max = 100, value = 20, step = 2.5  ),
          
          sliderInput("FalsePosSlider", "Probability of false positive test (%):",
            min = 0, max = 10, value = 0.3, step = 0.1 ),
          
          sliderInput("ComplianceSlider","Probability of agent compliance (%):",
            min = 0, max = 100,  value = 100,step = 5),
        
          h6(""),
          
          shiny::div( class = "custom-control custom-switch",
            tags$input( id = "SymRevomalCheck", type = "checkbox", class = "custom-control-input", checked = TRUE,
              onclick = HTML( "Shiny.setInputValue('SymRevomalCheck', document.getElementById('SymRevomalCheck').value);" )),
            tags$label("Isolate agents displaying symptoms",`for` = "SymRevomalCheck",class = "custom-control-label") ),
          
          h6(""),
          
          shiny::div(class = "custom-control custom-switch",
            tags$input(id = "GoodVentilationCheck",type = "checkbox", class = "custom-control-input",
              onclick = HTML("Shiny.setInputValue('GoodVentilationCheck', document.getElementById('GoodVentilationCheck').value);")),
            tags$label("Well ventilated environment", `for` = "GoodVentilationCheck", class = "custom-control-label")),
          
          h6(""),
          
          shiny::div(class = "custom-control custom-switch",
            tags$input(id = "MaskCheck",type = "checkbox",class = "custom-control-input",
            onclick = HTML("Shiny.setInputValue('MaskCheck', document.getElementById('MaskCheck').value);")),
            tags$label("Agents use masks", `for` = "MaskCheck", class = "custom-control-label"))
          
      )),
      
    wellPanel( style = "border-radius: 0px 0px 0px 0px",
               
      bsCollapsePanel(
        tags$p(strong("Mixing and Testing"), fa("chevron-down", fill = NULL)),
        
          fluidRow(column(6,
                          
                          wellPanel(
                            
                            prettyCheckboxGroup("MixDaysCheck", strong("Mixing days:"),
                              choices = c("Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat"),
                              selected = c("Mon", "Tue", "Wed", "Thur", "Fri"),
                              inline = FALSE, shape = "round",status = "primary",
                              animation = "smooth" )
                          )
                        ),
                   
                   column(6,
                          wellPanel(
                            prettyCheckboxGroup("TestDaysCheck", strong("Testing days:"),
                              choices = c("Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat"),
                              selected = c("Mon", "Fri"),inline = FALSE, shape = "round",
                              status = "primary", animation = "smooth")
                          )
                        )
                   ),
          
          h6(""),
          
          sliderInput("NumWeeksSlider", "Number of weeks:",
            min = 1, max = 13, value = 4, step = 1),
          
          h6(""),
          
          shiny::div(class = "custom-control custom-switch",
            tags$input(id = "InitTestCheck", type = "checkbox",class = "custom-control-input",
              onclick = HTML( "Shiny.setInputValue('InitTestCheck', document.getElementById('InitTestCheck').value);")),
            tags$label("Delay testing until an agent displays symptoms",`for` = "InitTestCheck",class = "custom-control-label")),
          
          h6("")
          
      )),
      
      wellPanel(style = "border-radius: 0px 0px 0px 0px",
                
        h4(strong("Simulation Control"), align = "center"),
        
        h6(""),
        
        fluidRow( column( 6, align = "center",
            
            actionButton("StartSim","Start", align = "center",
              icon = icon("play", style = "font-size: 15px"))
            
          ),
          
          column( 6,align = "center",
                  
            actionButton("ResetButton", "Reset",align = "center",
              icon = icon("redo", style = "font-size: 15px"))
          )
          
        ),
        
        h6(""),
        
        column(12,align = "center",
               
          downloadButton("SaveResultsButton", "Export results", align = "center")
          
        )
      )
      
      
    
    #infection wellpanel ends here
    
    
    
  ),
  
  
  
  
  column(8,style='padding:0px',
         
         
         fluidRow(align = "centre",
                  
           column( 12, align = "center",
             
             wellPanel(style = "background: #66000000; border-radius: 0px 0px 0px 0px", align = "center",
               
               #results title
               fluidRow( align = "centre",
                 
                 column(6,plotOutput("TotInfections")),
                 
                 column(6,plotOutput("NumActiveInfections")),
                 
                 column(6,plotOutput("NumActiveRecoveriesPlot")),
                 
                 column(6,plotOutput("NumRecoveriesPlot"))
                 
               ),
               
               
               
               
               fluidRow(
                 align = "center",
                 
                 h6(""),
                 h6(""),
                 
                 column( 12, align = "center",
                   h5(strong("Summary results"), align = "center"),
                   tableOutput("SummaryTable")
                 )
                 
               )
               
               
               
             )
           )
         )),
  
  
  column(12, style = "padding:0px",
         wellPanel( style = " border-radius: 0px 0px 0px 0px",
                    
fluidRow(

  column(3,
         
         p("COVID-19 Intervention Simulator (2021) | v0.1.5 " , style = "font-size: 80%;position: absolute; top: 20%"),
         p("Latest deployment: ",DeloymentTime ,style = "font-size: 80%;position: absolute; top: 50%"),
         p(em("Content on this site is licensed under the MIT License."),style = "font-size: 80%;position: absolute; top: 80%")
         
         
         ),
  
  column(6,align="center", 
         
         conditionalPanel( condition = "input.dark_mode == 0",
            img(src = "UpdatedLogoSmall.png", width = '10%',height = 'auto',style = "position: absolute;top: 20%")
         ),
         
         conditionalPanel( condition = "input.dark_mode == 1",
            img(src = "UpdatedLogoSmallDM.png", width = '10%',height = 'auto',style = "position: absolute;top: 20%")
         ),
         
         ),
  
  column(3, style = "margin-bottom: 0px " ,
         
         p(em("In association with: "),style = "margin-bottom: 8px;font-size: 80% "),
         
         fluidRow(
           
         column(3, align = "left",
                img(src = "CUlogo.png",width = '100%', height = 'auto',style = "margin-bottom: 0px ")
         ),
         
         column(9,align = "left",
                img(src = "Jogl_long.png",width = '90%', height = 'auto',style = "margin-bottom: 10px ")
         )
         
         
         )
         
         
         )
  
  
  
  
      )))
  
  ,)
  
  
  
  
))
