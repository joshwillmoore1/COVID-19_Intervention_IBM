library(bslib)
library(numbers)
library(shinyWidgets)
library(fontawesome)
library(shinyBS)
library(shinyjs)
library(shinyalert)
library(tippy)
library(bsplus)

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
  
  #this is for the tippy colors
  includeCSS("TippyLightMode.css"),
  includeCSS("TippyDarkMode.css"),
  


  
  
  fluidRow(
    column(9,  style = 'padding:1.5%;',
           fluidRow(
             h3("    "),
             
             conditionalPanel( condition = "input.dark_mode == 0",
             
             titlePanel(title = shiny::div(img(
               src = 'UpdatedLogoSmall.png',height = round(344 * 0.4),width = round(344 * 0.4)),
               "COVID-19 Intervention Simulator", style = "float:left; font-size:130%"), 
               shiny::tags$head(shiny::tags$link(rel = "shortcut icon",
                                                 type="image/vnd.microsoft.icon",
                                                 href="https://www.noaa.gov/sites/all/themes/custom/noaa/favicon.ico"),
                                                 tags$title("COVID-19 Intervention Simulator")))
             
             ),
             
             conditionalPanel( condition = "input.dark_mode == 1",
                               
                               titlePanel(title = shiny::div(img(
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
               
               actionButton("GitButton",label=NULL,icon = shiny::icon("github",style = "font-size: 20px",lib = "font-awesome"), 
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
                 title = p(strong("Information"),(fa("chevron-down", fill = NULL))), 
                 value = "infoPanel", 
                 
                 hr(),
                 h4("The model"),
                 
                 ModelInfoText,
                 
                 column(12,align = "center",
                        img(src = "model_flowchart_nw.png",width = "60%",height = "auto")
                        ),
                 hr(),
                 
                 h4("Data"),
                 DataInfoText,
                 
                 hr(),
                 
                 h4("Results"),
                 ResultsInfoText
                 
                 )))
  )),
  
  
  
  
  # Collapse panels for parameters
  fluidRow(column( 4,style='padding:0px;',
    
    wellPanel( style = "border-radius: 0px 0px 0px 0px",
        bsCollapsePanel(
         tags$p(strong("Infection Parameters"), fa("chevron-down", fill = NULL)), 
          
         conditionalPanel(condition = "input.dark_mode == 0",
         column(12, align = "right",style = "position: absolute; top: 10%;right:0.5%", 
                
                actionButton("InfoTotAgents",label=NULL,icon = icon("info-circle") , style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                tippy_this(elementId = "InfoTotAgents", tooltip = InfoTotAgentsText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
         )),
         
         conditionalPanel(condition = "input.dark_mode == 1",
                          column(12, align = "right",style = "position: absolute; top: 10%;right:0.5%", 
                                 
                                 actionButton("InfoTotAgents_DM",label=NULL,icon = icon("info-circle") , style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                 tippy_this(elementId = "InfoTotAgents_DM", tooltip = InfoTotAgentsText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                          )),
         
         
            sliderInput( "NtotSlider", "Total number of agents:", min = 10,
              max = 300,  value = 30, step = 5),
         
         conditionalPanel(condition = "input.dark_mode == 0",
         column(12, align = "right",style = "position: absolute; top: 24.8%;right:0.5%", 
                actionButton("InfoSubgroup",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                tippy_this(elementId = "InfoSubgroup", tooltip = InfoSubgroupText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
         )),
         
         conditionalPanel(condition = "input.dark_mode == 1",
                          column(12, align = "right",style = "position: absolute; top: 24.8%;right:0.5%", 
                                 actionButton("InfoSubgroup_DM",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                 tippy_this(elementId = "InfoSubgroup_DM", tooltip = InfoSubgroupText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                          )),
            
            sliderInput("SubgroupSlider",  "Subgroup size:",  min = 1,
              max = 30, value = 5 ),
         
         
         
         conditionalPanel(condition = "input.dark_mode == 0",
         column(12, align = "right",style = "position: absolute; top: 39.3%;right:0.5%",
                actionButton("InfoBackPrev",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                tippy_this(elementId = "InfoBackPrev", tooltip = InfoBackPrevText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
         )),
         
         conditionalPanel(condition = "input.dark_mode == 1",
                          column(12, align = "right",style = "position: absolute; top: 39.3%;right:0.5%",
                                 actionButton("InfoBackPrev_DM",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                 tippy_this(elementId = "InfoBackPrev_DM", tooltip = InfoBackPrevText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                          )),
            
            sliderInput( "PrevSlider", "Background prevalence (%):", min = 0,
              max = 15,   value = 1,  step = 0.5 ),
         
         
         conditionalPanel(condition = "input.dark_mode == 0",
         column(12, align = "right",style = "position: absolute; top: 53.9%;right:0.5%", 
                actionButton("InfoRnum",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                tippy_this(elementId = "InfoRnum", tooltip = InfoRnumText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
         )),
         
         conditionalPanel(condition = "input.dark_mode == 1",
                          column(12, align = "right",style = "position: absolute; top: 53.9%;right:0.5%", 
                                 actionButton("InfoRnum_DM",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                 tippy_this(elementId = "InfoRnum_DM", tooltip = InfoRnumText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                          )),
            
            sliderInput("Rlider","R number:", min = 0,
              max = 3, value = 1, step = 0.1),
         
         conditionalPanel(condition = "input.dark_mode == 0",
         column(12, align = "right",style = "position: absolute; top: 68.2%;right:0.5%", 
                actionButton("InfoSympt",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                tippy_this(elementId = "InfoSympt", tooltip = InfoSymptText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
         )),
         
         conditionalPanel(condition = "input.dark_mode == 1",
                          column(12, align = "right",style = "position: absolute; top: 68.2%;right:0.5%", 
                                 actionButton("InfoSympt_DM",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                 tippy_this(elementId = "InfoSympt_DM", tooltip = InfoSymptText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                          )),
            
            sliderInput( "SymptSlider", "Probability of symptomatic infections (%):",
              min = 0,  max = 100, value = 30 ),
         
         
         fluidRow(
           
           column(10,   shiny::div( class = "custom-control custom-switch",
                                    tags$input( id = "UseDataRCheck",  type = "checkbox", class = "custom-control-input",
                                                onclick = HTML( "Shiny.setInputValue('UseDataRCheck', document.getElementById('UseDataRCheck').value);" )),
                                    tags$label( p("Use current ", a("confirmed cases data", href = "https://coronavirus.data.gov.uk/details/download"), "in Wales"),
                                                `for` = "UseDataRCheck",
                                                class = "custom-control-label"))
                  
                  ),
           
           conditionalPanel(condition = "input.dark_mode == 0",
           column(2, align = "right",style = "position: absolute;top: 82.7%; right:1%",
                  actionButton("InfoCurrentData",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                  tippy_this(elementId = "InfoCurrentData", tooltip = InfoCurrentDataText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
                  )),
           
           conditionalPanel(condition = "input.dark_mode == 1",
                            column(2, align = "right",style = "position: absolute;top: 82.7%; right:1%",
                                   actionButton("InfoCurrentData_DM",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                   tippy_this(elementId = "InfoCurrentData_DM", tooltip = InfoCurrentDataText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                            ))
           
         ),
         
         
         fluidRow(
           column(10,
                  
                  shiny::div(class = "custom-control custom-switch",
                             tags$input(id = "RecoveryCheck", type = "checkbox", class = "custom-control-input", checked = TRUE,
                                        onclick = HTML("Shiny.setInputValue('RecoveryCheck', document.getElementById('RecoveryCheck').value);")),
                             tags$label( "Agents recover from infection",  `for` = "RecoveryCheck", class = "custom-control-label"  ) )
                  
                  ),
           
           conditionalPanel(condition = "input.dark_mode == 0",
           column(2,align = "right",style = "position: absolute;top: 88.2%; right:1%",
                  actionButton("InfoRecover",label=NULL,icon = icon("info-circle") , style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                  tippy_this(elementId = "InfoRecover", tooltip = InfoRecoverText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
                  
                  
                  )),
           
           conditionalPanel(condition = "input.dark_mode == 1",
                            column(2,align = "right",style = "position: absolute;top: 88.2%; right:1%",
                                   actionButton("InfoRecover_DM",label=NULL,icon = icon("info-circle") , style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                   tippy_this(elementId = "InfoRecover_DM", tooltip = InfoRecoverText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                                   
                                   
                            ))
           
         ),
         
         h6(""),
         
         fluidRow(
           column(10,
                  shiny::div( class = "custom-control custom-switch", 
                              tags$input(id = "PrevBoolCheck",type = "checkbox",class = "custom-control-input",
                                         onclick = HTML( "Shiny.setInputValue('PrevBoolCheck', document.getElementById('PrevBoolCheck').value);")),
                              
                              tags$label("Include infections from wider population",`for` = "PrevBoolCheck",class = "custom-control-label"))
                  
                  ),
           
           conditionalPanel(condition = "input.dark_mode == 0",
           column(2,align = "right",style = "position: absolute;top: 93.5%; right:1%",
                  actionButton("InfoPrevBool",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"), 
                  tippy_this(elementId = "InfoPrevBool", tooltip = InfoPrevBoolText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
                  
                  )),
           
           conditionalPanel(condition = "input.dark_mode == 1",
                            column(2,align = "right",style = "position: absolute;top: 93.5%; right:1%",
                                   actionButton("InfoPrevBool_DM",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"), 
                                   tippy_this(elementId = "InfoPrevBool_DM", tooltip = InfoPrevBoolText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                                   
                            ))
                  
                  
                  )
           
           
         
            
          
        )),
      
      #intervention panel collapse
      wellPanel( style = "border-radius: 0px 0px 0px 0px",
                 
      bsCollapsePanel(
        tags$p(strong("Intervention Parameters"), fa("chevron-down", fill = NULL)),
        
        conditionalPanel(condition = "input.dark_mode == 0",
        column(12, align = "right",style = "position: absolute; top: 14.2%;right:0.5%", 
               actionButton("InfoFalseNeg",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
               tippy_this(elementId = "InfoFalseNeg", tooltip = InfoFalseNegText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
               
        )),
        
        conditionalPanel(condition = "input.dark_mode == 1",
                         column(12, align = "right",style = "position: absolute; top: 14.2%;right:0.5%", 
                                actionButton("InfoFalseNeg_DM",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                tippy_this(elementId = "InfoFalseNeg_DM", tooltip = InfoFalseNegText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                                
                         )),
          
          sliderInput( "FalseNegSlider", "Probability of false negative test (%):",
            min = 0, max = 100, value = 20, step = 2.5  ),
        
        conditionalPanel(condition = "input.dark_mode == 0",
        column(12, align = "right",style = "position: absolute; top: 34.7%;right:0.5%", 
               actionButton("InfoFalsePos",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
               tippy_this(elementId = "InfoFalsePos", tooltip = InfoFalsePosText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
        )),
        
        
        conditionalPanel(condition = "input.dark_mode == 1",
                         column(12, align = "right",style = "position: absolute; top: 34.7%;right:0.5%", 
                                actionButton("InfoFalsePos_DM",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                tippy_this(elementId = "InfoFalsePos_DM", tooltip = InfoFalsePosText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                         )),
          
          sliderInput("FalsePosSlider", "Probability of false positive test (%):",
            min = 0, max = 10, value = 0.3, step = 0.1 ),
        
        
        conditionalPanel(condition = "input.dark_mode == 0",
        column(12, align = "right",style = "position: absolute; top: 55.2%;right:0.5%", 
               actionButton("InfoCompliance",label=NULL,icon = icon("info-circle") , style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
               tippy_this(elementId = "InfoCompliance", tooltip = InfoComplianceText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
        )),
        
        conditionalPanel(condition = "input.dark_mode == 1",
                         column(12, align = "right",style = "position: absolute; top: 55.2%;right:0.5%", 
                                actionButton("InfoCompliance_DM",label=NULL,icon = icon("info-circle") , style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                tippy_this(elementId = "InfoCompliance_DM", tooltip = InfoComplianceText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                         )),
          
          sliderInput("ComplianceSlider","Probability of agent compliance (%):",
            min = 0, max = 100,  value = 100,step = 5),
        

        
        
        fluidRow(
          
          column(10,
                 shiny::div( class = "custom-control custom-switch",
                             tags$input( id = "SymRevomalCheck", type = "checkbox", class = "custom-control-input", checked = TRUE,
                                         onclick = HTML( "Shiny.setInputValue('SymRevomalCheck', document.getElementById('SymRevomalCheck').value);" )),
                             tags$label("Isolate agents displaying symptoms",`for` = "SymRevomalCheck",class = "custom-control-label") )
                 ),
          
          conditionalPanel(condition = "input.dark_mode == 0",
          column(2,align = "right",style = "position: absolute; top: 75.2%;right:1%", 
                 
                 actionButton("InfoIsoSym",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                 tippy_this(elementId = "InfoIsoSym", tooltip = InfoIsoSymText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
                 
                 
                 )),
          
          conditionalPanel(condition = "input.dark_mode == 1",
                           column(2,align = "right",style = "position: absolute; top: 75.2%;right:1%", 
                                  
                                  actionButton("InfoIsoSym_DM",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                  tippy_this(elementId = "InfoIsoSym_DM", tooltip = InfoIsoSymText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                                  
                                  
                           ))
          
          
        ),
        
        
        h6(""),
        
        
        fluidRow(
          column(10,
                 shiny::div(class = "custom-control custom-switch",
                            tags$input(id = "GoodVentilationCheck",type = "checkbox", class = "custom-control-input",
                                       onclick = HTML("Shiny.setInputValue('GoodVentilationCheck', document.getElementById('GoodVentilationCheck').value);")),
                            tags$label("Agents are in a well ventilated environment", `for` = "GoodVentilationCheck", class = "custom-control-label"))
                 
                 ),
          
          conditionalPanel(condition = "input.dark_mode == 0",
          column(2, align = "right",style = "position: absolute; top: 83%;right:1%", 
                 
                 actionButton("InfoVentilation",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                 tippy_this(elementId = "InfoVentilation", tooltip = InfoVentilationText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
                 
                 
                 )),
          
          conditionalPanel(condition = "input.dark_mode == 1",
                           column(2, align = "right",style = "position: absolute; top: 83%;right:1%", 
                                  
                                  actionButton("InfoVentilation_DM",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                  tippy_this(elementId = "InfoVentilation_DM", tooltip = InfoVentilationText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                                  
                                  
                           ))
          
          
          
        ),
        
        h6(""),
        
        fluidRow(
          
          column(10,
                 
                 shiny::div(class = "custom-control custom-switch",
                            tags$input(id = "MaskCheck",type = "checkbox",class = "custom-control-input",
                                       onclick = HTML("Shiny.setInputValue('MaskCheck', document.getElementById('MaskCheck').value);")),
                            tags$label("Agents use masks", `for` = "MaskCheck", class = "custom-control-label"))
                 
                 ),
          
          conditionalPanel(condition = "input.dark_mode == 0",
          column(2, align = "right",style = "position: absolute; top: 90.5%;right:1%", 
                 
                 actionButton("InfoMasks",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                 tippy_this(elementId = "InfoMasks", tooltip = InfoMasksText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
                 )),
          
          conditionalPanel(condition = "input.dark_mode == 1",
                           column(2, align = "right",style = "position: absolute; top: 90.5%;right:1%", 
                                  
                                  actionButton("InfoMasks_DM",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                  tippy_this(elementId = "InfoMasks_DM", tooltip = InfoMasksText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                           ))
          
          
          
        )
        
      )),
      
    
    
    wellPanel( style = "border-radius: 0px 0px 0px 0px",
               
      bsCollapsePanel(
        tags$p(strong("Mixing and Testing"), fa("chevron-down", fill = NULL)),
        br(),
        
        conditionalPanel(condition = "input.dark_mode == 0",
        column(12, align = "right",style = "position: absolute; top: 12%;right:0.5%", 
               actionButton("InfoMixAndTest",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
               tippy_this(elementId = "InfoMixAndTest", tooltip = InfoMixAndTestText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
        )),
        
        conditionalPanel(condition = "input.dark_mode == 1",
                         column(12, align = "right",style = "position: absolute; top: 12%;right:0.5%", 
                                actionButton("InfoMixAndTest_DM",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                tippy_this(elementId = "InfoMixAndTest_DM", tooltip = InfoMixAndTestText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                         )),
        
        
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
        
        conditionalPanel(condition = "input.dark_mode == 0",
        column(12, align = "right",style = "position: absolute; top: 71.5%;right:0.5%", 
               actionButton("InfoWeeks",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
               tippy_this(elementId = "InfoWeeks", tooltip = InfoWeeksText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
        )),
        
        conditionalPanel(condition = "input.dark_mode == 1",
                         column(12, align = "right",style = "position: absolute; top: 71.5%;right:0.5%", 
                                actionButton("InfoWeeks_DM",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                tippy_this(elementId = "InfoWeeks_DM", tooltip = InfoWeeksText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                         )),
        
          sliderInput("NumWeeksSlider", "Number of weeks:",
            min = 1, max = 13, value = 4, step = 1),
          
  
          
        fluidRow(
          
          column(10,
                 
                 shiny::div(class = "custom-control custom-switch",
                            tags$input(id = "InitTestCheck", type = "checkbox",class = "custom-control-input",
                                       onclick = HTML( "Shiny.setInputValue('InitTestCheck', document.getElementById('InitTestCheck').value);")),
                            tags$label("Delay testing until an agent displays symptoms",`for` = "InitTestCheck",class = "custom-control-label")),
                 
                 
                 ),
          
          conditionalPanel(condition = "input.dark_mode == 0",
          column(2,align = "right",style = "position: absolute; top: 91%;right:1%", 
                 
                 actionButton("InfoDelay",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                 tippy_this(elementId = "InfoDelay", tooltip = InfoDelayText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
                 
                 )),
          
          conditionalPanel(condition = "input.dark_mode == 1",
                           column(2,align = "right",style = "position: absolute; top: 91%;right:1%", 
                                  
                                  actionButton("InfoDelay_DM",label=NULL,icon = icon("info-circle"), style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                  tippy_this(elementId = "InfoDelay_DM", tooltip = InfoDelayText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                                  
                           ))
          
          
        )
      
          
      )),
      
      wellPanel(style = "border-radius: 0px 0px 0px 0px",
                
        h4(strong("Simulation Control"), align = "center"),
        h6(""),
        
        conditionalPanel(condition = "input.dark_mode == 0",
        column(12, align = "right",style = "position: absolute; top: 26%;right:0.5%", actionButton("InfoSimID",label=NULL,icon = icon("info-circle")
                                                 , style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
               tippy_this(elementId = "InfoSimID", tooltip = simIDhelpText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode',trigger = 'click')
               )),
        
        conditionalPanel(condition = "input.dark_mode == 1",
                         column(12, align = "right",style = "position: absolute; top: 26%;right:0.5%", actionButton("InfoSimID_DM",label=NULL,icon = icon("info-circle")
                                                                                                                  , style = "padding:0.5%;background: #66000000;border: 0px;margin-bottom: 0px;font-size: 20px"),
                                tippy_this(elementId = "InfoSimID_DM", tooltip = simIDhelpText, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode',trigger = 'click')
                         )),
        
        textInput( inputId = "SimIDinput", label = "",placeholder = "Simulation ID (optional)"),

        
        h6(""),
        fluidRow( column( 6, align = "center",
            
            actionButton("StartSim","Start", align = "center", width = '60%',
              icon = icon("play", style = "font-size: 15px"))
            
          ),
          
          column( 6,align = "center",
                  
            actionButton("ResetButton", "Reset",align = "center", width = '60%',
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
                 
                 column(6,
                        
                        
                        conditionalPanel( condition = "input.dark_mode == 0",
                        plotOutput("TotInfections"),
                        tippy_this(elementId = "TotInfections", tooltip = totalInf_help_text, arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode')
                        ),
                        conditionalPanel( condition = "input.dark_mode == 1",
                        plotOutput("TotInfections_DM"),
                        tippy_this(elementId = "TotInfections_DM", tooltip = totalInf_help_text,arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode')
                        )
                        
                        ),
                 
                 column(6,
                        
                        conditionalPanel( condition = "input.dark_mode == 0",
                        plotOutput("NumActiveInfections"),
                        tippy_this(elementId = "NumActiveInfections", tooltip = activeInf_help_text,arrow = TRUE,placement = 'top',inertia =  TRUE,theme='lightMode')
                        ),
                        
                        conditionalPanel( condition = "input.dark_mode == 1",
                                          plotOutput("NumActiveInfections_DM"),
                                          tippy_this(elementId = "NumActiveInfections_DM", tooltip = activeInf_help_text,arrow = TRUE,placement = 'top',inertia =  TRUE,theme='darkMode')
                        )
                        
                        ),
                 
                 column(6,
                        
                        conditionalPanel( condition = "input.dark_mode == 0",
                        plotOutput("NumActiveRecoveriesPlot"),
                        tippy_this(elementId = "NumActiveRecoveriesPlot", tooltip = IsolationInf_help_text,arrow = TRUE,placement = 'bottom',inertia =  TRUE,theme='lightMode')
                        
                        ),
                        
                        conditionalPanel( condition = "input.dark_mode == 1",
                                          plotOutput("NumActiveRecoveriesPlot_DM"),
                                          tippy_this(elementId = "NumActiveRecoveriesPlot_DM", tooltip = IsolationInf_help_text,arrow = TRUE,placement = 'bottom',inertia =  TRUE,theme='darkMode')
                                          
                        )
                        
                        ),
                  
                 column(6,
                        
                        conditionalPanel( condition = "input.dark_mode == 0",
                        plotOutput("NumRecoveriesPlot"),
                        tippy_this(elementId = "NumRecoveriesPlot", tooltip = RatioInf_help_text,arrow = TRUE,placement = 'bottom',inertia =  TRUE,theme='lightMode' )
                        ),
                        
                        conditionalPanel( condition = "input.dark_mode == 1",
                                          plotOutput("NumRecoveriesPlot_DM"),
                                          tippy_this(elementId = "NumRecoveriesPlot_DM", tooltip = RatioInf_help_text,arrow = TRUE,placement = 'bottom',inertia =  TRUE,theme='darkMode' )
                        )
                        
                        )
                 
               ),
               
               
               
               
               fluidRow(
                 align = "center",
                 
                 h6(""),
                 h6(""),
                 
                 column( 12, align = "center",
                   h5(strong("Summary results"), align = "center"),
                   
                   
                   shiny::tableOutput("SummaryTable")
                     
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
