library(shiny)
library(ggplot2)
library(ggthemes)
library(numbers)
library(htmltools)
library(writexl)
library(shinyBS)



shinyServer(function(input, output, session) {
    
    #bslib::bs_themer()
    thematic::thematic_shiny()
  
  showModal(modalDialog(title = "Disclaimer",p("Any results extracted from this applet are purely theoretical and should not be used solely in any decision making process. We offer no warranty, nor guarantee for the performance, nor fitness of purpose of the software. The applet should be used in conjunction with a trained virologist’s expertise and the reference text which can be found" ,
                                               a("here.",href = "https://www.medrxiv.org/content/10.1101/2021.03.08.21253122v1"),
                                               " The source code can be found",a("here",href = "https://github.com/joshwillmoore1/COVID-19_Intervention_IBM"), 
                                               " as is offered under the MIT License. By using the software you are explicitly accepting these terms of service."), 
                        easyClose = FALSE, fade = TRUE,footer = modalButton("Accept & continue")))
    
    observe({
        session$setCurrentTheme(
            if (isTRUE(input$dark_mode)) dark else light
        )
    })
    
   observe({
      updateSliderInput(session, "SubgroupSlider",max = input$NtotSlider ,min = 1)
                          
   })
   
   observeEvent(input$UseDataRCheck, {
     if (input$UseDataRCheck == TRUE){
       updateSliderInput(session, "Rlider",value = (R_numbers$transmissionRateMax[1] + R_numbers$transmissionRateMin[1])*0.5 )
       updateSliderInput(session, "PrevSlider",value = sevenDayRollingPrev )
       shinyjs::disable("Rlider")
       shinyjs::disable("PrevSlider")
     }
     else{
       updateSliderInput(session, "Rlider",value = 1 )
       updateSliderInput(session, "PrevSlider",value = 1 )
       shinyjs::enable("Rlider")
       shinyjs::enable("PrevSlider")
     }
   })
   
   observeEvent(input$HelpButton, {
     showModal(modalDialog(title = "Contact us",p("To report an issue or to ask for help email us at", strong("cv19interventionibm@outlook.com")), easyClose = TRUE,
                           fade = TRUE,footer = NULL))
   })
   
   storingValues <- reactiveValues()
   storingValues$SummaryResults = matrix(,,5,dimnames = list(NULL,c("Simulation number", "Mean total infections", "Mean total recoveries","Percentage of total outbreaks (%)","Mean absence-infection ratio")))
   storingValues$count_check = 0;
   
   
   #plot the total number of infections
   output$TotInfections <- renderPlot({
     totInf <- data.frame(NaN,NaN)
     
     ggplot(totInf, aes(x=NaN, y=NaN)) +
       
       theme(text = element_text(size=20),axis.line = element_line(colour = "grey"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_blank(),
             panel.background = element_blank()) + xlab("Time (Days)") + ylab("Total mean infections (%)") + xlim(1,28) + ylim(0,100)
   })
   
   
   
   #plot the total number of active infections
   output$NumActiveInfections <- renderPlot({
     totInf <- data.frame(NaN,NaN)
     
     ggplot(totInf, aes(x=NaN, y=NaN)) +
       theme(text = element_text(size=20),axis.line = element_line(colour = "grey"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_blank(),
             panel.background = element_blank()) + xlab("Time (Days)") + ylab("Mean active infections (%)") + xlim(1,28) + ylim(0,100)
   })
   
   output$NumRecoveriesPlot <- renderPlot({
     totInf <- data.frame(NaN,NaN)
     
     ggplot(totInf, aes(x=NaN, y=NaN)) +
       
       theme(text = element_text(size=20),axis.line = element_line(colour = "grey"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_blank(),
             panel.background = element_blank()) + xlab("Total mean number of infections") + ylab("Total mean number of absent days")  + xlim(1,28) + ylim(0,30)
     
   })
   
   output$NumActiveRecoveriesPlot <- renderPlot({
     totInf <- data.frame(NaN,NaN)
     
     ggplot(totInf, aes(x=NaN, y=NaN)) +
       
       theme(text = element_text(size=20),axis.line = element_line(colour = "grey"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_blank(),
             panel.background = element_blank()) + xlab("Time (Days)") + ylab("Mean active isolations (%)") + xlim(1,28) + ylim(0,100)
   })
   
    output$SummaryTable <- renderTable(as.data.frame(storingValues$SummaryResults), align = c("c"), striped = FALSE, digits = 2)
   
   output$SaveResultsButton<- downloadHandler(
     filename = function() {
       "SummaryResults.csv"
     },
     content = function(file) {
       write.csv(as.data.frame(storingValues$SummaryResults), file, row.names = FALSE)
     }
   )
   
   
   
   
   #reset button
   observeEvent(input$ResetButton, {
     
     #slider resets
     updateSliderInput(session, "NtotSlider",min = 10, max = 300,
                       value = 30,step = 5)
     
     updateSliderInput(session, "SubgroupSlider", "Subgroup size:",
                       min = 1, max = 30,
                       value = 5)
     
     updateSliderInput(session,"PrevSlider", "Background prevalence (%):",
                       min = 0, max = 15,
                       value = 1,step = 0.5)
     
     updateSliderInput(session,"Rlider", "R number:",
                       min = 0, max = 3,
                       value = 1,step = 0.1)
     
     updateSliderInput(session,"SymptSlider", "Probability of symptomatic infections (%):",
                       min = 0, max = 100,
                       value = 30)
     
     updateSliderInput(session,"FalseNegSlider", "Probability of false negative test (%):",
                       min = 0, max = 100,
                       value = 20,step = 2.5)
     
     updateSliderInput(session,"FalsePosSlider", "Probability of false positive test (%):",
                       min = 0, max = 10,
                       value = 0.3,step = 0.1)
     
     updateSliderInput(session,"ComplianceSlider", "Probability of agent compliance (%):",
                       min = 0, max = 100,
                       value = 100,step = 5)
     
     updateSliderInput(session,"NumWeeksSlider", "Number of weeks:",
                       min = 1, max = 13,
                       value = 4,step = 1)
     
     
     #control switches
     updateCheckboxInput(session, "UseDataRCheck" ,value = FALSE)
     updateCheckboxInput(session, "RecoveryCheck" ,value = TRUE)
     updateCheckboxInput(session, "SymRevomalCheck" ,value = TRUE)
     updateCheckboxInput(session, "PrevBoolCheck" ,value = FALSE)
     updateCheckboxInput(session, "GoodVentilationCheck" ,value = FALSE)
     updateCheckboxInput(session, "MaskCheck" ,value = FALSE)
     updateCheckboxInput(session, "InitTestCheck" ,value = FALSE)
     
     #grouped checkboxes
     updateCheckboxGroupInput(session, "MixDaysCheck", selected = c("Mon","Tue","Wed","Thur","Fri"))
     updateCheckboxGroupInput(session, "TestDaysCheck", selected = c("Mon","Fri"))
     
     #plot the total number of infections
     output$TotInfections <- renderPlot({
       totInf <- data.frame(NaN,NaN)
       
       ggplot(totInf, aes(x=NaN, y=NaN)) +
         
         theme(text = element_text(size=20),axis.line = element_line(colour = "grey"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank()) + xlab("Time (Days)") + ylab("Total mean infections (%)") + xlim(1,28) + ylim(0,100)
     })
     
     
     
     #plot the total number of active infections
     output$NumActiveInfections <- renderPlot({
       totInf <- data.frame(NaN,NaN)
       
       ggplot(totInf, aes(x=NaN, y=NaN)) +
         theme(text = element_text(size=20),axis.line = element_line(colour = "grey"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank()) + xlab("Time (Days)") + ylab("Mean active infections (%)") + xlim(1,28) + ylim(0,100)
     })
     
     output$NumRecoveriesPlot <- renderPlot({
       totInf <- data.frame(NaN,NaN)
       
       ggplot(totInf, aes(x=NaN, y=NaN)) +
         
         theme(text = element_text(size=20),axis.line = element_line(colour = "grey"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank()) + xlab("Total mean number of infections") + ylab("Total mean number of absent days")  + xlim(1,28) + ylim(0,30)
       
     })
     
     output$NumActiveRecoveriesPlot <- renderPlot({
       totInf <- data.frame(NaN,NaN)
       
       ggplot(totInf, aes(x=NaN, y=NaN)) +
         
         theme(text = element_text(size=20),axis.line = element_line(colour = "grey"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank()) + xlab("Time (Days)") + ylab("Mean active isolations (%)") + xlim(1,28) + ylim(0,100)
     })
     
      storingValues$count_check = 0;
      storingValues$SummaryResults = matrix(,,5,dimnames = list(NULL,c("Simulation number", "Mean total infections", "Mean total recoveries","Percentage of total outbreaks (%)","Mean absence-infection ratio")))
      output$SummaryTable <- renderTable(as.data.frame(storingValues$SummaryResults), align = c("c"), striped = FALSE, digits = 2)
     
     output$SaveResultsButton<- downloadHandler(
       filename = function() {
         "SummaryResults.csv"
       },
       content = function(file) {
         write_xlsx(list("Data" = as.data.frame(SummaryResults)), file)
       }
     )
     
   })
   

    
    
    #start the simulation after pressing the compute button on UI
    
    #We now assume that simulations start on a sunday and tests only start with
    #and initial initial cases could be either symp to asymp and with normal
    #infect,detect,recover timers.
    
    #We assume a symptomatic shows signs of infection after tInf days, the same
    #time the student becomes infectious
    
    #Isolate the whole class on infection: SubgroupSize = Ntot
    #Isolate the individual on infection: SubgroupSize = 1
    #Isolate the group on infection: SubgroupSize in [2,Ntot-1] and divisor
    
    #ActivePop key: 0=susceptible, 1=infected (asympt), 2=recovered, 6=isolating,
    #3=isolating but healthy, 4 = infected (sympt), 5 = isolating but recovered
    
    observeEvent(input$StartSim, {
        #Parameter definitions
        
        #round the groupsize to a divisor of Ntot
        possibleValues = divisors(input$NtotSlider)
        currentValue = input$SubgroupSlider
        closestValueInd = which.min(abs(possibleValues-currentValue))
        updateSliderInput(session, "SubgroupSlider",max = input$NtotSlider ,min = 1,value = possibleValues[closestValueInd])
        
        #get mixing frequencies
        MixWeek = matrix(0,1,7);
        MixCheckOutput = input$MixDaysCheck
        if (is.null(MixCheckOutput)){
          MixWeek = matrix(0,1,7)
        } else{
          for (i in 1:length(MixCheckOutput)){
            if (MixCheckOutput[i] == "Sun"){
              MixWeek[1] = 1;
            } else if (MixCheckOutput[i] == "Mon"){
              MixWeek[2] = 1;
            }
            else if (MixCheckOutput[i] == "Tue"){
              MixWeek[3] = 1;
            }
            else if (MixCheckOutput[i] == "Wed"){
              MixWeek[4] = 1;
            }
            else if (MixCheckOutput[i] == "Thur"){
              MixWeek[5] = 1;
            }
            else if (MixCheckOutput[i] == "Fri"){
              MixWeek[6] = 1;
            }
            else if (MixCheckOutput[i] == "Sat"){
              MixWeek[7] = 1;
            }
          }
        }
        
        #get testing frequencies
        TestWeek = matrix(0,1,7);
        TestCheckOutput = input$TestDaysCheck
        if (is.null(TestCheckOutput)){
          TestWeek = matrix(0,1,7)
        } else{
          for (i in 1:length(TestCheckOutput)){
            if (TestCheckOutput[i] == "Sun"){
              TestWeek[1] = 1;
            } else if (TestCheckOutput[i] == "Mon"){
              TestWeek[2] = 1;
            }
            else if (TestCheckOutput[i] == "Tue"){
              TestWeek[3] = 1;
            }
            else if (TestCheckOutput[i] == "Wed"){
              TestWeek[4] = 1;
            }
            else if (TestCheckOutput[i] == "Thur"){
              TestWeek[5] = 1;
            }
            else if (TestCheckOutput[i] == "Fri"){
              TestWeek[6] = 1;
            }
            else if (TestCheckOutput[i] == "Sat"){
              TestWeek[7] = 1;
            }
          }
        }
        
        

        #Update parameters using current data
        if (input$UseDataRCheck == TRUE){
           R <- (R_numbers$transmissionRateMax[1] + R_numbers$transmissionRateMin[1])/2
           Irb <- sevenDayRollingPrev;
        }
        else{
          R = input$Rlider; #R number
          Irb = input$PrevSlider; #Background prevalence
        }
        
        Ntot = input$NtotSlider; #total pop. size
        SubgroupSize = possibleValues[closestValueInd]; #number of individuals in each group
        SocialDistancingModifier = 1; #for FE sims
        Cf = 1; #far R ratio
        PerFalseNeg = input$FalseNegSlider; #percentage of false negatives from tests
        CompIso = input$ComplianceSlider; #compliance with isolation for any individual
        MixingDays = rep(MixWeek, input$NumWeeksSlider); 
        TestingDays = rep(TestWeek, input$NumWeeksSlider); 
        PrevBool = input$PrevBoolCheck;
        RecoveryBool = input$RecoveryCheck; #bool
        PerSymptomatic = input$SymptSlider; #percentage of symptomatic
        RemoveSympt = input$SymRevomalCheck; #Boolean to remove those that show symptoms
        PerFalsePos = input$FalsePosSlider; #percentage of false positives
        DelayTesting = input$InitTestCheck; #start simulations with testing days active - false means to wait for sympto
        
        Parameters = c("Total number of agents", "Subgroup size","Background prevalence (%)", "R number" ,"Probability of false negative test (%)", "Probability of false positive test (%)",
                       "Probability of agent compliance","Probability of symptomatic infection", "Agents recover","Infections from wider pop.","Isolate symptomatic","Delay testing","Days simulated")
        Parameter_values = c(Ntot,SubgroupSize,Irb,R,PerFalseNeg,PerFalsePos,CompIso,PerSymptomatic,RecoveryBool,PrevBool,RemoveSympt,DelayTesting,7*input$NumWeeksSlider)
        
        Frequencies = data.frame(Days = c("Sun","Mon","Tues","Wed","Thur","Fri","Sat"),Mixing_days = MixingDays[1:7],Testing_days = TestingDays[1:7])
        ParameterValues = data.frame(Parameters,Parameter_values)
        
        if (DelayTesting  == 1){
          TestingDays_sched = TestingDays;
          TestingDays = rep(matrix(0,1,7), input$NumWeeksSlider);
        }
        else{
          SymptFlag = 1;
        }

        #mask and ventilation combinations - THESE VALUES NEED UPDATING FROM CFD PROJECT
      
        if (input$MaskCheck &&input$GoodVentilationCheck){
            Cc = 9;
            R = R/10;
        }
        else if (input$MaskCheck && !input$GoodVentilationCheck){
            Cc = 2;
            R = R/2;
        }
        else if (!input$MaskCheck && input$GoodVentilationCheck){
            Cc = 9;
            R = R/5;
        } else{
            Cc = 2;
            R = R;
        }
        
        #initiate progress bar
        progress <- Progress$new(session, min = 0, max = NoS )
        on.exit(progress$close())
        progress$set(message = 'Simulation in progress')
        count <- 0
        
        #Local infection prevalence of symptomatic (Percentage)
        Ir =  Irb;
        
        #if the subgroups are individuals, only infect others
        if (SubgroupSize == 1) {
            Cc = 0;
        }
        else if (SubgroupSize == Ntot){
            Cf = 0;
        }
        
        #heterogeneous transmission
        Rinter =R*(Cf/(Cc+Cf));
        Rintra = R*(Cc/(Cc+Cf));
        
        #infectious time (days) - should probably set this as a distribution for each
        #agent at some point (also get data for this)
        tInf = 3;
        
        #detectable time (days)
        tDet = 5;
        
        #Recovery time (days)
        tRec = 10;
        
        #number of uniform groups in the pop.
        Groups = Ntot/SubgroupSize;
        
        #checking for errors
        if (length(TestingDays) != length(MixingDays)){
            stop("EXPECTION: Testing days and agent mixing days must be the same length")
        }
        
        if (floor(Ntot/SubgroupSize) != (Ntot/SubgroupSize)){
            stop('EXCEPTION: subgroup size must be a divisor of pop size')
        }
        
        #percentage to decimal representation
        Ir = Ir/100;
        Irb = Irb/100;
        PerFalseNeg = PerFalseNeg/100;
        CompIso = CompIso/100;
        PerSymptomatic = PerSymptomatic/100;
        PerFalsePos = PerFalsePos/100;
        
        
        #Init data collection
        NumberOfInfections = matrix(0,NoS,length(MixingDays));
        MeanNumOfInfections = matrix(0,1,length(MixingDays));
        stdNumOfInfections = matrix(0,1,length(MixingDays));
        
        HealthyPeople = matrix(0,NoS,length(MixingDays));
        MeanNumOfHealthy = matrix(0,1,length(MixingDays));
        stdNumOfHealthy = matrix(0,1,length(MixingDays));
        
        NumberOfSympt = matrix(0,NoS,length(MixingDays));
        MeanNumberOfSympt = matrix(0,1,length(MixingDays));
        stdNumberOfSympt = matrix(0,1,length(MixingDays));
        
        NumberOfAsympt = matrix(0,NoS,length(MixingDays));
        MeanNumberOfAsympt = matrix(0,1,length(MixingDays));
        stdNumberOfAsympt = matrix(0,1,length(MixingDays));
        
        NumberOfRecoveries = matrix(0,NoS,length(MixingDays));
        stdNumberOfRecoveries = matrix(0,1,length(MixingDays));
        
        NumberOfCasesDetected = matrix(0,NoS,length(MixingDays));
        MeanNumberOfCasesDetected = matrix(0,1,length(MixingDays));
        stdNumberOfCasesDetected = matrix(0,1,length(MixingDays));
        
        IsolatingInfections = matrix(0,NoS,length(MixingDays));
        IsolatingHealthy = matrix(0,NoS,length(MixingDays));
        NumberOfIsolatingRecovered = matrix(0,NoS,length(MixingDays));
        NumberOfIsolatingHealthy = matrix(0,NoS,length(MixingDays));
        
        TotalOutbreakCounter = matrix(0,1,NoS);
        TotalAbsentDays = matrix(0,1,NoS);
        initTestingDay = matrix(0,1,NoS);
        
        # Repeat the sim for NoS time
        for (i in 1:NoS){
            
            for (j in 1:length(MixingDays)){
                
                #initialisation of the sim
                if (j == 1){
                    
                    #start the sympt tracker index
                    if (DelayTesting == 1){
                        SymptFlag = 0;
                    }
                    
                    #determine the number of sympt in the pop.
                    Infectedsym = runif(Ntot)<Ir;
                    Ninfsym = max(sum(Infectedsym),1);
                    
                    #vectors for timers
                    CountDownDetection = matrix(Inf,Groups,SubgroupSize); 
                    CountDownInfectious = matrix(Inf,Groups,SubgroupSize);
                    CountDownRecovery = matrix(Inf,Groups,SubgroupSize);
                    
                    #vector for population dynamics
                    ActivePop = matrix(0,Groups,SubgroupSize)
                    initInfectedIndicies = sample.int(Ntot, Ninfsym);
                    
                    #check the each index is different
                    while (length(initInfectedIndicies) != length(unique(initInfectedIndicies))){
                        initInfectedIndicies = sample.int(Ntot, Ninfsym);
                    }
                    
                    #randomly select an agent for infection
                    for (z in 1:Ninfsym){
                        
                        #infection index to matrix agent
                        InitInfGroup = floor((initInfectedIndicies[z]-1)/SubgroupSize)+1;
                        InitIndexWithinGroup = initInfectedIndicies[z] - SubgroupSize*(InitInfGroup-1);
                        
                        if (runif(1) < PerSymptomatic){
                            ActivePop[InitInfGroup,InitIndexWithinGroup] = 4;
                            
                        } else {
                            ActivePop[InitInfGroup,InitIndexWithinGroup] = 1;
                        }
                        
                        CountDownDetection[InitInfGroup,InitIndexWithinGroup] = tDet;
                        CountDownInfectious[InitInfGroup,InitIndexWithinGroup] = tInf;
                        CountDownRecovery[InitInfGroup,InitIndexWithinGroup] = tRec;
                        
                    }
                    
                  #end initialisation  
                }
                
                
                # if active, return all recovered students
                if (RecoveryBool == 1){
                    RecoveredIndx = which(CountDownRecovery <= 0, TRUE)
                    
                    if (is.numeric(nrow(RecoveredIndx))){
                      if (nrow(RecoveredIndx) > 0 ){
                        for (k in 1:nrow(RecoveredIndx)){
                          
                          if (ActivePop[RecoveredIndx[k,1],RecoveredIndx[k,2]] == 6){
                            ActivePop[RecoveredIndx[k,1],RecoveredIndx[k,2]] = 2;
                          } 
                          else if (ActivePop[RecoveredIndx[k,1],RecoveredIndx[k,2]] == 1){
                            ActivePop[RecoveredIndx[k,1],RecoveredIndx[k,2]] = 2;
                          }
                          else if (ActivePop[RecoveredIndx[k,1],RecoveredIndx[k,2]] == 3){
                            ActivePop[RecoveredIndx[k,1],RecoveredIndx[k,2]] = 0;
                          }
                          else if (ActivePop[RecoveredIndx[k,1],RecoveredIndx[k,2]] == 4){
                            ActivePop[RecoveredIndx[k,1],RecoveredIndx[k,2]] = 2;
                          }
                          else if (ActivePop[RecoveredIndx[k,1],RecoveredIndx[k,2]] == 5){
                            ActivePop[RecoveredIndx[k,1],RecoveredIndx[k,2]] = 2;
                          }
                          else if (ActivePop[RecoveredIndx[k,1],RecoveredIndx[k,2]] == 2){
                            ActivePop[RecoveredIndx[k,1],RecoveredIndx[k,2]] = 2;
                          }
                          else if (ActivePop[RecoveredIndx[k,1],RecoveredIndx[k,2]] == 0){
                            ActivePop[RecoveredIndx[k,1],RecoveredIndx[k,2]] = 0;
                          }
                          
                          CountDownDetection[RecoveredIndx[k,1],RecoveredIndx[k,2]] = Inf;
                          CountDownInfectious[RecoveredIndx[k,1],RecoveredIndx[k,2]] = Inf;
                          CountDownRecovery[RecoveredIndx[k,1],RecoveredIndx[k,2]] = Inf;
                          
                        }
                        
                      }
                      
                    }
                }
                
                #if active - students are tested using LFDs
              
                if (TestingDays[j]  == 1){
                    posTestIndices = c();
                    #get the indices of the students that are dectable and infected 
                    infectedDetectAgentsSymp = which(CountDownDetection <= 0 & ActivePop == 4,TRUE)
                    infectedDetectAgentsAsymp = which(CountDownDetection <= 0 & ActivePop == 1,TRUE)
                   
                    infectedDetectAgents = rbind(infectedDetectAgentsSymp,infectedDetectAgentsAsymp)
                    
                    if (is.numeric(nrow(infectedDetectAgents))){
                        if (nrow(infectedDetectAgents) > 0 ){
                            
                            for (k in 1:nrow(infectedDetectAgents)){
                                if (runif(1) > PerFalseNeg){
                                    if (runif(1) < CompIso){
                                        
                                        ActivePop[infectedDetectAgents[k,1],infectedDetectAgents[k,2]] = 6;
                                        CountDownDetection[infectedDetectAgents[k,1],infectedDetectAgents[k,2]]  = Inf;
                                        CountDownInfectious[infectedDetectAgents[k,1],infectedDetectAgents[k,2]]  = Inf;
                                        CountDownRecovery[infectedDetectAgents[k,1],infectedDetectAgents[k,2]]  = tRec;
                                        posTestIndices = rbind(posTestIndices ,c(infectedDetectAgents[k,1],infectedDetectAgents[k,2] ) );
                                        
                                    }
                                    
                                }
                            }
                        }
                    }
                    
                    #including false positives 
                    SusAgentIndex = which(ActivePop == 0,TRUE);
                    RecAgentIndex = which(ActivePop == 2,TRUE);
                    
                    if (as.numeric(nrow(SusAgentIndex))){
                        if (nrow(SusAgentIndex) > 0){
                            for (k in 1:nrow(SusAgentIndex)){
                                if (runif(1) < PerFalsePos ){
                                    if (runif(1) < CompIso ){
                                        
                                        ActivePop[SusAgentIndex[k,1],SusAgentIndex[k,2] ] = 3;
                                        CountDownDetection[SusAgentIndex[k,1],SusAgentIndex[k,2] ] = Inf;
                                        CountDownInfectious[SusAgentIndex[k,1],SusAgentIndex[k,2] ] = Inf;
                                        CountDownRecovery[SusAgentIndex[k,1],SusAgentIndex[k,2] ]= tRec;
                                        posTestIndices = rbind(posTestIndices ,c(SusAgentIndex[k,1],SusAgentIndex[k,2] ));
                                        
                                    }
                                }
                            }
                            
                        }
                    }
                    
                    if (as.numeric(nrow(RecAgentIndex))){
                        
                        if (nrow(RecAgentIndex) > 0){
                            for (k in 1:nrow(RecAgentIndex)){
                                if (runif(1) < PerFalsePos ){
                                    if (runif(1) < CompIso ){
                                        
                                        ActivePop[RecAgentIndex[k,1],RecAgentIndex[k,2]] = 5;
                                        CountDownDetection[RecAgentIndex[k,1],RecAgentIndex[k,2]] = Inf;
                                        CountDownInfectious[RecAgentIndex[k,1],RecAgentIndex[k,2]] = Inf;
                                        CountDownRecovery[RecAgentIndex[k,1],RecAgentIndex[k,2]]= tRec;
                                        posTestIndices = rbind(posTestIndices ,c(RecAgentIndex[k,1],RecAgentIndex[k,2]));
                                        
                                    }
                                }
                            }
                            
                        }
                    }
                    #run through the positive test groups and isolate the compliant agents
                    if (length(posTestIndices) > 0){
                        GroupsToIsolate = unique(posTestIndices[,1])
                        
                        for (k in 1:length(GroupsToIsolate)){
                            for (kk in 1:SubgroupSize){
                                
                                if (ActivePop[GroupsToIsolate[k], kk ] == 1 || ActivePop[GroupsToIsolate[k], kk ] == 4){
                                    
                                    if (runif(1) < CompIso){
                                        ActivePop[GroupsToIsolate[k], kk ]  = 6
                                        CountDownDetection[GroupsToIsolate[k], kk ] = Inf;
                                        CountDownInfectious[GroupsToIsolate[k], kk ]= Inf;
                                        CountDownRecovery[GroupsToIsolate[k], kk ]= tRec;
                                        
                                    }
                                }
                                
                                if (ActivePop[GroupsToIsolate[k], kk ] == 2 ||ActivePop[GroupsToIsolate[k], kk ] == 5  ){
                                    
                                    if (runif(1) < CompIso){
                                        ActivePop[GroupsToIsolate[k], kk ]  = 5
                                        CountDownDetection[GroupsToIsolate[k], kk ] = Inf;
                                        CountDownInfectious[GroupsToIsolate[k], kk ]= Inf;
                                        CountDownRecovery[GroupsToIsolate[k], kk ]= tRec;
                                        
                                    }
                                }
                                
                               if (ActivePop[GroupsToIsolate[k], kk ] == 0 ||ActivePop[GroupsToIsolate[k], kk ] == 3){
                                    
                                    if (runif(1) < CompIso){
                                        ActivePop[GroupsToIsolate[k], kk ]  = 3;
                                        CountDownDetection[GroupsToIsolate[k], kk ] = Inf;
                                        CountDownInfectious[GroupsToIsolate[k], kk ]= Inf;
                                        CountDownRecovery[GroupsToIsolate[k], kk ]= tRec;
                                        
                                    }
                                }
                                
                            }
                            
                        }
                    }
                    
                    #end testing loops
                }
                
                #test for undefine agent state change
                
                #infection transmission if agents are mixing
                if (MixingDays[j] == 1){
                    
                    #find the infectious agents in the pop.
                    infected_indices = which(CountDownInfectious<=0, TRUE);
                    NumOfInfectious = sum(CountDownInfectious<=0);
                    
                    if (NumOfInfectious > 0){
                      
                      
                      #intra-group infections
                      if (Cc != 0) {
                        SiReIndicesIntra = rpois(NumOfInfectious, lambda = Rintra)
                        
                      } else{
                        SiReIndicesIntra = matrix(0, 1, NumOfInfectious)
                      }
                      
                      #inter-group infections
                      if (Cf != 0) {
                        SiReIndicesInter = rpois(NumOfInfectious, lambda = Rinter)
                        
                      } else{
                        SiReIndicesInter = matrix(0, 1, NumOfInfectious)
                      }
                      
                        
                        #check that intra and inter infections are possible 
                        for (k in 1:NumOfInfectious){
                            
                            InfectiveGroup = infected_indices[k,1];
                            SiReIndicesIntra[k] = min(SiReIndicesIntra[k], sum(ActivePop[InfectiveGroup,] == 0 ))
                            
                            if (Cf != 0 ){
                              AvailGroups = setdiff(1:Groups,InfectiveGroup)
                              NumOfInterSus = 0;
                              for (kk in 1:length(AvailGroups)){
                                NumOfInterSus = NumOfInterSus + sum(ActivePop[AvailGroups[kk],] == 0)
                              }
                              
                              SiReIndicesInter[k] = min(SiReIndicesInter[k],NumOfInterSus)
                            }
                            
                            
                        }
                        #final check for over inter infections
                        while ( sum(SiReIndicesInter) > sum(ActivePop==0)){
                            SiReIndicesInter = SiReIndicesInter-1;
                            for (v in 1:length(SiReIndicesInter)){
                                if (SiReIndicesInter[v]<0){
                                    SiReIndicesInter[v] = 0;
                                }
                            }
                        }
                        
                        #allocate the intra infections
                        for (k in 1:NumOfInfectious){
                            
                            InfectiveGroup = infected_indices[k,1];
                            intraSusIndex = which(ActivePop[InfectiveGroup,]==0, TRUE);
                            
                            if (sum(intraSusIndex) != 0){
                                if (SiReIndicesIntra[k] > 0){
                                for (kk in 1:SiReIndicesIntra[k]){
                                    
                                    if (runif(1) < PerSymptomatic){
                                        ActivePop[InfectiveGroup,intraSusIndex[kk]] = 4;
                                        
                                    } else{
                                        ActivePop[InfectiveGroup,intraSusIndex[kk]] = 1;
                                    }
                                    
                                    CountDownDetection[InfectiveGroup,intraSusIndex[kk]] = tDet;
                                    CountDownInfectious[InfectiveGroup,intraSusIndex[kk]] = tInf;
                                    CountDownRecovery[InfectiveGroup,intraSusIndex[kk]] = tRec;
                                    
                                }
                            }
                            }
                            
                        }
                    
                        #randomly allocate inter infections
                        for  (k in 1:length(SiReIndicesInter)){
                            if (SiReIndicesInter[k] > 0){
                                InfectiveGroup = infected_indices[k,1];
                                AvailGroups = setdiff(1:Groups,InfectiveGroup);
                                
                                InterSus = which(ActivePop == 0, TRUE);
                                InterSus = InterSus[InterSus[,1] != InfectiveGroup ,]
                                
                                if ( is.numeric(nrow(InterSus)) ){
                                    
                                    if (nrow(InterSus) > 0){
                                        
                                        InterSus = InterSus[sample(1:nrow(InterSus)), ]
                                        
                                        for (kk in 1:min(SiReIndicesInter[k],nrow(InterSus))){
                                            
                                            if (runif(1) < PerSymptomatic){
                                                
                                                ActivePop[InterSus[kk,1],InterSus[kk,2]] = 4;
                                                
                                            } else{
                                                
                                                ActivePop[InterSus[kk,1],InterSus[kk,2]] = 1;
                                            }
                                            
                                            CountDownDetection[InterSus[kk,1],InterSus[kk,2]] = tDet;
                                            CountDownInfectious[InterSus[kk,1],InterSus[kk,2]] = tInf;
                                            CountDownRecovery[InterSus[kk,1],InterSus[kk,2]] = tRec;
                                            
                                        }
                                    }
                                }
                                
                            }
                            
                        }
                        
                    }
                    
                    
                  #end mixing   
                }
                
                #check for sympto flag to init testing days
                
                if (SymptFlag == 0 ){
                    infectedInfectiousAgentsSymp = which(CountDownInfectious <= 0 & ActivePop == 4,TRUE)
                    if ( is.numeric(nrow(infectedInfectiousAgentsSymp)) ){
                        if (nrow(infectedInfectiousAgentsSymp) > 0){
                            
                            SymptFlag = 1;
                            initTestingDay[i] = j;
                            TestingDays = TestingDays_sched
                        }
                    }
                }
                
                
                #if active-remove symptomatic agents showing signs of infection
                if (RemoveSympt == 1){
                    
                    RemoveSympIndices = c();
                    infectedInfectAgentsSymp = which(CountDownInfectious <= 0 & ActivePop == 4,TRUE)
                    
                    if (is.numeric(nrow(infectedInfectAgentsSymp))){
                        if (nrow(infectedInfectAgentsSymp) > 0 ){ 
                            for (k in 1:nrow(infectedInfectAgentsSymp)){
                                if (runif(1) < CompIso){
                                    
                                    ActivePop[infectedInfectAgentsSymp[k,1],infectedInfectAgentsSymp[k,2]] = 6;
                                    CountDownDetection[infectedInfectAgentsSymp[k,1],infectedInfectAgentsSymp[k,2]] = Inf;
                                    CountDownInfectious[infectedInfectAgentsSymp[k,1],infectedInfectAgentsSymp[k,2]] = Inf;
                                    CountDownRecovery[infectedInfectAgentsSymp[k,1],infectedInfectAgentsSymp[k,2]] = tRec;
                                    RemoveSympIndices = rbind(RemoveSympIndices,c(infectedInfectAgentsSymp[k,1],infectedInfectAgentsSymp[k,2]));
                                }
                            }
                        }
                    }
                    
                    if (length(RemoveSympIndices) > 0){
                        GroupsToIsolate = unique(RemoveSympIndices[,1])
                        for (k in 1:length(GroupsToIsolate)){
                            for (kk in 1:SubgroupSize){
                                
                                if (ActivePop[GroupsToIsolate[k], kk ] == 1 || ActivePop[GroupsToIsolate[k], kk ] == 4){
                                    
                                    if (runif(1) < CompIso){
                                        ActivePop[GroupsToIsolate[k], kk ]  = 6
                                        CountDownDetection[GroupsToIsolate[k], kk ] = Inf;
                                        CountDownInfectious[GroupsToIsolate[k], kk ]= Inf;
                                        CountDownRecovery[GroupsToIsolate[k], kk ]= tRec;
                                        
                                    }
                                }
                                
                                if (ActivePop[GroupsToIsolate[k], kk ] == 2){
                                    
                                    if (runif(1) < CompIso){
                                        ActivePop[GroupsToIsolate[k], kk ]  = 5
                                        CountDownDetection[GroupsToIsolate[k], kk ] = Inf;
                                        CountDownInfectious[GroupsToIsolate[k], kk ]= Inf;
                                        CountDownRecovery[GroupsToIsolate[k], kk ]= tRec;
                                        
                                    }
                                }
                                
                                if (ActivePop[GroupsToIsolate[k], kk ] == 0){
                                    
                                    if (runif(1) < CompIso){
                                        ActivePop[GroupsToIsolate[k], kk ]  = 3;
                                        CountDownDetection[GroupsToIsolate[k], kk ] = Inf;
                                        CountDownInfectious[GroupsToIsolate[k], kk ]= Inf;
                                        CountDownRecovery[GroupsToIsolate[k], kk ]= tRec;
                                        
                                    }
                                }
                                
                            }
                            
                        }
                        
                    }
                    
                 #end of remove sympt   
                }
                
                #if active - infect those suseptible using background rate
                if (PrevBool == 1){
                   SusAgentIndex = c()
                    SusAgentIndex = which(ActivePop == 0,TRUE);
                    
                    if (is.numeric(nrow(SusAgentIndex ))){
                        if (nrow(SusAgentIndex) > 0 ){
                            
                            if (nrow(SusAgentIndex) > 1 ){
                              SusAgentIndex = SusAgentIndex[sample(1:nrow(SusAgentIndex)), ]
                            }
                          
                            for (k in 1:nrow(SusAgentIndex)){
                                if (runif(1) < Irb){
                                    if (runif(1) < PerSymptomatic){
                                        ActivePop[SusAgentIndex[k,1], SusAgentIndex[k,2]] = 4;
                                        
                                    } else {
                                        ActivePop[SusAgentIndex[k,1], SusAgentIndex[k,2]] = 1;
                                    }
                                    CountDownDetection[SusAgentIndex[k,1], SusAgentIndex[k,2]] = tDet;
                                    CountDownInfectious[SusAgentIndex[k,1], SusAgentIndex[k,2]] = tInf;
                                    CountDownRecovery[SusAgentIndex[k,1], SusAgentIndex[k,2]] = tRec;
                                }
                            }
                        }
                    }
                    
                    #end prev bool
                }
                
                #Day is over: remove a day from each timer
                CountDownDetection = CountDownDetection - 1;
                CountDownInfectious = CountDownInfectious - 1;
                CountDownRecovery = CountDownRecovery - 1;
                
                #Track data
                NumberOfInfections[i,j] = sum((ActivePop==1)) + sum((ActivePop==4)) ;
                NumberOfRecoveries[i,j] = sum((ActivePop==2)) +sum(ActivePop==5);
                NumberOfCasesDetected[i,j] = sum(ActivePop == 6)+sum(ActivePop==3) +sum(ActivePop==5) ;
                HealthyPeople[i,j]= sum(ActivePop==0) + sum(ActivePop==3);
                
                NumberOfSympt[i,j] = sum(ActivePop==4);
                NumberOfAsympt[i,j] = sum(ActivePop==1);
                
                IsolatingInfections[i,j] = sum(ActivePop == 6);
                IsolatingHealthy[i,j] = sum(ActivePop==3);
                NumberOfIsolatingRecovered[i,j] = sum(ActivePop==5);
                NumberOfIsolatingHealthy[i,j] = sum(ActivePop==3);
                
                #we don't record absences from education on weekends
                if ( j %% 7 !=1 || j %% 7  != 0){
                  TotalAbsentDays[i] = TotalAbsentDays[i] + NumberOfCasesDetected[i,j];
                }
                
   
            }
            
            
            # keep track if all agents become infected
            if (sum(ActivePop==0) == 0){
                TotalOutbreakCounter[i] = 1;
            }
            
            #Update progress bar
            count <- count + 1
            progress$set(value = count)
        }
        
        # summary data output
        z_value = 1.96;
        Days = c(1:length(MixingDays));
        Mean_Number_of_active_infected = colMeans(NumberOfInfections)
        Mean_Number_of_recoveries= colMeans(NumberOfRecoveries)
        Mean_Number_of_active_isolations = colMeans(NumberOfCasesDetected)
        Mean_total_number_of_infections = colMeans(Ntot-HealthyPeople)
        
        Data_output = data.frame(Day = Days,Mean_Number_of_active_infected,Mean_Number_of_recoveries,Mean_Number_of_active_isolations,Mean_total_number_of_infections)
        
        TotInfections = Ntot-HealthyPeople
        FinalTotInfections = (TotInfections[,length(Days)])
        FinalTotRecoveries = NumberOfRecoveries[,length(Days)]
        NumberOfTotalOutbreaks = sum(TotalOutbreakCounter)
        AbsentDaysInfectionRatio = TotalAbsentDays/FinalTotInfections
        
        
        for (i in 1:length(AbsentDaysInfectionRatio)){
          if (is.infinite(AbsentDaysInfectionRatio[i])){
            AbsentDaysInfectionRatio[i] = 0;
          }
        }
        
        storingValues$count_check = storingValues$count_check + 1;
        storingValues$SummaryResults = rbind(storingValues$SummaryResults,c(storingValues$count_check,mean(FinalTotInfections), mean(FinalTotRecoveries),(NumberOfTotalOutbreaks/NoS)*100,  round(mean(AbsentDaysInfectionRatio),digits = 2) ))

        MeanTotalInfections=colMeans(Ntot-HealthyPeople);
        MeanTotalInfections_Pos95 =  MeanTotalInfections + z_value*apply((Ntot-HealthyPeople),2,sd)
        MeanTotalInfections_Neg95 =  MeanTotalInfections - z_value*apply((Ntot-HealthyPeople),2,sd)
        
        MeanNumberOfInfections = colMeans(NumberOfInfections);
        MeanNumberOfInfections_Pos95 = MeanNumberOfInfections + z_value*apply((NumberOfInfections),2,sd)
        MeanNumberOfInfections_Neg95 = MeanNumberOfInfections - z_value*apply((NumberOfInfections),2,sd)
        
        MeanNumberOfRecoveries = colMeans(NumberOfRecoveries);
        MeanNumberOfRecoveries_Pos95 = MeanNumberOfRecoveries  + z_value*apply((NumberOfRecoveries),2,sd)
        MeanNumberOfRecoveries_Neg95 = MeanNumberOfRecoveries  - z_value*apply((NumberOfRecoveries),2,sd)
        
        MeanNumberOfCasesDetected = colMeans(NumberOfCasesDetected);
        MeanNumberOfCasesDetected_Pos95 = MeanNumberOfCasesDetected  + z_value*apply((NumberOfCasesDetected),2,sd)
        MeanNumberOfCasesDetected_Neg95 = MeanNumberOfCasesDetected - z_value*apply((NumberOfCasesDetected),2,sd)
        
        for (i in 1:length(Days)){
            if (MeanTotalInfections_Pos95[i] > Ntot){
                MeanTotalInfections_Pos95[i] = Ntot
            }
            if (MeanTotalInfections_Neg95[i] < 0){
                MeanTotalInfections_Neg95[i] = 0
            }
            
            if (MeanNumberOfInfections_Pos95[i] > Ntot){
                MeanNumberOfInfections_Pos95[i] = Ntot
            }
            if (MeanNumberOfInfections_Neg95[i] < 0){
                MeanNumberOfInfections_Neg95[i] = 0
            }
            
            if (MeanNumberOfRecoveries_Pos95[i] > Ntot){
                MeanNumberOfRecoveries_Pos95[i] = Ntot
            }
            if (MeanNumberOfRecoveries_Neg95[i] < 0){
                MeanNumberOfRecoveries_Neg95[i] = 0
            }
          
          if (MeanNumberOfCasesDetected_Pos95[i] > Ntot){
            MeanNumberOfCasesDetected_Pos95[i] = Ntot
          }
          if (MeanNumberOfCasesDetected_Neg95[i] < 0){
            MeanNumberOfCasesDetected_Neg95[i] = 0
          }
            
        }
        
        #change to percentage of total pop
        MeanTotalInfections = (MeanTotalInfections/Ntot)*100
        MeanTotalInfections_Pos95 = (MeanTotalInfections_Pos95/Ntot)*100
        MeanTotalInfections_Neg95 = (MeanTotalInfections_Neg95/Ntot)*100
        
        MeanNumberOfInfections = (MeanNumberOfInfections/Ntot)*100
        MeanNumberOfInfections_Pos95 = (MeanNumberOfInfections_Pos95/Ntot)*100
        MeanNumberOfInfections_Neg95 = (MeanNumberOfInfections_Neg95/Ntot)*100
        
        MeanNumberOfRecoveries = (MeanNumberOfRecoveries/Ntot)*100
        MeanNumberOfRecoveries_Pos95 = (MeanNumberOfRecoveries_Pos95/Ntot)*100
        MeanNumberOfRecoveries_Neg95 = (MeanNumberOfRecoveries_Neg95/Ntot)*100
        
        MeanNumberOfCasesDetected = (MeanNumberOfCasesDetected/Ntot)*100
        MeanNumberOfCasesDetected_Pos95 = (MeanNumberOfCasesDetected_Pos95/Ntot)*100
        MeanNumberOfCasesDetected_Neg95 = (MeanNumberOfCasesDetected_Neg95/Ntot)*100
        
        
        #plot the total number of infections
        output$TotInfections <- renderPlot({
            totInf <- data.frame(Days,MeanTotalInfections)
            
            ggplot(totInf, aes(x=Days, y=MeanTotalInfections)) +
                geom_line( color="#D55E00", size=1.5, alpha=0.8, linetype=1) +
                geom_ribbon(
                            aes(ymin=MeanTotalInfections_Neg95,ymax=MeanTotalInfections_Pos95 ), fill="#D55E00", alpha=0.1) + 
                geom_line(aes(x=Days,y = MeanTotalInfections_Pos95), color="#D55E00", size=1, alpha=0.4, linetype=1) +
                geom_line(aes(x=Days,y = MeanTotalInfections_Neg95), color="#D55E00", size=1, alpha=0.4, linetype=1) +
                geom_line(aes(x=Days,y = MeanTotalInfections), color="#D55E00", size=1.5, alpha=1, linetype=1) +
              theme(text = element_text(size=20),axis.line = element_line(colour = "grey"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank()) + xlab("Time (Days)") + ylab("Total mean infections (%)") + xlim(1,length(Days)) #+ ylim(0,Ntot)
                })
      
        
        #plot the total number of active infections
        output$NumActiveInfections <- renderPlot({
            totInf <- data.frame(Days,MeanNumberOfInfections)
            
            ggplot(totInf, aes(x=Days, y=MeanNumberOfInfections)) +
                geom_line( color="#0072B2", size=1.5, alpha=0.8, linetype=1) +
                geom_ribbon(
                    aes(ymin=MeanNumberOfInfections_Neg95,ymax=MeanNumberOfInfections_Pos95 ), fill="#0072B2", alpha=0.1) + 
                geom_line(aes(x=Days,y = MeanNumberOfInfections_Pos95), color="#0072B2", size=1, alpha=0.4, linetype=1) +
                geom_line(aes(x=Days,y = MeanNumberOfInfections_Neg95), color="#0072B2", size=1, alpha=0.4, linetype=1) +
                geom_line(aes(x=Days,y = MeanNumberOfInfections), color="#0072B2", size=1.5, alpha=1, linetype=1) +
              theme(text = element_text(size=20),axis.line = element_line(colour = "grey"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank()) + xlab("Time (Days)") + ylab("Mean active infections (%)") + xlim(1,length(Days)) #+ ylim(0,Ntot)
               })
        
        output$NumActiveRecoveriesPlot <- renderPlot({
          totInf <- data.frame(Days,MeanNumberOfCasesDetected)
          
          ggplot(totInf, aes(x=Days, y=MeanNumberOfCasesDetected)) +
            geom_line( color="#009E73", size=1.5, alpha=0.8, linetype=1) +
            geom_ribbon(
              aes(ymin=MeanNumberOfCasesDetected_Neg95,ymax=MeanNumberOfCasesDetected_Pos95 ), fill="#009E73", alpha=0.1) + 
            geom_line(aes(x=Days,y = MeanNumberOfCasesDetected_Pos95), color="#009E73", size=1, alpha=0.4, linetype=1) +
            geom_line(aes(x=Days,y = MeanNumberOfCasesDetected_Neg95), color="#009E73", size=1, alpha=0.4, linetype=1) +
            geom_line(aes(x=Days,y = MeanNumberOfCasesDetected), color="#009E73", size=1.5, alpha=1, linetype=1) +
            theme(text = element_text(size=20),axis.line = element_line(colour = "grey"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank()) + xlab("Time (Days)") + ylab("Mean active isolations (%)") + xlim(1,length(Days)) #+ ylim(0,Ntot)
        })
  
        
        output$NumRecoveriesPlot <- renderPlot({
          totInf <- data.frame(FinalTotInfections,TotalAbsentDays)
          
          ggplot(totInf, aes(x=FinalTotInfections, y=TotalAbsentDays)) +
            geom_point( size=5, alpha=0.2) +
            geom_segment(aes(x = min(FinalTotInfections)*(0.95) , y = mean(TotalAbsentDays), xend = mean(FinalTotInfections), yend = mean(TotalAbsentDays)),linetype=2,size = 1, color = "turquoise3")+
            geom_segment(aes(x = mean(FinalTotInfections), y = min(TotalAbsentDays)*0.95, xend = mean(FinalTotInfections), yend = mean(TotalAbsentDays)),linetype=2,size = 1, color = "turquoise3")+
            geom_point(size=5,aes(x=mean(FinalTotInfections), y=mean(TotalAbsentDays)),color = "turquoise3") + 
            theme(text = element_text(size=20),axis.line = element_line(colour = "grey"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank()) + xlab("Total mean number of infections") + ylab("Total mean number of absent days") 
        
        })
        
        output$SummaryTable <- renderTable(na.omit(as.data.frame(storingValues$SummaryResults)), align = c("c"), striped = FALSE, digits = 2)
        
        
        output$SaveResultsButton<- downloadHandler(
          filename = function() {
            "SummaryResults.xlsx"
          },
          content = function(file) {
            write_xlsx(list("Data" = Data_output,  "Parameter values" = ParameterValues,"Frequencies" = Frequencies), file)
          }
        )
        
    })
    

})
