clc
clear 
%testing only starts at the sign of a symptomatic in the class 

Ntot = 30; %total pop. size
SubgroupSize =  5; %number of individuals in each group
Irb = 2; %Background prevalence
R = 4; %R number
Cf = 1; %far R ratio
Cc = 1; %close R ratio
PerFalseNeg = 20; %percentage of false negatives from tests
CompIso = 100; %compliance with isolation for any individual
MixInd = 1;%see mixing days in genral function
TestInd = 2; %this now goes into the changeTestingSchedule function
PrevBool = false;
RecoveryBool = 1; %bool  for infected recovery
PerSymptomatic = 30; %percentage of symptomatic
RemoveSympt = 1; %Boolean to remove those that show symptoms
PerFalsePos = 0.3; %percentage of false positives
InitWithTesting = true; %start simulations with testing days active - false means to wait for sympto
SocialDistancingModifier = 1;



Para = [Ntot,SubgroupSize,Irb,R,Cf,Cc,PerFalseNeg,CompIso,MixInd,TestInd...
    ,PrevBool,RecoveryBool,PerSymptomatic,RemoveSympt,PerFalsePos,InitWithTesting,SocialDistancingModifier ];

Para08 = [Ntot,SubgroupSize,Irb,R,Cf,Cc,PerFalseNeg,CompIso,MixInd,TestInd...
    ,PrevBool,RecoveryBool,PerSymptomatic,RemoveSympt,PerFalsePos,InitWithTesting,SocialDistancingModifier, 0.4 ];

Para1 = [Ntot,SubgroupSize,Irb,R,Cf,Cc,PerFalseNeg,CompIso,MixInd,TestInd...
    ,PrevBool,RecoveryBool,PerSymptomatic,RemoveSympt,PerFalsePos,InitWithTesting,SocialDistancingModifier,0.6 ];

Para12 = [Ntot,SubgroupSize,Irb,R,Cf,Cc,PerFalseNeg,CompIso,MixInd,TestInd...
    ,PrevBool,RecoveryBool,PerSymptomatic,RemoveSympt,PerFalsePos,InitWithTesting,SocialDistancingModifier,0.8 ];

tic
[MeanNumOfHealthy,MeanTotalInfections,MeanNumberOfSympt...
    ,MeanNumberOfAsympt,initTestingDay,MeanNumOfRecoveries,...
    MeanNumberOfCasesDetected,MeanProportionOfInfectedIsolations] = General_Delay_LFT(Para);
toc

tic
[MeanNumOfHealthy_ln,MeanTotalInfections_ln08,MeanNumberOfSympt_ln...
    ,MeanNumberOfAsympt_ln,initTestingDay_ln,MeanNumOfRecoveries_ln,...
    MeanNumberOfCasesDetected_ln08,MeanProportionOfInfectedIsolations_ln] = General_Delay_LFT_LN_NORM(Para08);
toc

tic
[MeanNumOfHealthy_ln,MeanTotalInfections_ln1,MeanNumberOfSympt_ln...
    ,MeanNumberOfAsympt_ln,initTestingDay_ln,MeanNumOfRecoveries_ln,...
    MeanNumberOfCasesDetected_ln1,MeanProportionOfInfectedIsolations_ln] = General_Delay_LFT_LN_NORM(Para1);
toc

tic
[MeanNumOfHealthy_ln,MeanTotalInfections_ln12,MeanNumberOfSympt_ln...
    ,MeanNumberOfAsympt_ln,initTestingDay_ln,MeanNumOfRecoveries_ln,...
    MeanNumberOfCasesDetected_ln12,MeanProportionOfInfectedIsolations_ln] = General_Delay_LFT_LN_NORM(Para12);
toc


%% plot the results
close all
figure()
subplot(1,2,1)
plot(1:28,MeanTotalInfections)
hold on
plot(1:28,MeanTotalInfections_ln08)
plot(1:28,MeanTotalInfections_ln1)
plot(1:28,MeanTotalInfections_ln12)
ylabel({"Mean number"," of total infections"})
xlabel("Number of days")
legend("Poisson","Log-normal $\sigma = 0.4$","Log-normal $\sigma = 0.6$","Log-normal $\sigma = 0.8$",'location','best')
ylim([0,Ntot])


subplot(1,2,2)
plot(1:28,MeanNumberOfCasesDetected)
hold on
plot(1:28,MeanNumberOfCasesDetected_ln08)
plot(1:28,MeanNumberOfCasesDetected_ln1)
plot(1:28,MeanNumberOfCasesDetected_ln12)
ylabel({"Mean number" ,"of isolating individuals"})
xlabel("Number of days")
legend("Poisson","Log-normal $\sigma = 0.4$","Log-normal $\sigma = 0.6$","Log-normal $\sigma = 0.8$",'location','best')
ylim([0,Ntot])

