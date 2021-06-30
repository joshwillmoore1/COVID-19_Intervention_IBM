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


tic
[MeanNumOfHealthy,MeanTotalInfections,MeanNumberOfSympt...
    ,MeanNumberOfAsympt,initTestingDay,MeanNumOfRecoveries,...
    MeanNumberOfCasesDetected,MeanProportionOfInfectedIsolations] = General_Delay_LFT(Para);
toc



%% plot the results
close all
figure()
subplot(1,2,1)
plot(1:28,MeanTotalInfections)
ylabel({"Mean number"," of total infections"})
xlabel("Number of days")
ylim([0,Ntot])


subplot(1,2,2)
plot(1:28,MeanNumberOfCasesDetected)
ylabel({"Mean number" ,"of isolating individuals"})
xlabel("Number of days")
ylim([0,Ntot])

