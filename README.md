# COVID-19_Intervention_IBM

Link to the applet: https://bit.ly/CV19_INTER_IBM

<h2>The model</h2>
This application uses a stochastic agent-based model to simulate infection transimission of COVID-19 coupled with various non-pharmaceutical interventions in educational settings. The employed model allows for the agents to be grouped together to simulate the affect of close range contact on transmission and various isolation policies, for example, the subgroups can be considered as tables groups in a classroom or classmembers within a school of agents. In addition, the software allows for control of agent mixing and testing frequencies over a characteristic week. For more details on the agent-based infection transmission model, see the reference text which can be found [here.](https://www.medrxiv.org/content/10.1101/2021.03.08.21253122v1)

<h2>Data</h2>
The data used to update the parmaters is sourced from the offical UK Government COVID-19 database, which can be found [here.](https://coronavirus.data.gov.uk/details/download) On startup, this software retrieves the most up-to-date information for the transmission rates, positive tests and administered tests in Wales from the database. The background prevalence is estimated as the a severn day average of the percentage of postive cases in total daily tests administered. As the transmission data is updated biweekly, the R number is estimated using the most recent data entries only.

<h2>Applet</h2>
The web-app was created using the Shiny library in R. In the Source_code folder, we provide full access to the source code for both the simulator and the UI. In addition, we also provide the original Matlab code for the computations used in the reference text. 


