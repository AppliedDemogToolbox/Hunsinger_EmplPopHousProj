##############################################################################################################################
##############################################################################################################################
##R CODE FOR AN EMPLOYMENT-BASED POPULATION AND HOUSING PROJECTION SHINY APP
##
##EDDIE HUNSINGER (AFFILIATION: ALASKA DEPARTMENT OF LABOR AND WORKFORCE DEVELOPMENT), MARCH 2014 (UPDATED FOR SHINY APP 2025 PROJECTION IN DECEMBER 2015, LAST UPDATED IN JULY 2025)
##https://edyhsgr.github.io/
##edyhsgr@protonmail.com
##
##THIS IS BASED ON R CODE AVAILABLE AT: https://applieddemogtoolbox.github.io/#EmplPopHousProj
##IF YOU WOULD LIKE TO USE, SHARE OR REPRODUCE ANY INFORMATION OR IDEAS FROM THIS WORK, BE SURE TO CITE THE SOURCE
##
##
##THE INPUTS USED HERE ARE NOT OFFICIAL OR CAREFULLY DEVELOPED OR USEFUL FOR ANY PARTICULAR AREA, AND SHOULD ONLY BE USED FOR EXAMPLE
##THERE IS NO WARRANTY FOR THIS CODE
##THIS CODE HAS NOT BEEN CAREFULLY REVIEWED
##############################################################################################################################
##############################################################################################################################

library(shiny)
ui<-fluidPage(
	tags$h3("Alaska 2025: An Employment-Based Population and Housing Projection Interface"),
	p("Related",
	tags$a(href="https://www.r-project.org/", "R"),
	"code and information about the methodology:",
	tags$a(href="https://applieddemogtoolbox.github.io/#EmplPopHousProj", 
	"Eddie's Employment-Based Population and Housing Projection Code")
),

hr(),

sidebarLayout(

sidebarPanel(
	numericInput(inputId = "EmpGrowth15to20",
		label = "Average annual civilian employment growth for 2015 to 2020",
		value = 1000, min = -20000, max = 20000,width=250,step=100),
	numericInput(inputId = "EmpGrowth20to25",
		label = "Average annual civilian employment growth for 2020 to 2025",
		value = 1000, min = -20000, max = 20000,width=250,step=100),	
	numericInput(inputId = "LF_2020",
		label = "Civilian labor force share of working age population in 2020",
		value = .66, min = .25, max = 1,width=250,step=.01),
	numericInput(inputId = "LF_2025",
		label = "Civilian labor force share of working age population in 2025",
		value = .65, min = .25, max = 1,width=250,step=.01),
	numericInput(inputId = "UE_2020",
		label = "Unemployment rate in 2020",
		value = .07, min = 0, max = .25,width=250,step=.005),
	numericInput(inputId = "UE_2025",
		label = "Unemployment rate in 2025",
		value = .07, min = 0, max = .25,width=250,step=.005),
	numericInput(inputId = "PPH_2020",
		label = "Persons per household in 2020",
		value = 2.7, min = 1, max = 5,width=250,step=.1),
	numericInput(inputId = "PPH_2025",
		label = "Persons per household in 2025",
		value = 2.7, min = 1, max = 5,width=250,step=.1),
	numericInput(inputId = "VAC_2020",
		label = "Vacancy rate in 2020 (this is census vacancy, not market vacancy)",
		value = .14, min = 0, max = .5,width=250,step=.01),
	numericInput(inputId = "VAC_2025",
		label = "Vacancy rate in 2025 (this is census vacancy, not market vacancy)",
		value = .14, min = 0, max = .5,width=250,step=.01),
	numericInput(inputId = "TFR_2020",
		label = "Total fertility rate for 2015 to 2020",
		value = 2.2, min = 1, max = 4,width=250,step=.1),
	numericInput(inputId = "TFR_2025",
		label = "Total fertility rate for 2020 to 2025",
		value = 2.2, min = 1, max = 4,width=250,step=.1),

	tags$small(paste0(
	"Other variables for the projection include: out-migration, commuter rate, women's share of total civilian employment, and brass alpha (for mortality)."
        )),
	tags$small(paste0(
	"The 2010 and 2015 data on this page are projected from 2005 population estimates."
        )),
	tags$small(paste0(
	"Related R code and information about the methodology is available at: https://applieddemogtoolbox.github.io/#EmplPopHousProj ('Eddie's Employment-Based Population and Housing Projection Code')."
	)),
	tags$small(paste0(
	"The interface was made with Shiny (http://shiny.rstudio.com/)."
	)),
	tags$small(paste0(
	"Eddie Hunsinger, December 2015 (updated July 2025)."
	)),
width=3
),

mainPanel(
	plotOutput("plots"),width=9
))
)

server<-function(input, output) {
	output$plots<-renderPlot({
par(mfrow=c(2,2))
	
##############################################################################################################################
##############################################################################################################################
##SELECT THE INPUT DATA
##############################################################################################################################
##############################################################################################################################

#############################################################################################################################
#############################################################################################################################
##DIMENSIONS
##SIZE OF PROJECTION MATRIX
SIZE<-21

##NUMBER OF PROJECTION STEPS
STEPS<-4
BASEANDSTEPS<-STEPS+1

##EMPLOYMENT PARAMETERS
##
##TOTAL CIVILIAN EMPLOYMENT (INCLUDING SELF-EMPLOYED) FORECAST IN FIVE YEAR STEPS FOR 2005 THROUGH 2025 (POINT IN TIME)
TE_base<-323000
TE_cons<-c(TE_base,338000,353000,0,0)
TE_cons[4]<-TE_cons[3]+input$EmpGrowth15to20*5
TE_cons[5]<-TE_cons[4]+input$EmpGrowth20to25*5

##CIVILIAN LABOR FORCE SHARE OF TOTAL WORKING AGE POPULATION FORECAST IN FIVE YEAR STEPS FOR 2005 THROUGH 2025 (POINT IN TIME)
##THIS IS A LABOR FORCE PARTICIPATION INDEX - SET TO 69 PERCENT AND TO DECLINE 2 PERCENT PER DECADE
##MILITARY WOULD ADD SOME PERCENT TO IT (THIS IS CIVILIAN-BASED) - COULD BRING IN COUNTS OF THE RESIDENTS THAT ARE ACTIVE DUTY MILITARY
LF_base<-.685
LF_cons<-c(LF_base,.67,.665,input$LF_2020,input$LF_2025)

##UNEMPLOYMENT RATE FORECAST IN FIVE YEAR STEPS FOR 2005 THROUGH 2035 (POINT IN TIME)
##SET TO 6 PERCENT EXCEPT IN SECOND STEP 8 PERCENT
UE_base<-.07
UE_cons<-c(UE_base,.08,.065,input$UE_2020,input$UE_2025)

##COMMUTER RATE (SHARE COMMUTING TO WORK FROM OUTSIDE) FORECAST IN FIVE YEAR STEPS FOR 2005 THROUGH 2025 (POINT IN TIME)
##SET HERE AS A TINY FRACTION JUST TO EXHIBIT - LESS THAN 1 PERCENT
COMM_base<-.00
COMM_cons<-c(COMM_base,.00,.00,.00,.00)

##WOMENS SHARE OF TOTAL CIVILIAN EMPLOYMENT - SET TO 48 PERCENT AND TO 49 PERCENT STARTING ONE DECADE OUT
EmpF_base<-.48
EmpF_cons<-c(EmpF_base,.48,.49,.49,.49)

##HOUSING PARAMETERS
##
##PERSONS PER HOUSEHOLD IN FIVE YEAR STEPS FOR 2005 THROUGH 2025 (POINT IN TIME)
PPH_base<-2.7
PPH_cons<-c(PPH_base,2.7,2.7,input$PPH_2020,input$PPH_2025)

##VACANCY RATE IN FIVE YEAR STEPS FOR 2005 THROUGH 2025 (POINT IN TIME)
##THIS IS CENSUS VACANCY (PRIMARY RESIDENCE), NOT MARKET VACANCY - SET TO 15 PERCENT
VAC_base<-.15
VAC_cons<-c(VAC_base,.14,.14,input$VAC_2020,input$VAC_2025)

##SURVIVAL PARAMETERS
##
##YEAR 2000 US LIFE TABLE lx CURVES
Survival<-read.table(file="https://raw.githubusercontent.com/edyhsgr/Hunsinger_EmplPopHousProj/refs/heads/master/Inputs/lx2000_US_NCHS.csv",header=TRUE,sep=",")
lxF<-Survival$F_2000
lxM<-Survival$M_2000

##"BA" IS THE BRASS RELATIONAL LOGIT MODEL ALPHA. IT IS CALIBRATED FOR THE JUMP-OFF PERIOD AND SET TO FOLLOW A PATH OF INCREASE
##OF .03 FOR EACH FIVE-YEAR STEP
BA_baseF<-.03
BA_baseM<-.08
BA_consF<-.03
BA_consM<-.03

##FERTILITY PARAMETERS
##
##YEAR 2005 US FERTILITY RATES, SUMMED TO 1
Fertility<-read.table(file="https://raw.githubusercontent.com/edyhsgr/Hunsinger_EmplPopHousProj/refs/heads/master/Inputs/Fx2005_US_NCHS.csv",header=TRUE,sep=",")
PropFx<-c(Fertility$PropFx)

##FRACTION FEMALE AT BIRTH
ffab<-.4886

##"TFR" IS THE TOTAL FERTILITY RATE. IT IS SET TO 2.3 FOR EACH FORECAST STEP
TFR_base<-2.35
TFR_cons<-c(TFR_base,2.3,2.25,input$TFR_2020,input$TFR_2025)

##MIGRATION PARAMETERS
##
##MIGRATION PROFILES, SUMMED TO 1 FOR IN AND OUT
Migration<-read.table(file="https://raw.githubusercontent.com/edyhsgr/Hunsinger_EmplPopHousProj/refs/heads/master/Inputs/MigProf2004to2014.csv",header=TRUE,sep=",")
PropInM<-c(Migration$MIn)
PropInF<-c(Migration$FIn)
PropOutM<-c(Migration$MOut)
PropOutF<-c(Migration$FOut)

##THE OUT MIGRATION RATE, WHICH IS SET TO 140,000 (DUE TO RETURN MIGRATION: ANNUALOUT*~3to3.5 RATHER THAN ANNUALOUT*5) FOR EACH FIVE YEAR FORECAST STEP
OutRate<-140000

##BASE POPULATION
##
##POPULATION ESTIMATES BY AGE AND SEX
K05<-read.table(file="https://raw.githubusercontent.com/edyhsgr/Hunsinger_EmplPopHousProj/refs/heads/master/Inputs/AgeSex2005.csv",header=TRUE,sep=",")
KF05<-K05$F_2005
KM05<-K05$M_2005

##############################################################################################################################
##############################################################################################################################
##RUN THE FORECAST CODE
##############################################################################################################################
##############################################################################################################################
##SURVIVAL
BAF<-array(0,c(BASEANDSTEPS))
BAF[1]<-BA_baseF
for(i in 2:BASEANDSTEPS){BAF[i]<-BAF[i-1]+BA_consF}
BAF<-t(BAF)
BrassF00<-data.frame(Alpha=BAF[1],Beta=1)
BrassF05<-data.frame(Alpha=BAF[2],Beta=1)
BrassF10<-data.frame(Alpha=BAF[3],Beta=1)
BrassF15<-data.frame(Alpha=BAF[4],Beta=1)
BrassF20<-data.frame(Alpha=BAF[5],Beta=1)
BAM<-array(0,c(BASEANDSTEPS))
BAM[1]<-BA_baseM
for(i in 2:BASEANDSTEPS){BAM[i]<-BAM[i-1]+BA_consM}
BAM<-t(BAM)
BrassM00<-data.frame(Alpha=BAM[1],Beta=1)
BrassM05<-data.frame(Alpha=BAM[2],Beta=1)
BrassM10<-data.frame(Alpha=BAM[3],Beta=1)
BrassM15<-data.frame(Alpha=BAM[4],Beta=1)
BrassM20<-data.frame(Alpha=BAM[5],Beta=1)

##FERTILITY
fmab <- 1-ffab
Fx<-array(0,c(SIZE))
Fx[1:SIZE]<-PropFx
TFRFORE<-array(0,c(BASEANDSTEPS))
TFRFORE[1]<-TFR_base
for(i in 2:BASEANDSTEPS){TFRFORE[i]<-TFR_cons[i]}
TFRFORE<-t(TFRFORE)

##EMPLOYMENT BASED WORKING AGE POPULATION
EmpBasedPop2010<-((TE_cons[2]*(1-COMM_cons[2]))*(1/(1-UE_cons[2])))*(1/(LF_cons[2]))
EmpBasedPop2015<-((TE_cons[3]*(1-COMM_cons[3]))*(1/(1-UE_cons[3])))*(1/(LF_cons[3]))
EmpBasedPop2020<-((TE_cons[4]*(1-COMM_cons[4]))*(1/(1-UE_cons[4])))*(1/(LF_cons[4]))
EmpBasedPop2025<-((TE_cons[5]*(1-COMM_cons[5]))*(1/(1-UE_cons[5])))*(1/(LF_cons[5]))

##CALCULATE THE Yx FOR THE lx'S
YxM<-YxF<-NULL
for (i in 1:length(lxF)){YxF[i]<-.5*log(lxF[i]/(1-lxF[i]))}
for (i in 1:length(lxM)){YxM[i]<-.5*log(lxM[i]/(1-lxM[i]))}

##IMPROVE SURVIVAL AND MAKE lx's FOR EACH PERIOD
lxF20<-lxF15<-lxF10<-lxF05<-lxF00<-array(0,c(SIZE+1))
lxM20<-lxM15<-lxM10<-lxM05<-lxM00<-array(0,c(SIZE+1))
for (i in 1:length(lxF)){lxF00[i]<-1/(1+exp(-2*BrassF00$Alpha-2*BrassF00$Beta*YxF[i]))}
for (i in 1:length(lxM)){lxM00[i]<-1/(1+exp(-2*BrassM00$Alpha-2*BrassM00$Beta*YxM[i]))}
for (i in 1:length(lxF)){lxF05[i]<-1/(1+exp(-2*BrassF05$Alpha-2*BrassF05$Beta*YxF[i]))}
for (i in 1:length(lxM)){lxM05[i]<-1/(1+exp(-2*BrassM05$Alpha-2*BrassM05$Beta*YxM[i]))}
for (i in 1:length(lxF)){lxF10[i]<-1/(1+exp(-2*BrassF10$Alpha-2*BrassF10$Beta*YxF[i]))}
for (i in 1:length(lxM)){lxM10[i]<-1/(1+exp(-2*BrassM10$Alpha-2*BrassM10$Beta*YxM[i]))}
for (i in 1:length(lxF)){lxF15[i]<-1/(1+exp(-2*BrassF15$Alpha-2*BrassF15$Beta*YxF[i]))}
for (i in 1:length(lxM)){lxM15[i]<-1/(1+exp(-2*BrassM15$Alpha-2*BrassM15$Beta*YxM[i]))}
for (i in 1:length(lxF)){lxF20[i]<-1/(1+exp(-2*BrassF20$Alpha-2*BrassF20$Beta*YxF[i]))}
for (i in 1:length(lxM)){lxM20[i]<-1/(1+exp(-2*BrassM20$Alpha-2*BrassM20$Beta*YxM[i]))}

##MAKE nLx's FOR EACH PERIOD
LxF20<-LxF15<-LxF10<-LxF05<-array(0,c(SIZE))
LxM20<-LxM15<-LxM10<-LxM05<-array(0,c(SIZE))
##**THIS IS A LITTLE OFF FOR THE FIRST AGE GROUP**
for (i in 1:SIZE){LxF05[i]<-.5*(lxF05[i]+lxF05[i+1])}
for (i in 1:SIZE){LxM05[i]<-.5*(lxM05[i]+lxM05[i+1])}
for (i in 1:SIZE){LxF10[i]<-.5*(lxF10[i]+lxF10[i+1])}
for (i in 1:SIZE){LxM10[i]<-.5*(lxM10[i]+lxM10[i+1])}
for (i in 1:SIZE){LxF15[i]<-.5*(lxF15[i]+lxF15[i+1])}
for (i in 1:SIZE){LxM15[i]<-.5*(lxM15[i]+lxM15[i+1])}
for (i in 1:SIZE){LxF20[i]<-.5*(lxF20[i]+lxF20[i+1])}
for (i in 1:SIZE){LxM20[i]<-.5*(lxM20[i]+lxM20[i+1])}

##TABLE e0
e0MFORE<-array(0,c(BASEANDSTEPS))
e0MFORE[2]<-sum(LxM05*5)
e0MFORE[3]<-sum(LxM10*5)
e0MFORE[4]<-sum(LxM15*5)
e0MFORE[5]<-sum(LxM20*5)

e0FFORE<-array(0,c(BASEANDSTEPS))
e0FFORE[2]<-sum(LxF05*5)
e0FFORE[3]<-sum(LxF10*5)
e0FFORE[4]<-sum(LxF15*5)
e0FFORE[5]<-sum(LxF20*5)

##MAKE nSx's FOR EACH PERIOD
SxF20<-SxF15<-SxF10<-SxF05<-array(0,c(SIZE-1))
SxM20<-SxM15<-SxM10<-SxM05<-array(0,c(SIZE-1))
for (i in 1:SIZE-1){SxF05[i]<-(LxF05[i+1]/LxF05[i])}
for (i in 1:SIZE-1){SxM05[i]<-(LxM05[i+1]/LxM05[i])}
for (i in 1:SIZE-1){SxF10[i]<-(LxF10[i+1]/LxF10[i])}
for (i in 1:SIZE-1){SxM10[i]<-(LxM10[i+1]/LxM10[i])}
for (i in 1:SIZE-1){SxF15[i]<-(LxF15[i+1]/LxF15[i])}
for (i in 1:SIZE-1){SxM15[i]<-(LxM15[i+1]/LxM15[i])}
for (i in 1:SIZE-1){SxF20[i]<-(LxF20[i+1]/LxF20[i])}
for (i in 1:SIZE-1){SxM20[i]<-(LxM20[i+1]/LxM20[i])}

##PUT THE Sx DATA INTO THE SUBDIAGONAL OF WHAT WILL BE THE LESLIE MATRICES
SF20<-SF15<-SF10<-SF05<-array(0,c(SIZE,SIZE))
SM20<-SM15<-SM10<-SM05<-array(0,c(SIZE,SIZE))
SF05<-rbind(0,cbind(diag(SxF05),0))
SF10<-rbind(0,cbind(diag(SxF10),0))
SF15<-rbind(0,cbind(diag(SxF15),0))
SF20<-rbind(0,cbind(diag(SxF20),0))
SM05<-rbind(0,cbind(diag(SxM05),0))
SM10<-rbind(0,cbind(diag(SxM10),0))
SM15<-rbind(0,cbind(diag(SxM15),0))
SM20<-rbind(0,cbind(diag(SxM20),0))

##PUT FERTILITY INTO AGE PROFILES
TFR2005<-TFRFORE[2]
TFR2010<-TFRFORE[3]
TFR2015<-TFRFORE[4]
TFR2020<-TFRFORE[5]

Fert2005<-TFR2005*Fx
Fert2010<-TFR2010*Fx
Fert2015<-TFR2015*Fx
Fert2020<-TFR2020*Fx

##MAKE MIGRATION AGE PROFILES
IxM<-array(0,c(SIZE))
IxM[1:SIZE]<-PropInM
IxF<-array(0,c(SIZE))
IxF[1:SIZE]<-PropInF

OxM<-array(0,c(SIZE))
OxM[1:SIZE]<-PropOutM
OxF<-array(0,c(SIZE))
OxF[1:SIZE]<-PropOutF

##WORKER MIGRATION SHARES
WAIxF<-sum(IxF[5:length(IxF)])+IxF[4]*4/5
WAIxM<-sum(IxM[5:length(IxM)])+IxM[4]*4/5
FIxF<-WAIxF/(WAIxF+WAIxM)
FIxM<-1-FIxF

##MAKE THE LESLIE MATRICES FOR FEMALES
BF20<-BF15<-BF10<-BF05<-0*SF05
for(j in 1:SIZE-1)
  {BF05[1,j]<-(LxF05[1]/2)*(Fert2005[j]+Fert2005[j+1]*(SxF05[j]))*ffab}
AF05 = SF05 + BF05
for(j in 1:SIZE-1)
  {BF10[1,j]<-(LxF10[1]/2)*(Fert2010[j]+Fert2010[j+1]*(SxF10[j]))*ffab}
AF10 = SF10 + BF10
for(j in 1:SIZE-1)
  {BF15[1,j]<-(LxF15[1]/2)*(Fert2015[j]+Fert2015[j+1]*(SxF15[j]))*ffab}
AF15 = SF15 + BF15
for(j in 1:SIZE-1)
  {BF20[1,j]<-(LxF20[1]/2)*(Fert2020[j]+Fert2020[j+1]*(SxF20[j]))*ffab}
AF20 = SF20 + BF20

##MAKE ARRAYS TO HOLD THE DATA
KF05<-array(KF05,c(SIZE,1))
KF10<-array(0,c(SIZE,1))
KF25<-KF20<-KF15<-KF10

##PROJECT THE FEMALE POPULATION (NATURAL INCREASE, LESS OUT MIGRATION, PLUS IN MIGRATION 
##(IN MIGRATION IS OUT MIGRATION SUM PLUS NET MIGRATION SUM)
##OVERALL MIGRATION IS SCALED ON WORKING AGE MIGRATION
##WORKING AGE IS SET TO TRADITIONAL 16+ HERE, BUT COULD BE EASILY MODIFIED TO 16 TO 80, OR ANYTHING 
Out2005<-array(0,c(SIZE,1))
Out2005<-OxF*OutRate
KF10temp<-(AF05%*%KF05)-t(t(Out2005))
In2005<-array(0,c(SIZE,1))
In2005<-(EmpBasedPop2010*EmpF_cons[2]-(sum(KF10temp[5:length(KF10temp)])+KF10temp[4]*4/5))*(IxF*(1/((sum(IxF[5:length(IxF)])+IxF[4]*4/5))))
KF10<-KF10temp+t(t(In2005))
Out2010<-array(0,c(SIZE,1))
Out2010<-OxF*OutRate
KF15temp<-(AF10%*%KF10)-t(t(Out2010))
In2010<-array(0,c(SIZE,1))
In2010<-(EmpBasedPop2015*EmpF_cons[3]-(sum(KF15temp[5:length(KF15temp)])+KF15temp[4]*4/5))*(IxF*(1/((sum(IxF[5:length(IxF)])+IxF[4]*4/5))))
KF15<-KF15temp+t(t(In2010))

Out2015<-array(0,c(SIZE,1))
Out2015<-OxF*OutRate
KF20temp<-(AF15%*%KF15)-t(t(Out2015))
In2015<-array(0,c(SIZE,1))
In2015<-(EmpBasedPop2020*EmpF_cons[4]-(sum(KF20temp[5:length(KF20temp)])+KF20temp[4]*4/5))*(IxF*(1/((sum(IxF[5:length(IxF)])+IxF[4]*4/5))))
KF20<-KF20temp+t(t(In2015))

Out2020<-array(0,c(SIZE,1))
Out2020<-OxF*OutRate
KF25temp<-(AF20%*%KF20)-t(t(Out2020))
In2020<-array(0,c(SIZE,1))
In2020<-(EmpBasedPop2025*EmpF_cons[5]-(sum(KF25temp[5:length(KF25temp)])+KF25temp[4]*4/5))*(IxF*(1/((sum(IxF[5:length(IxF)])+IxF[4]*4/5))))
KF25<-KF25temp+t(t(In2020))

##MAKE THE LESLIE MATRICES FOR MALES
BM20<-BM15<-BM10<-BM05<-0*SF05
for(j in 1:SIZE-1)
  {BM05[1,j]<-(LxM05[1]/2)*(Fert2005[j]+Fert2005[j+1]*(SxF05[j]))*fmab}
AM05 = SM05 + BM05
for(j in 1:SIZE-1)
  {BM10[1,j]<-(LxM10[1]/2)*(Fert2010[j]+Fert2010[j+1]*(SxF10[j]))*fmab}
AM10 = SM10 + BM10
for(j in 1:SIZE-1)
  {BM15[1,j]<-(LxM15[1]/2)*(Fert2015[j]+Fert2015[j+1]*(SxF15[j]))*fmab}
AM15 = SM15 + BM15
for(j in 1:SIZE-1)
  {BM20[1,j]<-(LxM20[1]/2)*(Fert2020[j]+Fert2020[j+1]*(SxF20[j]))*fmab}
AM20 = SM20 + BM20

##MAKE ARRAYS TO HOLD THE DATA
KM05<-array(KM05,c(SIZE,1))
KM10<-array(0,c(SIZE,1))
KBM10<-array(0,c(SIZE,1))
KSM10<-array(0,c(SIZE,1))
KM25<-KM20<-KM15<-KM10
KBM25<-KBM20<-KBM15<-KBM10
KSM25<-KSM20<-KSM15<-KSM10

##PROJECT THE MALE POPULATION (NATURAL INCREASE, LESS OUT MIGRATION, PLUS IN MIGRATION 
##(IN MIGRATION IS OUT MIGRATION SUM PLUS NET MIGRATION SUM)
##OVERALL MIGRATION IS SCALED ON WORKING AGE MIGRATION
##WORKING AGE IS SET TO TRADITIONAL 16+ HERE, BUT COULD BE EASILY MODIFIED TO 16 TO 80, OR ANYTHING 
OutM2005<-array(0,c(SIZE))
OutM2005<-OxM*OutRate
KBM10<-(BM05%*%KF05)
KSM10<-(SM05%*%KM05)-t(t(OutM2005))

KM10temp<-(KBM10+KSM10)
InM2005<-array(0,c(SIZE))
InM2005<-(EmpBasedPop2010*(1-EmpF_cons[2])-(sum(KM10temp[5:length(KM10temp)])+KM10temp[4]*4/5))*(IxM*(1/((sum(IxM[5:length(IxM)])+IxM[4]*4/5))))
KM10<-KM10temp+t(t(InM2005))

OutM2010<-array(0,c(SIZE))
OutM2010<-OxM*OutRate
KBM15<-(BM10%*%KF10)
KSM15<-(SM10%*%KM10)-t(t(OutM2010))
KM15temp<-(KBM15+KSM15)
InM2010<-array(0,c(SIZE))
InM2010<-(EmpBasedPop2015*(1-EmpF_cons[3])-(sum(KM15temp[5:length(KM15temp)])+KM15temp[4]*4/5))*(IxM*(1/((sum(IxM[5:length(IxM)])+IxM[4]*4/5))))
KM15<-KM15temp+t(t(InM2010))

OutM2015<-array(0,c(SIZE))
OutM2015<-OxM*OutRate
KBM20<-(BM15%*%KF15)
KSM20<-(SM15%*%KM15)-t(t(OutM2015))
KM20temp<-(KBM20+KSM20)
InM2015<-array(0,c(SIZE))
InM2015<-(EmpBasedPop2020*(1-EmpF_cons[4])-(sum(KM20temp[5:length(KM20temp)])+KM20temp[4]*4/5))*(IxM*(1/((sum(IxM[5:length(IxM)])+IxM[4]*4/5))))
KM20<-KM20temp+t(t(InM2015))

OutM2020<-array(0,c(SIZE))
OutM2020<-OxM*OutRate
KBM25<-(BM20%*%KF20)
KSM25<-(SM20%*%KM20)-t(t(OutM2020))
KM25temp<-(KBM25+KSM25)
InM2020<-array(0,c(SIZE))
InM2020<-(EmpBasedPop2025*(1-EmpF_cons[5])-(sum(KM25temp[5:length(KM25temp)])+KM25temp[4]*4/5))*(IxM*(1/((sum(IxM[5:length(IxM)])+IxM[4]*4/5))))
KM25<-KM25temp+t(t(InM2020))

##MAKE TABLES OF DATA OF INTEREST
KT05<-sum(KF05)+sum(KM05)
KT10<-sum(KF10)+sum(KM10)
KT15<-sum(KF15)+sum(KM15)
KT20<-sum(KF20)+sum(KM20)
KT25<-sum(KF25)+sum(KM25)
KT<-c(KT05,KT10,KT15,KT20,KT25)

TNR<-array(0,c(STEPS))
TNR[1]<-(sum(InM2005)+sum(In2005))-(sum(OutM2005)+sum(Out2005))
TNR[2]<-(sum(InM2010)+sum(In2010))-(sum(OutM2010)+sum(Out2010))
TNR[3]<-(sum(InM2015)+sum(In2015))-(sum(OutM2015)+sum(Out2015))
TNR[4]<-(sum(InM2020)+sum(In2020))-(sum(OutM2020)+sum(Out2020))

HU<-array(0,c(BASEANDSTEPS))
HU[1]<-((KT05/PPH_cons[1]))*(1/(1-VAC_cons[1]))
HU[2]<-((KT10/PPH_cons[2]))*(1/(1-VAC_cons[2]))
HU[3]<-((KT15/PPH_cons[3]))*(1/(1-VAC_cons[3]))
HU[4]<-((KT20/PPH_cons[4]))*(1/(1-VAC_cons[4]))
HU[5]<-((KT25/PPH_cons[5]))*(1/(1-VAC_cons[5]))

#############################################################################################################################
#############################################################################################################################
KTH<-array(0,c(12))
KTH<-c(308500,384100,419800,543900,553171,601581,628346,667146,KT[2:5])
plot(KTH,type="l",ylim=c(0,1200000),xlim=c(0,12),col="black",xlab="Year",ylab="Population",axes=F,lwd=8,scipen=5,digits=10)
axis(side=1,at=1:12,labels=c("1970","1975","1980","1985","1990","1995","2000","2005","2010","2015","2020","2025"),cex.axis=0.8)
axis(side=2,cex.axis=0.8)
title("TOTAL POPULATION",cex.main=1)
mtext(side=1,line=-22,adj=.2,text="2010: ",font=2,cex=1)
mtext(side=1,line=-22,adj=.35,text=round(KT[2],0),font=2,cex=1)
mtext(side=1,line=-21,adj=.2,text="2015: ",font=2,cex=1)
mtext(side=1,line=-21,adj=.35,text=round(KT[3],0),font=2,cex=1)
mtext(side=1,line=-20,adj=.2,text="2020: ",font=2,cex=1)
mtext(side=1,line=-20,adj=.35,text=round(KT[4],0),font=2,cex=1)
mtext(side=1,line=-19,adj=.2,text="2025: ",font=2,cex=1)
mtext(side=1,line=-19,adj=.35,text=round(KT[5],0),font=2,cex=1)

TNRFORE<-array(0,c(11))
TNRFORE<-c(47469,881,75984,-39444,3942,-9861,3579,TNR)
plot(TNRFORE,type="l",ylim=c(-100000,100000),xlim=c(0,11),col="black",xlab="Time Period",ylab="Net Migration",lwd=8,axes=F,scipen=5,digits=10)
axis(side=1,at=1:11,labels=c("1970-75","1975-80","1980-85","1985-90","1990-95","1995-00","2000-05","2005-10","2010-15","2015-20","2020-25"),cex.axis=0.8)
axis(side=2,cex.axis=0.8)
title("NET MIGRATION",cex.main=1)
mtext(side=1,line=-22,adj=.55,text="2005 to 2010: ",font=2,cex=1)
mtext(side=1,line=-22,adj=.8,text=round(TNR[1],0),font=2,cex=1)
mtext(side=1,line=-21,adj=.55,text="2010 to 2015: ",font=2,cex=1)
mtext(side=1,line=-21,adj=.8,text=round(TNR[2],0),font=2,cex=1)
mtext(side=1,line=-20,adj=.55,text="2015 to 2020: ",font=2,cex=1)
mtext(side=1,line=-20,adj=.8,text=round(TNR[3],0),font=2,cex=1)
mtext(side=1,line=-19,adj=.55,text="2020 to 2025: ",font=2,cex=1)
mtext(side=1,line=-19,adj=.8,text=round(TNR[4],0),font=2,cex=1)

Housing<-array(0,c(5))
Housing<-c(HU)

plot(Housing,type="l",ylim=c(0,500000),xlim=c(0,5),col="black",xlab="Year",ylab="Housing Units",axes=F,lwd=8,scipen=5,digits=10)
axis(side=1,at=1:5,labels=c("2005","2010","2015","2020","2025"),font=1,cex=.75)
axis(side=2,cex.axis=0.8)
title("TOTAL HOUSING UNITS",cex.main=1)
mtext(side=1,line=-22,adj=.2,text="2010: ",font=2,cex=1)
mtext(side=1,line=-22,adj=.35,text=round(HU[2],0),font=2,cex=1)
mtext(side=1,line=-21,adj=.2,text="2015: ",font=2,cex=1)
mtext(side=1,line=-21,adj=.35,text=round(HU[3],0),font=2,cex=1)
mtext(side=1,line=-20,adj=.2,text="2020: ",font=2,cex=1)
mtext(side=1,line=-20,adj=.35,text=round(HU[4],0),font=2,cex=1)
mtext(side=1,line=-19,adj=.2,text="2025: ",font=2,cex=1)
mtext(side=1,line=-19,adj=.35,text=round(HU[5],0),font=2,cex=1)

plot(KF20,type="l",lwd=8,col="orange",ylim=c(0,50000),xlab="Age (five year groups)",ylab="Population",axes=F,scipen=5,digits=10)
lines(KM20,lwd=8,col="blue")
lines(KF25,lwd=8,col="green")
lines(KM25,lwd=8,col="purple")
axis(side=1,at=1:21,labels=c("0to4",seq(5,95,5),"100+"))
axis(side=2,cex.axis=0.8)
title("2020 AND 2025 POPULATION BY AGE AND SEX",cex.main=1)
mtext(side=1,line=-21,adj=1,text="Female 2020",font=2,col="orange",cex=1)
mtext(side=1,line=-20,adj=1,text="Male 2020",font=2,col="blue",cex=1)
mtext(side=1,line=-18,adj=1,text="Female 2025",font=2,col="green",cex=1)
mtext(side=1,line=-17,adj=1,text="Male 2025",font=2,col="purple",cex=1)

},height=800,width=800)

}

shinyApp(ui = ui, server = server)

