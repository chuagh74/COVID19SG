###########################################################################
## DSAPAC Shiny App Dashboard
###########################################################################

nodename = Sys.info()["nodename"]



###############################################################################
## # Import Libraries
###############################################################################
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(DT)
library(dplyr)
library(htmltools)
library(data.table)
library(tidyr)
library(DBI)
#library(RSQLite)
#library(TTR)
#library(heatmaply)
#library(rpart)
library(visNetwork)
#library(sparkline)
library(leaflet)
library(leaflet.minicharts)
#library(dygraphs)
#library(xts)
library(RColorBrewer)
#library(scales)
#library(circlize)
#library(forecast)
library(readxl)
library(timevis)
library(zoo)
library(collapsibleTree)
library(ggmap)
# library(survival)
# library(survminer)
library(plyr)
library(strex)


###########################################################################
######################### Format data table
###########################################################################
formatDTDisplay=function(a,selectChoice='multiple',currencyCol=NULL,roundCol=NULL,roundDigit=2,pagelen=50,escape=FALSE)
{
	a= a %>% datatable(#extensions = 'Buttons',
			selection=selectChoice,
			rownames=FALSE,
			filter='top',
			escape=escape,
		  options = list(
			pageLength = pagelen,
			scrollX=TRUE,
			scrollY="500px",
			dom = 'T<"clear">lBfrtip'
			#buttons = c('copy', 'excel', 'pdf', 'print')
			
		  )
		) 
	
	if (!is.null(currencyCol))
	{
		a = a %>% formatCurrency(currencyCol, currency = "", interval = 3, mark = ",")
	} 

	if (!is.null(roundCol))
	{
		a = a %>% formatRound(roundCol,digits=roundDigit)
	}
	
	return(a)
}

#########################################################
# Function: flattenCorrMatrix
#########################################################
flattenCorrMatrix <- function(cormat) {
  ut <- lower.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
	stringsAsFactors=FALSE
  )
}

###############################################################################
###############################################################################
createLink <- function(val,disp='Link') {
	sprintf('<a href="%s" target="_blank" class="btn btn-primary">%s</a>',val,disp)
}

################################################################################
readCaseReport=function(rfn)
{
	sepStr='###################################################'
	instr=paste(readLines(rfn),collapse="\n")
	
	strList=unlist(strsplit(instr,sepStr))
	
	reportList=list()
	for (i in 1:length(strList))
	{
		content=unlist(strsplit(strList[i],"\n"))
		if (length(content)>0)
		{
			caseNum=NULL
			for (j in 1:length(content))
			{
				contentStr=trimws(content[j])
				if (length(grep('^Case',contentStr))>0)
				{
					caseNum=as.character(str_first_number(contentStr))
					break;
				}
			}
			if (!is.null(caseNum))
			{
				if (caseNum %in% names(reportList))
				{
					reportList[[caseNum]]=sprintf("%s\n%s",reportList[[caseNum]],strList[i])
				} else
				{
					reportList[[caseNum]]=strList[i]
				}
				
			} else
			{
				print(sprintf("No result for %d: %s",i,fileList[i]))
			}
		}
		
	}
	return(reportList)
}



# Google Sign In Setting
###############################################################################
### To set up google signin and restrict 
### Try to follow instruction in https://developers.google.com/adwords/api/docs/guides/authentication
###############################################################################
# options(googleAuthR.webapp.client_id = "860920627740-c93t16apfoa3mou4mgjmtr2pobl7splf.apps.googleusercontent.com")

baseDir='C:\\Users\\Gek_Huey_Chua\\OneDrive - Dell Technologies\\Customer_Stickiness\\CustS'
DataDir=sprintf("%s\\DB",baseDir)

rfn='/opt/shiny-server/samples/sample-apps/COVID-19/CaseReport.txt'
reportList=readCaseReport(rfn)


##################################### Color Code
dayColorCode=function(days)
{
	colorCode='#F8BDB1' ### light red
	if (days>=28)
	{
		colorCode='#D2F991' # light green
	} else
	if (days>=14)
	{
		colorCode='#E0E1DD' # light grey
	} else
	if (days>=7)
	{
		colorCode='#F9E591' # light yellow
	} else
	if (days>=3)
	{
		colorCode='#F9BA91' # light orange
	}
	return(colorCode)
}


##################################### Color Code
dayIconColorCode=function(days)
{
	colorCode='red' ###  red
	if (days>=28)
	{
		colorCode='green' #  green
	} else
	if (days>=14)
	{
		colorCode='grey' #  grey
	} else
	if (days>=7)
	{
		colorCode='yellow' #  yellow
	} else
	if (days>=3)
	{
		colorCode='orange' #  orange
	}
	return(colorCode)
}

#####################################
indicatorColorMap=setNames(c('green','yellow','yellow','orange','orange','red'),c('0','1','2','3','4','5'))


##########################################################
### Questions
##########################################################
## Identify products that establishes long term relationship with customers
## Product family association
## Product association
## Are there one time product
## Definition of customer churn



timeVisStartDate=as.Date('2020-01-01')


##########################################################
#### APP START
##########################################################
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
### colors = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))[1:n]

if (nodename=="ip-172-30-0-48")
{
	reportDir='/opt/shiny-server/samples/sample-apps/COVID-19/Reports'
	filename='/srv/shiny-server/sample-apps/COVID-19/Virus.xlsx'
	iconDir='/srv/shiny-server/sample-apps/COVID-19/icons'
} else
{
	filename='virus.xlsx'
	iconDir='icons'
}
sheets <- readxl::excel_sheets(filename)

dataList=list()
for (sh in sheets)
{
	dataList[[sh]]=read_excel(filename,sheet=sh)
}
##################################################################

todayDate=max(as.Date(dataList[['TimeTrack']]$Date),as.Date(dataList[['PERSON']]$DateConfirmed))
date5Day=todayDate-14
##################################################################
HospMap=setNames(dataList[['HospMap']]$PlaceID,dataList[['HospMap']]$Name)

##### Get Geocode for PLACE
load('/opt/shiny-server/samples/sample-apps/RoutePlanner/PLACEGeo.rData')
uncodedNames=setdiff(dataList[['PLACE']]$Name,PLACEGeo$Name)
PLACEGeoTmp=NULL
if (length(uncodedNames)>0)
{
	if (length(uncodedNames)==1)
	{
		uncodedNames=c(uncodedNames,uncodedNames)
	}
	register_google(key = API)
	geoResult=ggmap::geocode(sprintf("%s SG",uncodedNames),output='all')
	latList=unlist(lapply(geoResult,function (x) format(x$results[[1]]$geometry$location$lat, digits=20)))
	lngList=unlist(lapply(geoResult,function (x) format(x$results[[1]]$geometry$location$lng, digits=20)))
	PLACEGeoTmp=data.frame(Name=uncodedNames,latlng=sprintf("%0.6f,%0.6f",as.numeric(latList),as.numeric(lngList)),latitude=as.numeric(latList),longitude=as.numeric(lngList),stringsAsFactors=FALSE)
	
}
PLACEGeo=rbind(PLACEGeo,unique(PLACEGeoTmp))
PLACEGeo=ddply(PLACEGeo,.(Name), head,1)
save(PLACEGeo,file='/opt/shiny-server/samples/sample-apps/RoutePlanner/PLACEGeo.rData')

dataList[['PLACE']]=merge(dataList[['PLACE']],PLACEGeo[,c('Name','latitude','longitude')],by='Name',all.x=TRUE)

##################################

#dataList[['PLACE']]$latitude=as.numeric(unlist(lapply(dataList[['PLACE']]$GeoLocation,function(x) trimws(unlist(strsplit(x,",")))[1])))
#dataList[['PLACE']]$longitude=as.numeric(unlist(lapply(dataList[['PLACE']]$GeoLocation,function(x) trimws(unlist(strsplit(x,",")))[2])))
reportMap=setNames(reportList,names(reportList))
dataList[['PERSON']]$Report=''
dataList[['PERSON']]$Report=reportMap[sprintf("%s",dataList[['PERSON']]$PersonID)]

# for (i in 1:nrow(dataList[['PERSON']]))
# {
	# filename=sprintf("%s/Case%d",reportDir,dataList[['PERSON']]$PersonID[i])
	# if (file.exists(filename))
	# {
		# reportOrg=readChar(filename, file.info(filename)$size)
		# report=gsub("\r\n","<br />",reportOrg)
		# dataList[['PERSON']]$Report[i]=report
	# }
# }


PLACE=dataList[['PLACE']]
####
placeNameMap=setNames(PLACE$Name,PLACE$PlaceID)
logitudeMap=setNames(PLACE$longitude,PLACE$PlaceID)
latitudeMap=setNames(PLACE$latitude,PLACE$PlaceID)

#### Warded Info
dataList[['PERSONPLACE']]=dataList[['PERSONPLACE']][which(dataList[['PERSONPLACE']]$Remarks!='Warded'),]
dataList[['PERSONPLACE']]=rbind(dataList[['PERSONPLACE']][,c('PersonID','PlaceID','DateStart','DateEnd','Remarks')],data.frame(PersonID=dataList[['PERSON']]$PersonID,PlaceID=HospMap[dataList[['PERSON']]$Warded],DateStart=dataList[['PERSON']]$SuspectDate,DateEnd=NA,Remarks='Warded',stringsAsFactors=FALSE))


PERSONPLACE=dataList[['PERSONPLACE']]
PERSONPLACE$Name=placeNameMap[PERSONPLACE$PlaceID]
PERSON=dataList[['PERSON']]
reportMap=setNames(PERSON$Report,PERSON$PersonID)
PERSON$SymptomsFirst=format(as.Date(PERSON$SymptomsFirst,"%d-%m-%Y"),"%d-%b-%Y")
PERSON$DateConfirmed=format(as.Date(PERSON$DateConfirmed,"%d-%m-%Y"),"%d-%b-%Y")
PERSON$DateOfArrival=format(as.Date(PERSON$DateOfArrival,"%d-%m-%Y"),"%d-%b-%Y")
PERSON$Discharge=format(as.Date(PERSON$Discharge,"%d-%m-%Y"),"%d-%b-%Y")
PERSON$Info=ifelse(PERSON$Source!='Local',sprintf("From %s arrived on %s",PERSON$Source,PERSON$DateOfArrival),'')
PERSON$Info1=ifelse(is.na(PERSON$Discharge),'',sprintf("Discharged on %s",PERSON$Discharge))
infoMap=setNames(sprintf("Case%s (%s, Age:%s)<br />%s<br />Symptom: %s, Confirmed: %s<br />",PERSON$PersonID,PERSON$Gender,PERSON$Age,PERSON$Info,PERSON$SymptomsFirst,PERSON$DateConfirmed),PERSON$PersonID)


PERSON$ExpStartDate=PERSON$DateOfArrival
PERSON$ExpStartDate[which(is.na(PERSON$ExpStartDate))]=format(as.Date(PERSON$SymptomsFirst[which(is.na(PERSON$ExpStartDate))],'%d-%b-%Y')-14,'%d-%b-%Y')
PersonIsolation=merge(data.frame(PersonID=PERSON$PersonID,stringsAsFactors=FALSE),data.table(PERSONPLACE)[which(PERSONPLACE$Remarks=='Warded'),][,.(IsolateDate=max(DateStart)),by=list(PersonID)],by='PersonID',all.x=TRUE)
SuspectDateMap=setNames(PERSON$SuspectDate,PERSON$PersonID)
PersonIsolation$IsolateDate[which(is.na(PersonIsolation$IsolateDate))]=SuspectDateMap[PersonIsolation$PersonID[which(is.na(PersonIsolation$IsolateDate))]]
PersonIsolationMap=setNames(PersonIsolation$IsolateDate,PersonIsolation$PersonID)
PERSON$IsolationDate=PersonIsolationMap[PERSON$PersonID]
PERSON$IsolationDate[which(is.na(PERSON$IsolationDate))]=format(as.Date(PERSON$ExpStartDate[which(is.na(PERSON$IsolationDate))],'%d-%b-%Y')+1,'%d-%b-%Y')

### If cannot find start date, then use isolation date -14
PERSON$ExpStartDate[which(is.na(PERSON$ExpStartDate))]=format(as.Date(PERSON$IsolationDate[which(is.na(PERSON$ExpStartDate))],'%d-%b-%Y')-14,'%d-%b-%Y')
PERSON$ExpStartDate[which(PERSON$Transport=='Evacuation')]='30-Jan-2020'
PERSON$IsolationDate=format(PERSON$IsolationDate,'%d-%b-%Y')
PERSON$IsolationDate[which(PERSON$Transport=='Evacuation')]='30-Jan-2020'
PERSON$IsolationDate[which(!is.na(PERSON$QuarantineDate))]=format(PERSON$QuarantineDate[which(!is.na(PERSON$QuarantineDate))],'%d-%b-%Y')
PERSON$DaysOfPublicExposure=as.numeric(difftime(as.Date(PERSON$IsolationDate,'%d=%b-%Y'),as.Date(PERSON$ExpStartDate,'%d=%b-%Y'),unit='days'))

# ################################## Survival analysis
PERSON$SymptomsFirst[which(is.na(PERSON$SymptomsFirst))]=PERSON$SuspectDate[which(is.na(PERSON$SymptomsFirst))]
PERSON$RecoveryDays=as.numeric(difftime(as.Date(PERSON$Discharge,'%d-%b-%Y'),as.Date(PERSON$SymptomsFirst,'%d-%b-%Y'),unit='days'))
PERSON$DetectionTime=as.numeric(difftime(as.Date(PERSON$DateConfirmed,'%d-%b-%Y'),as.Date(PERSON$SymptomsFirst,'%d-%b-%Y'),unit='days'))


medianHospDays=median(PERSON$RecoveryDays[which(!is.na(PERSON$RecoveryDays))])
rangeHospDay=range(PERSON$RecoveryDays[which(!is.na(PERSON$RecoveryDays))])
medianDetectionTime=median(PERSON$DetectionTime[which(!is.na(PERSON$DetectionTime))])
rangeDetectionTime=range(PERSON$DetectionTime[which(!is.na(PERSON$DetectionTime))])
GenderTable=table(PERSON$Gender)
GPVisitMedian=median(PERSON$GPVisit,na.rm=TRUE)
GPVisitRange=range(PERSON$GPVisit,na.rm=TRUE)

SurvivalData=PERSON[which(!is.na(PERSON$SymptomsFirst)),]
SurvivalData$DischargeFlag='Inpatient'
SurvivalData$DischargeFlag[which(!is.na(SurvivalData$Discharge))]='Discharged'

SurvivalData$AgeCat=as.factor(substr(sprintf("%02.0f",SurvivalData$Age),1,1))
SurvivalData$HospDays=as.numeric(difftime(as.Date(SurvivalData$Discharge,"%d-%b-%Y"),as.Date(SurvivalData$SymptomsFirst,"%d-%b-%Y"),unit='days'))
SurvivalData$HospDays[which(is.na(SurvivalData$HospDays))]=as.numeric(difftime(as.Date(todayDate,"%d-%b-%Y"),as.Date(SurvivalData$SymptomsFirst[which(is.na(SurvivalData$HospDays))],"%d-%b-%Y"),unit='days'))
SurvivalData$Gender=as.factor(SurvivalData$Gender)


# km <- with(SurvivalData, Surv(HospDays, DischargeCensored))
# km_trt_fit <- survfit(Surv(HospDays, DischargeCensored) ~ AgeCat, data=SurvivalData)
# ggsurvplot(km_trt_fit, data = SurvivalData, pval = TRUE)


# cox <- coxph(Surv(HospDays, DischargeCensored) ~ Gender + Age , data = SurvivalData)
# cox_fit <- survfit(cox)

# ggsurvplot(cox_fit, data = SurvivalData, pval = TRUE)


#####################################################
PERSONPLACE$PersonInfo=infoMap[PERSONPLACE$PersonID]
PERSONPLACE$Report=reportMap[PERSONPLACE$PersonID]
PERSONPLACE$longitude=logitudeMap[PERSONPLACE$PlaceID]
PERSONPLACE$latitude=latitudeMap[PERSONPLACE$PlaceID]
PERSONPLACE$DateStart=format(as.Date(PERSONPLACE$DateStart,"%d-%m-%Y"),"%d-%b-%Y")
PERSONPLACE$DateEnd=format(as.Date(PERSONPLACE$DateEnd,"%d-%m-%Y"),"%d-%b-%Y")
PERSONPLACE$DateStart[which(is.na(PERSONPLACE$DateStart))]=format(as.Date(PERSONPLACE$DateEnd[which(is.na(PERSONPLACE$DateStart))],'%d-%b-%Y')-14,'%d-%b-%Y')
PERSONPLACE$DateEnd[which(is.na(PERSONPLACE$DateEnd))]=format(todayDate,'%d-%b-%Y')


PERSONPLACE$DaysFrDateEnd=as.numeric(difftime(todayDate,as.Date(PERSONPLACE$DateEnd,"%d-%b-%Y"),unit='days'))
PERSONPLACE$VisitInfo=sprintf("<bold>%s</bold><br />Possible Visit Period: %s - %s (%d days from today %s)",PERSONPLACE$Name,PERSONPLACE$DateStart,PERSONPLACE$DateEnd,PERSONPLACE$DaysFrDateEnd,format(todayDate,"%d-%b-%Y"))
PERSONPLACE$Display=sprintf("<table border=1><tr bgcolor='lightgrey'><td>%s<br />Case%d - %s</td></tr><tr><td>%s</td></tr><tr><td>%s</td></tr></table>",PERSONPLACE$VisitInfo,PERSONPLACE$PersonID,PERSONPLACE$Remarks,PERSONPLACE$PersonInfo,PERSONPLACE$Report)
PERSONPLACE$colorCode=unlist(lapply(PERSONPLACE$DaysFrDateEnd,dayIconColorCode))
PERSONPLACE$icon=sprintf("%s_%s",PERSONPLACE$Remarks,PERSONPLACE$colorCode)

dataList[['PERSON']]=PERSON
dataList[['PERSONPLACE']]=PERSONPLACE



PERSONPLACEAgg=data.table(PERSONPLACE)[,.(CaseCnt=length(unique(PersonID)),DateStart=min(DateStart),DateEnd=max(DateEnd),DaysFrDateEnd=min(DaysFrDateEnd),Display=paste(Display,collapse="<br />"),Remarks=head(Remarks,1),Name=head(Name,1)),by=list(PlaceID,longitude,latitude)]
PERSONPLACEAgg$colorCode=unlist(lapply(PERSONPLACEAgg$DaysFrDateEnd,dayIconColorCode))
PERSONPLACEAgg$icon=sprintf("%s_%s",PERSONPLACEAgg$Remarks,PERSONPLACEAgg$colorCode)
PERSONPLACEAgg=PERSONPLACEAgg[order(PERSONPLACEAgg$colorCode),]
dataList[['PERSONPLACEAgg']]=PERSONPLACEAgg

######################### TimeTrack
dataList[['TimeTrack']]$DeathInc=c(dataList[['TimeTrack']]$Death[1],diff(dataList[['TimeTrack']]$Death,1))
dataList[['TimeTrack']]$PosInc=c(dataList[['TimeTrack']]$Positive[1],diff(dataList[['TimeTrack']]$Positive,1))
dataList[['TimeTrack']]$PosIncInc=c(dataList[['TimeTrack']]$PosInc[1],diff(dataList[['TimeTrack']]$PosInc,1))
dataList[['TimeTrack']]$QuarantineCurrentInc=c(dataList[['TimeTrack']]$QuarantineCurrent[1],diff(dataList[['TimeTrack']]$QuarantineCurrent,1))

dataList[['TimeTrack']]$DischargeInc=c(dataList[['TimeTrack']]$Discharge[1],diff(dataList[['TimeTrack']]$Discharge,1))
dataList[['TimeTrack']]$PosInWard=dataList[['TimeTrack']]$Positive-dataList[['TimeTrack']]$Discharge
dataList[['TimeTrack']]$WardInc=dataList[['TimeTrack']]$PosInc-dataList[['TimeTrack']]$DischargeInc
dataList[['TimeTrack']]$Samples=dataList[['TimeTrack']]$Negative+dataList[['TimeTrack']]$Positive+dataList[['TimeTrack']]$Pending
dataList[['TimeTrack']]$SamplesInc=c(dataList[['TimeTrack']]$Samples[1],diff(dataList[['TimeTrack']]$Samples,1))
dataList[['TimeTrack']]$CloseCOntactInc=c(dataList[['TimeTrack']]$CloseContact[1],diff(dataList[['TimeTrack']]$CloseContact,1))

x=dataList[['TimeTrack']]$CloseCOntactInc
y=NULL
for (i in 2:length(x)) {y=c(y,ifelse(x[i]>median(x[1:(i-1)]),1,0))}
dataList[['TimeTrack']]$CloseContactIncInd=c(1,y)

x=dataList[['TimeTrack']]$PosInc
y=NULL
for (i in 2:length(x)) {y=c(y,ifelse(x[i]>median(x[1:(i-1)]),1,0))}
dataList[['TimeTrack']]$PosIncInd=c(1,y)


x=dataList[['TimeTrack']]$WardInc
y=NULL
for (i in 2:length(x)) {y=c(y,ifelse(x[i]>median(x[1:(i-1)]),1,0))}
dataList[['TimeTrack']]$WardIncInd=c(1,y)

x=dataList[['TimeTrack']]$QuarantineCurrentInc
y=NULL
for (i in 2:length(x)) {y=c(y,ifelse(x[i]>median(x[1:(i-1)]),1,0))}
dataList[['TimeTrack']]$QuarantineCurrentIncInd=c(1,y)


x=dataList[['TimeTrack']]$PosIncInc
y=NULL
for (i in 2:length(x)) {y=c(y,ifelse(x[i]>median(x[1:(i-1)]),1,0))}
dataList[['TimeTrack']]$PosIncIncInd=c(1,y)

indicatorSource=setNames(c('WardInc','CloseCOntactInc','QuarantineCurrentInc',NA,NA),c('WardIncInd','CloseContactIncInd','QuarantineCurrentIncInd','UnlinkCnt','ExpCaseInd'))


##################################### Map icons
# "Hotel"         "Warded"        "Checkup" "Public"  "Home"          "Public"        "Work"          "Event"     
# placeIconsList <-NULL
# for (i in unique(dataList[['PERSONPLACE']]$Remarks))
# {
	# for (cc in c('green','grey','orange','red','yellow'))
	# {
		# evalStr=sprintf("%s_%s=makeIcon(\"%s/%s_%s.png\",\"%s/%s_%s.png\",12.5,20.5)",i,cc,iconDir,i,cc,iconDir,i,cc)
		# placeIconsList=c(placeIconsList,eval(parse(text=evalStr)))
	# }
# }

# placeIcons=iconList(placeIconsList)

# unImptIcon <- makeIcon(sprintf("%s/Hotel_blue.png",iconDir),sprintf("%s/Hotel_blue.png",iconDir),20,20)


placeIcons <- iconList(
	Event_blue=makeIcon(sprintf("%s/Event_blue.png",iconDir),sprintf("%s/Event_blue.png",iconDir),30,30)
	,Warded_blue=makeIcon(sprintf("%s/Warded_blue.png",iconDir),sprintf("%s/Warded_blue.png",iconDir),30,30)
	,Home_blue=makeIcon(sprintf("%s/Home_blue.png",iconDir),sprintf("%s/Home_blue.png",iconDir),30,30)
	,Checkup_blue=makeIcon(sprintf("%s/Checkup_blue.png",iconDir),sprintf("%s/Checkup_blue.png",iconDir),30,30)
	,Public_blue=makeIcon(sprintf("%s/Public_blue.png",iconDir),sprintf("%s/Public_blue.png",iconDir),30,30)
	,Work_blue=makeIcon(sprintf("%s/Work_blue.png",iconDir),sprintf("%s/Work_blue.png",iconDir),30,30)
	,Hotel_blue=makeIcon(sprintf("%s/Hotel_blue.png",iconDir),sprintf("%s/Hotel_blue.png",iconDir),30,30)
	
	,Event_red=makeIcon(sprintf("%s/Event_red.png",iconDir),sprintf("%s/Event_red.png",iconDir),30,30)
	,Warded_red=makeIcon(sprintf("%s/Warded_red.png",iconDir),sprintf("%s/Warded_red.png",iconDir),30,30)
	,Home_red=makeIcon(sprintf("%s/Home_red.png",iconDir),sprintf("%s/Home_red.png",iconDir),30,30)
	,Checkup_red=makeIcon(sprintf("%s/Checkup_red.png",iconDir),sprintf("%s/Checkup_red.png",iconDir),30,30)
	,Public_red=makeIcon(sprintf("%s/Public_red.png",iconDir),sprintf("%s/Public_red.png",iconDir),30,30)
	,Work_red=makeIcon(sprintf("%s/Work_red.png",iconDir),sprintf("%s/Work_red.png",iconDir),30,30)
	,Hotel_red=makeIcon(sprintf("%s/Hotel_red.png",iconDir),sprintf("%s/Hotel_red.png",iconDir),30,30)
	
	,Event_red=makeIcon(sprintf("%s/Event_green.png",iconDir),sprintf("%s/Event_green.png",iconDir),30,30)
	,Warded_green=makeIcon(sprintf("%s/Warded_green.png",iconDir),sprintf("%s/Warded_green.png",iconDir),30,30)
	,Home_green=makeIcon(sprintf("%s/Home_green.png",iconDir),sprintf("%s/Home_green.png",iconDir),30,30)
	,Checkup_green=makeIcon(sprintf("%s/Checkup_green.png",iconDir),sprintf("%s/Checkup_green.png",iconDir),30,30)
	,Public_green=makeIcon(sprintf("%s/Public_green.png",iconDir),sprintf("%s/Public_green.png",iconDir),30,30)
	,Work_green=makeIcon(sprintf("%s/Work_green.png",iconDir),sprintf("%s/Work_green.png",iconDir),30,30)
	,Hotel_green=makeIcon(sprintf("%s/Hotel_green.png",iconDir),sprintf("%s/Hotel_green.png",iconDir),30,30)
	
	,Event_yellow=makeIcon(sprintf("%s/Event_yellow.png",iconDir),sprintf("%s/Event_yellow.png",iconDir),30,30)
	,Warded_yellow=makeIcon(sprintf("%s/Warded_yellow.png",iconDir),sprintf("%s/Warded_yellow.png",iconDir),30,30)
	,Home_yellow=makeIcon(sprintf("%s/Home_yellow.png",iconDir),sprintf("%s/Home_yellow.png",iconDir),30,30)
	,Checkup_yellow=makeIcon(sprintf("%s/Checkup_yellow.png",iconDir),sprintf("%s/Checkup_yellow.png",iconDir),30,30)
	,Public_yellow=makeIcon(sprintf("%s/Public_yellow.png",iconDir),sprintf("%s/Public_yellow.png",iconDir),30,30)
	,Work_yellow=makeIcon(sprintf("%s/Work_yellow.png",iconDir),sprintf("%s/Work_yellow.png",iconDir),30,30)
	,Hotel_yellow=makeIcon(sprintf("%s/Hotel_yellow.png",iconDir),sprintf("%s/Hotel_yellow.png",iconDir),30,30)
	
	,Event_orange=makeIcon(sprintf("%s/Event_orange.png",iconDir),sprintf("%s/Event_orange.png",iconDir),30,30)
	,Warded_orange=makeIcon(sprintf("%s/Warded_orange.png",iconDir),sprintf("%s/Warded_orange.png",iconDir),30,30)
	,Home_orange=makeIcon(sprintf("%s/Home_orange.png",iconDir),sprintf("%s/Home_orange.png",iconDir),30,30)
	,Checkup_orange=makeIcon(sprintf("%s/Checkup_orange.png",iconDir),sprintf("%s/Checkup_orange.png",iconDir),30,30)
	,Public_orange=makeIcon(sprintf("%s/Public_orange.png",iconDir),sprintf("%s/Public_orange.png",iconDir),30,30)
	,Work_orange=makeIcon(sprintf("%s/Work_orange.png",iconDir),sprintf("%s/Work_orange.png",iconDir),30,30)
	,Hotel_orange=makeIcon(sprintf("%s/Hotel_orange.png",iconDir),sprintf("%s/Hotel_orange.png",iconDir),30,30)
	
	,Event_grey=makeIcon(sprintf("%s/Event_grey.png",iconDir),sprintf("%s/Event_grey.png",iconDir),30,30)
	,Warded_grey=makeIcon(sprintf("%s/Warded_grey.png",iconDir),sprintf("%s/Warded_grey.png",iconDir),30,30)
	,Home_grey=makeIcon(sprintf("%s/Home_grey.png",iconDir),sprintf("%s/Home_grey.png",iconDir),30,30)
	,Checkup_grey=makeIcon(sprintf("%s/Checkup_grey.png",iconDir),sprintf("%s/Checkup_grey.png",iconDir),30,30)
	,Public_grey=makeIcon(sprintf("%s/Public_grey.png",iconDir),sprintf("%s/Public_grey.png",iconDir),30,30)
	,Work_grey=makeIcon(sprintf("%s/Work_grey.png",iconDir),sprintf("%s/Work_grey.png",iconDir),30,30)
	,Hotel_grey=makeIcon(sprintf("%s/Hotel_grey.png",iconDir),sprintf("%s/Hotel_grey.png",iconDir),30,30)
	
	,Star=makeIcon(sprintf("%s/Star.png",iconDir),sprintf("%s/Star.png",iconDir),30,30)
)



######################################
### Discharged
for (pid in dataList[['PERSON']]$PersonID[which(!is.na(dataList[['PERSON']]$Discharge))])
{
	dataList[['PERSONPLACE']]$DateEnd[which(dataList[['PERSONPLACE']]$PersonID==pid & dataList[['PERSONPLACE']]$Remarks=='Warded')]==dataList[['PERSON']]$Discharge[which(dataList[['PERSON']]$PersonID==pid)]
}





#### Cluster for linked but non cluster
personPersonIDList=unique(c(dataList[['PERSONPERSON']]$PersonID1,dataList[['PERSONPERSON']]$PersonID2))
personPersonIDList=setdiff(personPersonIDList,dataList[['CLUSTER']]$PersonID)

secondLink=intersect(personPersonIDList,dataList[['PERSONPERSON']]$PersonID2)
firstLink=setdiff(personPersonIDList,secondLink)

dataList[['CLUSTER']]=rbind(dataList[['CLUSTER']],data.frame(PersonID=c(firstLink,secondLink),Cluster='OTH_Linked',LinkType=c(rep('Direct',length(firstLink)),rep('Indirect',length(secondLink))),stringsAsFactors=FALSE))

### unlinked keep as a cluster
unlinked=setdiff(dataList[['PERSON']]$PersonID,dataList[['CLUSTER']]$PersonID)
dataList[['CLUSTER']]=rbind(dataList[['CLUSTER']],data.frame(PersonID=c(unlinked),Cluster='UNLINKED',LinkType=c(rep('Direct',length(unlinked))),stringsAsFactors=FALSE))



CLUSTER=dataList[['CLUSTER']]
clusterMap=setNames(CLUSTER$Cluster,CLUSTER$PersonID)

clusterSizeDf=data.frame(table(dataList[['CLUSTER']]$Cluster))
clusterSizeMap=setNames(clusterSizeDf$Freq,clusterSizeDf$Var1)



######################################
## Cluster monitor
personSub=dataList[['PERSON']][which(dataList[['PERSON']]$PersonID %in% dataList[['CLUSTER']]$PersonID),]
clusterMap=setNames(dataList[['CLUSTER']]$Cluster,dataList[['CLUSTER']]$PersonID)
personSub$Cluster=clusterMap[as.character(personSub$PersonID)]
ClusterSummaryA=data.table(personSub)[,.(ExpStartDate=min(ExpStartDate),LastExpDate=max(IsolationDate),CaseCount=.N),by=list(Cluster)]

personSub$DateConfirmed=as.Date(personSub$DateConfirmed,'%d-%b-%Y')
ClusterSummary=merge(ClusterSummaryA,data.table(personSub)[,.(EarliestConfirmDate=min(DateConfirmed),LatestConfirmDate=max(DateConfirmed)),by=list(Cluster)],by='Cluster',all.x=TRUE,all.y=TRUE)

ClusterSummary$DaysFrLastConfirm=as.numeric(difftime(todayDate,ClusterSummary$LatestConfirmDate))


######################################
hc=hclust(dist(dataList[['PLACE']][,c('latitude','longitude')]))
dataList[['PLACE']]$PlaceCluster <- cutree(hc, h = 0.05)
PlaceClusterMap=setNames(dataList[['PLACE']]$PlaceCluster,dataList[['PLACE']]$PlaceID)
ClusterLongitudeDf=aggregate(longitude~PlaceCluster,dataList[['PLACE']],mean)
ClusterLatitudeDf=aggregate(latitude~PlaceCluster,dataList[['PLACE']],mean)

ClusterLongitudeMap=setNames(ClusterLongitudeDf$longitude,ClusterLatitudeDf$PlaceCluster)
ClusterLatitudeMap=setNames(ClusterLatitudeDf$latitude,ClusterLatitudeDf$PlaceCluster)

dataList[['PERSONPLACE']]$PlaceCluster=PlaceClusterMap[as.character(dataList[['PERSONPLACE']]$PlaceID)]

dataList[['PERSONPLACE']]$PlaceClusterLongitude=ClusterLongitudeMap[as.character(dataList[['PERSONPLACE']]$PlaceCluster)]
dataList[['PERSONPLACE']]$PlaceClusterLatitude=ClusterLatitudeMap[as.character(dataList[['PERSONPLACE']]$PlaceCluster)]
dataList[['PERSONPLACE']]$DateEnd[which(is.na(dataList[['PERSONPLACE']]$DateEnd))]=format(todayDate,"%d-%b-%Y")
dataList[['PERSONPLACE']]$DaysExposure=as.numeric(difftime(as.Date(dataList[['PERSONPLACE']]$DateEnd,'%d-%b-%Y'),as.Date(dataList[['PERSONPLACE']]$DateStart,'%d-%b-%Y'),unit='days'))

#######################################
longitudeMap=setNames(dataList[['PLACE']]$longitude,sprintf("Place%d",dataList[['PLACE']]$PlaceID))
latitudeMap=setNames(dataList[['PLACE']]$latitude,sprintf("Place%d",dataList[['PLACE']]$PlaceID))


longitudeMean=mean(longitudeMap)
latitudeMean=mean(latitudeMap)
longitudeMap=longitudeMap-longitudeMean
latitudeMap=latitudeMap-latitudeMean


########################################
dataList[['TimeTrack']]$UnlinkCnt=NA
dataList[['TimeTrack']]$ExpCasePeakDate=NA
for (i in 1:nrow(dataList[['TimeTrack']]))
{
	iDate=as.Date(dataList[['TimeTrack']]$Date[i])
	PersonNew=dataList[['PERSON']][which(as.Date(dataList[['PERSON']]$AnnouncementDate)==iDate),]
	if (nrow(PersonNew)>0)
	{
		PERSONPERSONSub=dataList[['PERSONPERSON']][which(as.Date(dataList[['PERSONPERSON']]$Date)<=iDate),]
		CLUSTERSub=dataList[['CLUSTER']][which(dataList[['CLUSTER']]$PersonID<=max(PersonNew$PersonID)),]
		CLUSTERSub=CLUSTERSub[which(CLUSTERSub$Cluster %in% names(which(table(CLUSTERSub$Cluster)>1))),]
		
		#traceable=PersonNew$PersonID %in% c(CLUSTERSub$PersonID,PERSONPERSONSub$`PersonID1`,PERSONPERSONSub$`PersonID2`)
		traceable=which(PersonNew$PersonID %in% dataList[['CLUSTER']]$PersonID[which(dataList[['CLUSTER']]$Cluster=='UNLINKED')])
		
		dataList[['TimeTrack']]$UnlinkCnt[i]=length(traceable)
	} else
	{
		dataList[['TimeTrack']]$UnlinkCnt[i]=0
	}
	
	######### Exposure period
	personSub=dataList[['PERSON']][which(as.Date(dataList[['PERSON']]$AnnouncementDate)<=iDate),]

	minDate=min(as.Date(personSub$ExpStartDate,'%d-%b-%Y'))
	maxDate=max(as.Date(personSub$IsolationDate,'%d-%b-%Y'))
	tmpData=NULL
	for (j in as.character(seq.Date(minDate,maxDate,by='day')))
	{
		jDate=as.Date(j,'%Y-%m-%d')
		tmpData=rbind(tmpData,data.frame(count=length(which(as.Date(personSub$ExpStartDate,'%d-%b-%Y')<=jDate & as.Date(personSub$IsolationDate,'%d-%b-%Y')>=jDate)),Date=format(jDate,'%Y-%m-%d'),stringsAsFactors=FALSE))
	}
	
	dataList[['TimeTrack']]$ExpCasePeakDate[i]=tmpData$Date[which.max(tmpData$count)]
}

dataList[['TimeTrack']]$ExpCaseInd=NA
dataList[['TimeTrack']]$ExpCaseInd[1]=1
for (i in 2:nrow(dataList[['TimeTrack']]))
{
	dataList[['TimeTrack']]$ExpCaseInd[i]=ifelse(dataList[['TimeTrack']]$ExpCasePeakDate[i]>max(dataList[['TimeTrack']]$ExpCasePeakDate[1:(i-1)]),1,0)
}

####################
clusterMap=setNames(dataList[['CLUSTER']]$Cluster,dataList[['CLUSTER']]$PersonID)
dataList[['PERSON']]$groupName=clusterMap[as.character(dataList[['PERSON']]$PersonID)]


familyRelation=c('Son','Wife','Father','Husband','Daughter','Family')

############### Cluster Information

ClusterSummary=data.table(dataList[['PERSON']])[,.(CaseCount=.N),by=list(groupName)]

ClusterSummary$LastCaseDate=NA
for (i in 1:nrow(ClusterSummary))
{
	personSub=dataList[['PERSON']][which(dataList[['PERSON']]$groupName %in% ClusterSummary$groupName[i]),]
	ClusterSummary$LastCaseDate[i]=format(max(personSub$AnnouncementDate),'%d-%b-%Y')
	ClusterSummary$FirstCaseDate[i]=format(min(personSub$AnnouncementDate),'%d-%b-%Y')
	ClusterSummary$DaysFrLastCase[i]=as.numeric(difftime(as.Date(todayDate,'%Y-%m-%d'),max(personSub$AnnouncementDate),unit='days'))
	ClusterSummary$colorCode[i]=dayIconColorCode(ClusterSummary$DaysFrLastCase[i])
}

ClusterSummary$colorCode[which(ClusterSummary$colorCode=='yellow')]='#D9DE48'
ClusterSummaryNoteMap=setNames(sprintf("<font color='%s'>%s<br />Cases: %d<br />Days Free from cases: %s<br />Confirmed Date Range: %s-%s</font>",ClusterSummary$colorCode,ClusterSummary$groupName,ClusterSummary$CaseCount,ClusterSummary$DaysFrLastCase,ClusterSummary$FirstCaseDate,ClusterSummary$LastCaseDate),ClusterSummary$groupName)

##################### Indicator display
displayMap=setNames(c('Inpatient','Close Contact','LabTests','Unlinked Cases','Case Exposure'),c('WardIncInd','CloseContactIncInd','QuarantineCurrentInc','UnlinkCnt','ExpCaseInd'))
indicatorDisplay=setNames(
	c('Inpatient: This is an indicator derived from the difference in count of confirmed cases still hospitalized. Flag 1 is given when increased esle 0.'
	,'Close Contact: This is an indicator derived from the difference in count of close contacts. Flag 1 is given when increased esle 0.'
	,'Quarantine: This is an indicator derived from the difference in count of current quarantined'
	,'Unlinked Cases: This is an indicator derived from whether there are new unlinked cases on the day. Flag 1 is there are new unlinked cases.'
	,'Case Exposure: This is derived from the historical count of case exposure in the past date. If maximum peak in the graph increases, then flag 1 is assigned.')
	,c('WardIncInd','CloseContactIncInd','QuarantineCurrentInc','UnlinkCnt','ExpCaseInd')
)
indcolorList=unique(indicatorColorMap)
indPlotData=data.frame(dataList[['TimeTrack']])[,c('Date','WardIncInd','CloseContactIncInd','QuarantineCurrentInc','UnlinkCnt','ExpCaseInd')]
for (iD in 1:length(displayMap))
{	
	i=names(displayMap)[iD]
	indPlotData[,sprintf("%s_rollSum",i)]=c(indPlotData[1:4,i],rollsum(as.numeric(indPlotData[,i]),5))
	indPlotData[,sprintf("%s_colorCode",i)]=factor(indicatorColorMap[as.character(indPlotData[,sprintf("%s_rollSum",i)])],levels=indcolorList)

}

dataList[['PERSON']]$ResidencyType[is.na(dataList[['PERSON']]$ResidencyType)]=dataList[['PERSON']]$Nationality[is.na(dataList[['PERSON']]$ResidencyType)]


importClusters=c('Imported(Early)','ImportNew','Evacuation','Masjid Al Muttaqin')


unlinkedData=NULL
for (dateSel in unique(dataList[['PERSON']]$DateConfirmed))
{
	PersonNewTmp=dataList[['PERSON']][which(as.Date(dataList[['PERSON']]$AnnouncementDate)==as.Date(dateSel,'%d-%b-%Y')),]
	tracable=unique(c(intersect(PersonNewTmp$PersonID,dataList[['CLUSTER']]$PersonID[which(!(dataList[['CLUSTER']]$Cluster %in% c('UNLINKED','OTH_Linked')))]),intersect(PersonNewTmp$PersonID,dataList[['PERSONPERSON']]$PersonID2)))
	unlinkedData=rbind(unlinkedData,data.frame(Date=dateSel,NewUnlinked=(nrow(PersonNewTmp)-length(tracable)),stringsAsFactors=FALSE))
	
	
}


unlinkedData$CumuulativeUnlinked=cumsum(unlinkedData$NewUnlinked)

# personID=dataList[['PERSON']]$PersonID
# unlinkedPersonID=setdiff(personID,dataList[['CLUSTER']]$PersonID[which(!(dataList[['CLUSTER']]$Cluster %in% c('UNLINKED','OTH_Linked')))])

# length(setdiff(unlinkedPersonID,dataList[['PERSONPERSON']]$PersonID2))



###################################################################################
### World data
###################################################################################
library(readr)


load('/opt/shiny-server/samples/sample-apps/COVID-19/Data/ctryDataB.rData')

if (max(ctryDataB$Date)<as.Date(Sys.Date()-1) & hour(Sys.time())>12)
{
	urlfile='https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv'
	ctryDataB<-data.table(read_csv(url(urlfile)))
	save(ctryDataB,file='/opt/shiny-server/samples/sample-apps/COVID-19/Data/ctryDataB.rData')
}

ctryDataB$Recovered[which(is.na(ctryDataB$Recovered))]=0
ctryDataB$Confirmed[which(is.na(ctryDataB$Confirmed))]=0
ctryDataB$Deaths[which(is.na(ctryDataB$Deaths))]=0

todayDate=max(ctryDataB$Date)

populationDf=data.frame(read_excel('/opt/shiny-server/samples/sample-apps/COVID-19/Data/WorldPopulation2020.xlsx',sheet=1),stringsAsFactors=FALSE)
populationDf$Population2020=as.numeric(populationDf$Population2020)

populationDf$Pop2020Mil=populationDf$Population2020/1000000
populationDf$Country[which(populationDf$Country=='United States')]='US'
populationDf$Country[which(populationDf$Country=='Czech Republic (Czechia)')]='Czechia'


ctryDataB$Country[which(ctryDataB$Country=="Bahamas, The")]="Bahamas"
ctryDataB$Country[which(ctryDataB$Country=="Korea, South")]="South Korea"

ctryDataC=merge(ctryDataB,populationDf[,c('Country','Pop2020Mil')],by='Country')
ctryDataC$ConfirmedPerPopM=ctryDataC$Confirmed/ctryDataC$Pop2020Mil
#dataDir='/opt/shiny-server/samples/sample-apps/COVID-19/Data'


# recoveredData=fread(sprintf('%s/time_series_2019-ncov-Recovered.csv',dataDir),stringsAsFactors=FALSE)
# deathData=fread(sprintf('%s/time_series_2019-ncov-Deaths.csv',dataDir),stringsAsFactors=FALSE)
# confirmedData=fread(sprintf('%s/time_series_2019-ncov-Confirmed.csv',dataDir),stringsAsFactors=FALSE)

# dateCol=setdiff(names(confirmedData),c('Province/State','Country/Region','Lat','Long'))
# recoveredCtryData=recoveredData[,lapply(.SD,sum),by=(`Country/Region`),.SDcols=dateCol]
# deathCtryData=deathData[,lapply(.SD,sum),by=(`Country/Region`),.SDcols=dateCol]
# confirmedCtryData=confirmedData[,lapply(.SD,sum),by=(`Country/Region`),.SDcols=dateCol]

# recoveredCtryMelt=melt(recoveredCtryData,id.vars=c('Country/Region'))
# names(recoveredCtryMelt)[which(names(recoveredCtryMelt)=='value')]='Recovered'
# deathCtryMelt=melt(deathCtryData,id.vars=c('Country/Region'))
# names(deathCtryMelt)[which(names(deathCtryMelt)=='value')]='Deaths'
# confirmedCtryMelt=melt(confirmedCtryData,id.vars=c('Country/Region'))
# names(confirmedCtryMelt)[which(names(confirmedCtryMelt)=='value')]='Confirmed'

# #### Consolidate data
# ctryDataA=merge(confirmedCtryMelt,recoveredCtryMelt,by=c('Country/Region','variable'),all.x=TRUE)
# ctryDataB=merge(ctryDataA,deathCtryMelt,by=c('Country/Region','variable'),all.x=TRUE)
# names(ctryDataB)[which(names(ctryDataB)=='variable')]='Date'

# ctryDataB$Date=as.Date(as.character(ctryDataB$Date),"%m/%d/%Y")
# ctryDataB=ctryDataB[order(ctryDataB$Date),]
# ctryDataB=ctryDataB[order(ctryDataB$`Country/Region`),]
# names(ctryDataB)[which(names(ctryDataB)=='Country/Region')]='Country'
#,RecoveredInc=diff(Recovered),DeathInc=diff(Deaths)


ctryDataC$CurInfected=ctryDataC$Confirmed-ctryDataC$Deaths-ctryDataC$Recovered
ctryDataC=ctryDataC[order(ctryDataC$Date),]
ctryDataC=ctryDataC[order(ctryDataC$Country),]

#### INC1
ctryDataC$ConfirmedInc1=NA
ctryDataC$RecoveredInc1=NA
ctryDataC$DeathsInc1=NA
ctryDataC$CurInfectedInc1=NA
for (ctry in unique(ctryDataC$Country))
{
	ctryDataC$ConfirmedInc1[which(ctryDataC$Country==ctry)]=c(NA,diff(ctryDataC$Confirmed[which(ctryDataC$Country==ctry)],1))
	ctryDataC$RecoveredInc1[which(ctryDataC$Country==ctry)]=c(NA,diff(ctryDataC$Recovered[which(ctryDataC$Country==ctry)],1))
	ctryDataC$DeathsInc1[which(ctryDataC$Country==ctry)]=c(NA,diff(ctryDataC$Deaths[which(ctryDataC$Country==ctry)],1))
	ctryDataC$CurInfectedInc1[which(ctryDataC$Country==ctry)]=c(NA,diff(ctryDataC$CurInfected[which(ctryDataC$Country==ctry)],1))
}

#### INC2
ctryDataC$ConfirmedInc2=NA
ctryDataC$RecoveredInc2=NA
ctryDataC$DeathsInc2=NA
for (ctry in unique(ctryDataC$Country))
{
	ctryDataC$ConfirmedInc2[which(ctryDataC$Country==ctry)]=c(NA,diff(ctryDataC$ConfirmedInc1[which(ctryDataC$Country==ctry)],1))
	ctryDataC$RecoveredInc2[which(ctryDataC$Country==ctry)]=c(NA,diff(ctryDataC$RecoveredInc1[which(ctryDataC$Country==ctry)],1))
	ctryDataC$DeathsInc2[which(ctryDataC$Country==ctry)]=c(NA,diff(ctryDataC$DeathsInc1[which(ctryDataC$Country==ctry)],1))
}


###############
ctryFirstInfectDate=aggregate(Date~`Country`,ctryDataC[which(ctryDataC$Confirmed>0),],min)
ctryFirstInfectDate=ctryFirstInfectDate[order(ctryFirstInfectDate$Date),]
names(ctryFirstInfectDate)[which(names(ctryFirstInfectDate)=='Date')]='FirstInfectDate'

ctryFirst1PerMPopDate=aggregate(Date~`Country`,ctryDataC[which(ctryDataC$ConfirmedPerPopM>1),],min)
ctryFirst1PerMPopDate=ctryFirst1PerMPopDate[order(ctryFirst1PerMPopDate$Date),]
names(ctryFirst1PerMPopDate)[which(names(ctryFirst1PerMPopDate)=='Date')]='FirstPerMPopDate'


ctryDataD=merge(ctryDataC,ctryFirstInfectDate,by='Country',all.x=TRUE)
ctryDataF=merge(ctryDataD,ctryFirst1PerMPopDate,by='Country',all.x=TRUE)


ctryDataF$DayInfect=as.numeric(difftime(ctryDataF$Date,ctryDataF$FirstPerMPopDate,unit='days'))

ctryDataFClean=ctryDataF[which(ctryDataF$DayInfect>0),]

ctryDataFClean$ConfirmedInc1PC=ctryDataFClean$ConfirmedInc1/ctryDataFClean$Confirmed*100
ctryDataFClean$RcvryGtrCfmFlag=-1
ctryDataFClean$RcvryGtrCfmFlag[which(ctryDataFClean$RecoveredInc1==ctryDataFClean$ConfirmedInc1)]=0
ctryDataFClean$RcvryGtrCfmFlag[which(ctryDataFClean$RecoveredInc1>ctryDataFClean$ConfirmedInc1)]=1

ctryDataFClean=ctryDataFClean[order(ctryDataFClean$Date),]
ctryDataFClean=ctryDataFClean[,RcvryGtrCfmFlagCumSum:=cumsum(RcvryGtrCfmFlag),by=list(`Country`)]
ctryDataFClean$CurInfectedPerM=ctryDataFClean$CurInfected/ctryDataFClean$Pop2020Mil
ctryDataFClean$DeathsPerM=ctryDataFClean$Deaths/ctryDataFClean$Pop2020Mil
ctryDataFClean$RecoveredPerM=ctryDataFClean$Recovered/ctryDataFClean$Pop2020Mil
ctryDataFClean$ConfirmedInc1PerM=ctryDataFClean$ConfirmedInc1/ctryDataFClean$Pop2020Mil


countryList=unique(ctryDataFClean$Country)
countryList=countryList[order(countryList)]


#############################################################
# ctryCurrentDataMeltA=ctryCurrentData[,list(Country,CurInfected,ConfirmedInc1)]
# ctryCurrentDataMeltB=ctryCurrentData[,list(Country,Recovered,ConfirmedInc1)]
# ctryCurrentDataMeltC=ctryCurrentData[,list(Country,Deaths,ConfirmedInc1)]


# names(ctryCurrentDataMeltA)[which(names(ctryCurrentDataMeltA)=='CurInfected')]='Count'
# names(ctryCurrentDataMeltB)[which(names(ctryCurrentDataMeltB)=='Recovered')]='Count'
# names(ctryCurrentDataMeltC)[which(names(ctryCurrentDataMeltC)=='Deaths')]='Count'


# ctryCurrentDataMeltA$Type='CurrentInfected'
# ctryCurrentDataMeltB$Type='Recovered'
# ctryCurrentDataMeltC$Type='Deaths'


# ctryCurrentDataMelt=rbind(ctryCurrentDataMeltA,ctryCurrentDataMeltB,ctryCurrentDataMeltC)
# ctryCurrentDataMelt$Type=as.factor(ctryCurrentDataMelt$Type)


##########################################################
# ctryCurrentDataMeltPMA=ctryCurrentData[,list(Country,CurInfectedPerM,ConfirmedInc1PerM)]
# ctryCurrentDataMeltPMB=ctryCurrentData[,list(Country,RecoveredPerM,ConfirmedInc1PerM)]
# ctryCurrentDataMeltPMC=ctryCurrentData[,list(Country,DeathsPerM,ConfirmedInc1PerM)]


# names(ctryCurrentDataMeltPMA)[which(names(ctryCurrentDataMeltPMA)=='CurInfected')]='Count'
# names(ctryCurrentDataMeltPMB)[which(names(ctryCurrentDataMeltPMB)=='Recovered')]='Count'
# names(ctryCurrentDataMeltPMC)[which(names(ctryCurrentDataMeltPMC)=='Deaths')]='Count'


# ctryCurrentDataMeltPMA$Type='CurrentInfected'
# ctryCurrentDataMeltPMB$Type='Recovered'
# ctryCurrentDataMeltPMC$Type='Deaths'


# ctryCurrentDataMeltPM=rbind(ctryCurrentDataMeltPMA,ctryCurrentDataMeltPMB,ctryCurrentDataMeltPMC)
# ctryCurrentDataMeltPM$Type=as.factor(ctryCurrentDataMeltPM$Type)









		
