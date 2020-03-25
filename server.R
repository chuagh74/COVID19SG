shinyServer(function(input, output, session) {
    
    source("global.R")
	######################################################################
	# Main View
	######################################################################
	output$colorMsg=renderUI({
		
		msg=sprintf("Color codes used for icons. <br />Color is used to differentiate places with recent visits by infected cases from those that already has a longer period not being visited.")
		
		HTML(msg)
	})
	
	
	output$unlinkedPlot=renderPlotly({
		unlinkedData$Date=as.Date(unlinkedData$Date,'%d-%b-%Y')
		unlinkedData=unlinkedData[order(unlinkedData$Date),]
		plot_ly(unlinkedData,x=~Date,y=~CumuulativeUnlinked,type='scatter',mode='lines+markers') %>% layout(title='Cumulative unlinked cases')
	})
	
	output$localImportPlot=renderPlotly({
		importData=dataList[['CLUSTER']][which(dataList[['CLUSTER']]$Cluster %in% importClusters & dataList[['CLUSTER']]$LinkType=='Direct'),]
		
		importedAgg=aggregate(PersonID~DateConfirmed,dataList[['PERSON']][which(dataList[['PERSON']]$PersonID %in% importData$PersonID),],length)
		names(importedAgg)[which(names(importedAgg)=='PersonID')]='Imported'
		localAgg=aggregate(PersonID~DateConfirmed,dataList[['PERSON']][which(!(dataList[['PERSON']]$PersonID %in% importData$PersonID)),],length)
		names(localAgg)[which(names(localAgg)=='PersonID')]='Local'
		
		importLocalData=merge(importedAgg,localAgg,by='DateConfirmed',all.x=TRUE,all.y=TRUE)
		importLocalData[is.na(importLocalData)]=0
		importLocalData$DateConfirmed=as.Date(as.character(importLocalData$DateConfirmed),'%d-%b-%Y')
		importLocalDataMelt=melt(importLocalData,id.vars='DateConfirmed')
		names(importLocalDataMelt)[which(names(importLocalDataMelt)=='variable')]='CaseType'
		importLocalDataMelt=importLocalDataMelt[order(importLocalDataMelt$DateConfirmed),]
		plot_ly(importLocalDataMelt,x=~DateConfirmed,y=~value,color=~CaseType,type='scatter',mode='lines+markers') %>% layout(title='Imported cases vs Local cases')
	})
	
	
	
	output$importImpact=renderDT({
		indirectImports=dataList[['CLUSTER']][which(dataList[['CLUSTER']]$Cluster=='ImportNew'),]
		indirectImports=indirectImports[which(indirectImports$LinkType=='Indirect'),]
		indirectImportsRel=dataList[['PERSONPERSON']][which(dataList[['PERSONPERSON']]$PersonID1 %in% indirectImports$PersonID | dataList[['PERSONPERSON']]$PersonID2 %in% indirectImports$PersonID),]
		importLinkDf=NULL
		for (i in 1:nrow(indirectImportsRel))
		{
			importLinkDf=rbind(importLinkDf,data.frame('Import'=sprintf("Case %s: %s",indirectImportsRel$PersonID1[i],dataList[['PERSON']]$Notes[which(dataList[['PERSON']]$PersonID==indirectImportsRel$PersonID1[i])]),'Infected by Import'=sprintf("Case %s: %s",indirectImportsRel$PersonID2[i],dataList[['PERSON']]$Notes[which(dataList[['PERSON']]$PersonID==indirectImportsRel$PersonID2[i])]),Relation=sprintf("%s - %s",indirectImportsRel$Relation1[i],indirectImportsRel$Relation2[i]),stringsAsFactors=FALSE))
		}
		formatDTDisplay(importLinkDf)
	})
	
	output$importForm=renderUI({
		dataList[['PERSON']]$DateConfirmed=as.Date(dataList[['PERSON']]$DateConfirmed,'%d-%b-%Y')
		inputPanel(
			sliderInput(inputId = "importDateSelect", label = "Import Case Analysis Date",min=min(as.Date(dataList[['PERSON']]$DateConfirmed)),max=max(as.Date(dataList[['PERSON']]$DateConfirmed)),value=c(min(as.Date(dataList[['PERSON']]$DateConfirmed)),max(as.Date(dataList[['PERSON']]$DateConfirmed))))
		)
	})
	
	residencyType=reactive({
		residencyType=unique(dataList[['PERSON']]$ResidencyType)
		return(residencyType)
	})
	
	resiColors=reactive({
		n=length(unique(dataList[['PERSON']]$ResidencyType))
		resiColors = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))[1:n]
		return(resiColors)
	})
	
	importCaseData=reactive({
		importDateSelect=as.Date(input$importDateSelect)
		residencyType=residencyType()
		dataList[['PERSON']]$ResidencyType[is.na(dataList[['PERSON']]$ResidencyType)]=dataList[['PERSON']]$Nationality[is.na(dataList[['PERSON']]$ResidencyType)]
		importCaseData=dataList[['PERSON']][which(dataList[['PERSON']]$PersonID %in% dataList[['CLUSTER']]$PersonID[which(dataList[['CLUSTER']]$LinkType=='Direct' & dataList[['CLUSTER']]$Cluster %in% importClusters)]),]
		importCaseData$DateConfirmed=as.Date(importCaseData$DateConfirmed,'%d-%b-%Y')
		importCaseData=importCaseData[which(importCaseData$DateConfirmed>=importDateSelect[1] & importCaseData$DateConfirmed<=importDateSelect[2]),]
		importCaseData$DateConfirmed=as.Date(importCaseData$DateConfirmed,'%d-%b-%Y')
		importCaseData$ResidencyType=factor(importCaseData$ResidencyType,levels=residencyType)
		return(importCaseData)
	})
	
	caseResidImpData=reactive({
		importCaseData=importCaseData()
		caseResidImpData=aggregate(PersonID~ResidencyType+DateConfirmed,importCaseData,length)
		caseResidImpData$PersonID=as.numeric(caseResidImpData$PersonID)
		return(caseResidImpData)
	})
	
	output$caseResImportPlot=renderPlotly({
		caseResidImpData=caseResidImpData()
		resiColors=resiColors()
		plot_ly(caseResidImpData,x=~DateConfirmed,y=~PersonID,color=~ResidencyType,type='bar',colors=resiColors) %>% layout(barmode='stack', title='Import Cases residency type against date confirmed')
	})
	
	
	output$caseResImportPie=renderPlotly({
		caseResidImpData=caseResidImpData()
		resiColors=resiColors()
		caseResidImpAgg=aggregate(PersonID~ResidencyType,caseResidImpData,sum)
		totalCnt=sum(caseResidImpAgg$PersonID)
		plot_ly(caseResidImpAgg,values=~PersonID,labels=~ResidencyType,type='pie',hole = 0.5,marker=list(colors=resiColors,line = list(color = '#FFFFFF', width = 1)),textposition='inside',textinfo='label+percent+value') %>% layout(title='Import Cases Residency type',xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),annotations=list(text=as.character(totalCnt),'showarrow'=F, font=list(size = 40)),showlegend = FALSE)
	
	})
	
	
	
	caseResidData=reactive({
		residencyType=residencyType()
		inputData=dataList[['PERSON']]
		inputData$DateConfirmed=as.Date(inputData$DateConfirmed,'%d-%b-%Y')
		caseResidData=aggregate(PersonID~ResidencyType+DateConfirmed,inputData,length)
		caseResidData$ResidencyType=factor(as.character(caseResidData$ResidencyType),levels=residencyType)
		return(caseResidData)
	})
	
	
	
	output$caseResidencyPlot=renderPlotly({
		caseResidData=caseResidData()
		resiColors=resiColors()
		plot_ly(caseResidData,x=~DateConfirmed,y=~PersonID,color=~ResidencyType,type='bar',marker=list(colors=resiColors)) %>% layout(barmode='stack', title='Cases residency type against date confirmed')
	})
	
	output$caseResidencyPie=renderPlotly({
		caseResidData=caseResidData()
		resiColors=resiColors()
		caseResidAgg=aggregate(PersonID~ResidencyType,caseResidData,sum)
		plot_ly(caseResidAgg,values=~PersonID,labels=~ResidencyType,type='pie',textposition='inside',textinfo='label+percent+value',marker=list(colors=resiColors)) %>% layout(title='Cases Residency type',xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),showlegend = FALSE)
	})
	
	caseNatData=reactive({
		inputData=dataList[['PERSON']]
		inputData$DateConfirmed=as.Date(inputData$DateConfirmed,'%d-%b-%Y')
		caseNatData=aggregate(PersonID~Nationality+DateConfirmed,inputData,length)
		return(caseNatData)
	})
	
	
	
	output$caseNationalePlot=renderPlotly({
		caseNatData=caseNatData()
		plot_ly(caseNatData,x=~DateConfirmed,y=~PersonID,color=~Nationality,type='bar') %>% layout(barmode='stack', title='Cases nationality type against date confirmed')
	})
	
	output$caseNationalePie=renderPlotly({
		caseNatData=caseNatData()
		caseNatAgg=aggregate(PersonID~Nationality,caseNatData,sum)
		plot_ly(caseNatAgg,values=~PersonID,labels=~Nationality,type='pie',textposition='inside',textinfo='label+percent+value') %>% layout(title='Cases Nationality type',xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),showlegend = FALSE)
	})
	
	
	
	output$colorMsgTable=renderUI({
		colorCodeAgg=aggregate(PlaceID~colorCode,PERSONPLACEAgg,length)
		colocCodeCaseCnt=aggregate(CaseCnt~colorCode,PERSONPLACEAgg,sum)
		colorCodeCntMap=setNames(colorCodeAgg$PlaceID,colorCodeAgg$colorCode)
		colocCodeCaseCntMap=setNames(colocCodeCaseCnt$CaseCnt,colocCodeCaseCnt$colorCode)
		
		cntMsg=''
		caseMsg=''
		for (i in c('red','orange','yellow','grey','green'))
		{
			cntMsg=sprintf("%s<td>%s</td>",cntMsg,ifelse(is.na(colorCodeCntMap[i]),0,colorCodeCntMap[i]))
			caseMsg=sprintf("%s<td>%s</td>",caseMsg,ifelse(is.na(colocCodeCaseCntMap[i]),0,colocCodeCaseCntMap[i]))
		}
		
		msg=sprintf("<table border=1 width='50%%'><tr  align='center'><td><b>Color</b></td><td>Red</td><td>Orange</td><td>Yellow</td><td>Grey</td><td>Green</td></tr><tr align='center'><td><b>Days free fr Visit</b></td><td>>=0-3<</td><td>>=3-7<</td><td>>=7-14<</td><td>>=14-28<</td><td>>=28</td></tr><tr align='center'><td><b>Place Count</b></td>%s</tr><tr align='center'><td><b>Case x Visit Count</b></td>%s</tr></table>",cntMsg,caseMsg)
		
		HTML(msg)
	})
	
	PERSONPLACEAggSub=reactive({
		mapColorSelect=input$mapColorSelect
		PERSONPLACEAggSub=PERSONPLACEAgg[which(PERSONPLACEAgg$colorCode %in% mapColorSelect),]
		return(PERSONPLACEAggSub)
	})
	
	output$PPAggSub=renderDT({
		PERSONPLACEAggSub=PERSONPLACEAggSub()
		formatDTDisplay(PERSONPLACEAggSub[,c('Name','Remarks','colorCode','CaseCnt','DaysFrDateEnd','DateStart','DateEnd')],selectChoice='single')
	})
	
	output$PERSONPLACEAggMsg=renderUI({
		PERSONPLACEAggSub=PERSONPLACEAggSub()
		PPAggSub_rows_selected=input$PPAggSub_rows_selected
			if (length(PPAggSub_rows_selected)>0)
			{
				HTML(PERSONPLACEAggSub$Display[PPAggSub_rows_selected])
			}
		
	})
	
	output$trackMap=renderLeaflet({
		mapColorSelect=input$mapColorSelect
		PERSONPLACEAggSub=PERSONPLACEAggSub()
		PPAggSub_rows_selected=input$PPAggSub_rows_selected
		
		if (length(PPAggSub_rows_selected)>0)
		{
			PERSONPLACEAggSub$icon[PPAggSub_rows_selected]='Star'
		}
		
		basemap=leaflet() %>% addTiles(group = "OSM")  #%>% addProviderTiles("Stamen.TonerLite")
		basemap  %>% addMarkers(data = PERSONPLACEAggSub, ~longitude,~latitude,popup = ~Display,icon=placeIcons[PERSONPLACEAggSub$icon],clusterOptions = markerClusterOptions(),layerId=PERSONPLACEAggSub$PlaceID)
	})
	
	output$placeTimeVisTbl=renderTimevis({
		PPAggSub_rows_selected=input$PPAggSub_rows_selected
		PERSONPLACEAggSub=PERSONPLACEAggSub()
		if (length(PPAggSub_rows_selected)>0)
		{
			PERSONPLACESub=PERSONPLACE[which(PERSONPLACE$PlaceID %in% PERSONPLACEAggSub$PlaceID[PPAggSub_rows_selected]),]
		
			data1=data.frame(id=PERSONPLACESub$PersonID,start=PERSONPLACESub$DateStart,end=PERSONPLACESub$DateEnd,content=sprintf("Case%d (%s)",PERSONPLACESub$PersonID,PERSONPLACESub$Remarks),stringsAsFactors=FALSE)
			data1=data.frame(id=PERSONPLACESub$PersonID,start=PERSONPLACESub$DateStart,end=PERSONPLACESub$DateEnd,content=sprintf("Case%d (%s)",PERSONPLACESub$PersonID,PERSONPLACESub$Remarks),stringsAsFactors=FALSE)
			data1$type='range'
			data1=rbind(data1,data.frame(id=max(data1$id)+1,start=format(timeVisStartDate,"%d-%b-%Y"),end=format(todayDate,"%d-%b-%Y"),content='',type='background'))
			timevis(data1,options = list(editable = TRUE, multiselect = TRUE, align = "center"))
		}
		
	})
	
	
	
	
	output$clusterMap=renderLeaflet({
		mapColorSelect=input$mapColorSelect
		
		PersonPlaceSub=data.table(dataList[['PERSONPLACE']][which(dataList[['PERSONPLACE']]$Remarks!='Warded' & dataList[['PERSONPLACE']]$colorCode %in% mapColorSelect),])
		basemap=leaflet() %>% addTiles(group = "OSM")  #%>% addProviderTiles("Stamen.TonerLite")
		if (nrow(PersonPlaceSub)>0)
		{
			ClusterSummary=PersonPlaceSub[,.(DateEnd=max(DateEnd),DateStart=min(DateStart),CaseVisitDayCnt=.N,DaysFrDateEnd=min(DaysFrDateEnd),Places=paste(unique(Name),collapse=", ")),by=list(PlaceCluster,PlaceClusterLongitude,PlaceClusterLatitude)]
			basemap %>% addMinicharts(ClusterSummary$PlaceClusterLongitude,ClusterSummary$PlaceClusterLatitude,chartdata=ClusterSummary$CaseVisitDayCnt,showLabels=TRUE,width=45)
		} else
		{
			basemap
		}
	})
	
	output$clusterDT=renderDT({
		mapColorSelect=input$mapColorSelect
		
		PersonPlaceSub=data.table(dataList[['PERSONPLACE']][which(dataList[['PERSONPLACE']]$Remarks!='Warded' & dataList[['PERSONPLACE']]$colorCode %in% mapColorSelect),])
		basemap=leaflet() %>% addTiles(group = "OSM")  #%>% addProviderTiles("Stamen.TonerLite")
		PersonPlaceSub$Case=sprintf("Case%d",PersonPlaceSub$PersonID)
		formatDTDisplay(PersonPlaceSub[,c('PlaceCluster','Name','Case','colorCode','DateStart','DateEnd','DaysFrDateEnd')])
	})
	
	# observeEvent(input$placeTimeVisTbl_selected, {
		# showModal(modalDialog(
		  # title = "Somewhat important message",
		  # paste(input$placeTimeVisTbl_selected, "has been selected"),
		  # easyClose = TRUE,
		  # footer = NULL
		# ))
	  # })
	
	data <- reactiveValues(clickedMarker=NULL)
	
	observeEvent(input$trackMap_marker_click,{
		data$clickedMarker = input$trackMap_marker_click
		
		selectedPERSONPLACE=PERSONPLACEAgg[which(PERSONPLACEAgg$PlaceID==data$clickedMarker$id),]
		PERSONPLACESub=PERSONPLACE[which(PERSONPLACE$PlaceID %in% selectedPERSONPLACE$PlaceID),]
		
		output$mapSelectMsg=renderUI({
			#DateEnd=as.Date(max(selectedPERSONPLACE$DateEnd),format='%d-%b-%Y')
			#daysPast=as.numeric(difftime(todayDate,DateEnd,unit='days'))
			# msg=sprintf("<table><tr bgcolor='%s'><td><b>%s</b><br />Last visit: %s (Estimated last appearance %d days ago)<br />%d case(s) visited</td></tr></table>",dayColorCode(daysPast),unique(selectedPERSONPLACE$Name),DateEnd,daysPast,nrow(selectedPERSONPLACE))
			# msg=sprintf("%s<br><table>",msg)
			# for (i in 1:nrow(selectedPERSONPLACE))
			# {
				# msg=sprintf("%s<tr bgcolor='lightgray'><td>Case%s</td></tr>",msg,selectedPERSONPLACE$PersonID[i])
				# msg=sprintf("%s<tr><td>%s</td></tr>",msg,selectedPERSONPLACE$VisitInfo[i])
				# msg=sprintf("%s<tr><td>%s</td></tr>",msg,selectedPERSONPLACE$Report[i])
			# }
			# msg=sprintf("%s</table>",msg)
			
			HTML(selectedPERSONPLACE$Display)
		})
		
		
		output$placeTimeVis=renderTimevis({
			
			data1=data.frame(id=PERSONPLACESub$PersonID,start=PERSONPLACESub$DateStart,end=PERSONPLACESub$DateEnd,content=sprintf("Case%d (%s)",PERSONPLACESub$PersonID,PERSONPLACESub$Remarks),type='range',stringsAsFactors=FALSE)
			
			data1=rbind(data1,data.frame(id='period',start=format(timeVisStartDate,"%d-%b-%Y"),end=format(todayDate,"%d-%b-%Y"),content='',type='background'))
			timevis(data1)
		})
	})
	
	
	output$clusterForm=renderUI({
		CLUSTER=dataList[['CLUSTER']]
		selectInput('clusterSelect','Highght Cluster',c('All',unique(CLUSTER$Cluster)),'All')
	})
	
	
	networkList=reactive({
			
		shiny::isolate({
			shiny::withProgress({
				shiny::setProgress(message = "Preparing network")
				
				reportMap=setNames(dataList[['PERSON']]$Notes,as.character(dataList[['PERSON']]$PersonID))
				
				nodesPerson=data.frame(id=sprintf("Case%d",dataList[['PERSON']]$PersonID),label=sprintf("Case%s %s(%s, %s)",dataList[['PERSON']]$PersonID,dataList[['PERSON']]$Gender,dataList[['PERSON']]$Age,dataList[['PERSON']]$Source),Source=dataList[['PERSON']]$Source,stringsAsFactors=FALSE)
				nodesPerson$Type='Warded'
				nodesPerson$Source=NULL
				nodesPerson$title=sprintf("%s<br />%s",nodesPerson$id,reportMap[gsub("Case","",nodesPerson$id)])
				#nodesPlace=data.frame(id=sprintf("Place%d",dataList[['PLACE']]$PlaceID),label=sprintf("%s",dataList[['PLACE']]$Name),stringsAsFactors=FALSE)
				# nodesPlace$Type='PLACE'
				
				nodesCluster=data.frame(id=unique(dataList[['CLUSTER']]$Cluster),label=sprintf("(Cluster)%s",unique(unique(dataList[['CLUSTER']]$Cluster))),stringsAsFactors=FALSE)
				nodesCluster$Type='Cluster'
				
				CLUSTER=dataList[['CLUSTER']]
				CLUSTER$Cluster[which(CLUSTER$Cluster %in% c('UNLINKED','OTH_Linked'))]='Local unknown source'
				clusterAgg=aggregate(PersonID~Cluster+LinkType,CLUSTER,length)
				clusterAggSrd=spread(clusterAgg,key='LinkType',value='PersonID')
				clusterAggSrd[is.na(clusterAggSrd)]=0
				clusterMap=setNames(sprintf("%d cases directly linked. %d cases indirectly linked.",clusterAggSrd$Direct,clusterAggSrd$Indirect),clusterAggSrd$Cluster)
				nodesCluster$title=sprintf("%s<br />%s",nodesCluster$id,clusterMap[nodesCluster$id])
				
				#nodes=rbind(nodesPerson,nodesPlace)
				nodes=rbind(nodesPerson,nodesCluster)
				
				colors = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))[1:length(unique(nodes$Type))]

				#nodes$color=colors[as.numeric(as.factor(nodes$Type))]
				
				#shapeMap=setNames(c('dot','triangle','star'),c('PERSON','PLACE','Cluster'))
				#nodes$shape=shapeMap[nodes$Type]
				#nodes$shape=shapeMap[nodes$Type]
				
				edgePerson=data.frame(from=sprintf("Case%d",dataList[['PERSONPERSON']]$`PersonID1`),to=sprintf("Case%d",dataList[['PERSONPERSON']]$`PersonID2`),label=sprintf("(Case%d)%s-(Case%d)%s",dataList[['PERSONPERSON']]$`PersonID1`,dataList[['PERSONPERSON']]$Relation1,dataList[['PERSONPERSON']]$`PersonID2`,dataList[['PERSONPERSON']]$Relation2),stringsAsFactors=FALSE)
				edgePerson$Type='PERSON'
				
				#placeMap=setNames(dataList[['PLACE']]$Name,dataList[['PLACE']]$PlaceID)
				
				#edgePlace=data.frame(from=sprintf("Case%d",dataList[['PERSONPLACE']]$`PersonID`),to=sprintf("Place%d",dataList[['PERSONPLACE']]$`PlaceID`),label=sprintf("%s-%s-%s (%s-%s)",dataList[['PERSONPLACE']]$PersonID,dataList[['PERSONPLACE']]$Remarks,placeMap[dataList[['PERSONPLACE']]$PlaceID],dataList[['PERSONPLACE']]$DateStart,dataList[['PERSONPLACE']]$DateEnd),Remarks=dataList[['PERSONPLACE']]$Remarks,stringsAsFactors=FALSE)
				#edgePlace$Type='PLACE'
				#edgePlace$Type[which(edgePlace$Remarks=='Warded')]='HOSP'
				#edgePlace$Remarks=NULL
				
				
				
				edgeCluster=data.frame(from=dataList[['CLUSTER']]$Cluster,to=sprintf("Case%d",dataList[['CLUSTER']]$PersonID),label=sprintf("Case%s linked to Cluster %s",dataList[['CLUSTER']]$PersonID,dataList[['CLUSTER']]$Cluster),stringsAsFactors=FALSE)
				edgeCluster$Type='Cluster'
				
				#edges=rbind(edgePerson,edgePlace)
				edges=rbind(edgePerson,edgeCluster)
				
				nodeValue=table(c(edges$to[which(edges$Type %in% c('PERSON','Cluster'))],edges$from[which(edges$Type %in% c('PERSON','Cluster'))]))
				
				nodes$value=nodeValue[nodes$id]
				nodes$value[which(is.na(nodes$value))]=0
				
				nodes$title=nodes$label
				
				edges$title=edges$label
				
				edges$label=''
				#edges$label[which(edges$value<=5)]=''
				
				nodes$font.size=unlist(lapply(nodes$value,function(x) min(x*10,100)))
				nodes$font.size[which(nodes$Type!='Cluster')]=10
				
				#nodes$color[which(nodes$Type=='Cluster')]='orange'
				
				# for (i in 1:nrow(nodes))
				# {
					# if (length(grep("PERSON",nodes$Type[i]))>0)
					# {
						# filename=sprintf("%s/%s",reportDir,nodes$id[i])
						# if (file.exists(filename))
						# {
							# reportOrg=readChar(filename, file.info(filename)$size)
							# report=gsub("\r\n","<br />",reportOrg)
							# #print(report)
							# nodes$title[i]=sprintf("%s<br />%s",nodes$title[i],report)
						# }
					# }
				# }
				
				# nodes$title[i]=sprintf("%s<br />%s",nodes$title[i],reportMap[gsub("Case","",nodes$id)])
				
				deathList=dataList[['PERSON']]$PersonID[which(!is.na(dataList[['PERSON']]$Death))]
				dischargeList=setdiff(dataList[['PERSON']]$PersonID[which(!is.na(dataList[['PERSON']]$Discharge))],deathList)
				
				nodes$group=nodes$Type
				nodes$group[which(nodes$id %in% sprintf("Case%d",deathList))]='Deceased'
				nodes$group[which(nodes$id %in% sprintf("Case%d",dischargeList))]='Discharged'
				
				networkList=list()
				networkList[['nodes']]=nodes
				networkList[['edges']]=edges
				
				return(networkList)
			})
		})
	})
		
		
		
	output$personDT=renderDT({
		PERSONTMP=dataList[['PERSON']]
		PERSONTMP$Report=NULL
		PERSONTMP$Info=NULL
		PERSONTMP$Info1=NULL
		formatDTDisplay(PERSONTMP)
	})
	
	output$lobNetwork=renderVisNetwork({
		#clusterSelect=input$clusterSelect
		#personDT_rows_selected=input$personDT_rows_selected
		networkList=networkList()
		#if (!is.null(clusterSelect))
		#{
			#CLUSTER=dataList[['CLUSTER']]
			nodes=networkList[['nodes']]
			edges=networkList[['edges']]
			#edges=edges[which(edges$Type!='HOSP'),]
			
			
			#nodes$color[which(nodes$id %in% sprintf("Case%d",dataList[['PERSON']]$PersonID[which(dataList[['PERSON']]$Transport=='Evacuation')]))]='lightblue'
			
			# if (length(personDT_rows_selected)>0)
			# {
				# nodes$shape[which(nodes$id %in% sprintf("Case%d",dataList[['PERSON']]$PersonID[personDT_rows_selected]))]='diamond'
				# nodes$color[which(nodes$id %in% sprintf("Case%d",dataList[['PERSON']]$PersonID[personDT_rows_selected]))]='red'
				# nodes$value[which(nodes$id %in% sprintf("Case%d",dataList[['PERSON']]$PersonID[personDT_rows_selected]))]=10
				
			# }
			
			
			
			# if (clusterSelect!='All')
			# {
				
				# clusterList=CLUSTER$PersonID[which(CLUSTER$Cluster %in% clusterSelect)]
				# nodes$groupback=nodes$group
				# nodes$group=sprintf("%s-grey",nodes$groupback)
				# nodes$group[which(nodes$id %in% sprintf("Case%d",clusterList))]=nodes$groupback[which(nodes$id %in% sprintf("Case%d",clusterList))]
				# nodes$group[which(nodes$id %in% sprintf("Place%d",dataList[['PERSONPLACE']]$PlaceID[which(dataList[['PERSONPLACE']]$PersonID %in% clusterList)]))]=nodes$groupback[which(nodes$id %in% sprintf("Place%d",dataList[['PERSONPLACE']]$PlaceID[which(dataList[['PERSONPLACE']]$PersonID %in% clusterList)]))]
				# nodes$group[which(nodes$id==clusterSelect)]=nodes$groupback[which(nodes$id==clusterSelect)]
				
			# }
		
			
			
			
			
			#graph <- graph.data.frame(edges)
			#degree_value <- degree(graph)
			
			nodes$value=2*nodes$value
			
			if (nrow(nodes)>0 & nrow(edges)>0)
			{
				visNetworkGraph=visNetwork(nodes, edges, width = "100%") %>% visEvents(click = "function(nodes){ Shiny.onInputChange('click', nodes.nodes[0]);;}") %>% visGroups(groupname="Discharged",color='lightgreen',shape='dot') %>% visGroups(groupname="Deceased",color='red',shape='dot') %>% visGroups(groupname="Warded",color='lightblue',shape='dot') %>% visGroups(groupname="Cluster",color='orange',shape='star') %>% visLegend()
				# if (clusterSelect!='All')
				# {
					# visNetworkGraph = visNetworkGraph %>% visGroups(groupname="Imported-grey",color='grey',shape='dot') %>% visGroups(groupname="Local Tx-grey",color='grey',shape='dot') %>% visGroups(groupname="PLACE-grey",color='grey',shape='triangle') %>% visGroups(groupname="HOSP-grey",color='grey',shape='diamond') %>% visGroups(groupname="Cluster-grey",color='grey',shape='star')
				# }
				return(visNetworkGraph)
			} else
			{
				return(NULL)
			}
		#}
	})
	
	
	output$personVisitMapMsg=renderUI({
		nodeid=input$click
		networkList=networkList()
		nodes=networkList[['nodes']]
		selectedNode=nodes[which(nodes$id==nodeid),]
		if (length(grep('^Case',selectedNode$id))>0)
		{
			msg=sprintf("Places visited by %s",selectedNode$id)
		} else
		if (length(grep('^Place',selectedNode$id))>0)
		{
			placename=dataList[['PLACE']]$Name[which(sprintf("Place%d",dataList[['PLACE']]$PlaceID)==nodeid)]
			msg=sprintf("Location: %s (%s)<br />Exposure period of cases at this location",selectedNode$id,placename)
		} else
		{
			msg=sprintf("Cluster: %s<br />Cases in cluster public exposure period",selectedNode$id)
		}
		HTML(msg)
	})
	
	output$personVisitMap=renderLeaflet({
		nodeid=input$click
		
		if (length(grep("^Case",nodeid))>0)
		{
			networkList=networkList()
			nodes=networkList[['nodes']]
			selectedNode=nodes[which(nodes$id==nodeid),]
			PERSONPLACE=dataList[['PERSONPLACE']]
			if (length(grep("PERSON",selectedNode$Type))>0)
			{
				PersonID=gsub("Case","",selectedNode$id)
				PERSONPLACESub=PERSONPLACE[which(PERSONPLACE$PersonID==PersonID),]
				basemap=leaflet() %>% addTiles(group = "OSM")  #%>% addProviderTiles("Stamen.TonerLite")
				basemap  %>% addMarkers(data = PERSONPLACESub, ~longitude,~latitude,icon=placeIcons[PERSONPLACESub$icon],popup = ~Display,clusterOptions = markerClusterOptions())
			}
		}
	})
	
	
	output$mapMsg=renderUI({
		nodeid=input$click
		if ('click' %in% names(input))
		{
			if (!is.null(nodeid))
			{
				if (length(grep("^Case",nodeid))>0)
				{
					htmltools::tagList(list(leafletOutput('personVisitMap'),renderUI(HTML(dataList[['PERSON']]$Report[which(dataList[['PERSON']]$PersonID==gsub('Case','',nodeid))]))))
					
				} else
				if (length(grep("^Place",nodeid))>0)
				{
					timevisOutput('mapMsgTimevisPlace')
				} else
				if (nodeid %in% dataList[['CLUSTER']]$Cluster)
				{
					timevisOutput('mapMsgTimevisCluster')
					#dataList[['CLUSTER']][which(dataList[['CLUSTER']]$Cluster==nodeid)]
				}
			}
		}
	})
	
	output$mapMsgTimevisPlace=renderTimevis({
		nodeid=input$click
		PERSONPLACESub=dataList[['PERSONPLACE']][which(dataList[['PERSONPLACE']]$PlaceID %in% gsub("Place","",nodeid)),]
		tmpData=data.frame(start=PERSONPLACESub$DateStart,end=PERSONPLACESub$DateEnd,content=sprintf("Case%d",PERSONPLACESub$PersonID),type='range',stringsAsFactors=FALSE)
		tmpDataA=rbind(tmpData,data.frame(start=format(min(as.Date(tmpData$start,'%d-%b-%Y')),'%d-%b-%Y'),end=format(todayDate,'%d-%b-%Y'),content='',type='background'))
		timevis(tmpDataA)
	})
	
	output$mapMsgTimevisCluster=renderTimevis({
		data1=data1()
		nodeid=input$click
		
		data1Sub=data1[which(data1$groupName==nodeid),]
		timevis(data1Sub)
	})
	
	data1=reactive({
		PERSON=dataList[['PERSON']]
		PERSON$Notes[which(is.na(PERSON$Notes))]=''
		data1=data.frame(Age=PERSON$Age,id=PERSON$PersonID,start=PERSON$ExpStartDate,end=PERSON$IsolationDate,content=sprintf("Case%d (%s,%s yrs old), %s, Confirmed: %s",PERSON$PersonID,PERSON$Gender,PERSON$Age,PERSON$Notes,format(PERSON$AnnouncementDate,'%d-%b-%Y')),groupName=PERSON$groupName,stringsAsFactors=FALSE)
		
		data1=data1[order(data1$id,decreasing=TRUE),]
		groupMap=setNames(1:length(unique(data1$groupName)),unique(data1$groupName))
		data1$group=groupMap[data1$groupName]
		#data1$end=format(data1$end,"%d-%b-%Y")
		data1$end[which(is.na(data1$end))]=format(todayDate,"%d-%b-%Y")
		data1$className="greenBg"
		data1$className[which(data1$Age>60)]="blueBg"
		data1$type='range'
		data1$Age=NULL
		data1=rbind(data1,data.frame(id=max(data1$id)+1,start=format(timeVisStartDate,"%d-%b-%Y"),end=format(todayDate,"%d-%b-%Y"),content='',type='background',groupName=NA,group=NA,className='whiteBg'))
		data1$start=as.Date(data1$start,'%d-%b-%Y')
		data1$end=as.Date(data1$end,'%d-%b-%Y')
		return(data1)
	})
	
	output$personExpTimeline=renderTimevis({
		data1=data1()
		groups=data.frame(id=1:length(unique(data1$groupName[which(!is.na(data1$groupName))])),content=ClusterSummaryNoteMap[unique(data1$groupName[which(!is.na(data1$groupName))])])
		timevis(data1,groups)
	})
	
	
	
	
	
	output$expCntChart=renderPlotly({
		data1=data1()
		minDate=min(data1$start)
		maxDate=max(data1$end)
		
		expDateDf=NULL
		for (i in as.character(seq(minDate,maxDate,by='days')))
		{
			iDate=as.Date(i,'%Y-%m-%d')
			tmpData=data1[which(data1$start<=iDate & data1$end>=iDate),]
			expDateDf=rbind(expDateDf,data.frame(Date=i,Count=nrow(tmpData),stringsAsFactors=FALSE))
		}
		
		expDateDf$Date=as.Date(expDateDf$Date,'%Y-%m-%d')
		plot_ly(expDateDf,x=~Date,y=~Count,mode='lines+markers')
	})
	
	output$data1=renderDT({
	
		data1=data1()
		formatDTDisplay(data1)
	})
	
	
	
	
	
	##################################################################
	# By Date
	##################################################################
	# output$DateInfectPlot=renderPlotly({
		# 
	# })
	
	output$expCntChartDate=renderPlotly({
		data1=data1()
		minDate=min(data1$start)
		maxDate=max(data1$end)
		PERSON$DateConfirmed=as.Date(PERSON$DateConfirmed,'%d-%b-%Y')
		expDateDf=NULL
		for (i in as.character(seq(minDate,maxDate,by='days')))
		{
			iDate=as.Date(i,'%Y-%m-%d')
			tmpData=data1[which(data1$start<=iDate & data1$end>=iDate),]
			cntConfirmed=ifelse(length(which(as.character(dataList[['TimeTrack']]$Date,'%Y-%m-%d')==i))>0,dataList[['TimeTrack']]$Positive[which(as.character(dataList[['TimeTrack']]$Date,'%Y-%m-%d')==i)],0)
			expDateDf=rbind(expDateDf,data.frame(Date=i,ExposureCount=nrow(tmpData),InfectedCount=cntConfirmed,stringsAsFactors=FALSE))
		}
		
		expDateDf$Date=as.Date(expDateDf$Date,'%Y-%m-%d')
		expDateDfMelt=melt(expDateDf,id.vars='Date')
		plot_ly(expDateDfMelt,x=~Date,y=~value,color=~variable,mode='lines+markers')
	})
	
	TimeTrackSub=reactive({
		TimeTrack=dataList[['TimeTrack']]
		TimeTrack=TimeTrack[order(TimeTrack$Date),]
		TimeTrackSub=data.frame(TimeTrack[,c('Date','Pending','Positive','Negative','Isolated','CloseContactInSG','Critical','Discharge')])
		
		for (i in colnames(TimeTrackSub)[2:ncol(TimeTrackSub)])
		{
			timeSeries=as.numeric(TimeTrackSub[,i])
			index=which(is.na(timeSeries))
			for (j in 1:length(index))
			{
				timeSeries[index[j]]=floor((timeSeries[index[j]-1]+timeSeries[index[j]+1])/2)
			}
			TimeTrackSub[,i]=timeSeries
		}
		TimeTrackSub$CloseContactFactor=TimeTrackSub$CloseContactInSG/TimeTrackSub$Positive
		return(TimeTrackSub)
	})
	
	TimeTrackMelt=reactive({
		TimeTrackSub=TimeTrackSub()
		TimeTrackMelt=melt(TimeTrackSub,id.vars='Date')
		TimeTrackMelt$Date=as.Date(TimeTrackMelt$Date)
		return(TimeTrackMelt)
	})
	
	TimeTrackDiff=reactive({
		TimeTrackSub=TimeTrackSub()
		
		TimeTrackDiff=data.frame(Date=TimeTrackSub$Date[2:nrow(TimeTrackSub)])
		for (i in c(setdiff(colnames(TimeTrackSub),c('Date','Pending','Critical','Discharge','CloseContactFactor'))))
		{
			TimeTrackDiff[,i]=diff(as.numeric(TimeTrackSub[,i]),1)
		}
		
		return(TimeTrackDiff)
	})
	
	
	
	# tsDf=data.frame(x=1:nrow(TimeTrackMelt),y=as.numeric(TimeTrackMelt$value[which(TimeTrackMelt$variable=='CloseContactFactor')]),stringsAsFactors=FALSE)
	# coef(lm(y~x,tsDf))
	
	# tsDf=data.frame(x=1:nrow(TimeTrackDiffMelt),y=as.numeric(TimeTrackDiffMelt$value[which(TimeTrackDiffMelt$variable=='CloseContactIncFactor')]),stringsAsFactors=FALSE)
	# tsDf$y[which(tsDf$y==Inf)]=0
	# coef(lm(y~x,tsDf))
	
	# tsDf=data.frame(x=1:nrow(TimeTrackDiffMelt),y=as.numeric(TimeTrackDiffMelt$value[which(TimeTrackDiffMelt$variable=='PositiveInc')]),stringsAsFactors=FALSE)
	# coef(lm(y~x,tsDf))
	
	
	
	
	output$contactTraceChartDate=renderPlotly({
		TimeTrackMelt=TimeTrackMelt()
		
		TimeTrackMeltSub=TimeTrackMelt[which(TimeTrackMelt$variable %in% c('Positive','CloseContactFactor')),]
		plot_ly(TimeTrackMeltSub,x=~Date,y=~value,color=~variable,mode='lines+markers')
	})
	
	TimeTrackDiffMelt=reactive({
		TimeTrackDiff=TimeTrackDiff()
		TimeTrackDiff$CloseContactIncFactor=TimeTrackDiff$CloseContactInSG/TimeTrackDiff$Positive
		TimeTrackDiff$CloseContactIncFactor[which(is.na(TimeTrackDiff$CloseContactFactor))]=0
		TimeTrackDiff$PositiveInc=TimeTrackDiff$Positive
		TimeTrackDiffMelt=melt(TimeTrackDiff[,c('Date','PositiveInc','CloseContactIncFactor')],id.vars='Date')
		return(TimeTrackDiffMelt)
	})
	
	
	output$traceFactorIncPlot=renderPlotly({
		TimeTrackDiffMelt=TimeTrackDiffMelt()
		plot_ly(TimeTrackDiffMelt,x=~Date,y=~value,color=~variable,mode='lines+markers')
	})
	
	TimeTrackDiffSprd=reactive({
		TimeTrackDiff=TimeTrackDiff()
		TimeTrackDiff$PosNegRatio=TimeTrackDiff$Positive/(TimeTrackDiff$Negative+TimeTrackDiff$Positive)
		TimeTrackDiffSprd=melt(TimeTrackDiff[,c('Date','Positive','Negative','PosNegRatio')],id.vars=c('Date','PosNegRatio'))
		#plot_ly(TimeTrackDiffSprd,x=~Date,y=~value,color=~variable,type='bar') %>% layout(barmode='stack')
		TimeTrackDiffSprd=TimeTrackDiffSprd[order(TimeTrackDiffSprd$Date),]
		return(TimeTrackDiffSprd)
	})
	
	
	output$posNegRatioPlot=renderPlotly({
		TimeTrackDiffSprd=TimeTrackDiffSprd()
		plot_ly(TimeTrackDiffSprd) %>% add_trace(x=~Date,y=~value,color=~variable,type='bar') %>% add_trace(x=~Date,y=~PosNegRatio,yaxis='y2',type = 'scatter', mode = 'lines',name='PosOutcomeRatio') %>% layout(barmode = 'stack',title = 'Srv Sales', xaxis = list(title = "FY_QR"), yaxis = list(side = 'left', title = 'TestCount', showgrid = FALSE, zeroline = FALSE), yaxis2 = list(side = 'right', overlaying = "y", title = 'Ratio',range=c(0,1), showgrid = FALSE, zeroline = FALSE))
	})
	
	output$CaseTrend=renderPlotly({
		TimeTrackSub=TimeTrackSub()
		
		TimeTrackSubSub=TimeTrackSub[,c('Date','Positive','Discharge','Critical')]
		TimeTrackSubSub$NormalWard=TimeTrackSubSub$Positive-TimeTrackSubSub$Discharge-TimeTrackSubSub$Critical
		CaseMelt=melt(TimeTrackSubSub[,c('Date','NormalWard','Critical','Discharge')],id.vars='Date')
		
		plot_ly(CaseMelt,x=~Date,y=~value,color=~variable,type='bar') %>% layout(barmode='stack')
	})
	
	######################################################################
	# Indicators
	######################################################################
	### Admission rate vs discharge rate
	
	output$dateMsg=renderUI({
		DateSelect=input$DateSelect
		HTML(sprintf("<h3>Information as of %s</h3>",DateSelect))
	})
	
	output$indicatorPlot <- renderUI({
		indCols=c('Date','WardIncInd','CloseContactIncInd','QuarantineCurrentIncInd','UnlinkCnt','ExpCaseInd')
		selectedCol=c(indCols,indicatorSource[indCols])
		selectedCol=selectedCol[which(!is.na(selectedCol))]
		plotData=dataList[['TimeTrack']][,selectedCol]
		displayMap=setNames(c('Inpatient','Close Contact','Quarantined','Unlinked Cases','Case Exposure'),c('WardIncInd','CloseContactIncInd','QuarantineCurrentIncInd','UnlinkCnt','ExpCaseInd'))
		tagList(
		  lapply(c('WardIncInd','CloseContactIncInd','QuarantineCurrentIncInd','UnlinkCnt','ExpCaseInd'), function(i){
			box(width=6,title=sprintf("Indicator %s and its source historical trend",i)
				,renderPlotly({
					subplotList=list()
					dataTmp=plotData[,c('Date',i)]
					names(dataTmp)[2]='Indicator'
					dataTmp$Indicator[which(dataTmp$Indicator>0)]=1
					dataTmp$Sum5DayInd=c(dataTmp$Indicator[1:4],rollsum(dataTmp$Indicator,5))
					colorCode=factor(indicatorColorMap[as.character(dataTmp$Sum5DayInd)],levels=unique(indicatorColorMap[as.character(dataTmp$Sum5DayInd)]))
					subplotList[[1]]=plot_ly(dataTmp) %>% add_trace(x=~Date,y=~Sum5DayInd,mode='markers',color=colorCode,colors=unique(indicatorColorMap[as.character(dataTmp$Sum5DayInd)]),marker=list(size=15)) %>% layout(yaxis=list(title=displayMap[i],range=c(-1,6)))
					if (!is.na(indicatorSource[i]))
					{
						dataTmp=plotData[,c('Date',indicatorSource[i])]
						names(dataTmp)[2]='IndicatorSrc'
						subplotList[[2]]=plot_ly(dataTmp) %>% add_trace(x=~Date,y=~IndicatorSrc,mode='lines') %>% layout(yaxis=list(title=sprintf("Source of %s",displayMap[i]))) %>% add_segments(x=min(dataTmp$Date),xend=max(dataTmp$Date),y=~median(dataTmp$IndicatorSrc),yend=~median(dataTmp$IndicatorSrc),mode='lines')
					}
					if (i=='UnlinkCnt')
					{
						dataTmp=plotData[,c('Date',i)]
						names(dataTmp)[2]='IndicatorSrc'
						subplotList[[2]]=plot_ly(dataTmp) %>% add_trace(x=~Date,y=~IndicatorSrc,mode='lines') %>% layout(yaxis=list(title=sprintf("Source of %s",displayMap[i]))) %>% add_segments(x=min(dataTmp$Date),xend=max(dataTmp$Date),y=~median(dataTmp$IndicatorSrc),yend=~median(dataTmp$IndicatorSrc),mode='lines')
					}
					subplot(subplotList,nrows=1)
				})
			)
			
		  })
		)
	})
	
	# output$indicatorPlot=renderUI({
		# plotData=dataList[['TimeTrack']][,c('Date','WardIncInd','CloseContactIncInd','QuarantineCurrentIncInd','UnlinkCnt','ExpCaseInd')]
		
		# displayMap=setNames(c('Inpatient','Close Contact','Quarantined','Unlinked Cases','Case Exposure'),c('WardIncInd','CloseContactIncInd','QuarantineCurrentIncInd','UnlinkCnt','ExpCaseInd'))
		# for (i in c('WardIncInd','CloseContactIncInd','QuarantineCurrentIncInd','UnlinkCnt','ExpCaseInd'))
		# tagList(lapply(1:length(c('WardIncInd','CloseContactIncInd','QuarantineCurrentIncInd','UnlinkCnt','ExpCaseInd')),function(i)
		# {
			# box(width=12
				# ,renderPlotly({
					# dataTmp=plotData[,c('Date',i)]
					# names(dataTmp)[2]='Indicator'
					# dataTmp$Indicator[which(dataTmp$Indicator>0)]=1
					# dataTmp$Sum5DayInd=c(dataTmp$Indicator[1:4],rollsum(dataTmp$Indicator,5))
					# colorCode=factor(indicatorColorMap[as.character(dataTmp$Sum5DayInd)],levels=unique(indicatorColorMap[as.character(dataTmp$Sum5DayInd)]))
					# plot_ly(dataTmp) %>% add_trace(x=~Date,y=~Sum5DayInd,mode='markers',color=colorCode,colors=unique(indicatorColorMap[as.character(dataTmp$Sum5DayInd)]),marker=list(size=15)) %>% layout(yaxis=list(title=displayMap[i],range=c(-1,6)))
				# })
			# )
		# }))
		
	# })
	
	output$indicatorPlotUI=renderUI({
		msg=paste(indicatorDisplay,collapse="<br />")
		return(HTML(msg))
	})
	
	output$trafficLight=renderUI({
		DateSelect=input$DateSelect
		
		#TimeTrackSubPrev=dataList[['TimeTrack']][which(dataList[['TimeTrack']]$Date<=(as.Date(DateSelect)-1)),]
		TimeTrackSub=dataList[['TimeTrack']][which(dataList[['TimeTrack']]$Date<=DateSelect),]
		TimeTrackSub=TimeTrackSub[order(TimeTrackSub$Date),]
		TimeTrackSubPrev=TimeTrackSub[1:(nrow(TimeTrackSub)-1),]
		
		#################################
		indicatorListPrev=list()
		
		indicatorListPrev[['Inpatient']]=list()
		WardIncInd=sum(tail(TimeTrackSubPrev$WardIncInd,5))
		indicatorListPrev[['Inpatient']][['bgcolor']]=indicatorColorMap[as.character(WardIncInd)]
		indicatorListPrev[['Inpatient']][['ind']]=WardIncInd
		
		indicatorListPrev[['Close Contact']]=list()
		ContactInd=sum(tail(TimeTrackSubPrev$CloseContactIncInd,5))
		indicatorListPrev[['Close Contact']][['bgcolor']]=indicatorColorMap[as.character(ContactInd)]
		indicatorListPrev[['Close Contact']][['ind']]=ContactInd
		
		indicatorListPrev[['Quaran-tined']]=list()
		QuarantineCurrentIncInd=sum(tail(TimeTrackSubPrev$QuarantineCurrentIncInd,5))
		indicatorListPrev[['Quaran-tined']][['bgcolor']]=indicatorColorMap[as.character(QuarantineCurrentIncInd)]
		indicatorListPrev[['Quaran-tined']][['ind']]=QuarantineCurrentIncInd
		
		indicatorListPrev[['Unlinked Cases']]=list()
		TimeTrackSubPrev$UnlinkCnt[which(TimeTrackSubPrev$UnlinkCnt>0)]=1
		unlinkInd=sum(tail(TimeTrackSubPrev$UnlinkCnt,5))
		indicatorListPrev[['Unlinked Cases']][['bgcolor']]=indicatorColorMap[as.character(unlinkInd)]
		indicatorListPrev[['Unlinked Cases']][['ind']]=unlinkInd
		
		# indicatorListPrev[['Case Exposure']]=list()
		# expCaseInd=sum(tail(TimeTrackSubPrev$ExpCaseInd,5))
		# indicatorListPrev[['Case Exposure']][['bgcolor']]=indicatorColorMap[as.character(expCaseInd)]
		# indicatorListPrev[['Case Exposure']][['ind']]=expCaseInd
		
		
		####################
		indicatorList=list()
		
		indicatorList[['Inpatient']]=list()
		WardIncInd=sum(tail(TimeTrackSub$WardIncInd,5))
		indicatorList[['Inpatient']][['bgcolor']]=indicatorColorMap[as.character(WardIncInd)]
		indicatorList[['Inpatient']][['ind']]=WardIncInd
		
		indicatorList[['Close Contact']]=list()
		ContactInd=sum(tail(TimeTrackSub$CloseContactIncInd,5))
		indicatorList[['Close Contact']][['bgcolor']]=indicatorColorMap[as.character(ContactInd)]
		indicatorList[['Close Contact']][['ind']]=ContactInd
		
		indicatorList[['Quaran-tined']]=list()
		QuarantineCurrentIncInd=sum(tail(TimeTrackSub$QuarantineCurrentIncInd,5))
		indicatorList[['Quaran-tined']][['bgcolor']]=indicatorColorMap[as.character(QuarantineCurrentIncInd)]
		indicatorList[['Quaran-tined']][['ind']]=QuarantineCurrentIncInd
		
		indicatorList[['Unlinked Cases']]=list()
		TimeTrackSub$UnlinkCnt[which(TimeTrackSub$UnlinkCnt>0)]=1
		unlinkInd=sum(tail(TimeTrackSub$UnlinkCnt,5))
		indicatorList[['Unlinked Cases']][['bgcolor']]=indicatorColorMap[as.character(unlinkInd)]
		indicatorList[['Unlinked Cases']][['ind']]=unlinkInd
		
		# indicatorList[['Case Exposure']]=list()
		# expCaseInd=sum(tail(TimeTrackSub$ExpCaseInd,5))
		# indicatorList[['Case Exposure']][['bgcolor']]=indicatorColorMap[as.character(expCaseInd)]
		# indicatorList[['Case Exposure']][['ind']]=expCaseInd
		
		##################
		indicatorChangeList=list()
		for (i in names(indicatorList))
		{
			indicatorChangeList[[i]]=ifelse(indicatorListPrev[[i]][['ind']]==indicatorList[[i]][['ind']],"--",ifelse(indicatorListPrev[[i]][['ind']]>indicatorList[[i]][['ind']],"<font color='green'><b>Better</b></font>","<font color='red'><b>Worse</b></font>"))
		}
		
		
		width=60
		widthTable=width*length(indicatorList)
		dorconMsg=sprintf("<table border=1 width='%dpx' height='%dpx><tr valign='center' align='center'><td bgcolor='%s' align='center'><big><b>DORSCON</b></big></td><tr /></table>",widthTable,width,tail(TimeTrackSub$DORSCON,1))
		indicatorRowMsg=sprintf("%s<b>Predict Indicators</b><table border=1 width='%dpx' height='%dpx><tr valign='center' align='center'>",dorconMsg,widthTable,width)
		for (i in names(indicatorList))
		{
			indicatorRowMsg=sprintf("%s<td bgcolor='%s' width='%dpx' align='center'>%s<br />%s</td>",indicatorRowMsg,indicatorList[[i]][['bgcolor']],width,i,indicatorChangeList[[i]])
		}
		indicatorRowMsg=sprintf("%s</tr>",indicatorRowMsg)
		msg=sprintf("%s</table>",indicatorRowMsg)
		return(HTML(msg))
	})
	
	output$trafficLightMsg=renderUI({
		indicatorMsg=sprintf('<h5><b>Indicator Color Code (Green, Yellow, Orange, Red)</b></h5><h5>The color shows if situation is getting better based on some indicators.<br />Inpatent/CloseContact: If increase in inpatient or Close Contact count is higher than the previous period median, it is flagged as 1. The color code is determined by sum of flags for the past 5 days with Green=0, Yellow=1,2, Orange=3,4 and Red=5. LabTests represents number of samples sent to lab each day.</h5>')
		return(HTML(indicatorMsg))
	})
	
	
	output$interestingMsg=renderUI({
		DateSelect=input$DateSelect
		
		PERSONT=dataList[['PERSON']][which(dataList[['PERSON']]$AnnouncementDate<=DateSelect),]
		PERSONT$RecoveryDays=as.numeric(difftime(as.Date(PERSONT$Discharge,'%d-%b-%Y'),as.Date(PERSONT$SymptomsFirst,'%d-%b-%Y'),unit='days'))
		PERSONT$DetectionTime=as.numeric(difftime(as.Date(PERSONT$DateConfirmed,'%d-%b-%Y'),as.Date(PERSONT$SymptomsFirst,'%d-%b-%Y'),unit='days'))


		medianHospDays=median(PERSONT$RecoveryDays[which(!is.na(PERSONT$RecoveryDays))])
		rangeHospDay=range(PERSONT$RecoveryDays[which(!is.na(PERSONT$RecoveryDays))])
		
		
		medianHospDays=median(PERSONT$RecoveryDays[which(!is.na(PERSONT$RecoveryDays))])
		rangeHospDay=range(PERSONT$RecoveryDays[which(!is.na(PERSONT$RecoveryDays))])
		medianDetectionTime=median(PERSONT$DetectionTime[which(!is.na(PERSONT$DetectionTime))])
		rangeDetectionTime=range(PERSONT$DetectionTime[which(!is.na(PERSONT$DetectionTime))])
		GenderTable=table(PERSONT$Gender)
		GPVisitMedian=median(PERSONT$GPVisit,na.rm=TRUE)
		GPVisitRange=range(PERSONT$GPVisit,na.rm=TRUE)

		
		msg=sprintf("Date: %s<br />Median <b>recovery</b> days <font size=6>%0.1f</font><font size=4> (range %d - %d days)</font><small>(first symptom to discharge, based on dischared data only)</small>",DateSelect,medianHospDays,rangeHospDay[1],rangeHospDay[2])
		msg=sprintf("%s<br />Median <b>detection</b> days: <font size=6>%0.1f</font><font size=4> (range %d - %d days)</font><small>(first symptom to confirm date)</small>",msg,medianDetectionTime,rangeDetectionTime[1],rangeHospDay[2])
		msg=sprintf("%s<br />Median no. of <b>GP visits</b> before admitted: <font size=6>%0.2f</font><font size=4> (range %d - %d times)</font><small>(For those with information)</small>",msg,GPVisitMedian,GPVisitRange[1],GPVisitRange[2])
		msg=sprintf("%s<br /><b>Gender Ratio</b>: Male (%d) to female (%d) ratio is <font size=6>%0.2f</font> SG resident gender ratio is 1.0422",msg,GenderTable['M'],GenderTable['F'],GenderTable['M']/GenderTable['F'])
		
		HTML(msg)
		
	})
	
	
	############ hospitalization observation
	output$wardedPlot=renderPlotly({
		wardedPerson=dataList[['PERSON']][which(is.na(dataList[['PERSON']]$Discharge)),]
		wardedPerson$hospDays=as.numeric(difftime(Sys.Date()+1,as.Date(wardedPerson$DateConfirmed,'%d-%b-%Y'),unit='days'))
		wardedAgg=data.frame(table(wardedPerson$hospDays))
		names(wardedAgg)[1]='HospDay'
		wardedAgg$HospDay=as.numeric(as.character(wardedAgg$HospDay))
		wardedPerson$PersonID=sprintf("Case%s (%s)",wardedPerson$PersonID,wardedPerson$Notes)
		wardedText=aggregate(PersonID~hospDays,wardedPerson,paste,collapse="<br />")
		wardedTextMap=setNames(wardedText$PersonID,as.character(wardedText$hospDays))
		wardedAgg$text=wardedTextMap[as.character(wardedAgg$HospDay)]
		medianAge=median(as.numeric(wardedPerson$Age),na.rm=TRUE)
		plot_ly(wardedAgg,x=~HospDay,y=~Freq,type='bar', text=~text) %>% layout(title=sprintf('Count of currently warded by days hospitalized (median age=%0.1f)',medianAge))
	})
	
	output$dischargePlot=renderPlotly({
		dischgPerson=dataList[['PERSON']][which(!is.na(dataList[['PERSON']]$Discharge) & is.na(dataList[['PERSON']]$Death)),]
		dischgAgg=data.frame(table(dischgPerson$RecoveryDays))
		names(dischgAgg)[1]='RecoveryDays'
		dischgAgg$RecoveryDays=as.numeric(dischgAgg$RecoveryDays)
		dischgPerson$PersonID=sprintf("Case%s (%s)",dischgPerson$PersonID,dischgPerson$Notes)
		recoveryText=aggregate(PersonID~RecoveryDays,dischgPerson,paste,collapse="<br />")
		recoveryTextMap=setNames(recoveryText$PersonID,recoveryText$RecoveryDays)
		dischgAgg$text=recoveryTextMap[as.character(dischgAgg$RecoveryDays)]
		medianAge=median(as.numeric(dischgPerson$Age),na.rm=TRUE)
		medianRecoveryDays=median(dischgPerson$RecoveryDays,na.rm=TRUE)
		plot_ly(dischgAgg,x=~RecoveryDays,y=~Freq,type='bar', text=~text) %>% layout(title=sprintf('Count of discharged person hosp days (median age=%0.1f, median recovery days=%0.1f)',medianAge,medianRecoveryDays))
	})
	
	
	######################### Update
	output$posPlot=renderPlotly({
		DateSelect=input$DateSelect
		TimeTrackSub=dataList[['TimeTrack']][which(dataList[['TimeTrack']]$Date<=DateSelect),]
		plot_ly(TimeTrackSub) %>% add_trace(x=~Date,y=~Positive,mode='lines+markers',name='Positive') %>% add_trace(x=~Date,y=~PosInWard,mode='lines+markers',name='Pos Still in ward') %>% add_trace(x=~Date,y=~QuarantineCurrent,yaxis='y2',type='scatter',mode='lines+markers') %>% layout(xaxis = list(title=''), yaxis = list(side = 'left', title = '', showgrid = FALSE, zeroline = FALSE), yaxis2 = list(side = 'right', overlaying = "y", title = '', showgrid = FALSE, zeroline = FALSE),showlegend = FALSE)
	})
	
	output$posPlotMsg=renderUI({
		DateSelect=input$DateSelect
		TimeTrackSub=dataList[['TimeTrack']][which(dataList[['TimeTrack']]$Date<=DateSelect),]
		msg=NULL
		notIncDays=nrow(TimeTrackSub)-max(which(TimeTrackSub$WardInc>0))
		posNotIncDays=nrow(TimeTrackSub)-max(which(TimeTrackSub$PosIncInc>0))
		quarantineDecDay=nrow(TimeTrackSub)-max(which(TimeTrackSub$QuarantineCurrentInc>0))
		if (notIncDays>1)
		{
			msg=sprintf("<font color='green'><b>Cases still in hospitals have decreased or remained same for consecutive %d days.</b></font>",notIncDays)
		}
		if (posNotIncDays>1)
		{
			msg=sprintf("%s<br /><font color='green'><b>Per day postivie decreases or remained same for consecutive %d days.</b></font>",msg,posNotIncDays)
		}
		if (quarantineDecDay>1)
		{
			msg=sprintf("%s<br /><font color='green'><b>Persons under quarantine decreased for consecutive %d days.</b></font>",msg,quarantineDecDay)
		}
		
		HTML(msg)
	})
	
	PersonNew=reactive({
		DateSelect=input$DateSelect
		PersonNew=dataList[['PERSON']][which(as.Date(dataList[['PERSON']]$AnnouncementDate)==DateSelect),]
		return(PersonNew)
	})
	
	
	
	
	output$latestUpdate=renderUI({
		DateSelect=input$DateSelect
		PersonNew=PersonNew()
		TimeTrackSub=dataList[['TimeTrack']][which(dataList[['TimeTrack']]$Date<=DateSelect),]
		newCases=tail(TimeTrackSub$PosInc,1)
		
		ClusterNewDf=dataList[['CLUSTER']][which(dataList[['CLUSTER']]$PersonID %in% PersonNew$PersonID),]
		
		msg=''
		if (tail(TimeTrackSub$PosInc,1)==max(TimeTrackSub$PosInc))
		{
			msg="<font color='red'>New high in one-day new cases</font>"
		}
		
		if (tail(TimeTrackSub$DeathInc,1)>0)
		{
			msg=sprintf("%s<font size=5><table border=1 width='100%%'><tr align='center'><td colspan=3>Today</td></tr><tr align='center'><td>New Case</td><td>Discharge</td><td><font color='red'>Death</font></td></tr><tr align='center'><td>%d</td><td>%d</td><td><font color='red'>%d</font></td></tr></table></font>",msg,newCases,tail(TimeTrackSub$DischargeInc,1),tail(TimeTrackSub$DeathInc,1))
		} else
		{
			msg=sprintf("%s<font size=5><table border=1 width='100%%'><tr align='center'><td colspan=2>Today</td></tr><tr align='center'><td>New Case</td><td>Discharge</td></tr><tr align='center'><td>%d</td><td>%d</td></tr></table></font>",msg,newCases,tail(TimeTrackSub$DischargeInc,1))
		}
		msg=sprintf("%s<font size=3><table border=1 width='100%%'><tr align='Center'><td colspan=6>As of %s</td></tr><tr align='Center'><td>Positive</td><td>Warded</td><td>Discharged</td><td>Critical</td><td>Death</td><td>Quarantine</td></tr><tr align='center'><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr></table></font>",msg,DateSelect,tail(TimeTrackSub$Positive,1),tail(TimeTrackSub$Positive-TimeTrackSub$Discharge-TimeTrackSub$Death,1),tail(TimeTrackSub$Discharge,1),tail(TimeTrackSub$Critical,1),tail(TimeTrackSub$Death,1),tail(TimeTrackSub$QuarantineCurrent,1))
		
		
		
		# msg=sprintf("<small>On %s</small><br /><h4>%d new cases, %d discharged, %d more close contacts</h4><small>As of %s</small><br /><h4>Warded: %d (%d critical), total %d close contacts<br />Total positive: %d; Total discharged: %d</h4>",DateSelect,newCases,tail(TimeTrackSub$DischargeInc,1),tail(TimeTrackSub$CloseCOntactInc,1),DateSelect,tail(TimeTrackSub$Positive-TimeTrackSub$Discharge,1),tail(TimeTrackSub$Critical,1),tail(TimeTrackSub$CloseContact,1),tail(TimeTrackSub$Positive,1),tail(TimeTrackSub$Discharge,1))
		
		PERSONPERSONSub=dataList[['PERSONPERSON']][which(as.Date(dataList[['PERSONPERSON']]$Date) <= DateSelect),]
		PERSONPERSONSub=PERSONPERSONSub[which(PERSONPERSONSub$`PersonID1` %in% PersonNew$PersonID | PERSONPERSONSub$`PersonID2` %in% PersonNew$PersonID),]
		
		tracable=unique(c(intersect(PersonNew$PersonID,dataList[['CLUSTER']]$PersonID[which(!(dataList[['CLUSTER']]$Cluster %in% c('UNLINKED','OTH_Linked')))]),intersect(PersonNew$PersonID,dataList[['PERSONPERSON']]$PersonID2)))
		
		
		if (nrow(PersonNew)==0)
		{
			msg=sprintf("%s<br /><big><b><font color=green>No confirmed cases</font></b></big><br />",msg)
		} else
		if (length(tracable)==nrow(PersonNew))
		{
			msg=sprintf("%s<br /><big><b><font color=green>All %d case(s) is/are linked to previous confirmed cases</font></b></big><br />",msg,nrow(PersonNew))
		} else
		{
			msg=sprintf("%s<br /><big><b><font color=red>%d case(s) (%s) has/have not yet establish links to previous cases</font></b></big><br />",msg,nrow(PersonNew)-length(tracable),paste(sprintf("Case%s",setdiff(PersonNew$PersonID,tracable)),collapse=", "))
		}
		
		if (nrow(ClusterNewDf)>0)
		{
			ClusterNewAgg=aggregate(PersonID~Cluster,ClusterNewDf,length)
			ClusterNewAgg=ClusterNewAgg[which(ClusterNewAgg$Cluster!='UNLINKED'),]
			ClusterNewAgg$Display=sprintf("%d New cases from cluster %s",ClusterNewAgg$PersonID,ClusterNewAgg$Cluster)
			msg=sprintf("%s<font color=blue>%s</font>",msg,paste(ClusterNewAgg$Display,collapse="<br />"))
		}
		
		if (nrow(PERSONPERSONSub)>0)
		{
			for (i in 1:nrow(PERSONPERSONSub))
			{
				msg=sprintf("%s<br /><font color=blue>Case%s and Case%s are linked</font>",msg,PERSONPERSONSub$`PersonID1`[i],PERSONPERSONSub$`PersonID2`[i])
			}
		}
		
		HTML(msg)
	})
	
	output$NewCaseDT=renderDT({
		PersonNew=dataList[['PERSON']]
		PersonNew=PersonNew[order(PersonNew$PersonID,decreasing=TRUE),]
		if (nrow(PersonNew)>0)
		{
			NewCaseDT=NULL
			for (i in 1:nrow(PersonNew))
			{
				msg=sprintf("<h5>Case%d</h5>",PersonNew$PersonID[i])
				if (PersonNew$PersonID[i] %in% dataList[['CLUSTER']]$PersonID)
				{
					msg=sprintf("%s%s",msg,dataList[['CLUSTER']]$Cluster[which(dataList[['CLUSTER']]$PersonID==PersonNew$PersonID[i])])
				}
				msg=sprintf("%s%s",msg,PersonNew$Report[i])
				
				NewCaseDT=rbind(NewCaseDT,data.frame(CaseDetails=msg))
			}
			
			return(formatDTDisplay(NewCaseDT,escape=FALSE))
		}
	})
	
	output$latestPlaceList=renderUI({
		PersonNew=PersonNew()
		placeIdList=dataList[['PERSONPLACE']]$PlaceID[which(dataList[['PERSONPLACE']]$PersonID %in% PersonNew$PersonID & dataList[['PERSONPLACE']]$Remarks!='Warded')]
		
		msg=sprintf("<h3>Places exposed to latest cases: %s</h3>", paste(unique(dataList[['PLACE']]$Name[which(dataList[['PLACE']]$PlaceID %in% placeIdList)]),collapse=", "))
		HTML(msg)
	})
	
	
	output$newPlaceMap=renderLeaflet({
		PersonNew=PersonNew()
		DateSelect=as.Date(input$DateSelect)
		dateCutoff=DateSelect-14
		PERSONPLACESub=dataList[['PERSONPLACE']][which(as.Date(dataList[['PERSONPLACE']]$DateEnd,'%d-%b-%Y')>=dateCutoff & dataList[['PERSONPLACE']]$Remarks!='Warded'),]
		if (nrow(PERSONPLACESub)>0)
		{
			basemap=leaflet() %>% addTiles(group = "OSM")  #%>% addProviderTiles("Stamen.TonerLite")
			basemap  %>% addMarkers(data = PERSONPLACESub, ~longitude,~latitude,popup = ~Display,icon=placeIcons[PERSONPLACESub$icon],clusterOptions = markerClusterOptions(),layerId=PERSONPLACESub$PlaceID)
		}
	})
	
	
	
	
	treeList=reactive({
		#https://adeelk93.github.io/collapsibleTree/
		# dataList[['PERSONPERSON']]$PersonID1=unlist(apply(dataList[['PERSONPERSON']],1,function(x) return(ifelse(x['PersonID1']<x['PersonID2'],x['PersonID1'],x['PersonID2']))))
		# dataList[['PERSONPERSON']]$PersonID1=trimws(dataList[['PERSONPERSON']]$PersonID1)
		# dataList[['PERSONPERSON']]$PersonID2=unlist(apply(dataList[['PERSONPERSON']],1,function(x) return(ifelse(x['PersonID1']<x['PersonID2'],x['PersonID2'],x['PersonID1']))))
		# dataList[['PERSONPERSON']]$PersonID2=trimws(dataList[['PERSONPERSON']]$PersonID2)
		
		##############################################################################
		PersonNoteMap=setNames(dataList[['PERSON']]$Notes,as.character(dataList[['PERSON']]$PersonID))
		PersonNoteMap[is.na(PersonNoteMap)]=''
		
		
		treeData1=data.frame(parent=sprintf("%s (%d cases)",dataList[['CLUSTER']]$Cluster,clusterSizeMap[dataList[['CLUSTER']]$Cluster]),node=sprintf("Case %s %s",dataList[['CLUSTER']]$PersonID,PersonNoteMap[as.character(dataList[['CLUSTER']]$PersonID)]),LinkType=dataList[['CLUSTER']]$LinkType,rawNode=dataList[['CLUSTER']]$PersonID,stringsAsFactors=FALSE)
		treeData1$Name=treeData1$node
		
		treeData1=treeData1[which(treeData1$LinkType=='Direct' | (treeData1$LinkType=='Indirect' & !(treeData1$rawNode %in% dataList[['PERSONPERSON']]$PersonID2))),]
		treeData1$LinkType=NULL
		
		clusterNode=data.frame(parent=NA,node=sprintf("%s (%d cases)",unique(dataList[['CLUSTER']]$Cluster),clusterSizeMap[unique(dataList[['CLUSTER']]$Cluster)]),rawNode=unique(dataList[['CLUSTER']]$Cluster),stringsAsFactors=FALSE)
		clusterNode$Name=clusterNode$node
		
		treeData=rbind(treeData1,clusterNode)
		
		
		#treeData=rbind(treeData,data.frame(parent='COVID19SG',node=unique(treeData$parent),Name=NA,stringsAsFactors=FALSE))
		#treeData=rbind(treeData,data.frame(parent=NA,node='COVID19SG',Name=sprintf('COVID19SG (%d cases)',nrow(dataList[['PERSON']])),stringsAsFactors=FALSE))
		
		
		missingNodes=dataList[['PERSON']]$PersonID[which(!(as.character(dataList[['PERSON']]$PersonID) %in% treeData1$rawNode))]
		missingNodes=missingNodes[order(missingNodes,decreasing=TRUE)]
		acctedId=as.numeric(treeData1$rawNode)
		acctedId=acctedId[!is.na(acctedId)]
		treeData5=NULL
		for (nid in missingNodes)
		{
			if (!(nid %in% acctedId))
			{
				currentID=nid
				linkedID=setNames(currentID,sprintf("Case%s",currentID))
				while (currentID %in% dataList[['PERSONPERSON']]$PersonID2)
				{
					nextID=max(as.numeric(dataList[['PERSONPERSON']]$PersonID1[which(dataList[['PERSONPERSON']]$PersonID2==currentID)]))
					#reln=dataList[['PERSONPERSON']]$Relation2[which(dataList[['PERSONPERSON']]$PersonID1==nextID & dataList[['PERSONPERSON']]$PersonID2==currentID)]
					linkedID=c(setNames(nextID,sprintf("Case%s",nextID)),linkedID)
					currentID=nextID
					#print(sprintf("%s %s",currentID,nextID))
				}
				
				for (j in 1:(length(linkedID)-1))
				{
					tmptree=data.frame(parent=sprintf("Case %s %s",linkedID[j],PersonNoteMap[linkedID[j]]),node=sprintf("Case %s %s",linkedID[j+1],PersonNoteMap[linkedID[j+1]]),rawNode=linkedID[j+1],stringsAsFactors=FALSE)
					tmptree$Name=tmptree$node
					treeData5=rbind(treeData5,tmptree)
				}
				
				acctedId=c(acctedId,linkedID)
			}
		}
		treeData5=unique(treeData5)
		treeData=rbind(treeData,unique(treeData5))
		
		dischargeList=as.character(dataList[['PERSON']]$PersonID[which(!is.na(dataList[['PERSON']]$Discharge))])
		deathList=as.character(dataList[['PERSON']]$PersonID[which(!is.na(dataList[['PERSON']]$Death))])
		treeData$Color='lightblue'
		treeData$Color[which(treeData$rawNode %in% as.character(dataList[['PERSON']]$PersonID))]='pink'
		treeData$Color[which(treeData$rawNode %in% dischargeList)]='lightgreen'
		treeData$Color[which(treeData$rawNode %in% deathList)]='red'
		
		
		treeNameMap=setNames(treeData$Name,treeData$node)
		treeData$node=treeData$Name
		treeData$parent=treeNameMap[treeData$parent]
		
		# treeData$Name[which(treeData$node %in% c(treeData1$parent))]=sprintf("%s (%d cases)",treeData$node[which(treeData$node %in% c(treeData1$parent))],clusterSizeMap[treeData$node[which(treeData$node %in% c(treeData1$parent))]])
		
		# treeData$Color = 'lightblue'
		# treeData$Color[grep("[0-9]",treeData$node)] = 'pink'
		# treeData$Color[which(treeData$node %in% dataList[['PERSON']]$PersonID[which(!is.na(dataList[['PERSON']]$Discharge))])]='lightgreen'
		# treeData$Color[which(treeData$node=='COVID19SG')]='lightblue'
		
		# clusterList=unique(c(intersect(dataList[['CLUSTER']]$Cluster,treeData$parent)))
		# nameMap=setNames(unique(treeData$Name),unique(treeData$node))
		# treeData$parent[which(treeData$parent %in% names(nameMap))]=nameMap[treeData$parent[which(treeData$parent %in% names(nameMap))]]
		# treeData$node[which(treeData$node %in% names(nameMap))]=nameMap[treeData$node[which(treeData$node %in% names(nameMap))]]
		
		################### Split Tree #################################################
		nodesAccted=NULL
		treeList=list()
		for (cl in clusterNode$node)
		{
			treeTmp=treeData[which(treeData$parent %in% cl),]
			
			treeNode=treeTmp$node
			newNode=treeTmp$node
			while (length(newNode)>0)
			{
				newNode=setdiff(treeData$node[which(treeData$parent %in% treeNode)],treeNode)
				treeNode=c(treeNode,newNode)
			}
			
						
			treeList[[cl]]=treeData[which(treeData$parent %in% c(cl,treeNode) | treeData$node %in% c(cl,treeNode)),]
			#treeList[[cl]]=rbind(treeList[[cl]][which(is.na()),],data.frame(parent=NA,node=cl,Name=cl,Color='lightblue'))
			#flag=flag+1
			#nodesAccted=unique(c(nodesAccted,c(treeList[[cl]]$parent,treeList[[cl]]$node)))
		}
		
		
		return(treeList)
		
		
	})
	
	
	output$networkUI=renderUI({
		treeList=treeList()
		
		treeInfo=NULL
		for (i in names(treeList))
		{
			treeInfo=rbind(treeInfo,data.frame(cluster=i,Discharged=length(unique(treeList[[i]]$node[which(treeList[[i]]$Color=='lightgreen')])),Warded=length(unique(treeList[[i]]$node[which(treeList[[i]]$Color=='pink')])),stringsAsFactors=FALSE))
		}
		treeInfo=treeInfo[order(treeInfo$Warded,decreasing=TRUE),]
		
		do.call(tagList,lapply(treeInfo$cluster,function(x) {
			# renderUI({
				# HTML(sprintf("%s",x))
			# })
			box(width=6,title=sprintf("%s [Discharged:%d, Warded=%d]",x,treeInfo$Discharge[which(treeInfo$cluster==x)],treeInfo$Warded[which(treeInfo$cluster==x)])
				,renderCollapsibleTree({
					collapsibleTreeNetwork(treeList[[x]], attribute = "Name", nodeSize="leafCount", fill="Color",collapsed = FALSE)
				})
			)
		}))
		# # x=1
		# # collapsibleTreeNetwork(treeList[[x]], attribute = "Name", nodeSize="leafCount", fill="Color",collapsed = FALSE)
	})
	
	
	output$ClusterTrack=renderPlotly({
		DateSelect=as.Date(input$DateSelect)
		
		ClusterSub=data.table(dataList[['PERSON']][which(as.Date(dataList[['PERSON']]$DateConfirmed,'%d-%b-%Y')<=DateSelect),])[,.(CaseCount=.N,text=paste(sprintf("Case%d",PersonID),collapse=",")),by=list(groupName,DateConfirmed)]
		ClusterSub$LastCaseDate=NA
		for (i in 1:nrow(ClusterSub))
		{
			personSub=dataList[['PERSON']][which(dataList[['PERSON']]$groupName %in% ClusterSub$groupName[i]),]
			ClusterSub$LastCaseDate[i]=format(max(personSub$AnnouncementDate),'%d-%b-%Y')
			ClusterSub$FirstCaseDate[i]=format(min(personSub$AnnouncementDate),'%d-%b-%Y')
			ClusterSub$DaysFrLastCase[i]=as.numeric(difftime(as.Date(todayDate,'%Y-%m-%d'),max(personSub$AnnouncementDate),unit='days'))
			ClusterSub$colorCode[i]=dayIconColorCode(ClusterSub$DaysFrLastCase[i])
		}
		ClusterSub$groupName[which(ClusterSub$groupName=='OTH_LOCAL')]='Unlinked Local Cases'
		groupCaseCnt=aggregate(CaseCount~groupName,ClusterSub,sum)
		groupCaseCntMap=setNames(groupCaseCnt$CaseCount,groupCaseCnt$groupName)
		ClusterSub$groupName=sprintf("%s (%d cases)",ClusterSub$groupName,groupCaseCntMap[ClusterSub$groupName])
		ClusterSub=ClusterSub[order(ClusterSub$DaysFrLastCase,decreasing=TRUE),]
		clusterLevels=unique(ClusterSub$groupName)
		ClusterSub$groupName=factor(ClusterSub$groupName,levels=clusterLevels)
		ClusterSub$DateConfirmed=as.Date(ClusterSub$DateConfirmed,'%d-%b-%Y')
		
		ClusterSub=ClusterSub[order(ClusterSub$colorCode),]
		colorLevels=unique(ClusterSub$colorCode)
		ClusterSub$colorCode=factor(ClusterSub$colorCode,levels=colorLevels)
		ClusterSub=ClusterSub[order(ClusterSub$DateConfirmed),]
		ClusterSub$display=sprintf("%s<br />%d cases",ClusterSub$text,ClusterSub$CaseCount)
		
		clusterColor=unique(ClusterSub[,c('groupName','colorCode')])
		clusterSprdTxt=paste(clusterColor$groupName[which(clusterColor$colorCode=='red')],collapse=",")
		
		plot_ly(ClusterSub,x=~DateConfirmed,y=~groupName,text=~display,mode='markers',type = 'scatter',color=~colorCode,colors=~colorLevels,size=~CaseCount) %>% layout(title=sprintf("%d active clusters: %s",nrow(clusterColor[which(clusterColor$color=='red')]),clusterSprdTxt))
		
	})
	
	
	####################################################################
	### World data
	#####################################################################
	plotList=reactive({
		currentSlider=as.numeric(input$currentSlider)
		
		ctryCurrentData=ctryDataFClean[which(ctryDataFClean$Date==max(ctryDataFClean$Date)),]
		ctryDataToday=ctryCurrentData
		
		
		ctryCurrentData=ctryCurrentData[order(ctryCurrentData$CurInfected,decreasing=TRUE),]
		
		ctryCurrentDataTopA=ctryCurrentData[currentSlider[1]:currentSlider[2],list(Country,CurInfected,ConfirmedInc1)]
		ctryCurrentDataTopB=ctryCurrentData[currentSlider[1]:currentSlider[2],list(Country,Recovered,ConfirmedInc1)]
		ctryCurrentDataTopC=ctryCurrentData[currentSlider[1]:currentSlider[2],list(Country,Deaths,ConfirmedInc1)]
		
		ctryFactor=ctryCurrentDataTopA$Country[order(ctryCurrentDataTopA$CurInfected,decreasing=TRUE)]
		
		ctryCurrentDataTopA$Country=factor(ctryCurrentDataTopA$Country,levels=ctryFactor)
		ctryCurrentDataTopB$Country=factor(ctryCurrentDataTopB$Country,levels=ctryFactor)
		ctryCurrentDataTopC$Country=factor(ctryCurrentDataTopC$Country,levels=ctryFactor)
		
		
		names(ctryCurrentDataTopA)[which(names(ctryCurrentDataTopA)=='CurInfected')]='Count'
		names(ctryCurrentDataTopB)[which(names(ctryCurrentDataTopB)=='Recovered')]='Count'
		names(ctryCurrentDataTopC)[which(names(ctryCurrentDataTopC)=='Deaths')]='Count'
		
		
		ctryCurrentDataTopA$Type='CurrentInfected'
		ctryCurrentDataTopB$Type='Recovered'
		ctryCurrentDataTopC$Type='Deaths'
		
		
		ctryCurrentDataTop=rbind(ctryCurrentDataTopA,ctryCurrentDataTopB,ctryCurrentDataTopC)
		ctryCurrentDataTop$Type=as.factor(ctryCurrentDataTop$Type)
		
		##########################################################################
		ctryDataFCleanSub=ctryDataFClean[which(ctryDataFClean$Country %in% ctryCurrentDataTop$Country),]
		
		ctryDataFCleanSub$CurInfectedPerM=ctryDataFCleanSub$CurInfected/ctryDataFCleanSub$Pop2020Mil
		ctryDataFCleanSub$Country=factor(ctryDataFCleanSub$Country,levels=ctryFactor)
		ctryDataFCleanSub=ctryDataFCleanSub[which(ctryDataFCleanSub$DayInfect>0),]
		
		
		###############################################################
		ctryDataToday=ctryDataToday[order(ctryDataToday$CurInfected,decreasing=TRUE),]
		
		##########################################################################
		plotList=list()
		plotList[['ctryCurrentDataTop']]=ctryCurrentDataTop
		plotList[['ctryDataFCleanSub']]=ctryDataFCleanSub
		plotList[['ctryDataToday']]=ctryDataToday
		return(plotList)
	})
	
	ctryCurrentDataTop=reactive(plotList()[['ctryCurrentDataTop']])
	ctryDataFCleanSub=reactive(plotList()[['ctryDataFCleanSub']])
	ctryDataToday=reactive(plotList()[['ctryDataToday']])
	
	output$currentTop=renderPlotly({
		ctryCurrentDataTop=ctryCurrentDataTop()

		plot_ly(ctryCurrentDataTop,x=~Country,y=~Count,type='bar',color=~Type) %>% add_trace(x=~Country,y=~ConfirmedInc1,type='scatter',mode='lines',yaxis='y2',name='1-day increase') %>% layout(barmode='stack',title = 'Currently Infected/1-day case increase',  xaxis = list(title = ""),  yaxis = list(side = 'left', title = 'Currently Infected', showgrid = FALSE, zeroline = FALSE), yaxis2 = list(side = 'right', overlaying = "y", title = 'Increase', showgrid = FALSE, zeroline = FALSE))
	})
	
	
	output$currentPerMPop=renderPlotly({
		ctryDataFCleanSub=ctryDataFCleanSub()
		
		ctryDataFCleanSub=ctryDataFCleanSub[order(ctryDataFCleanSub$DayInfect),]
		
		maxDayInfecCtry=aggregate(DayInfect~Country,ctryDataFCleanSub,max)
		ctryDataFCleanSubA=merge(ctryDataFCleanSub,maxDayInfecCtry,by='Country')
		ctryDataFCleanSubA=ctryDataFCleanSubA[which(ctryDataFCleanSubA$DayInfect.x==ctryDataFCleanSubA$DayInfect.y),]
		
		a=list(x=ctryDataFCleanSubA$DayInfect.x,y=ctryDataFCleanSubA$CurInfectedPerM,text=as.character(ctryDataFCleanSubA$Country))
		
		plot_ly(ctryDataFCleanSub,x=~DayInfect,y=~CurInfectedPerM,color=~Country,type='scatter',mode='lines+markers') %>% layout(title='Currenly infected per mil popln vs Days since cases hit 1 in 1M popln',annotations = a)
	})
	
	output$confirmedPerMPop=renderPlotly({
		ctryDataFCleanSub=ctryDataFCleanSub()
		
		ctryDataFCleanSub=ctryDataFCleanSub[order(ctryDataFCleanSub$DayInfect),]
		maxDayInfecCtry=aggregate(DayInfect~Country,ctryDataFCleanSub,max)
		ctryDataFCleanSubA=merge(ctryDataFCleanSub,maxDayInfecCtry,by='Country')
		ctryDataFCleanSubA=ctryDataFCleanSubA[which(ctryDataFCleanSubA$DayInfect.x==ctryDataFCleanSubA$DayInfect.y),]
		
		a=list(x=ctryDataFCleanSubA$DayInfect.x,y=ctryDataFCleanSubA$ConfirmedPerPopM,text=as.character(ctryDataFCleanSubA$Country))
		
		plot_ly(ctryDataFCleanSub,x=~DayInfect,y=~ConfirmedPerPopM,color=~Country,type='scatter',mode='lines+markers') %>% layout(title='Confirmed cases per mil popln vs Days since cases hit 1 in 1M popln',annotations = a)
	})
	
	
	
	ctryClusterData=reactive({
		wctrySelect=input$wctrySelect
		
		ctryDaysInfect=max(ctryDataFClean$DayInfect[which(ctryDataFClean$Country==wctrySelect)])
		
		ctryDataFCleanSub=ctryDataFClean[which(ctryDataFClean$DayInfect>=0 & ctryDataFClean$DayInfect<=ctryDaysInfect),]
		ctryDataFCleanSub=ctryDataFCleanSub[order(ctryDataFCleanSub$DayInfect),]
		
		ctryDISprd=data.frame(spread(ctryDataFCleanSub[,c('ConfirmedPerPopM','DayInfect','Country')],key='DayInfect',value='ConfirmedPerPopM'))
		rownames(ctryDISprd)=as.character(ctryDISprd$Country)
		ctryDISprd[is.na(ctryDISprd)]=0
		ctryDISprd$Country=NULL
		
		#ctryDISprdM=as.matrix(ctryDISprd)
		distMat=flattenCorrMatrix(as.matrix(dist(ctryDISprd)))
		distMatSub=distMat[which(distMat$row==wctrySelect | distMat$column==wctrySelect),]
		distMatSub=distMatSub[order(distMatSub$cor),]
		distMatSub=distMatSub[1:10,]
		
		ctryClusterData=ctryDataFClean[which(ctryDataFClean$Country %in% c(distMatSub$row,distMatSub$column)),]
		return(ctryClusterData)
	})
	
	output$ctryCluster=renderPlotly({
		wctrySelect=input$wctrySelect
		ctryClusterData=ctryClusterData()
		ctryClusterData=ctryClusterData[order(ctryClusterData$DayInfect),]
		
		ctryClusterDataLab=ddply(ctryClusterData,.(Country), tail,1)

		a=list(x=ctryClusterDataLab$DayInfect,y=ctryClusterDataLab$ConfirmedPerPopM,text=as.character(ctryClusterDataLab$Country))
		
		plot_ly(ctryClusterData,x=~DayInfect,y=~ConfirmedPerPopM,color=~Country,type='scatter',mode='lines+markers') %>% layout(title=sprintf("Top 10 countries with most similar trend as %s",wctrySelect),annotations = a)
		
	})
	
	
	output$ctryRankTable=renderDT({
		input$refresh
		ctryDataToday=ctryDataToday()
		
		displayOrder=c('Country','Date','DayInfect','CurInfectedPerM','ConfirmedPerPopM','Confirmed','Recovered','Deaths')
		displayOrder=c(displayOrder,setdiff(names(ctryDataToday),displayOrder))
		
		
		formatDTDisplay(ctryDataToday)
	})
	
	
	output$ctrySelectplot=renderUI({
		ctryDataToday=ctryDataToday()
		ctryRankTable_rows_selected=input$ctryRankTable_rows_selected
		if (length(ctryRankTable_rows_selected)==0)
		{
			ctryRankTable_rows_selected=1
		}
		
		ctrySelected=ctryDataToday$Country[ctryRankTable_rows_selected]
		ctryDataFCleanSub=ctryDataFClean[which(ctryDataFClean$Country %in% ctrySelected),]
		
		ctryDataFCleanSub=ctryDataFCleanSub[order(ctryDataFCleanSub$Date),]
		labelDf=ddply(ctryDataFCleanSub,.(Country), tail,1)
		labelList=list(x=labelDf$DayInfect,y=labelDf$CurInfectedPerM,text=as.character(labelDf$Country))
		labelList1=list(x=labelDf$DayInfect,y=labelDf$ConfirmedPerPopM,text=as.character(labelDf$Country))
		
		list(
			box(width=12
				,renderPlotly(plot_ly(ctryDataFCleanSub,x=~DayInfect,y=~CurInfectedPerM,color=~Country,type='scatter',mode='lines+markers') %>% layout(title='Currenly infected per mil popln vs Days since cases hit 1 in 1M popln',annotations = labelList))
			#,renderPlotly(plot_ly(ctryDataFCleanSub,x=~DayInfect,y=~CurInfectedPerM,color=~Country)
			)
			,box(width=12
				,renderPlotly(plot_ly(ctryDataFCleanSub,x=~DayInfect,y=~ConfirmedPerPopM,color=~Country,type='scatter',mode='lines+markers') %>% layout(title='Confirmed cases per mil popln vs Days since cases hit 1 in 1M popln',annotations = labelList1))
			)
		)
	})
	
	#####################################################################
	#####################################################################
	#SurvivalData$DischargeCensored=as.factor(SurvivalData$DischargeCensored)
	#plot_ly(SurvivalData,x=~Age,y=~HospDays,color=~DischargeCensored,colors=c('red','blue'),type='scatter')
	
	
	######################################################################
	# Data View
	######################################################################
	
	
	
})













