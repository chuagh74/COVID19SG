
#source("chooser.R")
source("global.R")
############################
############################
# Header
############################
############################
fluidPage(theme = "apac.css")

header <- dashboardHeader(
    title = "COVID-19(SG)"
)


############################
############################
# Sidebar
############################
############################

siderbar <- dashboardSidebar(
	collapsed = FALSE,
	shinyjs::useShinyjs(),
	div(style = "padding: 20px 0 20px 20px", 'Public'),
    
	sidebarMenu(id = "mainSidebar"
		# Main Navigation
		,shinyWidgets::sliderTextInput(inputId = "DateSelect", label = "Select a date or click the play button",  choices = as.Date(dataList[['TimeTrack']]$Date[6:nrow(dataList[['TimeTrack']])]),selected=tail(as.Date(dataList[['TimeTrack']]$Date,'%d-%b-%Y'),1),animate=animationOptions(interval=1000))
		,menuItem("SG", tabName = "mainView", icon = icon("project-diagram"))
		,menuItem("World", tabName = "worldView", icon = icon("project-diagram"))
		,menuItem("Data", tabName = "dataView", icon = icon("project-diagram"))
	)
	,p(style = "font-color: grey; text-align: right; padding: 16px;", sprintf('Last updated:%s',todayDate))
)


#############################
#############################
# Body
#############################
#############################

############################
# Main Tab Content
############################

mainContent <- tabItem(tabName = "mainView",
	fluidRow(
		tags$style(
			  ".redBg { background: pink; }
			  .blueBg { background: lightblue; }
			  .whiteBg { background: white; }
			  .greenBg { background: lightgreen; }
			  .orangeBg { background: orange; }"
			)
		,tags$style(HTML("
		  .vis-item .vis-item-overflow { overflow: visible; }
		"))
		
		,tabsetPanel(
			tabPanel('Overview'
				,box(width=12
					,uiOutput('dateMsg')
					,box(width=4
						,uiOutput('latestUpdate')
						#,uiOutput('trafficLight')
					)
					,box(width=4
						,tags$h5('(Cumulative) Orange=Still warded; Blue=Test pos; Green=Quarantine')
						,uiOutput('posPlotMsg')
						,plotlyOutput('posPlot',height='250px')
					)
					,box(width=4
						,tags$h5('Different indicators to show if situation getting better or worse')
						,uiOutput('trafficLight')
					)
				)
				,box(width=12
					,box(width=4,title='Separating local txn and imported'
						,plotlyOutput('localImportPlot')
					)
					
					,box(width=4,title='Unlinked Cases'
						,plotlyOutput('unlinkedPlot')
					)
					# ,box(width=4,title='Some derived numbers'
						# ,uiOutput('interestingMsg')
					# )
				)
				,box(width=12,collapsible=TRUE, title='Import Cases Analysis'
					,uiOutput('importForm')
					,box(width=6
						,plotlyOutput('caseResImportPlot')
					)
					,box(width=6
						,plotlyOutput('caseResImportPie')
					)
				)
				,box(width=12,collapsible=TRUE, title='Cases Residency/Nationality Type'
					,box(width=6
						,plotlyOutput('caseResidencyPlot')
					)
					,box(width=6
						,plotlyOutput('caseNationalePlot')
					)
					,box(width=6
						,plotlyOutput('caseResidencyPie')
					)
					,box(width=6
						,plotlyOutput('caseNationalePie')
					)
					
				)
				,box(width=12,collapsible=TRUE, title='Recovery/Hospitalization. Scroll over bar to see cases info'
					,box(width=6
						,plotlyOutput('wardedPlot')
					)
					,box(width=6
						,plotlyOutput('dischargePlot')
					)
				)
				,box(width=12,collapsible=TRUE
					,box(width=12,title='Cluster case occurrence. Cluster with latest case on top of list. Clusters represented in green are clusters that have been cleared from spreading. Clusters in red are the ones with high risk of spreading.'
						,plotlyOutput('ClusterTrack',height='600px')
					)
					,box(width=12,title='Network of cases (Blue: Cluster; Red: Deceased; Pink: Warded; Green: Discharged). Size relative to linked cases'
						,uiOutput('networkUI')
						#,collapsibleTreeOutput('newCaseNetwork',height='600px')
					)
				)
				,box(width=12,title='Details on cases',collapsible=TRUE
					,DTOutput('NewCaseDT')
				)
			)
			,tabPanel('Imports Impact'
				,box(width=12
					,tags$h5('Locally transmitted due to imports')
					,DTOutput('importImpact')
				)
			)
			,tabPanel('View By Indicators'
				,box(width=12
					,tabPanel('Historical Indicator Color'
						,tags$h5('Indicators are used to indicate if situation is getting better and color codes are given to 5 days roll sum')
						,tags$h5('The graphs below shows the historical color code in the past and each color code is derived based on sum of flags in past 5 days.')
						,uiOutput('indicatorPlot')
						,uiOutput('indicatorPlotUI')
					)
				)
			)
			,tabPanel('View By Places'
				,box(width=12,collapsible=TRUE,title='View by PLACES'
					,tabsetPanel(
					
						tabPanel('Mapview'
							,box(width=12,title='Places possibly exposed to virus in past 14 days',collapsible=TRUE
								,uiOutput('latestPlaceList')
								,leafletOutput('newPlaceMap',height='600px')
							)
							,box(width=6,collapsible=TRUE
								,uiOutput('colorMsg')
								,selectInput('mapColorSelect','Select which places of color codes to be shown on map',choices=c('red','orange','yellow','grey','green'),c('red','orange','yellow','grey','green'),multiple=TRUE)
							)
							,box(width=6,collapsible=TRUE,title='Icon Color Code (Collapsible box)'
								,uiOutput('colorMsgTable')
							)
							,tabsetPanel(
								tabPanel('Place Cluster View'
									,box(width=6
										,leafletOutput('clusterMap',height='600px')
									)
									,box(width=6
										,DTOutput('clusterDT')
									)
								)
								,tabPanel('Detailed Map View'
									,box(width=12,collapsible=TRUE,title='Map view of places'
										,box(width=9
											,tags$h3('Where are the places visited by the confirmed cases')
											,tags$h5('Click on the icon to get details on who visited, Please note color code is for icons, not for the aggregation circles. Do not get confused by the green circles with numbers in it. The numbers shows the number of places that re in that area.)')
											
											,leafletOutput('trackMap',height='600px')
										)
										,box(width=3
											,tags$h5('Case exposure period')
											,timevisOutput('placeTimeVis')
											,tags$h5('Case details')
											,uiOutput('mapSelectMsg')
										)
									)
								)
								
							)
							
						)
						,tabPanel('Table'
							,box(width=12,collapsible=TRUE,title='List of places'
								,box(width=9
									,tags$h5('List of places. Click on any row and details on the place will be displayed on the right.')
									,DTOutput('PPAggSub')
								)
								,box(width=3
									,tags$h5('Place exposure period')
									,timevisOutput('placeTimeVisTbl')
									,tags$h5('Details of cases visiting selected place')
									,uiOutput('PERSONPLACEAggMsg')
								)
							)
						)
					)
					
				)
			)
			,tabPanel('View by Person'
				,box(width=12,collapsible=TRUE,title='View by PERSON'
					,tabsetPanel(
						tabPanel('Timeline'
							,box(width=12
								,tags$h3('Count of confirmed infected cases exposure in each day (Hindsight)') 
								,tags$h5('This chart shows which date has the most number of infected persons exposed without isolation. The peak can be indicative of how well contained the spread is. As the peak moves further away from current date, it shows the spread is getting contained.')
								,plotlyOutput('expCntChart')
								,tags$h3('Exposure period from arrival date or show of symptoms -14 days till isolation')
								,tags$h5('This view can assist visual inspection of which cluster is contained. If there are new cases arising from the cluster, the bars will for the cluster will shift further away from current date indicating there is no more spread from that cluster.')
								,timevisOutput("personExpTimeline")
								,DTOutput('data1')
							)
						)
						,tabPanel('Network'
							,box(width=12
								,tags$h3('Network')
								,uiOutput('clusterForm')
								,box(width=7
									,visNetworkOutput('lobNetwork',height = "800px")
								)
								,box(width=5
									,uiOutput('personVisitMapMsg')
									,uiOutput('mapMsg')
								)
							)
							,box(width=12,collapsible=TRUE
								,DTOutput('personDT')
							)
							
						)
						
					)
				)
			)
			,tabPanel('View by Date'
				,box(width=12,collapsible=TRUE,title='View by Date'
					,tabsetPanel(
						tabPanel('Timeline'
							,tags$h5('Note that 7 Feb 2020 data is interpolated')
							,box(width=12
								,box(width=6
									,tags$h5('To observe trend of closeContactFactor=cumulative count of close contact in SG/Positive')
									,tags$h5('The factor shows the average number of close contacts to each positive case. The trend should decrease as time goes by if situation gets better with more infected having less contacts.')
									,plotlyOutput('contactTraceChartDate')
								)
								,box(width=6
									,tags$h5('To observe trend of factor of increased close contact wrt increase in positive')
									,tags$h5('This graph is similar to the graph on the left but more specific to the average close contacts for the new confirmed cases.')
									,plotlyOutput('traceFactorIncPlot')
								)
								
							)
							,box(width=12
								,box(width=6
									,tags$h5('To observe Discharged, Critical and Confirmed Cases count by day')
									,plotlyOutput('CaseTrend')
								)
								,box(width=6
									,tags$h5('To obesrve trend of ratio of test outcome')
									,tags$h5('Just to observe the trend of tests done and trend of cases.')
									,plotlyOutput('posNegRatioPlot')
								)
								
							)
							,box(width=12
								,box(width=6
									,tags$h5('To observe trend of count of exposure from confirmed cases by date and cumulative count of confirmed cases')
									,plotlyOutput('expCntChartDate')
								)
							)
						)
					)
				)
			)
		)
		
		
		
		
	)
)



worldContent <- tabItem(tabName = "worldView",
	fluidRow(
		tabsetPanel(
			tabPanel('Overview'
				,box(width=12
					,box(width=12
						,sliderInput('currentSlider','Top n countries',min=1,max=length(countryList),value=c(1,20))
						,plotlyOutput('currentTop')
					)
					,box(width=12
						,plotlyOutput('confirmedPerMPop')
					)
					,box(width=12
						,plotlyOutput('currentPerMPop')
					)
				)
			)
			,tabPanel('Ctry'
				,box(width=12
					,selectInput('wctrySelect','Country',countryList,'US')
					,plotlyOutput('ctryCluster')
				)
			)
			,tabPanel('Data'
				,box(width=12
					,actionButton('refresh','Clear Selection')
					,box(widht=6
						,DTOutput('ctryRankTable')
					)
					,box(width=6
						,uiOutput('ctrySelectplot')
					)
				)
			)
		)
	)
)

# ############################
# # Data View
# ############################
dataContent <- tabItem(tabName = "dataView",
	fluidRow(
		box(title='',width=12
			,tabsetPanel(
				tabPanel(tagList("Data")
					,box(width=12,title='Data'
						,uiOutput('dataUI')
					)
				)
			)
		)
	)
)


####################################################################################


############################
# Compile Dashbard Body
############################
body <- dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    )
	,fluidRow(id='main',
		tabItems(
			#acctContent
			#,decTreeContent
			mainContent
			,worldContent
			,dataContent
		)
	)
)


#############################
#############################
# Initialise
#############################
#############################
dashboardPage(header, siderbar, body)












