#######################################################
## R Shiny Application for DMA evaluation            ##
##   Written By: Leah Crowe 2018                     ##
#######################################################

#############
##  Global ##
#############

	source('./scripts/DMAglobal.R', local = TRUE)$value1

####################
## User interface ##
####################

ui = source('./scripts/DMAui.R', local = TRUE)$value
############
## Server ##
############

	## Define server logic 
	server = function(input, output, session) {
	  
	  ## rwData
	  source('./scripts/DMAserver.R', local = TRUE)$value
		
	}

#########################
## Create Shiny object ##
#########################

	shinyApp(ui = ui, server = server)
