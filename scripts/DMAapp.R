#######################################################
## R Shiny Application for DMA evaluation            ##
##   Written By: Leah Crowe 2018                     ##
#######################################################

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

	shinyApp(ui = ui, server = server, options = list(height = 1080))
