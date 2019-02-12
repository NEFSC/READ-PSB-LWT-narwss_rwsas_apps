#######################################################
## R Shiny Application for NARWSS aerial survey data ##
##   Written By: Leah Crowe                          ##
##   Updated: January 2019                           ##             
#######################################################


####################
## User interface ##
####################

ui = source('./scripts/NARWSSui.R', local = TRUE)$value
############
## Server ##
############

	## Define server logic 
	server = function(input, output, session) {
	  
	  ## rwData
	  source('./scripts/NARWSSserver.R', local = TRUE)$value
		
	}

#########################
## Create Shiny object ##
#########################

	shinyApp(ui = ui, server = server, options = list(height = 1080))
