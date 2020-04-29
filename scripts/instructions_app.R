##############################################
## R Shiny Application for the instructions ##
##   Written By: Leah Crowe                 ##
##   Updated: April 2020                    ##             
##############################################


####################
## User interface ##
####################

ui = source('./scripts/instructions_ui.R', local = TRUE)$value
############
## Server ##
############

	## Define server logic 
	server = function(input, output, session) {
	  
	  ## rwData
	  source('./scripts/instructions_server.R', local = TRUE)$value
		
	}

#########################
## Create Shiny object ##
#########################

	shinyApp(ui = ui, server = server, options = list(height = 1080))
