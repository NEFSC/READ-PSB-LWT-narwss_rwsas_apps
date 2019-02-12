#######################################################
## R Shiny Application for NARWSS aerial survey data ##
##   Written By: Leah Crowe                          ##
##   Updated: January 2019                           ##             
#######################################################

#############
##  Global ##
#############
 #assign('.lib.loc', '//net/home5/lcrowe/R/x86_64-redhat-linux-gnu-library/3.5', envir = environment(.libPaths))
 #assign('.lib.loc', 'C:/Users/leah.crowe/Documents/R/R-3.5.1/library', envir = environment(.libPaths))
	source('./scripts/NARWSSglobal.R', local = TRUE)$value1

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

	shinyApp(ui = ui, server = server)
