#######################################################
## R Shiny Application for NARWSS aerial survey data ##
##   Written By: Leah Crowe                          ##
##   Updated: January 2019                           ##             
#######################################################

####################
## User interface ##
####################

ui = secure_app(source('./scripts/NARWSSui.R', local = TRUE)$value)
############
## Server ##
############

	## Define server logic 
	server = function(input, output, session) {
	  
	  res_auth <- secure_server(
	    check_credentials = check_credentials(credentials)
	  )
	  
	  output$auth_output <- renderPrint({
	    reactiveValuesToList(res_auth)})
	  ## rwData
	  source('./scripts/NARWSSserver.R', local = TRUE)$value
		
	}

#########################
## Create Shiny object ##
#########################

	shinyApp(ui = ui, server = server, options = list(height = 1080))
