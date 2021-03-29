#######################################################
## R Shiny Application for DMA evaluation            ##
##   Written By: Leah Crowe 2018                     ##
#######################################################

####################
## User interface ##
####################

ui = secure_app(source('./scripts/DMAui.R', local = TRUE)$value)
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
	  source('./scripts/DMAserver.R', local = TRUE)$value
		
	}

#########################
## Create Shiny object ##
#########################

	shinyApp(ui = ui, server = server, options = list(height = 1080))
