#######################################################
## R Shiny Application for DMA evaluation            ##
##   Written By: Leah Crowe 2018                     ##
#######################################################

####################
## User interface ##
####################

if (file.exists('./scripts/creds.R') == TRUE){
  ui = shinymanager::secure_app(source('./scripts/DMAui.R', local = TRUE)$value)
} else {
  ui = source('./scripts/DMAui.R', local = TRUE)$value
}

############
## Server ##
############

	## Define server logic 
	server = function(input, output, session) {
	  
	  res_auth <- shinymanager::secure_server(
	    check_credentials = shinymanager::check_credentials(credentials),
	    timeout = 120
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
