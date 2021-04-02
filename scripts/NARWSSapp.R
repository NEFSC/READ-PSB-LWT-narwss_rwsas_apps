#######################################################
## R Shiny Application for NARWSS aerial survey data ##
##   Written By: Leah Crowe                          ##
##   Updated: January 2019                           ##             
#######################################################

####################
## User interface ##
####################
if (file.exists('./scripts/creds.R') == TRUE){
  ui = shinymanager::secure_app(source('./scripts/NARWSSui.R', local = TRUE)$value)
} else {
  ui = source('./scripts/NARWSSui.R', local = TRUE)$value
}
############
## Server ##
############

	## Define server logic 
	server = function(input, output, session) {

	  if (file.exists('./scripts/creds.R') == TRUE){	  	  
	  res_auth <- shinymanager::secure_server(
	    check_credentials = shinymanager::check_credentials(credentials),
	    timeout = 120
	    )
	  
	  output$auth_output <- renderPrint({
	    reactiveValuesToList(res_auth)})
	  }
	  
	  ## rwData
	  source('./scripts/NARWSSserver.R', local = TRUE)$value
	  
	}

#########################
## Create Shiny object ##
#########################

	shinyApp(ui = ui, server = server, options = list(height = 1080))
