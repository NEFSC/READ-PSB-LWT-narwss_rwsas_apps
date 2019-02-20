#######################################################
## R Shiny Application for gathering whale positions ##
##    for photo processing of NARWSS aerial data     ##  
##         Written By: Leah Crowe 2018               ##
#######################################################


####################
## User interface ##
####################

ui = source('./scripts/photo_ui.R', local = TRUE)$value
############
## Server ##
############

## Define server logic 
server = function(input, output, session) {
  
  ## rwData
  source('./scripts/photo_server.R', local = TRUE)$value
  
}

#########################
## Create Shiny object ##
#########################

shinyApp(ui = ui, server = server)