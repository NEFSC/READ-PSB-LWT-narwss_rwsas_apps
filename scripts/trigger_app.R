
## R Shiny Application for trigger analysis          ##
##   Written By: Leah Crowe 2018                     ##



## User interface -----


if (file.exists('./scripts/creds.R') == TRUE) {
  ui = shinymanager::secure_app(source('./scripts/trigger_ui.R', local = TRUE)$value)
} else {
  ui = source('./scripts/trigger_ui.R', local = TRUE)$value
}


## Server ----

## Define server logic
server = function(input, output, session) {
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(credentials),
    timeout = 120
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  ## rwData
  source('./scripts/trigger_server.R', local = TRUE)$value
  
}


## Create Shiny object ----

shinyApp(ui = ui,
         server = server,
         options = list(height = 1080))
