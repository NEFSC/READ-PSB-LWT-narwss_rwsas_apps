
## R Shiny Application for viewing active slow zones throughout history

## User interface ----

ui = source('./scripts/szone_ui.R', local = TRUE)$value

## Server ----

## Define server logic
server = function(input, output, session) {
  source('./scripts/szone_server.R', local = TRUE)$value
  
}


## Create Shiny object ----

shinyApp(ui = ui,
         server = server,
         options = list(height = 1080))
