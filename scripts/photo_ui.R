fluidPage(  
  useShinyjs(),
  titlePanel("Where are the whales?"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("tzone", "Camera Time Zone", choices = c("Atlantic Time","Eastern Time"), selected = "Eastern Time", inline = FALSE),
      textInput("permit", "Permit Number:", placeholder = "MMPA #####"),
      fileInput("imagesub", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      
      tags$hr(),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      (HTML(paste('<br/>',
                  "CSV must include these columns:",'<br/>',
                  '<br/>',
                  "Field EGNO, EG Letter, Local Time, Day, Month, Year, Latitude, Longitude, Area, Obs, Platform, Image Type, Assoc. Type, Behaviors, Notes, Photographer, Frames, First Edit, Second Edit, Final Edit",
                  '<br/>','<br/>',
                  'Latitude, Longitude, Area, Obs, Platform, and Image Type <strong>can be blank</strong>.',
                  '<br/>','<br/>',
                  "Local Time and EG Letter should <strong>not be blank</strong>.",
                  '<br/>','<br/>'))),
      br(),
      textOutput("finalmess"),
      br(),
      leafletOutput("finalleaf")
    )
  )
)