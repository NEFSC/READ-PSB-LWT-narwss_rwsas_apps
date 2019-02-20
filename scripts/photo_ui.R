fluidPage(  
  useShinyjs(),
  titlePanel("Where are the whales?"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("tzone", "Camera Time Zone", choices = c("Atlantic Time","Eastern Time"), selected = "Eastern Time", inline = FALSE),
      textInput("permit", "Permit Number:", placeholder = "MMPA ##### and/or DFO-MAR-####"),
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
                  "csv must include these columns:",'<br/>',
                  "Field EGNO, EG Letter, Local Time, Day, Month, Year, Latitude, Longitude, Area, Obs, Platform, Image Type, Assoc. Type, Behaviors, Notes, Photgrapher, Frames, First Edit, Second Edit, Final Edit",
                  '<br/>','Latitude, Longitude, Area, Obs, Platform, and Image Type <strong>can be blank</strong>.',
                  '<br/>',"Local Time and EG Letter should <strong>not be blank</strong>."))),
      textOutput("finalmess"),
      tableOutput("contents")
    )
  )
)