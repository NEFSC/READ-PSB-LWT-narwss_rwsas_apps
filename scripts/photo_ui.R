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
      tableOutput("contents")
    )
  )
)