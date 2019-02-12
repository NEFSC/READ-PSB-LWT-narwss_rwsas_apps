fluidPage(
  useShinyjs(),
  titlePanel("DMA eval"),
  splitLayout(radioButtons("where", "Survey Area", choices = c("US", "Canada"), selected = "US", inline = FALSE),
              dateInput("sasdate", "Date:")),
                           actionButton("query","Query Database"),
                           textOutput("error1"),
                           textOutput("error3"),
                           rHandsontableOutput("dailyeghot"),
                           actionButton("eval","Evaluate"),
                           tableOutput("egsastab"), ##egsas or egsastab?
                           leafletOutput("sasdma"),    
                           uiOutput("dmaoptions"),     
                           tableOutput("dmacoord"),    
                           actionButton("dmaup","Upload DMA to database"),
                           downloadButton("dmareport", "Download DMA Report"),
                           downloadButton("kml", "Download KML")
                           )
                           
