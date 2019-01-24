fluidPage(
  useShinyjs(),
  titlePanel("DMA eval"),
  splitLayout(radioButtons("where", "Survey Area", choices = c("US", "Canada"), selected = "US", inline = FALSE),
              dateInput("sasdate", "Date:")),
                           actionButton("eval","Evaluate"),
                           textOutput("error1"),
                           textOutput("error3"),
                           tableOutput("egsastab"), ##egsas or egsastab?
                           leafletOutput("sasdma"),    
                           uiOutput("dmaoptions"),     
                           tableOutput("dmacoord"),    
                           actionButton("dmaup","Upload DMA to database"),
                           downloadButton("kml", "Download KML")
                           )
                           
                  
    