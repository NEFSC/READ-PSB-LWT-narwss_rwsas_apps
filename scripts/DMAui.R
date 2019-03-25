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
                           tableOutput("dmanameout"), 
                           tableOutput("dmacoord"),    
                           actionButton("dmaup","Upload DMA to database"),
                           downloadButton("dmareport", "Download DMA Report"),
                           textInput("triggrp", "Who reported/observed these sightings?", placeholder = "example: New England Aquarium aerial survey team"),
                           downloadButton("dmaletter", "Download DMA Letter"),
                           downloadButton("kml", "Download KML")
                           )
                           
