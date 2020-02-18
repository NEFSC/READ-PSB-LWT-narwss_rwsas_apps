fluidPage(
  useShinyjs(),
  titlePanel("DMA evaluation"),
  splitLayout(radioButtons("sig_acou", "What do you want to evaluate?", choices = c("Visual Sightings", "Acoustic Detections"), selected = "Visual Sightings", inline = FALSE),
              dateInput("sasdate", "Date:")),
                           actionButton("query","Query Database"),
                           textOutput("error1"),
                           textOutput("error3"),
                           rHandsontableOutput("dailyeghot"),
                           actionButton("eval","Evaluate"),
                           tableOutput("egsastabout"), 
                           leafletOutput("sasdma"),    
                           tableOutput("dmanameout"), 
                           tableOutput("dmacoord"),    
                           actionButton("dmaup","Upload DMA to database"),
                           downloadButton("dmareport", "Download DMA Report"),
                           downloadButton("kml", "Download KML"),
                           textInput("triggrp", "Who reported/observed these sightings? OR Which buoy collected these calls?", width = 600, placeholder = "example: New England Aquarium aerial survey team OR acoustic glider in the Gulf of Maine"),
                           downloadButton("dmaletter", "Download DMA Letter")
                           
                           )
                           
