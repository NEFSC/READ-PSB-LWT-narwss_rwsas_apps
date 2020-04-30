fluidPage(
  useShinyjs(),
  titlePanel("Trigger analysis for DMAs and other protection areas from visual and acoustic detections"),
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
                           textInput("triggrp", "Who reported/observed these sightings?", width = 600, placeholder = "example: New England Aquarium aerial survey team"),
                           downloadButton("dmaletter", "Download DMA Letter")
                           
                           )
                           
