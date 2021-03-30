fluidPage(
  useShinyjs(),
  titlePanel("SLOW zone viewer"),
  splitLayout(dateInput("sasdate", "Date:")),
                           actionButton("query","Query Database"),
                           textOutput("error1"),
                           textOutput("error3"),
                           #rHandsontableOutput("dailyeghot"),
                           #actionButton("eval","Evaluate"),
                           #tableOutput("egsastabout"), 
                           leafletOutput("sasdma"),    
                           tableOutput("actdma"), 
                           tableOutput("actdma_bounds")#,    
                           #actionButton("dmaup","Upload to database"),
                           #downloadButton("dmareport", "Download Report"),
                           #downloadButton("kml", "Download KML"),
                           #textInput("triggrp", "Who reported/observed these sightings?", width = 600, placeholder = "e.g.: 'New England Aquarium aerial survey team' or 'public'"),
                           #downloadButton("dmaletter", "Download Letter")
                           
                           )
                           
