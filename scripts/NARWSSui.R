fluidPage(
  useShinyjs(),
  titlePanel("NEFSC Right Whale Data Processing"),
  sidebarLayout(
    sidebarPanel(
                 #radioButtons("where", "Survey Area", choices = c("US", "Canada"), selected = "US", inline = FALSE),
                 radioButtons("filepathway", "File Pathway", choices = c("Network", "Local"), selected = "Network", inline = FALSE), 
                 conditionalPanel(
                   condition = "input$filepathway == Local",
                   textInput("filepathinput", (HTML(paste("Local pathway", '<br/>', "Example: C:/2018/Flights/edit_data/"))))),
                   width = 3),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Aerial Survey",
                           (HTML(paste('<br/>',
                                       "For <strong>ONE</strong> flight days, files must be labeled as:",'<br/>', 
                                       "[YYMMDD].gps, [YYMMDD].eff, [YYMMDD].sig",'<br/>',
                                       '<br/>',
                                       "For <strong>TWO</strong> flight days, files must be labeled as:",'<br/>', 
                                       "[YYMMDD].gps, [YYMMDD] <strong>(1)</strong>.eff, [YYMMDD] <strong>(2)</strong>.eff, [YYMMDD] <strong>(1)</strong>.sig, [YYMMDD] <strong>(2)</strong>.sig",
                                       '<br/>'))),
                            position = 'left',
                           br(),
                           splitLayout(textInput("sd", "Survey Date", width = "90", placeholder = "YYMMDD"),
                                       radioButtons("multiflight", "Two flight Day?", choices = c("Yes", "No"), selected = "No", inline = FALSE),
                                       radioButtons("rawedits","Editted eff/sig files?", choices = c("Yes","No"), selected = "No", inline = FALSE)),
                           actionButton("rawupload", "Edit Raw Eff & Sig"),
                           br(),
                           textOutput("error"),
                           textOutput("error2"),
                           wellPanel(
                             div(rHandsontableOutput("handsES", height = 500), style = "font-size:80%")),
                           actionButton("edittable", "Open Sesame"),
                           br(),
                           wellPanel(
                             leafletOutput("egmap")),
                           textOutput("calferror"),
                           wellPanel(
                             div(rHandsontableOutput("handsrf", height = 800), style = "font-size:80%")),
                           actionButton("save", "Export CSV"),
                           br(),
                           leafletOutput("reportmap"),
                           tableOutput("egreport"),
                           tableOutput("netable"),
                           textAreaInput("reportnotes", "Report Notes", "Survey flown in XXX conditions. Beaufort ranged from X to X.", height = 100, width = 500),
                           textOutput("error3"),
                           textOutput("error4"),
                           downloadButton("report", "Generate Report")),
                  tabPanel("SAS & DMA Evaluation",
                           splitLayout(uiOutput("obspeeps_options"),
                                       uiOutput("plane_options")),
                           #tableOutput("egsastab"),
                           rHandsontableOutput("egsashot"),
                           textOutput("error5"),
                           actionButton("sas","Upload sightings to SAS"),
                           br(),
                           leafletOutput("sasdma"),    #1503
                           uiOutput("dmaoptions"),     #1544
                           tableOutput("dmacoord"),    #1481
                           actionButton("dmaup","Upload DMA to database"),
                           downloadButton("dmareport", "Download DMA Report"),
                           downloadButton("kml", "Download KML")
                           )
                           )
                  )))
    