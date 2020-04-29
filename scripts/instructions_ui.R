fluidPage(
  useShinyjs(),
  titlePanel("Instructions for NEFSC Shiny apps"),

      tabsetPanel(type = "tabs",
                  tabPanel("Aerial Survey Processing App",
                           imageOutput("./scripts/ASP_Page_1.png")),
                  tabPanel("Management Trigger Analysis"),
                  tabPanel("Photo Position Finder")
                           )
                           )
                  
    