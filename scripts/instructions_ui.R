fluidPage(
  useShinyjs(),
  titlePanel("Instructions for NEFSC Shiny apps"),

      tabsetPanel(type = "tabs",
                  tabPanel("Aerial Survey Processing App",
                           tagList(img(src = './instructions/ASP_Page_1.PNG'))
                           ),
                  tabPanel("Management Trigger Analysis"),
                  tabPanel("Photo Position Finder")
                           )
                           )
                  
    