######################################
## App file for NARWSS & RWSAS APPs ##
## Leah Crowe 2019                  ##
######################################


#############
##  Global ##
#############

Sys.setenv(ORACLE_HOME="/ora1/app/oracle/product/11.2.0/dbhome_1")
source('./scripts/global_libraries.R', local = TRUE)$value

if (file.exists('./scripts/creds.R') == TRUE){
  source('./scripts/creds.R', local = TRUE)$value}

####################
## User interface ##
####################

ui <- dashboardPage(
  dashboardHeader(title = "NERW Shiny"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem(icon = icon("plane"),"Aerial Survey Processing App", tabName = "NARWSS"),
      menuItem(icon = icon("calendar"),"Trigger Analysis", tabName = "DMA"),
      menuItem(icon = icon("camera"),"Photo Position Finder", tabName = "Photo"),
      menuItem(icon = icon("question-circle"),text = "Instructions Wiki", href = "https://github.com/leahcrowe/narwss_rwsas_apps/wiki")
    )
  ),
  ## Body content
  dashboardBody(tagList(img(src = 'noaanefsclogo.PNG'),br()),
                tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png")),
    tabItems(
      # First tab content
      tabItem(tabName = "NARWSS",
              source('./scripts/NARWSSapp.R', local = TRUE)$value
      ),
      
      # Second tab content
      tabItem(tabName = "DMA",
              source('./scripts/DMAapp.R', local = TRUE)$value
      ),
      
      # Third tab content
      tabItem(tabName = "Photo",
              source('./scripts/photo_app.R', local = TRUE)$value

      )
    )
  )
)

server = function(input, output, session) {}

shinyApp(ui, server)
