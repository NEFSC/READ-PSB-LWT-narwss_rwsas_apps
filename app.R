
## App file for NARWSS & RWSAS APPs
## Leah Crowe                       


# Global ----

Sys.setenv(OPENSSL_CONF="/dev/null") #added 202300925 on new container for server use of webshot/phantomjs
#Sys.setenv(ORACLE_HOME="/ora1/app/oracle/product/11.2.0/dbhome_1") #220707 HJF change with continued ODBC connection issues
source('./scripts/global_libraries.R', local = TRUE)$value

if (file.exists('./scripts/creds.R') == TRUE) {
  source('./scripts/creds.R', local = TRUE)$value
}


# User interface ----


ui <- dashboardPage(
  dashboardHeader(title = "NERW Shiny"),
  # Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        icon = icon("plane"),
        "Aerial Survey Data Processing",
        tabName = "NARWSS"
      ),
      menuItem(
        icon = icon("calendar"),
        "Trigger Analysis",
        tabName = "Trigger"
      ),
      menuItem(
        icon = icon("camera"),
        "Photo Position Finder",
        tabName = "Photo"
      ),
      menuItem(
        icon = icon("ship"),
        "SLOW zone viewer",
        tabName = "szone"
      ),
      menuItem(
        icon = icon("question-circle"),
        text = "Instructions Wiki",
        href = "https://github.com/leahcrowe/narwss_rwsas_apps/wiki"
      )
    )
  ),
  ## Body content
  dashboardBody(
    tagList(img(src = 'noaanefsclogo.PNG'), br()),
    tags$head(tags$link(
      rel = "icon", type = "image/png", href = "favicon.png"
    )),
    tabItems(
      # First tab content
      tabItem(tabName = "NARWSS",
              source('./scripts/NARWSSapp.R', local = TRUE)$value),
      
      # Second tab content
      tabItem(tabName = "Trigger",
              source('./scripts/trigger_app.R', local = TRUE)$value),
      
      # Third tab content
      tabItem(tabName = "Photo",
              source('./scripts/photo_app.R', local = TRUE)$value),
      
      # fourth tab content
      tabItem(tabName = "szone",
              source('./scripts/szone_app.R', local = TRUE)$value)
    )
  )
)

server = function(input, output, session) {
}

shinyApp(ui, server)
