##########################################
## App file for NARWSS & RWSAS DMA APPs ##
## Leah Crowe 2019                      ##
##########################################


#############
##  Global ##
#############

source('./scripts/global_libraries.R', local = TRUE)$value

####################
## User interface ##
####################

ui <- dashboardPage(
  dashboardHeader(title = "NERW Shiny"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Aerial Survey Processing App", tabName = "NARWSS"),
      menuItem("RWSAS DMA Evaluation", tabName = "RWSAS"),
      menuItem("Photo Position Finder", tabName = "Photo")
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
      tabItem(tabName = "RWSAS",
              source('./scripts/DMAapp.R', local = TRUE)$value
      ),
      
      # Third tab content
      tabItem(tabName = "Photo",
              source('./scripts/photo_app.R', local = TRUE)$value
      )
    )
  )
)

server = function(input, output, session) {
  

  #source('./scripts/NARWSSserver.R', local = TRUE)$value
  #source('./scripts/DMAserver.R', local = TRUE)$value
}

shinyApp(ui, server)
