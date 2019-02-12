
source('./scripts/global_libraries.R', local = TRUE)$value

ui <- dashboardPage(
  dashboardHeader(title = "Right Whale Shiny Dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Aerial Survey Processing App", tabName = "NARWSS"),
      menuItem("RWSAS DMA Evaluation", tabName = "RWSAS")
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "NARWSS",
              source('./scripts/NARWSSui.R', local = TRUE)$value
      ),
      
      # Second tab content
      tabItem(tabName = "Seabird",
              source('./scripts/DMAui.R', local = TRUE)$value
      )
    )
  )
)

server = function(input, output, session) {
  

  source('./scripts/NARWSSserver.R', local = TRUE)$value
  source('./scripts/DMAserver.R', local = TRUE)$value
}

shinyApp(ui, server)
