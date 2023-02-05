fluidPage(
  useShinyjs(),
  titlePanel("SLOW zone viewer"),
  splitLayout(dateInput("sasdate", "Date:")),
  actionButton("query", "Query Database"),
  textOutput("error1"),
  textOutput("error3"),
  leafletOutput("sasdma"),
  tableOutput("actdma"),
  tableOutput("actdma_bounds")
)
