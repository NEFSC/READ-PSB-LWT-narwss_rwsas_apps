# conditional report info ----

print(tempdir())

if (ftype == 20) {
  ftypesent <-
    "Only large whale sightings were recorded on this survey."
} else if (ftype == 21) {
  ftypesent <-
    "Only large whale sightings (excluding live minke whales) were recorded on this survey."
} else {
  ftypesent <- ""
}

rptnotes <- input$reportnotes

if (file.exists('./scripts/oracleaccess.R') == TRUE) {
  dmanamesexpsent <-
    paste0("Active right whale SLOW zone(s): ", dmanamesexp, ".")
  webshotpath <- paste0(getwd(), "/surveymap.png")
  source('./scripts/oracleaccess.R', local = TRUE)$value
  source('./scripts/input_sas.R', local = TRUE)$value
  source('./scripts/input_slowzone.R', local = TRUE)$value
  
} else {
  disable("dmaup")
  disable("dmareport")
  disable("kml")
  disable("dmaletter")
  
  dmanamesexpsent <- ""
  webshotpath <- paste0(path, "surveymap.png")
  
}

webshot::webshot("temp.html", file = webshotpath)
print("webshot")

file.copy("FlightReport.Rmd", tempReport, overwrite = FALSE)

params <-
  list(
    date1 = date_formats$date1,
    rptnotes = rptnotes,
    reportmap = reportmap,
    netable = netable,
    egreport = egreport,
    dmanamesexpsent = dmanamesexpsent,
    ftypesent = ftypesent,
    webshotpath = webshotpath
  )
print(webshotpath)
print(params)
print(file)
rmarkdown::render(
  tempReport,
  output_file = file,
  output_yaml = pdf_html,
  params = params,
  envir = new.env(parent = globalenv())
)