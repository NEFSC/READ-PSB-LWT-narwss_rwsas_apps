# conditional report info ----

print(tempdir())
print(class(date_formats$date1))
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
  dmanamesexpsent <- paste0("Active right whale SLOW zone(s): ", dmanamesexp, ".")
  #webshotpath <- paste0(getwd(), "/surveymap.png") #works with Phantomjs 1/3
  webshotpath <- file.path(getwd(), "surveymap.png") #don't think this will work - likely  need to delete
  
  source('./scripts/oracleaccess.R', local = TRUE)$value
  source('./scripts/input_sas.R', local = TRUE)$value
  source('./scripts/input_slowzone.R', local = TRUE)$value
  
} else {
  disable("dmaup")
  disable("dmareport")
  disable("kml")
  disable("dmaletter")
  
  dmanamesexpsent <- ""
  #webshotpath <- paste0(path, "surveymap.png") #works with Phantomjs 2/3
  webshotpath <- file.path(path, "surveymap.png") #ChatGPT's attempt with mapview/webshot
}
print("line 36")
#Save leaflet map to PNG w/o PhantomJS or Chromium
#saved as "temp.html" html widget in NARWSServer 1984 and/or input_slowzone 16 selfcontained = FALSE

#mapview::mapshot2(reportmap, file = webshotpath, selfcontained = TRUE) #needs widget first, not "temp.html" #mapview option
webshot::webshot("temp.html", file = webshotpath) #works with webshot::phantomjs 3/3
#webshot2::webshot( #Option 3 - webshot2
#"temp.html",
#file = webshotpath,
#)

#print(paste("Map saved to", webshotpath))
#print(class(reportmap))
#print(class(date_formats$date1)) #needs to be a string or a date for mapview to work
#print("webshot")

file.copy("FlightReport.Rmd", tempReport, overwrite = FALSE)

#print(date_formats)

params <- list(
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
#print(params) #trying to troubleshoot $ operator is invalid for atomic vectors errors which assume comes from date1 = date_formats$date1 above
#print(file)
#print(str(params))

rmarkdown::render(
  tempReport,
  output_file = file,
  params = params,
  envir = new.env(parent = globalenv())
)

