# narwss_rwsas_apps

For processing flight data and compiling the report while off the network including while surveying in Canada

Getting Started
Required packages include:

library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(rgeos)
library(rgdal)
library(sp)
library(raster)
library(scales)
library(extrafont)
library(fontcm)
library(leaflet)
library(leaflet.esri)
library(knitr)
library(lubridate)
library(data.table)
library(zoo)
library(rhandsontable)
library(RColorBrewer)
library(reshape)
library(geosphere)
library(maptools)
library(maps)
library(rmarkdown)
library(tinytex)
library(igraph)
library(htmlwidgets)
library(webshot)
library(rlist)
Finally, if you do not get TRUE when you run tinytex:::is_tinytex(), then you probably need to run this: tinytex::install_tinytex(force=TRUE). More info on this process and the TinyTex package can be found here: https://yihui.name/tinytex/

Running the App
The app can be launched by running

shiny::runGitHub("narwss_rwsas_apps", username = "leeyuhc", ref = "master")

in your RStudio environment. Click 'Run App' to get started. In the window that pops up, click "Open in Browser".