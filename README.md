# narwss_rwsas_apps

To compile the aerial survey report while off the network, including while working in Canada, follow the directions below:

## Getting Started

Required packages include:

```

library(data.table)
library(dplyr)
library(extrafont)
library(fontcm)
library(geosphere)
library(ggplot2)
library(htmlwidgets)
library(igraph)
library(knitr)
library(leaflet)
library(leaflet.esri)
library(lubridate)
library(maps)
library(maptools)
library(raster)
library(RColorBrewer)
library(reshape)
library(rgeos)
library(rgdal)
library(rhandsontable)
library(rlist)
library(rmarkdown)
library(scales)
library(sf)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(sp)
library(tinytex)
library(webshot)
library(zoo)

```

Finally, if you do not get TRUE when you run `tinytex:::is_tinytex()`, then you probably need to run this: `tinytex::install_tinytex(force=TRUE)`. More info on this process and the TinyTex package can be found here: https://yihui.name/tinytex/

## Running the App
The app can be launched by running

`shiny::runGitHub("narwss_rwsas_apps", username = "leahcrowe", ref = "master")`

in your RStudio environment. Click 'Run App' to get started. In the window that pops up, click "Open in Browser".


## Script Flow Chart

![](www/scriptflow.png)



