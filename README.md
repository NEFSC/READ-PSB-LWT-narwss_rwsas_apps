# narwss_rwsas_apps

For more info on the suite of apps in this repo, please check out the [Wiki](https://github.com/NEFSC/READ-PSB-LWT-narwss_rwsas_apps/wiki) as well as [this section in this NEFSC Shiny Book](https://nefsc.github.io/NEFSC-shiny-book/shiny-apps.html#northeast-right-whale-shiny-apps). 

## Getting Started

### Additional installations for downloading PDFs

`webshot::install_phantomjs()`

`install.packages('tinytex')`

`tinytex::install_tinytex()`

If you do not get TRUE when you run `tinytex:::is_tinytex()`, then you probably need to run this: `tinytex::install_tinytex(force=TRUE)`. More info on this process and the TinyTex package can be found here: https://yihui.name/tinytex/. Note the difference between tinytex the R package and TinyTeX the LaTeX distribution. Both commands above are needed. 

### Troubleshooting PDF downloads

If after doing the installations above you are still having trouble downloading PDFs, there are options to compile the report as an html page. See this [section of the wiki](https://github.com/NEFSC/READ-PSB-LWT-narwss_rwsas_apps/wiki/Aerial-Survey-Processing-App,-Aerial-Survey-Tab:-Part-3). Different computers have different setups related to LaTeX distributions which might be causing the issue.

## Running the App
The app can be launched by running

`shiny::runGitHub("READ-PSB-LWT-narwss_rwsas_apps", username = "NEFSC", ref = "master")`

in your RStudio environment. Click 'Run App' to get started. In the window that pops up, click "Open in Browser". 

### Example data

To take a spin with processing example aerial survey data and evaluating if sightings trigger SLOW zones, chose/enter the following details (NOTE: to download the report, you will have to host the [example data](https://github.com/NEFSC/READ-PSB-LWT-narwss_rwsas_apps/tree/master/example_data/210409) on a local path specific to your machine, and the SLOW zone analysis will then not be available):

<img src="www/example_data.png" width="600">

### There are several scenarios available to explore with different example survey days:

* 210226: Visual sightings that fall within a Seasonal Management Area. One flight day, includes option to load previously editted eff/sig file.
* 210407: Visual sightings that trigger a new SLOW Zone. One flight day, includes option to load previously editted eff/sig file.
* 210409: Visual sightings that extend two active and overlapping SLOW Zones. Two flight day, no editted eff/sig file.
* 210512: Visual sightings that fall within active SLOW Zones, but trigger no further action. One flight day, includes option to load previously editted eff/sig file.

## Script Structure
```
app.R                             Starting App file that defines the dashboard for all apps in this repo

  scripts/global_libraries.R      Required libraries
  scripts/creds.R*                Allowable credentials defined for accessing the Shiny app on the network server
  
  scripts/NARWSSapp.R             App file for the North Atlantic Right Whale Sighting Survey (NARWSS)vAerial Survey Processing App
  scripts/NARWSSui.R              User interface for the NARWSS Aerial Survey Processing App
  scripts/NARWSSserver.R          Server code for NARWSS Aerial Survey Processing App
  
  scripts/Trigger_app.R           App file for the Trigger Analysis App
  scripts/Trigger_ui.R            User interface for the Trigger Analysis App
  scripts/Trigger_server.R        Server code for the Trigger Analysis App
    
  scripts/szone_app.R             App file for the Slow Zone viewer App
  scripts/szone_ui.R              User interface for the Slow Zone viewer App
  scripts/szone_server.R          Server code for the Slow Zone viewer App
  
  scripts/photo_app.R             App file for the Photo Position Finder App
  scripts/photo_ui.R              User interface for the Photo Position Finder App
  scripts/photo_server.R          Server code for the Photo Position Finder App

-- Files called by the server for the NARWSS Aerial Survey Processing App, Trigger Analysis App, and the Slow Zone viewer App --
  scripts/reactive.R              Reactive values passed between different actions
  scripts/sma.R                   Determines active Seasonal Management Areas based on the date
  scripts/oracleaccess.R*         Credentials for accessing Oracle database
  scripts/active_slowzone.R       Determines active Slow Zones based on the date
  scripts/action & slowzone.R     Procedure for determining action codes and triggered Slow Zones for right whale detections

-- Files called by the server for the NARWSS Aerial Survey Processing App --  
  scripts/download_content.R      Content passed to download handler for flight report
  scripts/FlightReport.Rmd        Template flight report

-- Files called by the server for the NARWSS Aerial Survey Processing App and the Trigger Analysis App --  
  scripts/input_sas.R             Detections formatted and uploaded to the Sighting Advisory System
  scripts/input_slowzone.R        Triggered Slow Zone data formatted and uploaded to the database
    scripts/slowzone_Report.Rmd   Template for Slow Zone report
    scripts/slowzone_Letter.Rmd   Template for Slow Zone letter. Conditionally cycles through slowzone_let#.Rmd if more than one Slow Zone is triggered from one flight or on one day

-- Files called by the server for the Trigger Analysis App --
  scripts/Acoustic_datapull.R*    Queries right whale acoustic detections based on selected date  

*not managed on GitHub
```
## Script Flow Chart

![](www/scriptflow.png)

## Contributions

In the [NEFSC Shiny Book](https://nefsc.github.io/NEFSC-shiny-book/shiny-apps.html#northeast-right-whale-shiny-apps), you can find the point of contact for questions regarding this repository. In addition, problems can be reported or suggestions can be submitted via [this repository's issue tab](https://github.com/NEFSC/READ-PSB-LWT-narwss_rwsas_apps/issues). 

For substantial suggestions/changes, please first open the discussion by submitting an [issue](https://github.com/NEFSC/READ-PSB-LWT-narwss_rwsas_apps/issues).

This project, and everyone participating in it, is governed by [this Code of Conduct](https://github.com/nmfs-fish-tools/Resources/blob/main/CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code.

##

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.


