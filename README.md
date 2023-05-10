# narwss_rwsas_apps

For more info on this app, please check out the Wiki as well as the section in the Shiny Book: https://nefsc.github.io/NEFSC-shiny-book/shiny-apps.html#northeast-right-whale-shiny-apps

## Getting Started

### Additional installations

`webshot::install_phantomjs()`

`install.packages('tinytex')`

`tinytex::install_tinytex()`

If you do not get TRUE when you run `tinytex:::is_tinytex()`, then you probably need to run this: `tinytex::install_tinytex(force=TRUE)`. More info on this process and the TinyTex package can be found here: https://yihui.name/tinytex/. Note the difference between tinytex the R package and TinyTeX the LaTeX distribution. Both commands above are needed. 

## Running the App
The app can be launched by running

`shiny::runGitHub("READ-PSB-LWT-narwss_rwsas_apps", username = "NEFSC", ref = "master")`

in your RStudio environment. Click 'Run App' to get started. In the window that pops up, click "Open in Browser". 

### Example data

To take a spin with processing example aerial survey data and evaluating if sightings trigger SLOW zones, chose/enter the following details (NOTE: to download the report, you will have to host the [example data](https://github.com/NEFSC/READ-PSB-LWT-narwss_rwsas_apps/tree/master/example_data/210409) on a local path specific to your machine, and the SLOW zone analysis will then not be available):

![](www/example_data.png)

### There are different scenarios available to explore with different example survey days:

210226: Visual sightings that fall within a Seasonal Management Area. One flight day, includes option to load previously editted eff/sig file.

210407: Visual sightings that trigger a new SLOW Zone. One flight day, includes option to load previously editted eff/sig file.

210409: Visual sightings that extend two active and overlapping SLOW Zones. Two flight day, no editted eff/sig file.

210512: Visual sightings that do not trigger new protection measures. One flight day, includes option to load previously editted eff/sig file.

## Script Flow Chart

![](www/scriptflow.png)



This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.


