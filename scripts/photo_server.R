#20260304 Claude/HJF rewrite to not use sp functions - get everything to sf #20260326 bem incorporate gps_yymmdd (mysti)
observeEvent(input$photogo,{
  
  output$finalmess<-renderUI({NULL})
  
  phserv<-input$filepathway
  phyear<-input$photoyear
  phfile<-input$photofile
  pernum<-input$permit
  override<-NULL
  print(phserv)
  print(phyear)
  print(phfile)
  print(pernum)
  
  print(file.exists('./scripts/oracleaccess.R'))
  
  if (file.exists('./scripts/oracleaccess.R') == TRUE){
    override <- 'Network'
  } else {
    override <- 'Local'
  }
  
  
  
  if (override == phserv){
    
    if(phserv == 'Network'){
      pathway<-paste0('/mnt/PSD-Whale_Surveys/Fieldwrk/Aerials/',phyear,'/')
      pathimage<-paste0(pathway,phyear,'_digital_photos/Image Submission/',phfile,'.csv')
    } else if (phserv == 'Local'){
      pathway<-input$filepathinput
      pathimage<-paste0(input$filepathinput,phfile,'.csv')
    }
    
    print(pathway)
    print(pathimage)
    print(file.exists(pathimage))
    
    if (file.exists(pathimage) == TRUE){
      
      subraw<-read.csv(pathimage, header = TRUE, stringsAsFactors = FALSE)
      subraw$Month<-sprintf("%02d",subraw$Month)
      subraw$Day<-sprintf("%02d", subraw$Day)
      subraw<-data.frame(date_tz = "",subraw)
      subraw$date_tz<-dmy_hms(subraw$date_tz)
      print(head(subraw))
      
      withProgress(message = 'Finding whale positions from timestamp...', min = 0, max = nrow(subraw), {
        for(i in 1:nrow(subraw))
          if ( is.na(subraw$Latitude[i]) && subraw$Local.Time[i] != '' && !is.na(subraw$Year[i]) && subraw$Month[i] != 'NA' && subraw$Day[i] != 'NA' ){
            yr<-substr(subraw$Year[i],3,4)
            print(yr)
            print("yr")
            datestr<-paste0(yr,subraw$Month[i],subraw$Day[i])
            print(datestr)
            
            if(input$filepathway == 'Network'){
              base_path <- file.path(pathway, 'Flights/edit_data', datestr)
            } else if (input$filepathway == 'Local') {
              base_path <- file.path(pathway, datestr)
            }
            
        #look for vor gps first (keep for now)
            gps_list <- list.files(
                path = base_path,
                pattern = paste0("^", datestr, ".*\\.gps$"),
                full.names = TRUE
              )
           file_type <-  "gps"
           
        #if no vor, find mysti gps
          if (length(gps_list) == 0) {
            gps_list <- list.files(
              path = base_path,
              pattern = paste0("^gps_", datestr, "\\.csv$"),
              full.names = TRUE
            )
            file_type <- "csv"
          }
           
           #Check for .gps file and move to next row in loop if doesn't exist 20260312 HJF add (Mysticetus dates Dec 2025 and on don't have it)
           if (length(gps_list) == 0) {
             message(paste("No GPS files found for date:", datestr))
             incProgress(amount = 1)
             next  # skip to next iteration of the for loop
           }
          
           gps_files <- lapply(gps_list, function(x) {
             if (file_type == "gps") {
               read.csv(x, header = FALSE, stringsAsFactors = FALSE)
             } else {
               read.csv(x, header = TRUE, stringsAsFactors = FALSE)
             }
           })    

              
         # pathgps<-paste0(pathway,'Flights/edit_data/',datestr,'/',datestr,"*\\.gps")
           gps_all<-do.call(rbind, gps_files)
           #guard against rbind returning nothing 20260312 HJF add
           if (is.null(gps_all) || nrow(gps_all) == 0) {
             incProgress(amount = 1)
             next
           }
            #gps_files<-lapply(gps_list, function (x) read.csv(paste0(pathway,'Flights/edit_data/',datestr,'/',x), header=FALSE, stringsAsFactors = FALSE))
           
            gps <-as.data.frame(gps_all)
            
            if (file_type == "gps") {
              names(gps) <- c('DateTime','Latitude','Longitude','SPEED','HEADING','ALTITUDE','T1')
            } else {
              names(gps) <- c('DateTime','Latitude','Longitude','SPEED','HEADING','ALTITUDE')
              gps$T1 <- NA
            }  
            
            if (file_type == "gps") {
              gps$DateTime <- dmy_hms(gps$DateTime, tz = "GMT")
            } else {
              gps$DateTime <- ymd_hms(gps$DateTime, tz = "GMT")
            }
            
            if (input$tzone == 'Atlantic Time'){
              gps$date_tz <- with_tz(gps$DateTime, tzone = "America/Halifax")
            } else if (input$tzone == 'Eastern Time'){
              gps$date_tz <- with_tz(gps$DateTime, tzone = "America/New_York")
            }
            
            gps$date_tz<-as.POSIXct(gps$date_tz, format = "%Y-%m-%d %H:%M:%OS")
            
            newdate<-paste0(subraw$Year[i],'-',subraw$Month[i],'-',subraw$Day[i])
            date_time <- (paste(newdate, subraw$Local.Time[i]))
            
            if (input$tzone == 'Atlantic Time'){
              date_tz <- as.POSIXlt(date_time, tz = "America/Halifax", format = "%Y-%m-%d %H:%M:%OS")
            } else if (input$tzone == 'Eastern Time'){
              date_tz <- as.POSIXlt(date_time, tz = "America/New_York", format = "%Y-%m-%d %H:%M:%OS")
            }
            
            print(date_tz)
            subraw$date_tz[i]<-date_tz
            
            row<-subraw[i,]
            
            setDT(row)[,  Latitude := setDT(gps)[row, Latitude, on = "date_tz", roll = "nearest"]]
            setDT(row)[,  Longitude := setDT(gps)[row, Longitude, on = "date_tz", roll = "nearest"]]
            
            subraw[i,]<-row
            incProgress(amount = 1)
            
          }
      })
      
      # --- CRS definitions (sf-based) ---
      CRS.latlon <- st_crs(4269)   # NAD83 geographic
      CRS.new    <- st_crs(26919)  # UTM Zone 19N, NAD83
      
      subraw$Latitude[which(is.na(subraw$Latitude))]   <- 0
      subraw$Longitude[which(is.na(subraw$Longitude))] <- 0
      subraw$Latitude  <- as.numeric(subraw$Latitude)
      subraw$Longitude <- as.numeric(subraw$Longitude)
      
      # --- Convert subraw to sf and reproject ---
      subraw.tr <- st_as_sf(subraw, coords = c("Longitude", "Latitude"), crs = CRS.latlon, remove = FALSE)
      subraw.tr <- st_transform(subraw.tr, CRS.new)
      
      # --- Helper: build sf polygon from a data.frame of long/lat ---
      make_poly <- function(df) {
        coords <- as.matrix(rbind(df[, c("long","lat")], df[1, c("long","lat")]))  # close ring
        st_sf(geometry = st_sfc(st_polygon(list(coords)), crs = CRS.latlon)) |>
          st_transform(CRS.new)
      }
      
      # --- Polygon coordinate data frames (unchanged) ---
      bof<-data.frame(long=c(-67,-65,-65,-63,-63,-64,-64,-67),lat=c(44,44,45,45,45.6833,45.6833,46,46))
      jl <-data.frame(long=c(-71,-69.8333,-69.8333,-71),lat=c(42.6667,42.6667,43.3333,43.3333))
      mb <-data.frame(long=c(-71,-69.83333,-69.83333,-70,-70,-71),lat=c(42.6667,42.6667,42,42,42.0667,42.0667))
      gsc<-data.frame(long=c(-70,-69,-69,-67.75,-67.75,-70),lat=c(42,42,42.3333,42.3333,41,41))
      gom<-data.frame(long=c(-71,-69.8333,-69.8333,-69,-69,-67.75,-67.75,-66.5,-66.5,-66,-66,-67.4167,-67.4167,-67,-67,-71),lat=c(43.3333,43.3333,42,42,42.3333,42.3333,42.1667,42.1667,43,43,44,44,44.4167,44.4167,44.8333,44.8333))
      gmb<-data.frame(long=c(-67,-67.4167,-67.4167,-67),lat=c(44,44,44.4167,44.4167))
      rb <-data.frame(long=c(-66.5,-66.5,-66,-66,-65,-65,-64,-64,-65.6667,-66),lat=c(42.1667,43,43,44,44,45,45,42,42,42.1667))
      ess<-data.frame(long=c(-64,-64,-62,-62,-60,-60,-58,-58,-60),lat=c(45,42.5,42.5,43,43,43.5,43.5,46,46))
      gb <-data.frame(long=c(-70,-70,-69,-69,-68,-68,-67,-67,-66,-66,-65.6667,-65.6667,-66,-67.75,-67.75),lat=c(41,39.6667,39.6667,39.8333,39.8333,40.1667,40.1667,40.5,40.5,41.5,41.5,42,42.1667,42.1667,41))
      sne<-data.frame(long=c(-72,-71,-71,-70,-70,-72),lat=c(39.5,39.5,39.6667,39.6667,41.6667,41.6667))
      ccb<-data.frame(long=c(-70,-70,-71,-70.5),lat=c(41.7,42.0667,42.0667,41.7))
      gsl<-data.frame(long=c(-66.5,-66.5,-58,-58),lat=c(52,46,46,52))
      ny <-data.frame(long=c(-73.5,-72,-72,-72.75,-73.5),lat=c(40,40,41.3,41.1,40.8))
      nj <-data.frame(long=c(-75,-73.5,-73.5,-75),lat=c(38.79,38.79,40.48,40.48))
      
      # --- Build sf polygons ---
      bofpoly <- make_poly(bof)
      jlpoly  <- make_poly(jl)
      mbpoly  <- make_poly(mb)
      gscpoly <- make_poly(gsc)
      gompoly <- make_poly(gom)
      gmbpoly <- make_poly(gmb)
      rbpoly  <- make_poly(rb)
      esspoly <- make_poly(ess)
      gbpoly  <- make_poly(gb)
      snepoly <- make_poly(sne)
      ccbpoly <- make_poly(ccb)
      gslpoly <- make_poly(gsl)
      nypoly  <- make_poly(ny)
      njpoly  <- make_poly(nj)
      
      # Combined polygon collection for the leaflet map — must be WGS84 for leaflet
      allpoly <- do.call(rbind, lapply(
        list(bof, jl, mb, gsc, gom, gmb, rb, ess, gb, sne, ccb, gsl, ny, nj),
        function(df) {
          coords <- as.matrix(rbind(df[, c("long","lat")], df[1, c("long","lat")]))
          st_sf(geometry = st_sfc(st_polygon(list(coords)), crs = 4326))
        }
      ))
      
      # --- Point-in-polygon using st_intersects (returns sparse logical matrix) ---
      pip <- function(points_sf, poly_sf) {
        lengths(st_intersects(points_sf, poly_sf)) > 0
      }
      
      BOF <- pip(subraw.tr, bofpoly)
      JL  <- pip(subraw.tr, jlpoly)
      MB  <- pip(subraw.tr, mbpoly)
      GSC <- pip(subraw.tr, gscpoly)
      GOM <- pip(subraw.tr, gompoly)
      GMB <- pip(subraw.tr, gmbpoly)
      RB  <- pip(subraw.tr, rbpoly)
      ESS <- pip(subraw.tr, esspoly)
      GB  <- pip(subraw.tr, gbpoly)
      SNE <- pip(subraw.tr, snepoly)
      CCB <- pip(subraw.tr, ccbpoly)
      GSL <- pip(subraw.tr, gslpoly)
      NY  <- pip(subraw.tr, nypoly)
      NJ  <- pip(subraw.tr, njpoly)
      
      ######
      subraw<-cbind(subraw,BOF,JL,MB,GSC,GOM,GMB,RB,ESS,GB,SNE,CCB,GSL,NY,NJ)
      subraw$Area <- ''  # initialize so unmatched rows don't produce NA
      
      for (i in 1:nrow(subraw))
        if (isTRUE(subraw$BOF[i])){
          subraw$Area[i] = 'BOF'
        } else if (isTRUE(subraw$JL[i])){
          subraw$Area[i] = 'JL'
        } else if (isTRUE(subraw$MB[i])){
          subraw$Area[i] = 'MB'
        } else if (isTRUE(subraw$GSC[i])){
          subraw$Area[i] = 'GSC'
        } else if (isTRUE(subraw$GOM[i])){
          subraw$Area[i] = 'GOM'
        } else if (isTRUE(subraw$GMB[i])){
          subraw$Area[i] = 'GMB'
        } else if (isTRUE(subraw$RB[i])){
          subraw$Area[i] = 'RB'
        } else if (isTRUE(subraw$ESS[i])){
          subraw$Area[i] = 'ESS'
        } else if (isTRUE(subraw$GB[i])){
          subraw$Area[i] = 'GB'
        } else if (isTRUE(subraw$SNE[i])){
          subraw$Area[i] = 'SNE'
        } else if (isTRUE(subraw$CCB[i])){
          subraw$Area[i] = 'CCB'
        } else if (isTRUE(subraw$GSL[i])){
          subraw$Area[i] = 'GSL'
        } else if (isTRUE(subraw$NY[i])){
          subraw$Area[i] = 'NY'
        } else if (isTRUE(subraw$NJ[i])){
          subraw$Area[i] = 'NJ'
        }
      
      subraw$Latitude[which(subraw$Latitude == 0)]<-''
      subraw$Longitude[which(subraw$Longitude == 0)]<-''
      subraw$Day<-as.character(subraw$Day)
      subraw$Month<-as.character(subraw$Month)
      subraw$Day[which(subraw$Day == 'NA')]<-''
      subraw$Month[which(subraw$Month == 'NA')]<-''
      
      subed<-subraw%>%
        filter(subraw$Photographer != '')%>%
        dplyr::select(-date_tz,-BOF,-JL,-MB,-GSC,-GOM,-GMB,-RB,-ESS,-GB,-SNE,-CCB,-GSL,-NY,-NJ)
      
      subed$Obs = 'NEFSC/T'
      subed$Platform = 'A'
      subed$Image.Type = 'DS'
      
      print(pernum)
      # for (i in 1:nrow(subed))
      #   if (nchar(pernum) == 0){
      #     subed$Notes[i] = subed$Notes[i]
      #   } else if (nchar(subed$Notes[i]) == 0){
      #     subed$Notes[i] = paste("Permit Number:",pernum)
      #   } else {
      #     subed$Notes[i] = paste0(subed$Notes[i],". Permit Number: ",pernum)
      #   }
      
      #rewrite with errors above
      if (!is.null(pernum) && !is.na(pernum) && nchar(trimws(pernum)) > 0){
        for (i in 1:nrow(subed)){
          if (is.na(subed$Notes[i]) || nchar(trimws(subed$Notes[i])) == 0){
            subed$Notes[i] = paste("Permit Number:", pernum)
          } else {
            subed$Notes[i] = paste0(subed$Notes[i], ". Permit Number: ", pernum)
          }
        }
      }
      
      subed$Latitude<-as.numeric(subed$Latitude)
      subed$Longitude<-as.numeric(subed$Longitude)
      
      subed<-subed%>%
        dplyr::rename("Field EGNO" = Field.EGNO, "EG Letter" = EG.Letter, "Local Time" = Local.Time, "Image Type" = Image.Type, "Assoc. Type" = Assoc..Type, "First Edit" = First.Edit, "Second Edit" = Second.Edit, "Final Edit" = Final.Edit)
      
      if (input$filepathway == 'Network'){
        write.csv(subed, paste0('/mnt/PSD-Whale_Surveys/Fieldwrk/Aerials/20',yr,'/20',yr,'_digital_photos/Image Submission/NEFSC Sighting Data Table_Twin Otter_',Sys.Date(),'.csv'), na = '', row.names = FALSE)
      } else if (input$filepathway == 'Local'){
        write.csv(subed, paste0(input$filepathinput,'NEFSC Sighting Data Table_Twin Otter_',Sys.Date(),'.csv'), na = '', row.names = FALSE)
        print(paste0(input$filepathinput,'NEFSC Sighting Data Table_Twin Otter_',Sys.Date(),'.csv'))  
      }
      
      finalleaf<-leaflet(data = subed, options = leafletOptions(zoomControl = TRUE)) %>% 
        addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE) %>%
        addPolygons(data = allpoly, weight = 2, color = "blue") %>%
        addCircleMarkers(lng = ~subed$Longitude, lat = ~subed$Latitude, color = "black", stroke = FALSE, fillOpacity = 2, radius = 5, popup = paste0(subed$Year,"-",subed$Month,"-",subed$Day,"-",subed$`EG Letter`)) %>%
        addWMSTiles(
          "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
          layers = c("1-degree grid", "5-degree grid"),
          options = WMSTileOptions(format = "image/png8", transparent = TRUE),
          attribution = NULL)  
      output$finalleaf = renderLeaflet({print(finalleaf)})
      
      output$finalmess <- renderUI({
        
        msg <- if (input$filepathway == "Network") {
          
          paste0(
            "Saved to network 'Image Submission' folder as: NEFSC Sighting Data Table_Twin Otter_",
            Sys.Date(), ".csv"
          )
          
        } else if (input$filepathway == "Local") {
          
          "Saved locally"
          
        } else {
          
          "File does not exist"
          
        }
        
        tags$div(
          style = "font-weight: bold; color: #155724; background-color: #d4edda; padding: 10px; border-radius: 5px;",
          msg
        )
      })
    }
  }
    })
