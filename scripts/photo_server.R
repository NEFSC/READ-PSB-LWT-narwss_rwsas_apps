


output$contents <- renderTable({
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  inFile <- input$imagesub
  
  pernum=input$permit

  
  if (is.null(inFile)){
    return(NULL)
  
  }else{
  subraw<-read.csv(inFile$datapath, header = input$header, stringsAsFactors = FALSE)
  print(head (subraw))
  subraw$Month<-sprintf("%02d",subraw$Month)
  subraw$Day<-sprintf("%02d", subraw$Day)
  
  subraw<-data.frame(date_tz = "",
                     subraw)
  
  subraw$date_tz<-dmy_hms(subraw$date_tz)
  
  for(i in 1:nrow(subraw))
    if ( is.na(subraw$Latitude[i]) && subraw$Local.Time[i] != '' && !is.na(subraw$Year[i]) && subraw$Month[i] != 'NA' && subraw$Day[i] != 'NA' ){
    
    yr<-substr(subraw$Year[i],3,4)
    datestr<-paste0(yr,subraw$Month[i],subraw$Day[i])
    path<-paste0('//net/mmi/Fieldwrk/Aerials/20',yr,'/Flights/edit_data/')
    gps<-as.data.frame(read.csv(paste0(path,datestr,'/',datestr,'.gps'), header=FALSE, stringsAsFactors = FALSE))
    names(gps)<-c('DateTime','Latitude','Longitude','SPEED','HEADING','ALTITUDE','T1')
    gps$DateTime<-dmy_hms(gps$DateTime, tz = "GMT")
    
    if (input$tzone == 'Atlantic Time'){
    gps$date_tz<-with_tz(gps$DateTime, tzone = "America/New_York")
    } else if (input$tzone == 'Eastern Time'){
      gps$date_tz<-with_tz(gps$DateTime, tzone = "Canada/Atlantic")
      }
    
    gps$date_tz<-as.POSIXct(gps$date_tz, format = "%Y-%m-%d %H:%M:%OS")

    newdate<-paste0(subraw$Year[i],'-',subraw$Month[i],'-',subraw$Day[i])
    date_time <- (paste(newdate, subraw$Local.Time[i]))
    
    if (input$tzone == 'Atlantic Time'){
    date_tz<- as.POSIXlt(date_time, tz = "Canada/Atlantic", format = "%Y-%m-%d %H:%M:%OS")
    } else if (input$tzone == 'Eastern Time'){
      date_tz<- as.POSIXlt(date_time, tz = "America/New_York", format = "%Y-%m-%d %H:%M:%OS")
    }
    
    
    print(date_tz)
    subraw$date_tz[i]<-date_tz
    
    row<-subraw[i,]
    
    setDT(row)[,  Latitude := setDT(gps)[row, Latitude, on = "date_tz", roll = "nearest"]]
    setDT(row)[,  Longitude := setDT(gps)[row, Longitude, on = "date_tz", roll = "nearest"]]
    
    subraw[i,]<-row

    }
  
  CRS.latlon<-CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
  CRS.new<-CRS("+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  
  subraw$Latitude[which(is.na(subraw$Latitude))]<-0
  subraw$Longitude[which(is.na(subraw$Longitude))]<-0
  
  subraw.tr<-subraw
  coordinates(subraw.tr)<-~Longitude+Latitude
  proj4string(subraw.tr)<-CRS.latlon
  subraw.tr<-spTransform(subraw.tr, CRS.new)
  
  bof<-data.frame(
    long = c(-67,-65,-65,-63,-63,-64,-64,-67),
    lat = c(44,44,45,45,45.6833,45.6833,46,46))
  jl<-data.frame(
    long = c(-71,-69.8333,-69.8333,-71),
    lat = c(42.6667,42.6667,43.3333,43.3333))
  mb<-data.frame(
    long = c(-71,-69.83333,-69.83333,-70,-70,-71),
    lat = c(42.6667,42.6667,42,42,42.0667,42.0667))
  gsc<-data.frame(
    long = c(-70,-69,-69,-67.75,-67.75,-70),
    lat = c(42,42,42.3333,42.3333,41,41))
  gom<-data.frame(
    long = c(-71,-69.8333,-69.8333,-69,-69,-67.75,-67.75,-66.5,-66.5,-66,-66,-67.4167,-67.4167,-67,-67,-71),
    lat = c(43.3333,43.3333,42,42,42.3333,42.3333,42.1667,42.1667,43,43,44,44,44.4167,44.4167,44.8333,44.8333))
  gmb<-data.frame(
    long = c(-67,-67.4167,-67.4167,-67),
    lat = c(44,44,44.4167,44.4167))
  rb<-data.frame(
    long = c(-66.5,-66.5,-66,-66,-65,-65,-64,-64,-65.6667,-66),
    lat = c(42.1667,43,43,44,44,45,45,42,42,42.1667))
  ess<-data.frame(
    long = c(-64,-64,-62,-62,-60,-60,-58,-58,-60),
    lat = c(45,42.5,42.5,43,43,43.5,43.5,46,46))
  gb<-data.frame(
    long = c(-70,-70,-69,-69,-68,-68,-67,-67,-66,-66,-65.6667,-65.6667,-66,-67.75,-67.75),
    lat = c(41,39.6667,39.6667,39.8333,39.8333,40.1667,40.1667,40.5,40.5,41.5,41.5,42,42.1667,42.1667,41))
  sne<-data.frame(
    long = c(-72,-71,-71,-70,-70,-72),
    lat = c(39.5,39.5,39.6667,39.6667,41.6667,41.6667))
  ccb<-data.frame(
    long = c(-70,-70,-71,-70.5),
    lat = c(41.7,42.0667,42.0667,41.7))
  gsl<-data.frame(
    long = c(-66.5,-66.5,-58,-58),
    lat = c(52,46,46,52))
  
  bofpoly<-Polygons(list(Polygon(bof, hole=as.logical(NA))),ID =1)
  jlpoly<-Polygons(list(Polygon(jl, hole=as.logical(NA))),ID =2)
  mbpoly<-Polygons(list(Polygon(mb, hole=as.logical(NA))),ID =3)
  gscpoly<-Polygons(list(Polygon(gsc, hole=as.logical(NA))),ID =4)
  gompoly<-Polygons(list(Polygon(gom, hole=as.logical(NA))),ID =5)
  gmbpoly<-Polygons(list(Polygon(gmb, hole=as.logical(NA))),ID =6)
  rbpoly<-Polygons(list(Polygon(rb, hole=as.logical(NA))),ID =7)
  esspoly<-Polygons(list(Polygon(ess, hole=as.logical(NA))),ID =8)
  gbpoly<-Polygons(list(Polygon(gb, hole=as.logical(NA))),ID =9)
  snepoly<-Polygons(list(Polygon(sne, hole=as.logical(NA))),ID =10)
  ccbpoly<-Polygons(list(Polygon(ccb, hole=as.logical(NA))),ID =11)
  gslpoly<-Polygons(list(Polygon(gsl, hole=as.logical(NA))),ID =12)
  
  bofpoly<-SpatialPolygons(list(bofpoly))
  jlpoly<-SpatialPolygons(list(jlpoly))
  mbpoly<-SpatialPolygons(list(mbpoly))
  gscpoly<-SpatialPolygons(list(gscpoly))
  gompoly<-SpatialPolygons(list(gompoly))
  gmbpoly<-SpatialPolygons(list(gmbpoly))
  rbpoly<-SpatialPolygons(list(rbpoly))
  esspoly<-SpatialPolygons(list(esspoly))
  gbpoly<-SpatialPolygons(list(gbpoly))
  snepoly<-SpatialPolygons(list(snepoly))
  ccbpoly<-SpatialPolygons(list(ccbpoly))
  gslpoly<-SpatialPolygons(list(gslpoly))
  
  proj4string(bofpoly)<-CRS.latlon
  proj4string(jlpoly)<-CRS.latlon
  proj4string(mbpoly)<-CRS.latlon
  proj4string(gscpoly)<-CRS.latlon
  proj4string(gompoly)<-CRS.latlon
  proj4string(gmbpoly)<-CRS.latlon
  proj4string(rbpoly)<-CRS.latlon
  proj4string(esspoly)<-CRS.latlon
  proj4string(gbpoly)<-CRS.latlon
  proj4string(snepoly)<-CRS.latlon
  proj4string(ccbpoly)<-CRS.latlon
  proj4string(gslpoly)<-CRS.latlon
  
  bofpoly<-spTransform(bofpoly,CRS.new)
  jlpoly <-spTransform(jlpoly, CRS.new)
  mbpoly <-spTransform(mbpoly, CRS.new)
  gscpoly<-spTransform(gscpoly,CRS.new)
  gompoly<-spTransform(gompoly,CRS.new)
  gmbpoly<-spTransform(gmbpoly,CRS.new)
  rbpoly <-spTransform(rbpoly, CRS.new)
  esspoly<-spTransform(esspoly,CRS.new)
  gbpoly <-spTransform(gbpoly, CRS.new)
  snepoly<-spTransform(snepoly,CRS.new)
  ccbpoly<-spTransform(ccbpoly,CRS.new)
  gslpoly<-spTransform(gslpoly,CRS.new)
  
  BOF<-!is.na(sp::over(subraw.tr, as(bofpoly, "SpatialPolygons")))
  JL<-!is.na(sp::over(subraw.tr, as(jlpoly, "SpatialPolygons")))
  MB<-!is.na(sp::over(subraw.tr, as(mbpoly, "SpatialPolygons")))
  GSC<-!is.na(sp::over(subraw.tr, as(gscpoly, "SpatialPolygons")))
  GOM<-!is.na(sp::over(subraw.tr, as(gompoly, "SpatialPolygons")))
  GMB<-!is.na(sp::over(subraw.tr, as(gmbpoly, "SpatialPolygons")))
  RB<-!is.na(sp::over(subraw.tr, as(rbpoly, "SpatialPolygons")))
  ESS<-!is.na(sp::over(subraw.tr, as(esspoly, "SpatialPolygons")))
  GB<-!is.na(sp::over(subraw.tr, as(gbpoly, "SpatialPolygons")))
  SNE<-!is.na(sp::over(subraw.tr, as(snepoly, "SpatialPolygons")))
  CCB<-!is.na(sp::over(subraw.tr, as(ccbpoly, "SpatialPolygons")))
  GSL<-!is.na(sp::over(subraw.tr, as(gslpoly, "SpatialPolygons")))
  
  ######
  subraw<-cbind(subraw,BOF,JL,MB,GSC,GOM,GMB,RB,ESS,GB,SNE,CCB,GSL)
  
  for (i in 1:nrow(subraw))
    if (subraw$BOF[i] == TRUE){
      subraw$Area[i] = 'BOF'
    } else if (subraw$JL[i] == TRUE){
      subraw$Area[i] = 'JL'
    } else if (subraw$MB[i] == TRUE){
      subraw$Area[i] = 'MB'
    } else if (subraw$GSC[i] == TRUE){
      subraw$Area[i] = 'GSC'
    } else if (subraw$GOM[i] == TRUE){
      subraw$Area[i] = 'GOM'
    } else if (subraw$GMB[i] == TRUE){
      subraw$Area[i] = 'GMB'
    } else if (subraw$RB[i] == TRUE){
      subraw$Area[i] = 'RB'
    } else if (subraw$ESS[i] == TRUE){
      subraw$Area[i] = 'ESS'
    } else if (subraw$GB[i] == TRUE){
      subraw$Area[i] = 'GB'
    } else if (subraw$SNE[i] == TRUE){
      subraw$Area[i] = 'SNE'
    } else if (subraw$CCB[i] == TRUE){
      subraw$Area[i] = 'CCB'
    } else if (subraw$GSL[i] == TRUE){
      subraw$Area[i] = 'GSL'
    }
  
  subraw$Latitude[which(subraw$Latitude == 0)]<-''
  subraw$Longitude[which(subraw$Longitude == 0)]<-''
  subraw$Day<-as.character(subraw$Day)
  subraw$Month<-as.character(subraw$Month)
  subraw$Day[which(subraw$Day == 'NA')]<-''
  subraw$Month[which(subraw$Month == 'NA')]<-''
  
  subed<-subraw%>%
    filter(subraw$Photographer != '')%>%
    dplyr::select(-date_tz,-BOF,-JL,-MB,-GSC,-GOM,-GMB,-RB,-ESS,-GB,-SNE,-CCB,-GSL)
  
  subed$Obs = 'NEFSC/T'
  subed$Platform = 'A'
  subed$Image.Type = 'DS'
 
  print(pernum)
  for (i in 1:nrow(subed))
    if (nchar(pernum) == 0){
      subed$Notes[i] = subed$Notes[i]
    } else if (nchar(subed$Notes[i]) == 0){
      subed$Notes[i] = paste("Permit Number:",pernum)
    } else {
      subed$Notes[i] = paste0(subed$Notes[i],". Permit Number: ",pernum)
    }
  
  
  subed<-subed%>%
    dplyr::rename("Field EGNO" = Field.EGNO, "EG Letter" = EG.Letter, "Local Time" = Local.Time, "Image Type" = Image.Type, "Assoc. Type" = Assoc..Type, "First Edit" = First.Edit, "Second Edit" = Second.Edit, "Final Edit" = Final.Edit)

  write.csv(subed, paste0('//net/mmi/Fieldwrk/Aerials/20',yr,'/20',yr,'_digital_photos/Image Submission/NEFSC Sighting Data Table_Twin Otter_.csv'), na = '', row.names = FALSE)

  }
})

