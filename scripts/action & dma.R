###Action & DMA analysis


  ########
  ##ACTION
  ########
  ##Shapefiles
  
  ##copy for spatializing
  eg<-egsas
  ##declare which columns are coordinates
  coordinates(eg)<-~LONGITUDE+LATITUDE
  ##declare what kind of projection thy are in
  proj4string(eg)<-CRS.latlon
  ##change projection
  eg.tr<-spTransform(eg, CRS.new)
  
  #####
  ##in or out of the sma? TRUE = in
  inoutsma<-NULL
  
  for (i in 1:nrow(egsas))
    if (between(MODA,"01-01", "02-29")){
      inoutsma<-!is.na(sp::over(eg.tr, as(sma1, "SpatialPolygons")))
    } else if (between(MODA,"03-01","03-31")){
      inoutsma<-!is.na(sp::over(eg.tr, as(sma2, "SpatialPolygons")))
    } else if (between(MODA,"04-01","04-30")){
      inoutsma<-!is.na(sp::over(eg.tr, as(sma3, "SpatialPolygons")))
    } else if (between(MODA,"05-01","05-15")){
      inoutsma<-!is.na(sp::over(eg.tr, as(sma4, "SpatialPolygons")))
    } else if (between(MODA,"05-16","07-31")){
      inoutsma<-!is.na(sp::over(eg.tr, as(sma5, "SpatialPolygons")))
    } else if (between(MODA,"11-01","12-31")){
      inoutsma<-!is.na(sp::over(eg.tr, as(sma6, "SpatialPolygons")))
    } else {
      nrow(inoutsma) == nrow(egsas)
      inoutsma<-FALSE
    }
  
  Canada<-!is.na(sp::over(eg.tr, as(ecanada, "SpatialPolygons")))
  SPM<-!is.na(sp::over(eg.tr, as(spm, "SpatialPolygons")))
  aDMA<-over(eg.tr, activedma.tr)
  aDMA_TF<-!is.na(over(eg.tr, as(activedma.tr, "SpatialPolygons")))
  sightID<-1:nrow(egsas)
  ######
  egsas<-cbind(egsas,inoutsma,Canada,SPM,aDMA,aDMA_TF,sightID)
  print(head(egsas))
  ##extension date
  expext<-expext%>%
    arrange(ID)%>%
    mutate(extdate = EXPDATE - days(8),
           polyid = 1:n())
  print(expext)
  egsas<-left_join(egsas,expext,by=c("aDMA"="polyid"))
  print(egsas)
  ACTION_NEW<-NULL
  ## In US database, Canada/SPM == 6.
  ## need to figure out how to evaluate over a list of DMAs for extension
  ## 12/31 is a good proxy day for this
  for (i in 1:nrow(egsas))
    if(egsas$aDMA_TF[i] == TRUE & MODAYR > egsas$extdate[i]){
      egsas$ACTION_NEW[i] = 5 
    } else if(egsas$aDMA[i] == TRUE){
        egsas$ACTION_NEW[i] = 2   
    } else if (egsas$inoutsma[i] == TRUE){
      egsas$ACTION_NEW[i] = 2
    } else if (egsas$Canada[i] == TRUE){
      egsas$ACTION_NEW[i] = 6
    } else if (egsas$SPM[i] == TRUE){
      egsas$ACTION_NEW[i] = 6
      output$error3<-renderText({"Soc re bleu! One of these right whales was in France!"})
    } else if (egsas$inoutsma[i] == FALSE){
      egsas$ACTION_NEW[i] = NA
    } 
  
  ##############
  ## dma eval ##
  ##############
  
  #########
  #spatial analysis
  ## 1 nautical mile is 1852 meters
  m_nm<-1/1852
  ## eg density is 4 whales/100nm^2 (50 CFR Part 224)
  egden<-0.0416
  actionna<-egsas %>% filter(is.na(egsas$ACTION_NEW)) %>% dplyr::select("DateTime", "LATITUDE", "LONGITUDE", "GROUP_SIZE","sightID")
  ##distance between points matrix -- compares right whale sightings positions to each other
  combo<-reshape::expand.grid.df(actionna,actionna)
  names(combo)[6:10]<-c("DateTime2","LATITUDE2","LONGITUDE2","GROUP_SIZE2","sightID2")
  combo$GROUP_SIZE<-as.character(combo$GROUP_SIZE)
  combo$GROUP_SIZE<-as.numeric(combo$GROUP_SIZE)
  ##calculates core area
  setDT(combo)[ ,corer:=round(sqrt(GROUP_SIZE/(pi*egden)),2)] 
  ##calculates distance between points in nautical miles
  setDT(combo)[ ,dist_nm:=geosphere::distVincentyEllipsoid(matrix(c(LONGITUDE,LATITUDE), ncol = 2),
                                                           matrix(c(LONGITUDE2, LATITUDE2), ncol =2), 
                                                           a=6378137, f=1/298.257222101)*m_nm]
  print(combo)
  #filters out points compared where core radius is less than the distance between them (meaning that the position combo will not have overlapping core radii) and
  #keeps the single sightings where group size would be enough to trigger a DMA (0 nm dist means it is compared to itself)
  #I don't remember why I named this dmacand -- maybe dma combo and... then some?
  dmacand<-combo %>%
    dplyr::filter((combo$dist_nm != 0 & combo$dist_nm <= combo$corer) | (combo$GROUP_SIZE > 2 & combo$dist_nm == 0))
  #print(dmacand)
  ##filters for distinct sightings that should be considered for DMA calculation
  dmasightID<-data.frame(sightID = c(dmacand$sightID,dmacand$sightID2)) %>%
    distinct()
  #print(dmasightID)
  ##if not a dma animal, action == 1
  ##if a dma animal, action == 4
  ##if a dma animal that's in an ending dma and extending the dma, action == 5
  ##the below sees if the sightings are good for DMA calc (are in the dmasightID list), and assigns action codes accordingly which is part of NOAA database
  for (i in 1:nrow(egsas))
    if (egsas$sightID[i] %in% dmasightID$sightID) {
      egsas$ACTION_NEW[i] = 4
    } else if (is.na(egsas$ACTION_NEW[i])){
      egsas$ACTION_NEW[i] = 1
    } else {
      egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
    }
  
  ##FINAL PRODUCT IS EGSAS WITH ACTIONS   
  
  egsas$ACTION_NEW<-sprintf("%.0f",round(egsas$ACTION_NEW, digits = 0))
  
  ######
  ##Evaluate for DMA
  ######
  #only sightings with an action of 4 will be evaluated here for DMA
  if (4 %in% egsas$ACTION_NEW){
    ##################
    ##CREATING A DMA##
    ##################
    
    ##the below gets you all the sightings you need for DMA analysis from the combo matrix above
    dmasig<-inner_join(combo,dmasightID, by = "sightID")
    #print(dmasig)
    
    dmasights<-dmasig%>%
      dplyr::select(DateTime,LATITUDE,LONGITUDE,GROUP_SIZE, sightID)%>%
      distinct(DateTime,LATITUDE,LONGITUDE,GROUP_SIZE,sightID)%>%
      mutate(corer=round(sqrt(GROUP_SIZE/(pi*egden)),2))%>%
      as.data.frame()
    #print(dmasights)
    dmasights$GROUP_SIZE<-as.numeric(dmasights$GROUP_SIZE)
    
    PolyID<-rownames(dmasights)
    #print(PolyID)
    #core radius in meters
    corer_m<-dmasights$corer*1852
    dmasights<-cbind(dmasights,corer_m,PolyID)
    
    #copy for spatializing
    dmadf<-dmasights
    
    ########################
    ## df to spatial object ##
    ########################
    ##declare which values are coordinates
    coordinates(dmadf)<-~LONGITUDE+LATITUDE
    ##declare what projection they are in
    proj4string(dmadf)<-CRS.latlon
    ##transform projection
    dmadf.tr<-spTransform(dmadf, CRS.utm)
    ###########
    
    ##gbuffer needs utm to calculate radius in meters
    dmabuff<-gBuffer(dmadf.tr, byid=TRUE, width = dmadf$corer_m, capStyle = "ROUND")
    #print(dmabuff)
    ##data back to latlon dataframe
    ##this will be used later when sightings are clustered by overlapping core radiis
    clustdf<-spTransform(dmadf.tr, CRS.latlon)
    clustdf<-as.data.frame(clustdf)
    
    ##creates a dataframe from the density buffers put around sightings considered for DMA analysis
    polycoord<-dmabuff %>% fortify() %>% dplyr::select("long","lat","id")
    ##poly coordinates out of utm
    coordinates(polycoord)<-~long+lat
    proj4string(polycoord)<-CRS.utm
    polycoord.tr<-spTransform(polycoord, CRS.latlon)
    polycoorddf<-as.data.frame(polycoord.tr)

    #############           
    ##this part may not be necessary as things in here have changed quite a bit and I haven't had time to dissect this part
    ##things have changed a lot, but a spatialpolygon dataframe product is necessary to feed into the overlap analysis
    ## the circular core areas are the polygons in the below section
    idpoly<-split(polycoorddf, polycoorddf$id)

    idpoly<-lapply(idpoly, function(x) { x["id"] <- NULL; x })

    pcoord<-lapply(idpoly, Polygon)

    pcoord_<-lapply(seq_along(pcoord), function(i) Polygons(list(pcoord[[i]]), ID = names(idpoly)[i]))

    polycoorddf_sp<-SpatialPolygons(pcoord_, proj4string = CRS.latlon)

    ##############
    if (length(names(idpoly)) > 1){
      ##Overlap of whale density core area analysis
      polycomb<-data.frame(poly1=NA,poly2=NA,overlap=NA)
      ##creates a list of 2 combinations to compare
      #print(names(idpoly))
      combos<-combn(names(idpoly),2)
      ##compares the list
      for(i in seq_along(combos[1,])){
        poly1 <- combos[1,i]
        poly2 <- combos[2,i]
        #if they don't overlap, the result of the below "if statement" is NULL
        if(!is.null(gIntersection(polycoorddf_sp[poly1], polycoorddf_sp[poly2], byid = TRUE))){
          overlap = 'yes'
        } else {
          overlap = 'no'
        }
        df<-data.frame(poly1=poly1,poly2=poly2,overlap=overlap)
        polycomb<-rbind(polycomb,df)
      }
      
      polycomb$poly1<-as.numeric(polycomb$poly1)
      polycomb$poly2<-as.numeric(polycomb$poly2)
      polycluster<-polycomb%>%filter(!is.na(poly1))
    } else if (length(names(idpoly)) == 1) {
      polycluster<-data.frame(poly1 = 1, poly2 = 1, overlap = 'no')
    }
    
    ####clustering polygons that overlap
    polycluster_yes<-polycluster%>%
      filter(overlap=="yes")
    
    ###transitive property of oberlapping core areas
    polymat = graph_from_edgelist(as.matrix(polycluster_yes[,1:2]), directed=FALSE)
    #unique polygons
    upoly = sort(unique(c(polycluster_yes$poly1, polycluster_yes$poly2)))
    (cluster = components(polymat)$membership[upoly])
    #final cluster assignment df for overlap = yes
    (polyassign = data.frame(upoly, cluster, row.names=NULL))
    
    poly12<-rbind(unlist(polycluster$poly1),unlist(polycluster$poly2))
    poly12 <- data.frame(upoly = c(polycluster$poly1,polycluster$poly2))
    
    ##the sightings are NOT triggering on their own (without overlapping sightings) are assigned a cluster of -1
    not<-poly12%>%
      filter((!poly12$upoly %in% polyassign$upoly) | (!poly12$upoly %in% polyassign$upoly))%>%
      distinct()%>%
      mutate(cluster = -1)
    
    ##put together the trigger sightings that don't overlap with any other sightings, with those that do with assigned clusters
    totpolyassign<-rbind(polyassign,not)
    totpolyassign$cluster<-as.numeric(totpolyassign$cluster)
    ##clustmin is for a totpolyassign df without any overlapping triggers
    clustmin = 0
    ##assigns consecutive cluster numbers to those sightings that don't overlap, but are triggering all on their own
    for (i in 1:nrow(totpolyassign))
      if (totpolyassign$cluster[i] == -1 & max(totpolyassign$cluster) > 0){
        totpolyassign$cluster[i]<-max(totpolyassign$cluster)+1
      } else if (totpolyassign$cluster[i] == -1 & max(totpolyassign$cluster) < 0 ){
        totpolyassign$cluster[i]<-clustmin+1
      } else {                
      }
    
    #########
    
    clustdf$PolyID<-as.numeric(clustdf$PolyID)
    
    clustdf<-full_join(clustdf,totpolyassign,by=c("PolyID"="upoly"))
    
    polycoorddf$id<-as.numeric(polycoorddf$id)
    
    corepoly<-left_join(polycoorddf, clustdf, by=c('id'='PolyID')) 
    corepoly<-corepoly %>% dplyr::select("long","lat","id","DateTime","GROUP_SIZE","corer","corer_m", "LONGITUDE","LATITUDE","cluster")
    
    #######
    
    ##gets to the core for the cluster
    polymaxmin<-corepoly %>%
      group_by(cluster) %>%
      summarise(maxlat = max(lat), minlat = min(lat), maxlon = max(long), minlon = min(long))%>%
      as.data.frame()
    
    ##spatialize the corners
    corebounds_nw<-polymaxmin
    coordinates(corebounds_nw)<-~minlon+maxlat
    proj4string(corebounds_nw)<-CRS.latlon
    
    corebounds_sw<-polymaxmin
    coordinates(corebounds_sw)<-~minlon+minlat
    proj4string(corebounds_sw)<-CRS.latlon
    
    corebounds_ne<-polymaxmin
    coordinates(corebounds_ne)<-~maxlon+maxlat
    proj4string(corebounds_ne)<-CRS.latlon
    
    corebounds_se<-polymaxmin
    coordinates(corebounds_se)<-~maxlon+minlat
    proj4string(corebounds_se)<-CRS.latlon
    
    ##
    buffnm<-15
    ##the below calculates the distance that needs to be added to each corner (the hypotenuse) to add the 15nm buffer
    dmabuffnm<-buffnm/cos(45*pi/180)
    
    nw<-315
    sw<-225
    ne<-45
    se<-135
    
    ##coords needs to be in degrees
    nw_p=destPoint(corebounds_nw, nw, dmabuffnm/m_nm, a=6378137, f=1/298.257222101)
    sw_p=destPoint(corebounds_sw, sw, dmabuffnm/m_nm, a=6378137, f=1/298.257222101)
    ne_p=destPoint(corebounds_ne, ne, dmabuffnm/m_nm, a=6378137, f=1/298.257222101)
    se_p=destPoint(corebounds_se, se, dmabuffnm/m_nm, a=6378137, f=1/298.257222101)
    #make buffer polygons
    #cbind the 15nm buffer point with the original cardinal direction point
    #rbind them together and then group by Clustmax
    nwdf<-as.data.frame(cbind(corebounds_nw,nw_p))
    swdf<-as.data.frame(cbind(corebounds_sw,sw_p))
    nedf<-as.data.frame(cbind(corebounds_ne,ne_p))
    sedf<-as.data.frame(cbind(corebounds_se,se_p))
    
    dma15<-rbind(nwdf,swdf,sedf,nedf,nwdf)
    
    dma15<-dma15 %>% 
      dplyr::select(cluster, lon, lat)

    IDclust<-split(dma15, dma15$cluster)

    IDclust<-lapply(IDclust, function(x) { x["cluster"] <- NULL; x })

    polyclust<-lapply(IDclust, Polygon)
    polyclust_<-lapply(seq_along(polyclust), function(i) Polygons(list(polyclust[[i]]), 
                                                                  ID = names(IDclust)[i]  ))
    polyclust_sp<-SpatialPolygons(polyclust_, proj4string = CRS.latlon)

    polyclust_sp_df<-SpatialPolygonsDataFrame(polyclust_sp, data.frame(id = unique(dma15$cluster),
                                                                       row.names = unique(dma15$cluster)))
    ##for KML
    dmabounds<-polyclust_sp %>% 
      fortify() %>% 
      dplyr::select("long","lat","id","order") %>%
      mutate(lonround = round(long, 2), latround = round(lat, 2), 
             "Lon (Degree Minutes)" = measurements::conv_unit(long, from = 'dec_deg', to = 'deg_dec_min'),
             "Lat (Degree Minutes)" = measurements::conv_unit(lat, from = 'dec_deg', to = 'deg_dec_min'))
    ##for database (excludes the 5th point to close the polygon)
    dmacoord<-dmabounds%>%
      dplyr::rename(ID = id, Vertex = order, "Lon (Decimal Degrees)" = lonround, "Lat (Decimal Degrees)" = latround)%>%
      dplyr::select(-long, -lat)%>%
      filter(Vertex != 5)
    
    sasdma<-leaflet(data = egsas) %>% 
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE) %>%
      addPolygons(data = smapresent.sp, weight = 2, color = "red") %>%
      addPolygons(data = activedma, weight = 2, color = "yellow") %>%
      addPolygons(data = polyclust_sp, weight = 2, color = "blue") %>%
      addPolygons(data = polycoorddf_sp, weight = 2, color = "black")%>%
      addCircleMarkers(lng = ~egsas$LONGITUDE, lat = ~egsas$LATITUDE, radius = 5, stroke = FALSE, fillOpacity = 0.5 , color = "black", popup = egsas$DateTime)
    
    #####
    ##dma name
    
    center<-gCentroid(polyclust_sp)
    
    dmaname<-data.frame(port = c("Bay of Fundy Canada","Portland ME","Portsmouth NH","Boston MA",
                                 "Providence RI","New York NY","Atlantic City NJ", "Virginia Beach VA",
                                 "Martha's Vineyard", "Nantucket", "Cape Cod"),
                        lon = c(-66.9317,-70.2500,-70.7333,-71.0833,-71.4000,-73.9667,-74.4167,-75.9595,-70.6167,-70.0833,-69.9778),
                        lat = c(44.7533,43.6667,43.0833,42.3500,41.8333,40.7833,39.3500,36.8469, 41.4000,41.2833,41.8830),
                        priority = c(1,1,1,1,1,1,1,1,2,2,2),
                        cardinal = NA)
    
    dmadist<-dmaname
    coordinates(dmadist)<-~lon+lat
    proj4string(dmadist)<-CRS.latlon
    
    disttocenter_nm<-(geosphere::distVincentyEllipsoid(dmadist, center, a=6378137, f=1/298.257222101)*m_nm)
    bearing<-bearingRhumb(dmadist, center)
    dmaname<-cbind(dmaname, disttocenter_nm, bearing)
    
    dmaname$cardinal[dmaname$bearing >= 337.5 | dmaname$bearing < 22.5] <- 'N'
    dmaname$cardinal[dmaname$bearing >= 22.5 & dmaname$bearing < 67.5] <- 'NE'
    dmaname$cardinal[dmaname$bearing >= 67.5 & dmaname$bearing < 112.5] <- 'E'
    dmaname$cardinal[dmaname$bearing >= 112.5 & dmaname$bearing < 157.5] <- 'SE'
    dmaname$cardinal[dmaname$bearing >= 157.5 & dmaname$bearing < 202.5] <- 'S'
    dmaname$cardinal[dmaname$bearing >= 202.5 & dmaname$bearing < 247.5] <- 'SW'
    dmaname$cardinal[dmaname$bearing >= 247.5 & dmaname$bearing < 292.5] <- 'W'
    dmaname$cardinal[dmaname$bearing >= 292.5 & dmaname$bearing < 337.5] <- 'NW'
    
    dmanamefil<-dmaname%>%
      group_by(priority)%>%
      slice(which.min(disttocenter_nm))
    
    dmatitle<-paste0(round(dmanamefil$disttocenter_nm,0),'nm ',dmanamefil$cardinal,' ',dmanamefil$port)
    
    output$dmaoptions<-renderUI({
      options<-c(dmatitle[1],dmatitle[2])
      radioButtons("options","DMA Name Options", choices = options, selected = options[1])})
    
    output$sasdma = renderLeaflet({print(sasdma)})
    
    output$dmacoord<-renderTable({dmacoord}, digits = 2)
    #SAS=SIGHTDATE,GROUPSIZE,LAT,LON,SPECIES_CERT,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION,OBSERVER_PEOPLE,OBSERVER_PLATFORM,ID,OBSERVER_ORG,OOD
    egsastab<-egsas %>% 
      dplyr::select(DateTime,GROUP_SIZE,LATITUDE,LONGITUDE,ID_RELIABILITY,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION_NEW)
    ################

    ##if DMA:
    CRS.gearth <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
    
    dmabounds_kml<-dmabounds
    coordinates(dmabounds_kml)<-~lonround+latround
    proj4string(dmabounds_kml)<-CRS.latlon
    dmabounds_kml.tr<- spTransform(dmabounds_kml, CRS.gearth)
    
    dma_date<-paste0("DMA_",year(egsas$DateTime[1]),"_",strftime(egsas$DateTime[1], "%m"),"_",strftime(egsas$DateTime[1], "%d"))
    
    enable("dmaup")
    enable("kml")
  } else {
    
    egsastab<-egsas %>% 
      dplyr::select(DateTime,GROUP_SIZE,LATITUDE,LONGITUDE,ID_RELIABILITY,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION_NEW)
    
    sasdma<-leaflet(data = egsas) %>% 
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE) %>%
      addPolygons(data = smapresent.sp, weight = 2, color = "red") %>%
      addPolygons(data = activedma, weight = 2, color = "yellow") %>%
      addCircleMarkers(lng = ~egsas$LONGITUDE, lat = ~egsas$LATITUDE, radius = 5, stroke = FALSE, fillOpacity = 0.5 , color = "black", popup = egsas$DateTime)
    
    output$sasdma = renderLeaflet({print(sasdma)})
    output$egsastab<-renderTable({egsastab},  striped = TRUE)
    
  }
  
  
  ##KML making
  ##still in progress
  output$kml <- downloadHandler(
    filename = function() {
      paste0(dma_date,".kml")
    },
    content = function(file) {
      writeOGR(dmabounds_kml.tr, file, layer= "dmabounds_kml.tr", driver="KML")
      kmlPolygons(obj=polyclust_sp_df, kmlfile=file, 
                  name="KML DMA", description="", col=NULL, visibility=1, lwd=2,
                  border="yellow", kmlname="", kmldescription="")
    }
  )
  
  observeEvent(input$sas,{
    
    ######################
    ## upload to Oracle ##
    ######################  
    print("SAS button pressed")
    cnxn <- odbcConnect(server, uid=sasuid, pwd=saspswd,believeNRows=FALSE)
    
    ################
    ##egsas upload##
    ################
    
    if(is.null(input$obspeeps)){
      output$error5<-renderText({"Decide who you are."})
    } else if(is.null(input$plane)){
      output$error5<-renderText({"Which plane were you in?"})
    } else {
      output$error5<-renderText({""})
    
    OBSERVER_PEOPLE = input$obspeeps 
    OBSERVER_PLATFORM = input$plane
    ID = 99999 #SAS has a procedure to make this number chronological with the table, but it cannot be NULL
    OBSERVER_ORG = 1
    OOD = 929 #929 means it came through this Shiny App
    
    egsastab<-cbind(egsastab,OBSERVER_PEOPLE,OBSERVER_PLATFORM,ID,OBSERVER_ORG,OOD)
    
    SIGHTDATE_sql<-paste0("to_timestamp('",egsas$SIGHTDATE[1],"', 'YYYY-MM-DD HH24:MI:SS')")
      
      #print(egsastab)
      for (i in 1:nrow(egsastab)){
        egvalues<-egsastab
        egvalues$DateTime<-paste0("'to_timestamp('",egvalues$DateTime,"', 'YYYY-MM-DD HH24:MI:SS')")
        egvalues <- paste0(sapply(egvalues[i,], function(x) paste0("", paste0(x, collapse = "', '"), "'")), collapse = ", '")
        egvalues <- gsub("'to_", "to_", egvalues)
        egvalues <- gsub("')'", "')", egvalues)
        sqlQuery(cnxn, paste0("INSERT INTO SAS(SIGHTDATE,GROUPSIZE,LAT,LON,SPECIES_CERT,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION,OBSERVER_PEOPLE,OBSERVER_PLATFORM,ID,OBSERVER_ORG,OOD)
                              VALUES(", egvalues,");"))
        disable("sas")
        
      }}
    

  }) #input sas
  
  observeEvent(input$dmaup,{
    
    enable("kml")
    print("dma button pressed")
    
    dmareportmap<-fitBounds(sasdma,min(dmacoord$`Lon (Decimal Degrees)`), min(dmacoord$`Lat (Decimal Degrees)`), max(dmacoord$`Lon (Decimal Degrees)`), max(dmacoord$`Lat (Decimal Degrees)`))
    htmlwidgets::saveWidget(dmareportmap, "temp.html", selfcontained = FALSE)
    webshot::webshot("temp.html", file = paste0(date1,"_dmamap.png"))#,cliprect = bounds)
    ##password and user name removed for sharing
    cnxn <- odbcConnect(server, uid=sasuid, pwd=saspswd,believeNRows=FALSE)
    
    ###################  
    ##dma info upload##
    ###################
    
    maxidsql<-"SELECT max(ID)
    FROM RIGHTWHALESIGHT.DMAINFO"
    
    maxid<-sqlQuery(cnxn,maxidsql)
    maxid<-as.integer(maxid)
    egsas$GROUP_SIZE<-as.numeric(egsas$GROUP_SIZE)

    ##made this ACTION_NEW == 4 instead of ACTION to work with the survey app
    totaldmaeg<-egsas %>%
      filter(ACTION_NEW == 4) %>%
      summarise(total = sum(GROUP_SIZE)) %>%
      as.data.frame()
    
    triggersig<-egsas%>%
      filter(ACTION_NEW == 4 & GROUP_SIZE == max(GROUP_SIZE))%>%
      group_by(GROUP_SIZE)%>%
      mutate(rank = rank(GROUP_SIZE, ties.method = "first"))%>%
      filter(rank == 1)%>%
      ungroup()
      
    trigger<-triggersig%>%
      dplyr::select(DateTime)

    exp<-lubridate::ymd_hms(trigger) 
    
    hour(exp)<-0
    minute(exp)<-0
    second(exp)<-01
    
    exp <- exp + days(16)
    ###
    
    #if (exists("ID", where = df)) {print("TRUE")}
    if (exists("OBSERVER_ORG", where = triggersig)){
      trigorg<-triggersig%>%
        dplyr::select(OBSERVER_ORG)
    }else{
        trigorg<-1
      }

    SIGHTDATE_sql<-paste0("to_timestamp('",trigger,"', 'YYYY-MM-DD HH24:MI:SS')")
    
    dmanameselect<-input$options
    
    dmainfoinsert<-data.frame(ID = maxid+1,
                              NAME = dmanameselect,
                              EXPDATE = paste0("to_timestamp('",exp,"', 'YYYY-MM-DD HH24:MI:SS')"),
                              TRIGGERDATE = SIGHTDATE_sql,
                              INITOREXT = 'i',
                              TRIGGERORG = trigorg,
                              STARTDATE = paste0("to_timestamp('",ymd_hms(Sys.time()),"', 'YYYY-MM-DD HH24:MI:SS')"),
                              TRIGGERGROUPSIZE = totaldmaeg$total[1] 
    )
    

    dmainfovalues <- paste0(apply(dmainfoinsert, 1, function(x) paste0("'", paste0(x, collapse = "', '"), "'")), collapse = ", ")
    dmainfovalues <- gsub("')'", "')", dmainfovalues)
    dmainfovalues <- gsub("'to_", "to_", dmainfovalues)
    dmainfovalues <- gsub("Martha's", "Martha''s", dmainfovalues)
    ##
    print(dmainfovalues)
    
    sqlQuery(cnxn, paste0("INSERT INTO DMAINFO(ID, NAME, EXPDATE, TRIGGERDATE, INITOREXT, TRIGGERORG, STARTDATE, TRIGGERGROUPSIZE)
                                    VALUES(", dmainfovalues,");"))
    ####################
    ##dma coord upload##
    ####################
    
    dmacoordinsert<-data.frame(ID = dmainfoinsert$ID,
                               VERTEX = dmacoord$Vertex,
                               LAT = dmacoord$"Lat (Decimal Degrees)",
                               LON = dmacoord$"Lon (Decimal Degrees)",
                               ROWNUMBER = 999999)
    
    for (i in 1:nrow(dmacoordinsert)){
      
      dmacoordvalues <- paste0(apply(dmacoordinsert[i,], 1, function(x) paste0("'", paste0(x, collapse = "', '"), "'")), collapse = ", ")
      sqlQuery(cnxn, paste0("INSERT INTO DMACOORDS(ID, VERTEX, LAT, LON, ROWNUMBER)
                                    VALUES(", dmacoordvalues,");"))   
    }
    print("dma end")
    disable("dmaup")
    enable("dmareport")

    output$dmareport<-downloadHandler(
      filename = paste0(day1,month1,year1,"_PotentialDMA_Report.pdf"),
      content = function(file) {
        
        if (loc == 'Network'){
          print(tempdir())
          tempReport<-file.path("./scripts/DMAReport.Rmd")
        } else if (loc == 'Local'){
          tempReport<-file.path(paste0(inputpath,"/DMAReport.Rmd"))
        } 
        
        print(tempReport)
        file.copy("DMAReport.Rmd", tempReport, overwrite = TRUE)
        params<-list(SIGHTDATE_sql = SIGHTDATE_sql, dmanameselect = dmanameselect, date1 = date1, egsastab = egsastab, dmacoord = dmacoord)
        
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
                          
        )})
    
  })

  