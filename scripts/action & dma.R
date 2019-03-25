###Action & DMA analysis


  ########
  ##ACTION
  ########

  egsas$GROUP_SIZE<-as.numeric(egsas$GROUP_SIZE)
  
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
  bDMA<-!is.na(sp::over(eg.tr, as(benigndma.tr, "SpatialPolygons")))
  eDMA<-!is.na(sp::over(eg.tr, as(extensiondma.tr, "SpatialPolygons")))

  sightID<-1:nrow(egsas)
  ######
  egsas<-cbind(egsas,inoutsma,Canada,SPM,bDMA,eDMA,sightID)
  
  ACTION_NEW<-NULL
  for (i in 1:nrow(egsas))
    if (egsas$eDMA[i] == TRUE){ #extension dma?
      egsas$ACTION_NEW[i] = 55   
    } else if (egsas$bDMA[i] == TRUE){ #benign dma
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
  
  #########################################
  ## animals potential for DMA extension ##
  #########################################
  
  if (55 %in% egsas$ACTION_NEW) {
    ##animals that are in a DMA up for extension
    actionext<-egsas %>% 
      filter(egsas$ACTION_NEW == 55) %>% 
      dplyr::select("DateTime", "LATITUDE", "LONGITUDE", "GROUP_SIZE","sightID")
    ##assess which DMA they are in
    for (i in names(extdma.tr)){
      indDMA<-sp::over(eg.tr, as(extdma.tr[[i]], "SpatialPolygons"))
      indDMA[indDMA == 1] <- i
      actionext_ind<-cbind(actionext,indDMA)
      ## list the dfs of 55 sightings and they comparison to each DMA
      if(exists("actionext_list") == FALSE){
        actionext_list<-list(actionext_ind)
      } else if (length(actionext_list) > 0){
        actionext_list<-list.append(actionext_list,actionext_ind)#rlist::list.append
      }
    }
  #smush the df list together to make one df
  fullextlist<-rbindlist(actionext_list)
  #filter out the sightings that aren't in any of these DMAs up for extension
  fullextlist<-fullextlist %>% filter(!is.na(indDMA))
  #which dmas have sightings in them
  uniqueext<-unique(fullextlist$indDMA)
  
  #test if the sightings in each DMA will trigger an extension (are there enough within the right distance to eachother)
  for (i in uniqueext){
    print(i)
    
    actionfil<-fullextlist%>%
      filter(indDMA == i)%>%
      dplyr::select(-indDMA)
    #########
    ##distance between points matrix -- compares right whale sightings positions to each other
    comboext<-reshape::expand.grid.df(actionfil,actionfil)
    names(comboext)[6:10]<-c("DateTime2","LATITUDE2","LONGITUDE2","GROUP_SIZE2","sightID2")
    comboext$GROUP_SIZE<-as.character(comboext$GROUP_SIZE)
    comboext$GROUP_SIZE<-as.numeric(comboext$GROUP_SIZE)
    ##calculates core area
    setDT(comboext)[ ,corer:=round(sqrt(GROUP_SIZE/(pi*egden)),2)] 
    ##calculates distance between points in nautical miles
    setDT(comboext)[ ,dist_nm:=geosphere::distVincentyEllipsoid(matrix(c(LONGITUDE,LATITUDE), ncol = 2),
                                                                matrix(c(LONGITUDE2, LATITUDE2), ncol =2), 
                                                                a=6378137, f=1/298.257222101)*m_nm]
    #print(comboext)
    #filters out points compared where core radius is less than the distance between them (meaning that the position combo will not have overlapping core radii) and
    #keeps the single sightings where group size would be enough to trigger a DMA (0 nm dist means it is compared to itself)
    #I don't remember why I named this dmacand -- maybe dma combo and... then some?
    dmacandext<-comboext %>%
      dplyr::filter((comboext$dist_nm != 0 & comboext$dist_nm <= comboext$corer) | (comboext$GROUP_SIZE > 2 & comboext$dist_nm == 0))
    #print(dmacandext)
    ##filters for distinct sightings that should be considered for DMA calculation
    dmaextsightID<-data.frame(sightID = c(dmacandext$sightID,dmacandext$sightID2)) %>%
      distinct()
    #print(dmaextsightID)
    
    exttot<-left_join(dmaextsightID,egsas, by = "sightID")%>%
      dplyr::select(sightID,DateTime,LATITUDE,LONGITUDE,GROUP_SIZE)%>%
      distinct()%>%
      arrange(sightID)%>%
      summarise(total = sum(GROUP_SIZE))
    
    print(exttot)
    ##this will pass into the next for loop
    x <- i
    #blank df for the dmas to enter
    extdf<-data.frame(extDMAs = NA,
                      total = NA)

    for (i in 1:nrow(egsas))
      if (egsas$sightID[i] %in% dmaextsightID$sightID) {
        egsas$ACTION_NEW[i] = 5
        df<-data.frame(extDMAs = x,
                       total = exttot)
        extdf<-rbind(extdf,df)
      } else if (egsas$ACTION_NEW[i] == 55){
        egsas$ACTION_NEW[i] = 1
      } else {
        egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
      }
    
    print(df)
    print("list of DMAs needing extension")
    extdf<-extdf%>%
      filter(!is.na(extDMAs))%>%
      distinct()
    print(extdf)
    
    extdf$extDMAs<-as.integer(extdf$extDMAs)
    extdfbound<-left_join(extdf, actdmadf, by = c("extDMAs" = "ID"))
    print(extdfbound)
  }

  } #end 55 in action_new

  ###################################
  ## animals potential for new DMA ##
  ###################################
  
  ##only taking ACTION_NEW = na
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
  #print(combo)
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
  ##############
  
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
  
  ######
  ##Create DMA
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
    corepoly<-left_join(polycoorddf, clustdf, by=c('id'='PolyID'))%>%
              dplyr::select("long","lat","id","DateTime","GROUP_SIZE","corer","corer_m", "LONGITUDE","LATITUDE","cluster")
    
    #################
    ## for DMA insert
    clustid<-clustdf%>%
      dplyr::select(PolyID,cluster)
    sigs<-clustdf%>%
      dplyr::select(DateTime, GROUP_SIZE, sightID)
    clustersigs<-left_join(sigs,clustid, by = c("sightID" = "PolyID"))

    clustersigs$DateTime<-ymd_hms(clustersigs$DateTime)
    trigsize<-clustersigs %>% 
      group_by(cluster)%>%
      summarise(TRIGGER_GROUPSIZE = n(), TRIGGERDATE = min(DateTime))
    #################
    
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

    polyclust_<-lapply(seq_along(polyclust), function(i) Polygons(list(polyclust[[i]]), ID = names(IDclust)[i]))
    
    polyclust_sp<-SpatialPolygons(polyclust_, proj4string = CRS.latlon)

    polyclust_sp_df<-SpatialPolygonsDataFrame(polyclust_sp, data.frame(id = unique(dma15$cluster), row.names = unique(dma15$cluster)))
    
    ##############
    ##for KML
    dmabounds<-polyclust_sp %>% 
      fortify() %>% 
      dplyr::select("long","lat","id","order") %>%
      mutate(latround = round(lat, 2), lonround = round(long, 2), 
             "Lat (Degree Minutes)" = paste( trunc(lat), formatC(round((lat %% 1)*60,0), width = 2,flag = 0), "N", sep = " "),
             "Lon (Degree Minutes)" = paste( formatC(abs(trunc(long)), width = 3,flag = 0), formatC(round((abs(long) %% 1)*60,0), width = 2,flag = 0), "W", sep = " "))
    ##for database (excludes the 5th point to close the polygon)
    dmacoord<-dmabounds%>%
      dplyr::rename(ID = id, Vertex = order, "Lat (Decimal Degrees)" = latround, "Lon (Decimal Degrees)" = lonround)%>%
      dplyr::select(-long, -lat)%>%
      filter(Vertex != 5)
    
    sasdma<-leaflet(data = egsas) %>% 
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE) %>%
      addPolygons(data = smapresent.sp, weight = 2, color = "red") %>%
      addPolygons(data = benigndma, weight = 2, color = "yellow") %>%
      addPolygons(data = extensiondma, weight = 2, color = "orange") %>%
      addPolygons(data = polyclust_sp, weight = 2, color = "blue") %>%
      addPolygons(data = polycoorddf_sp, weight = 2, color = "black")%>%
      addCircleMarkers(lng = ~egsas$LONGITUDE, lat = ~egsas$LATITUDE, radius = 5, stroke = FALSE, fillOpacity = 0.5 , color = "black", popup = egsas$DateTime)
    
    #############
    ## dma name #
    #############
    
    ##port/landmark reference
    dmaname<-data.frame(port = c("Bay of Fundy Canada","Portland ME","Portsmouth NH","Boston MA",
                                 "Providence RI","New York NY","Atlantic City NJ", "Virginia Beach VA",
                                 "Martha's Vineyard MA", "Nantucket MA", "Cape Cod MA", "Cape Cod Bay"),
                        lon = c(-66.9317,-70.2500,-70.7333,-71.0833,-71.4000,-73.9667,-74.4167,-75.9595,-70.6167,-70.0833,-69.9778,-70.27),
                        lat = c(44.7533,43.6667,43.0833,42.3500,41.8333,40.7833,39.3500,36.8469, 41.4000,41.2833,41.8830,41.88),
                        cardinal = NA)
    
    dmadist<-dmaname
    coordinates(dmadist)<-~lon+lat
    proj4string(dmadist)<-CRS.latlon
    ##########
    names(polyclust_)<-names(IDclust)
    
    for (i in names(polyclust_)){

      x<-list(polyclust_[[i]])

      x_sp<-SpatialPolygons(x, proj4string = CRS.latlon)
      center<-rgeos::gCentroid(x_sp)
    
      dmaname<-dmaname%>%
        mutate(ID = i,
             disttocenter_nm = (geosphere::distVincentyEllipsoid(dmadist, center, a=6378137, f=1/298.257222101)*m_nm),
             bearing = bearingRhumb(dmadist, center))%>%
        dplyr::select(ID, everything())
    
    dmaname$cardinal[dmaname$bearing >= 337.5 | dmaname$bearing < 22.5] <- 'N'
    dmaname$cardinal[dmaname$bearing >=  22.5 & dmaname$bearing < 67.5] <- 'NE'
    dmaname$cardinal[dmaname$bearing >=  67.5 & dmaname$bearing < 112.5] <- 'E'
    dmaname$cardinal[dmaname$bearing >= 112.5 & dmaname$bearing < 157.5] <- 'SE'
    dmaname$cardinal[dmaname$bearing >= 157.5 & dmaname$bearing < 202.5] <- 'S'
    dmaname$cardinal[dmaname$bearing >= 202.5 & dmaname$bearing < 247.5] <- 'SW'
    dmaname$cardinal[dmaname$bearing >= 247.5 & dmaname$bearing < 292.5] <- 'W'
    dmaname$cardinal[dmaname$bearing >= 292.5 & dmaname$bearing < 337.5] <- 'NW'
    
    dmanametop<-dmaname%>%
      top_n(-1,disttocenter_nm)%>%
      arrange(disttocenter_nm)
    
    #rbind the list of options
    if(exists("dmanamedf") == FALSE){
      dmanamedf<-list(dmanametop)
    } else if (length(dmanamedf) > 0){
      dmanamedf<-list.append(dmanamedf,dmanametop)#rlist::list.append
    }
    
    }
    
    ##combine list of multiple dma names
    dmanamedf<-rbindlist(dmanamedf)
    landmark_ls<-as.list(unique(dmanamedf$port))
    landmark<-do.call("paste", c(landmark_ls, sep = ", "))
    landmark<-sub(",([^,]*)$", " and\\1", landmark)  
    ## paste together the title
    dmanamedf<-dmanamedf%>%
      mutate(NAME = paste0(round(dmanamedf$disttocenter_nm,0),'nm ',dmanamedf$cardinal,' ',dmanamedf$port),
             INITOREXT = 'i')%>%
      dplyr::select(ID,NAME,INITOREXT)
    ## rename so that CCB does not have bearing or distance
    dmanamedf$NAME[grepl('Cape Cod Bay',dmanamedf$NAME)] <- 'Cape Cod Bay'
    
    ##join on columns with same data type
    trigsize$cluster<-as.character(trigsize$cluster)
    dmanamedf<-left_join(dmanamedf,trigsize, by = c("ID" = "cluster"))
    print(dmanamedf)
    ##############
    ##join dmanamedf with ext dmas
    ##ext IDs should be 1 + max(ID) of new dmas
    
    
    dmanameout<-dmanamedf%>%
      dplyr::select(-INITOREXT)
    dmanameout$TRIGGERDATE<-as.character(dmanameout$TRIGGERDATE)
    print(dmanameout)
    output$dmanameout<-renderTable({dmanameout})
    
    output$dmacoord<-renderTable({dmacoord})
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
  } else { ##4 in egsas$action_new
    
    egsastab<-egsas %>% 
      dplyr::select(DateTime,GROUP_SIZE,LATITUDE,LONGITUDE,ID_RELIABILITY,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION_NEW)
    
    sasdma<-leaflet(data = egsas) %>% 
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE) %>%
      addPolygons(data = smapresent.sp, weight = 2, color = "red") %>%
      addPolygons(data = benigndma, weight = 2, color = "yellow") %>%
      addPolygons(data = extensiondma, weight = 2, color = "orange") %>%
      addCircleMarkers(lng = ~egsas$LONGITUDE, lat = ~egsas$LATITUDE, radius = 5, stroke = FALSE, fillOpacity = 0.5 , color = "black", popup = egsas$DateTime)
    
  }
  
  egsastab$ACTION_NEW<-sprintf("%.0f",round(egsastab$ACTION_NEW, digits = 0))
  egsastab$GROUP_SIZE<-sprintf("%.0f",round(egsastab$GROUP_SIZE, digits = 0))
  
  output$sasdma = renderLeaflet({print(sasdma)})
  output$egsastab<-renderTable({egsastab},  striped = TRUE)
  
  ##KML making
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
    
    ###################  
    ##dma info upload##
    ###################
    
    maxidsql<-"SELECT max(ID)
    FROM RIGHTWHALESIGHT.DMAINFO"
    
    maxid<-sqlQuery(cnxn,maxidsql)
    maxid<-as.integer(maxid)

    ##made this ACTION_NEW == 4 instead of ACTION to work with the survey app
    
    # ##trigger group size
    # totaldmaeg<-egsas %>%
    #   filter(ACTION_NEW == 4) %>%
    #   summarise(total = sum(GROUP_SIZE)) %>%
    #   as.data.frame()
    # print(totaldmaeg$total[1])
    # print(max(totaldmaeg$total))
    # triggersize<-max(totaldmaeg$total)
    # 
    #trigger date -- we really don't need this since we know what date it is, but whatever
    triggersig<-egsas%>%
      filter(ACTION_NEW == 4 & GROUP_SIZE == max(GROUP_SIZE))%>%
      group_by(GROUP_SIZE)%>%
      mutate(rank = rank(GROUP_SIZE, ties.method = "first"))%>%
      filter(rank == 1)%>%
      ungroup()
      
    trigger<-triggersig%>%
      dplyr::select(DateTime)
    trig<-lubridate::ymd_hms(trigger)
    trig<-force_tz(trig, tzone = "America/New_York")

    triggerdateletter<-format(date(trig), "%B %d, %Y")
    
    ##expiration date
    exp<-trig
    hour(exp)<-0
    minute(exp)<-0
    second(exp)<-01
    exp <- exp + days(16)

    expletter<-format(exp, "%H:%M:%S %Z %B %d, %Y")
    ###
    
    #choose group that saw the most to be the trigger org
    if (exists("OBSERVER_ORG", where = triggersig)){ 
      trigorg<-triggersig$OBSERVER_ORG
      print(trigorg)
    }else{
        trigorg<-1
      }

    
    dmanamedf$ID<-as.numeric(dmanamedf$ID) #a number to add to
    
    #### this is where new dmas and extensions need to be together in dmanamedf
    dmainfo<-dmanamedf%>%
      mutate(OLDID = ID,
             ID = ID + maxid,
             EXPDATE = paste0("to_timestamp('",exp,"', 'YYYY-MM-DD HH24:MI:SS')"),
             TRIGGERDATE = paste0("to_timestamp('",TRIGGERDATE,"', 'YYYY-MM-DD HH24:MI:SS')"),
             TRIGGERORG = trigorg,
             STARTDATE = paste0("to_timestamp('",ymd_hms(Sys.time()),"', 'YYYY-MM-DD HH24:MI:SS')"))%>%
      dplyr::select(OLDID,ID,NAME,EXPDATE,TRIGGERDATE,INITOREXT,TRIGGERORG,STARTDATE,TRIGGER_GROUPSIZE)
    print(dmainfo)
    
    dmainfoinsert<-dmainfo%>%
      dplyr::select(-OLDID)
    
    print(dmainfoinsert)
    
    newdmalist<-as.list(dmainfoinsert$NAME)
    dmanameselect<-do.call("paste", c(newdmalist, sep = ", "))
    dmanameselect<-sub(",([^,]*)$", " and\\1", dmanameselect)  
    
    for (i in 1:nrow(dmainfoinsert)){

    dmainfovalues <- paste0(apply(dmainfoinsert[i,], 1, function(x) paste0("'", paste0(x, collapse = "', '"), "'")), collapse = ", ")
    dmainfovalues <- gsub("')'", "')", dmainfovalues)
    dmainfovalues <- gsub("'to_", "to_", dmainfovalues)
    dmainfovalues <- gsub("Martha's", "Martha''s", dmainfovalues)
    ##
    print(dmainfovalues)
    
    sqlQuery(cnxn, paste0("INSERT INTO DMAINFO(ID, NAME, EXPDATE, TRIGGERDATE, INITOREXT, TRIGGERORG, STARTDATE, TRIGGERGROUPSIZE)
                                    VALUES(", dmainfovalues,");"))
    }
    
    ####################
    ##dma coord upload##
    ####################
    dmacoord$ID<-as.numeric(dmacoord$ID)
    dmacoordinsert<-left_join(dmainfo,dmacoord, by = c("OLDID" = "ID"))%>%
      dplyr::select(ID,Vertex,`Lat (Decimal Degrees)`,`Lon (Decimal Degrees)`)%>%
      dplyr::rename("VERTEX" = "Vertex","LAT" = "Lat (Decimal Degrees)","LON" = "Lon (Decimal Degrees)")%>%
      mutate(ROWNUMBER = 999999)
      
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
          tempReport<-file.path("./scripts/DMAReport.Rmd")
        } else if (loc == 'Local'){
          tempReport<-file.path(paste0(inputpath,"/DMAReport.Rmd"))
        } 
        
        print(tempReport)
        file.copy("DMAReport.Rmd", tempReport, overwrite = TRUE)
        params<-list(dmanameselect = dmanameselect, date1 = date1, egsastab = egsastab, dmanamedf = dmanamedf, dmacoord = dmacoord)
        
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )})
    
    #######
    ##number to word function
    numbers2words <- function(x){
      ## Function by John Fox found here: 
      ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
      ## Tweaks by AJH to add commas and "and"
      helper <- function(x){
        
        digits <- rev(strsplit(as.character(x), "")[[1]])
        nDigits <- length(digits)
        if (nDigits == 1) as.vector(ones[digits])
        else if (nDigits == 2)
          if (x <= 19) as.vector(teens[digits[1]])
        else trim(paste(tens[digits[2]],
                        Recall(as.numeric(digits[1]))))
        else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and", 
                                          Recall(makeNumber(digits[2:1]))))
        else {
          nSuffix <- ((nDigits + 2) %/% 3) - 1
          if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
          trim(paste(Recall(makeNumber(digits[
            nDigits:(3*nSuffix + 1)])),
            suffixes[nSuffix],"," ,
            Recall(makeNumber(digits[(3*nSuffix):1]))))
        }
      }
      trim <- function(text){
        #Tidy leading/trailing whitespace, space before comma
        text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
        #Clear any trailing " and"
        text=gsub(" and$","",text)
        text=gsub("*y ","y-",text) ## I added this part - for latex - lmc
        #Clear any trailing comma
        gsub("\ *,$","",text)
      }  
      makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
      #Disable scientific notation
      opts <- options(scipen=100) 
      on.exit(options(opts)) 
      ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
                "eight", "nine") 
      names(ones) <- 0:9 
      teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
                 "sixteen", " seventeen", "eighteen", "nineteen")
      names(teens) <- 0:9 
      tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
                "ninety") 
      names(tens) <- 2:9 
      x <- round(x)
      suffixes <- c("thousand", "million", "billion", "trillion")     
      if (length(x) > 1) return(trim(sapply(x, helper)))
      helper(x)
    }
    ##########
    direction<-function(x){
      dplyr::case_when(
        grepl(' N ',x) ~ 'north of',
        grepl(' NE ',x) ~ 'northeast of',
        grepl(' E ',x) ~ 'east of',
        grepl(' SE ',x) ~ 'southeast of',
        grepl(' S ',x) ~ 'south of',
        grepl(' SW ',x) ~ 'southwest of',
        grepl(' W ',x) ~ 'west of',
        grepl(' NW ',x) ~ 'northwest of',
        grepl('Cape Cod Bay',x) ~ 'in'
      )
    }
    ############
    
    letterdate<-format(Sys.Date(), '%B %d, %Y')
    triggerword<-numbers2words(triggersize)
    numberword<-dmanamedf%>%
      mutate(triggerword = numbers2words(dmanamedf$TRIGGER_GROUPSIZE))
    triggerword<-as.list(numberword$triggerword)
    triggerword<-do.call("paste", c(triggerword, sep = ", "))
    triggerword<-sub(",([^,]*)$", " and\\1", triggerword)  
    
    letterdirect<-direction(dmanameselect)
    
    observer<-input$triggrp
    #choose group that saw the most to be the trigger org
    if (exists("observer")){ 
      print(observer)
    }else{
      observer<-"NOAA North Atlantic Right Whale Sighting Survey"
    }
    
    letterbounds<-left_join(dmanamedf,dmacoord, by = "ID")
    print(letterbounds)
    title1<-letterbounds%>%filter(ID == 1)%>%dplyr::select(NAME)%>%distinct()
    title1<-title1$NAME[1]
    NLat1<-letterbounds%>%filter(ID == 1 & `Lat (Decimal Degrees)` == max(`Lat (Decimal Degrees)`))%>%dplyr::select(`Lat (Degree Minutes)`)%>%distinct()
    NLat1<-NLat1$`Lat (Degree Minutes)`[1]
    SLat1<-letterbounds%>%filter(ID == 1 & `Lat (Decimal Degrees)` == min(`Lat (Decimal Degrees)`))%>%dplyr::select(`Lat (Degree Minutes)`)%>%distinct()
    SLat1<-SLat1$`Lat (Degree Minutes)`[1]
    WLon1<-letterbounds%>%filter(ID == 1 & `Lon (Decimal Degrees)` == max(`Lon (Decimal Degrees)`))%>%dplyr::select(`Lon (Degree Minutes)`)%>%distinct()
    WLon1<-WLon1$`Lon (Degree Minutes)`[1]
    ELon1<-letterbounds%>%filter(ID == 1 & `Lon (Decimal Degrees)` == min(`Lon (Decimal Degrees)`))%>%dplyr::select(`Lon (Degree Minutes)`)%>%distinct()
    ELon1<-ELon1$`Lon (Degree Minutes)`[1]

    print(paste(title1,NLat1,SLat1,WLon1,ELon1))

    ###keep adding these bounds
    
    output$dmaletter <- downloadHandler(
      
      filename = function() {
        paste0("DMA ",year1,month2,day1," ",dmanameselect,".pdf")},
      
      content = function(file) {
        
        if (loc == 'Network'){
          tempReport<-file.path("./scripts/DMALetter.Rmd")
        } else if (loc == 'Local'){
          tempReport<-file.path(paste0(inputpath,"/DMALetter.Rmd"))
        }        
        
        file.copy("DMALetter.Rmd", tempReport, overwrite = TRUE)
        params<-list(letterdate = letterdate, date1 = date1, triggerdateletter = triggerdateletter, triggerword = triggerword, letterdirect = letterdirect, 
                     landmark = landmark, observer = observer, title1 = title1, NLat1 = NLat1, SLat1 = SLat1, WLon1 = WLon1, ELon1 = ELon1, expletter = expletter)
        
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
      )})
    
  })

  