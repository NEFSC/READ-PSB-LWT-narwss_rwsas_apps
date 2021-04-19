###Action & DMA analysis


##############
## FUNCTION ##
##############

##clustering overlapping sightings

clustdf_fun<-function(x,y){
  
  ##############
  if (length(names(x)) > 1){
    ##Overlap of whale density core area analysis
    polycomb<-data.frame(poly1=NA,poly2=NA,overlap=NA)
    ##creates a list of 2 combinations to compare
    #print(names(x))
    combos<-combn(names(x),2)
    ##compares the list
    for(i in seq_along(combos[1,])){
      poly1 <- combos[1,i]
      poly2 <- combos[2,i]
      #if they don't overlap, the result of the below "if statement" is NULL
      if(!is.null(gIntersection(y[poly1], y[poly2], byid = TRUE))){
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
  } else if (length(names(x)) == 1) {
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
  
  ##these sightings are NOT triggering on their own (or are trigger by one sighting of 3+ without overlapping sightings) are assigned a cluster of -1
  not<-poly12%>%
    filter((!poly12$upoly %in% polyassign$upoly) | (!poly12$upoly %in% polyassign$upoly))%>%
    distinct()%>%
    mutate(cluster = -1)
  
  ##put together the trigger sightings that don't overlap with any other sightings, with those that do with assigned clusters
  totpolyassign<-rbind(polyassign,not)
  totpolyassign$cluster<-as.numeric(totpolyassign$cluster)
  print(totpolyassign)
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
  totpolyassign
}

##################
## LEAFLET BASE ##
##################

sasdma<-leaflet(data = egsas, options = leafletOptions(zoomControl = FALSE)) %>% 
  addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE) %>%
  addPolygons(data = smapresent.sp, weight = 2, color = "red")%>%
  addPolylines(data = NEUS_shiplane.sp, weight = 1, color = "green", fill = F)
  
########
##ACTION
########

egsas$GROUP_SIZE<-as.numeric(egsas$GROUP_SIZE)
##copy for spatializing
eg<-egsas
#print(eg)
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
SPM<-!is.na(sp::over(eg.tr, as(spm.tr, "SpatialPolygons")))
sightID<-1:nrow(egsas)
egsas<-cbind(egsas,inoutsma,Canada,SPM,sightID)
egsas<-egsas%>%mutate(ACTION_NEW = NA)
#print(egsas)

if (isolate(criteria$loc) == 'Network'){
bDMA<-!is.na(sp::over(eg.tr, as(benigndma.tr, "SpatialPolygons")))
eDMA<-!is.na(sp::over(eg.tr, as(extensiondma.tr, "SpatialPolygons")))

bAPZ<-!is.na(sp::over(eg.tr, as(benignapz.tr, "SpatialPolygons")))
eAPZ<-!is.na(sp::over(eg.tr, as(extensionapz.tr, "SpatialPolygons")))

egsas<-cbind(egsas,bDMA,eDMA,bAPZ,eAPZ)
}

#print(egsas)

######

for (i in 1:nrow(egsas))
  if (egsas$inoutsma[i] == TRUE){
    egsas$ACTION_NEW[i] = 2
  } else if (egsas$Canada[i] == TRUE){
    egsas$ACTION_NEW[i] = 6
  } else if (egsas$SPM[i] == TRUE){
    egsas$ACTION_NEW[i] = 6
    output$error3<-renderText({"Soc re bleu! One of these right whales was in France!"})  
  } else if (isolate(criteria$loc) == 'Network'){
    #print("network if")
    if (egsas$eDMA[i] == TRUE & (isolate(criteria$DMAapp) == "vissig" | isolate(criteria$DMAapp) == "rwsurv")){ #visual detections in an extension eligible DMA 
      egsas$ACTION_NEW[i] = 55
    } else if (egsas$eAPZ[i] == TRUE & isolate(criteria$DMAapp) == "acoudet") { #acoustic detections in an extension eligible APZ 
      egsas$ACTION_NEW[i] = 55
    } else if (egsas$eDMA[i] == TRUE & isolate(criteria$DMAapp) == "acoudet") { #acoustic detections in an extension eligible DMA aka cannot extend 
      egsas$ACTION_NEW[i] = 2  
    } else if (egsas$eAPZ[i] == TRUE & (isolate(criteria$DMAapp) == "vissag" | isolate(criteria$DMAapp) == "rwsurv")) { #visual detections in an extension eligible APZ aka cannot extend 
      egsas$ACTION_NEW[i] = 2    
    } else if (egsas$bDMA[i] == TRUE | egsas$bAPZ[i] == TRUE) { #benign dma
      egsas$ACTION_NEW[i] = 2   
    }
  } else if (egsas$inoutsma[i] == FALSE){
    egsas$ACTION_NEW[i] = NA
  }
print("77")
print(egsas)
##############
## dma eval ##
##############

#spatial analysis
## 1 nautical mile is 1852 meters
m_nm<-1/1852
## eg density is 4 whales/100nm^2 (50 CFR Part 224)
egden<-0.0416

## these will get overwritten if there are DMAs to create or extend
alldmas<-NULL
dmacoord<-NULL
dmanameout<-NULL

#########################################
## animals potential for DMA extension ##
#########################################

if (55 %in% egsas$ACTION_NEW) {
  print("beg 55")
  
  if (isolate(criteria$DMAapp) == "acoudet"){
    prot.tr<-extapz.tr
  } else {
    prot.tr<-extdma.tr  
  }
  
  ##assess which DMA they are in
  for (i in names(prot.tr)){
    if(exists("actionext_indlist") == FALSE){
      indDMA<-sp::over(eg.tr, as(prot.tr[[i]], "SpatialPolygons"))
      indDMA[indDMA == 1] <- i
      actionext<-cbind(egsas,indDMA)
      actionext_sig<-actionext %>% 
        filter(ACTION_NEW == 55) %>% 
        dplyr::select("DateTime", "LATITUDE", "LONGITUDE", "GROUP_SIZE","sightID","indDMA")
      actionext_indlist<-list(actionext_sig)
    } else {
      indDMA<-sp::over(eg.tr, as(prot.tr[[i]], "SpatialPolygons"))
      indDMA[indDMA == 1] <- i
      actionext<-cbind(egsas,indDMA)
      actionext_sig<-actionext %>% 
        filter(ACTION_NEW == 55) %>% 
        dplyr::select("DateTime", "LATITUDE", "LONGITUDE", "GROUP_SIZE","sightID","indDMA")
      actionext_indlist<-list.append(actionext_indlist,actionext_sig)
    }
  }
  
  print(actionext_indlist)
  
  ##cycle through all animals for each ext dma
fullextlist<-lapply(actionext_indlist, function(x){
  print("enter the ext list")
  actionext_ind<-x
  ##animals that are in a DMA up for extension
  ##factor to numeric -- maybe not be necessary
  actionext_ind$indDMA<-as.character(actionext_ind$indDMA)
  actionext_ind$indDMA<-as.numeric(actionext_ind$indDMA)
  #filter out the sightings that aren't in any of these DMAs up for extension
  actionext_ind %>% filter(!is.na(indDMA))
})
#print(fullextlist)

uniqueextlist<-lapply(fullextlist,function(x){
  x%>%
    distinct(indDMA)
})
# print(uniqueextlist)
uniqueext<-bind_rows(uniqueextlist)
#print(uniqueext)
DMAlist<-as.list(uniqueext$indDMA)
print(DMAlist)

  #test if the sightings in each DMA will trigger an extension (are there enough within the right distance to eachother)
comboext<-lapply(fullextlist,function(x){
  #print(x)
  actionfil<-x#%>%
  #   dplyr::select(-indDMA)
  #print(actionfil)
    ##distance between points matrix -- compares right whale sightings positions to each other
    comboext<-reshape::expand.grid.df(actionfil,actionfil)
    names(comboext)[7:12]<-c("DateTime2","LATITUDE2","LONGITUDE2","GROUP_SIZE2","sightID2","indDMA2")
    comboext$GROUP_SIZE<-as.character(comboext$GROUP_SIZE)
    comboext$GROUP_SIZE<-as.numeric(comboext$GROUP_SIZE)
    ##calculates core area
    
    if (isolate(criteria$DMAapp) == "acoudet"){ 
      setDT(comboext)[ ,corer:=20]  
    } else {
      setDT(comboext)[ ,corer:=round(sqrt(GROUP_SIZE/(pi*egden)),2)]
    }  
    
    ##calculates distance between points in nautical miles
    setDT(comboext)[ ,dist_nm:=geosphere::distVincentyEllipsoid(matrix(c(LONGITUDE,LATITUDE), ncol = 2),
                                                                matrix(c(LONGITUDE2, LATITUDE2), ncol =2), 
                                                                a=6378137, f=1/298.257222101)*m_nm]
    comboext})
print("comboext")
print(comboext)
names(comboext)<-DMAlist
#print(comboext)
    #filters out points compared where core radius is less than the distance between them (meaning that the position combo will not have overlapping core radii) and
    #keeps the single sightings where group size would be enough to trigger a DMA (0 nm dist means it is compared to itself)
    #I don't remember why I named this dmacand -- maybe dma combo and... then some?
    # applied over the list of sightings that fall within each DMA

extdf_list<-lapply(comboext, function(x) {
    #print(x)
    dmacandext<-x %>%
      dplyr::filter((x$dist_nm != 0 & x$dist_nm <= x$corer) | (x$GROUP_SIZE > 2 & x$dist_nm == 0))
    #print(dmacandext)
    DMAid<-unique(x$indDMA)
    #print(DMAid)
    ##filters for distinct sightings that should be considered for DMA calculation
    dmaextsightID<-data.frame(sightID = c(dmacandext$sightID,dmacandext$sightID2)) %>%
      distinct()
    print("139")
    #print(dmaextsightID)
    
    #blank df for the dmas to enter
    extdf_list<-data.frame(extDMAs = NA,
                      TRIGGER_GROUPSIZE = NA,
                      TRIGGERDATE = NA,
                      TRIGGERORG = NA)
    
    if (nrow(dmaextsightID) > 0){
    
    ## this section determines observer_organization for extended dma input into the dmainfo table in Oracle. 
    ## For cases where sightings from multiple organizations are considered together, this code picks the organization that has the most sightings that contribute.
    if(isolate(criteria$triggrptrue) == TRUE & isolate(criteria$DMAapp) == "vissig"){
      
      obs_org<-left_join(dmaextsightID,egsas, by = "sightID")%>%
        group_by(GROUP_SIZE)%>%
        mutate(rank = rank(GROUP_SIZE, ties.method = "first"))%>%
        filter(rank == 1)%>%
        ungroup()
      #print(obs_org)
      
      obs_org<-obs_org%>%
        distinct(OBSERVER_ORG)
      #print(obs_org)
      
      exttot<-left_join(dmaextsightID,egsas, by = "sightID")%>%
        dplyr::select(sightID,DateTime,LATITUDE,LONGITUDE,GROUP_SIZE,OBSERVER_ORG)%>%
        distinct()%>%
        arrange(sightID)%>%
        summarise(total = sum(GROUP_SIZE), TRIGGERDATE = min(DateTime), OBSERVER_ORG = obs_org$OBSERVER_ORG)
      
      } else if (isolate(criteria$triggrptrue) == TRUE & isolate(criteria$DMAapp) == "acoudet") {
      
        exttot<-left_join(dmaextsightID,egsas, by = "sightID")%>%
          dplyr::select(sightID,DateTime,LATITUDE,LONGITUDE,GROUP_SIZE)%>%
          distinct()%>%
          arrange(sightID)%>%
          summarise(total = sum(GROUP_SIZE), TRIGGERDATE = min(DateTime), OBSERVER_ORG = 82) #82 is Robots4Whales
        
      } else {
      
      exttot<-left_join(dmaextsightID,egsas, by = "sightID")%>%
        dplyr::select(sightID,DateTime,LATITUDE,LONGITUDE,GROUP_SIZE)%>%
        distinct()%>%
        arrange(sightID)%>%
        summarise(total = sum(GROUP_SIZE), TRIGGERDATE = min(DateTime), OBSERVER_ORG = 1) #1 is NEFSC
    }
      print("extension total")
      print(exttot)
    }
    
    ##DMAid will pass into the next for loop
    ##the below doesn't mean anything going forward for egsas
    ##this is all part of the lapply to make the extdf_list
    print("#1")
    print(dmaextsightID)
    
    for (i in 1:nrow(egsas))
      if (egsas$sightID[i] %in% dmaextsightID$sightID) {
        #print(i)
        #print("1")
        egsas$ACTION_NEW[i] = 55
        #print(egsas)
        df<-data.frame(extDMAs = DMAid,
                       TRIGGER_GROUPSIZE = exttot$total,
                       TRIGGERDATE = exttot$TRIGGERDATE,
                       TRIGGERORG = exttot$OBSERVER_ORG)
        print("356")
        #print(df)
        extdf_list<-rbind(extdf_list,df)

      } else {
        #print(i)
        #print("3")
        egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
      }

    extdf_list
    })

for (i in 1:nrow(egsas))
  if (is.na(egsas$ACTION_NEW[i])){ #is.na = DMA 4 animals
    print(i)
    print("4")
    egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
  } else if (exists("dmaextsightID") && egsas$sightID[i] %in% dmaextsightID$sightID) {
    print(i)
    print("1")
    egsas$ACTION_NEW[i] = 55
    print(head(egsas))
    df<-data.frame(extDMAs = DMAid,
                   TRIGGER_GROUPSIZE = exttot$total,
                   TRIGGERDATE = exttot$TRIGGERDATE,
                   TRIGGERORG = exttot$OBSERVER_ORG)
    print(df)
    extdf_list<-rbind(extdf_list,df)
    #print(extdf)
  } else if (egsas$ACTION_NEW[i] == 55){
    print(i)
    print("2")
    egsas$ACTION_NEW[i] = 2 #still in protected zone, but not trigger anything
    print(egsas)
  } else {
    print(i)
    print("3")
    egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
  }
print("here")
  print(extdf_list)
  extdf<-bind_rows(extdf_list, .id = "column_label")
  print(extdf)
  #######
  extdf<-extdf%>%
    filter(!is.na(extDMAs))%>%
    distinct()
  #print(extdf)
  #print("extdfname")
  extdf$extDMAs<-as.integer(extdf$extDMAs)
  extdfname<-left_join(extdf, actdmadf, by = c("extDMAs" = "ID"))%>%
    mutate(INITOREXT = "e")%>%
    dplyr::select(extDMAs,NAME,INITOREXT,TRIGGER_GROUPSIZE,TRIGGERDATE,TRIGGERORG)%>%
    dplyr::rename("ID" = "extDMAs")%>%
    distinct()
  print(extdfname)
  
  extdfbounds<-left_join(extdf, actdmadf, by = c("extDMAs" = "ID"))%>%
    dplyr::select(extDMAs,VERTEX,LAT,LON)%>%
    dplyr::rename("ID" = "extDMAs")%>%
    distinct()
  print(extdfbounds)
  
  print("end 55")
  ##############
  ##this section I copied from above because I am not clever enough right now
  dmaextsightID<-lapply(comboext, function(x) {
    dmacandext<-x %>%
      dplyr::filter((x$dist_nm != 0 & x$dist_nm <= x$corer) | (x$GROUP_SIZE > 2 & x$dist_nm == 0))
    ##filters for distinct sightings that should be considered for DMA calculation
    data.frame(sightID = c(dmacandext$sightID,dmacandext$sightID2)) %>%
      distinct()
  })
  
  allcomboext<-bind_rows(comboext, .id = "column_label")
  alldmaextsightID<-bind_rows(dmaextsightID, .id = "column_label")
  
  print(allcomboext)
  print(alldmaextsightID)
  
  #################  
  
  dmaextsig<-inner_join(allcomboext,alldmaextsightID, by = "sightID")
  dmaextsights<-dmaextsig%>%
    dplyr::select(DateTime,LATITUDE,LONGITUDE,GROUP_SIZE,sightID,indDMA)%>%
    distinct()%>%
    mutate(corer=round(sqrt(GROUP_SIZE/(pi*egden)),2))%>%
    as.data.frame()

  dmaextsights$GROUP_SIZE<-as.numeric(dmaextsights$GROUP_SIZE)
  
  #core radius in meters
  dmaextsights<-dmaextsights%>%
    mutate(extcorer_m = dmaextsights$corer*1852,
           extPolyID = 1:nrow(dmaextsights))

  
  if (nrow(dmaextsights) > 0){
  
  #copy for spatializing
  dmaextdf<-dmaextsights
  
  ########################
  ## df to spatial object ##
  ########################
  ##declare which values are coordinates
  print("250")
  coordinates(dmaextdf)<-~LONGITUDE+LATITUDE
  ##declare what projection they are in
  proj4string(dmaextdf)<-CRS.latlon
  ##transform projection
  dmaextdf.tr<-spTransform(dmaextdf, CRS.utm)
  ###########
  
  ##gbuffer needs utm to calculate radius in meters
  dmaextbuff<-gBuffer(dmaextdf.tr, byid=TRUE, width = dmaextdf$extcorer_m, capStyle = "ROUND")
  ##data back to latlon dataframe
  ##this will be used later when sightings are clustered by overlapping core radiis
  extclustdf<-spTransform(dmaextdf.tr, CRS.latlon)
  extclustdf<-as.data.frame(extclustdf)
  print(extclustdf)
  ##creates a dataframe from the density buffers put around sightings considered for DMA analysis
  extpolycoord<-dmaextbuff %>% fortify() %>% dplyr::select("long","lat","id")
  ##poly coordinates out of utm
  print("297")
  coordinates(extpolycoord)<-~long+lat
  proj4string(extpolycoord)<-CRS.utm
  extpolycoord.tr<-spTransform(extpolycoord, CRS.latlon)
  extpolycoorddf<-as.data.frame(extpolycoord.tr)
  
  #############           
  ## the circular core areas are the polygons in the below section
  extidpoly<-split(extpolycoorddf, extpolycoorddf$id)
  extidpoly<-lapply(extidpoly, function(x) { x["id"] <- NULL; x })
  extpcoord<-lapply(extidpoly, Polygon)
  extpcoord_<-lapply(seq_along(extpcoord), function(i) Polygons(list(extpcoord[[i]]), ID = names(extidpoly)[i]))
  extpolycoorddf_sp<-SpatialPolygons(extpcoord_, proj4string = CRS.latlon)

  ext_clustdf_fun_out<-clustdf_fun(extidpoly,extpolycoorddf_sp)
  extclustdf$extPolyID<-as.numeric(extclustdf$extPolyID)
  
  extclustdf<-full_join(ext_clustdf_fun_out,extclustdf,by=c("upoly"="extPolyID"))
  #print(extclustdf)
  #################################
  extclusty<-extclustdf%>%
    group_by(cluster)%>%
    mutate(totes = sum(GROUP_SIZE))%>%
    filter(totes >= 3)
  extclustn<-extclustdf%>%
    group_by(cluster)%>%
    mutate(totes = sum(GROUP_SIZE))%>%
    filter(totes < 3)
  
  print("yes")
  print(extclusty)
  #print(extclustn)
  #print(egsas)
  
  totalnew<-extclusty%>%
    ungroup()%>%
    distinct(indDMA,cluster,totes)%>%
    group_by(indDMA)%>%
    summarise (n = sum(totes))
  
  print(totalnew)
  
  for (i in 1:nrow(egsas))
    if (is.na(egsas$ACTION_NEW[i])){ #is.na = DMA 4 animals
      egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
    } else if (egsas$sightID[i] %in% alldmaextsightID$sightID & (egsas$sightID[i] %in% extclusty$sightID)) {
      egsas$ACTION_NEW[i] = 5
      #print(head(egsas))
    } else if (egsas$ACTION_NEW[i] == 55){
      egsas$ACTION_NEW[i] = 2 #still in protected zone, but not trigger anything
    } else {
      egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
    }
  print(egsas)
  
  } #276
} #end 55 in action_new

###################################
## animals potential for new DMA ##
###################################
if (NA %in% egsas$ACTION_NEW) {
  print("beg NA")
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
  if (isolate(criteria$DMAapp) == "acoudet"){
    
    dmacand<-combo%>%
      mutate(dist_clust = case_when(
        dist_nm <= 20 ~ 1,
        dist_nm > 20 & dist_nm <= 40 ~ 2))%>%
      group_by(dist_clust)%>%
      arrange(DateTime)%>%
      slice(1)%>%
      ungroup()
    
  } else {
    
  dmacand<-combo %>%
    dplyr::filter((combo$dist_nm != 0 & combo$dist_nm <= combo$corer) | (combo$GROUP_SIZE > 2 & combo$dist_nm == 0))

  }
  
  #print("dmacand")
  #print(dmacand)
  ##filters for distinct sightings that should be considered for DMA calculation
  dmasightID<-data.frame(sightID = c(dmacand$sightID,dmacand$sightID2)) %>%
    distinct()
  
  ##############
  
  ##if not a dma animal, action == 1
  ##if a dma animal, action == 4
  ##if a dma animal that's in an ending dma and extending the dma, action == 5
  ##the below sees if the sightings are good for DMA calc (are in the dmasightID list), and assigns action codes accordingly which is part of NOAA database
  for (i in 1:nrow(egsas))
    if (egsas$sightID[i] %in% dmasightID$sightID) {
      egsas$ACTION_NEW[i] = 44
    } else if (isolate(criteria$DMAapp) == "acoudet" & is.na(egsas$ACTION_NEW[i])){
      egsas$ACTION_NEW[i] = 22 #acoustic detection within existing protection zone
    } else if (is.na(egsas$ACTION_NEW[i])){
      egsas$ACTION_NEW[i] = 1
    } else {
      egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
    }
} #end na

################
## Create DMA ##
################
#only sightings with an action of 4 will be evaluated here for DMA
#print(egsas)
if (44 %in% egsas$ACTION_NEW){
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
  print("359")
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
  print("378")
  coordinates(polycoord)<-~long+lat
  proj4string(polycoord)<-CRS.utm
  polycoord.tr<-spTransform(polycoord, CRS.latlon)
  polycoorddf<-as.data.frame(polycoord.tr)
  
  #############           
  ## the circular core areas are the polygons in the below section
  idpoly<-split(polycoorddf, polycoorddf$id)
  idpoly<-lapply(idpoly, function(x) { x["id"] <- NULL; x })
  pcoord<-lapply(idpoly, Polygon)
  pcoord_<-lapply(seq_along(pcoord), function(i) Polygons(list(pcoord[[i]]), ID = names(idpoly)[i]))
  
  polycoorddf_sp<-SpatialPolygons(pcoord_, proj4string = CRS.latlon)
  #print(polycoorddf_sp)
  #print(str(polycoorddf_sp))
  ##############
 
  clustdf_fun_out<-clustdf_fun(idpoly,polycoorddf_sp)
  clustdf$PolyID<-as.numeric(clustdf$PolyID)
  
  clustdf<-full_join(clustdf_fun_out,clustdf,by=c("upoly"="PolyID"))
  
  clusty<-clustdf%>%
    group_by(cluster)%>%
    mutate(totes = sum(GROUP_SIZE))%>%
    filter(totes >= 3)
  clustn<-clustdf%>%
    group_by(cluster)%>%
    mutate(totes = sum(GROUP_SIZE))%>%
    filter(totes < 3)
  
  for (i in 1:nrow(egsas))
    if (egsas$ACTION_NEW[i] == 44 & (egsas$sightID[i] %in% clusty$sightID)) {
      egsas$ACTION_NEW[i] = 4
    } else if (egsas$ACTION_NEW[i] == 44 & (egsas$sightID[i] %in% clustn$sightID)){
      egsas$ACTION_NEW[i] = 1
    } else {
      egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
    }
  
  clusty<-clusty%>%
    dplyr::rename('PolyID' = 'upoly')
  
  if (4 %in% egsas$ACTION_NEW){
    
  polycoorddf$id<-as.numeric(polycoorddf$id)
  corepoly<-right_join(polycoorddf, clusty, by=c('id'='PolyID'))%>%
    dplyr::select("long","lat","id","DateTime","GROUP_SIZE","corer","corer_m", "LONGITUDE","LATITUDE","cluster")
  #print(corepoly)
  
  #################
  ## for DMA insert
  
  clustersigs<-clusty%>%
    dplyr::select(PolyID,cluster,DateTime,GROUP_SIZE,sightID)
  
  clustersigs$DateTime<-ymd_hms(clustersigs$DateTime)
  #print(clustersigs)
  trigsize<-clustersigs %>% 
    group_by(cluster)%>%
    summarise(TRIGGER_GROUPSIZE = sum(GROUP_SIZE), TRIGGERDATE = min(DateTime))
  print(trigsize)
  #################
  
  ##gets to the core for the cluster
  polymaxmin<-corepoly %>%
    group_by(cluster) %>%
    summarise(maxlat = max(lat), minlat = min(lat), maxlon = max(long), minlon = min(long))%>%
    as.data.frame()
  print(polymaxmin)
  print("515")
  
  if (isolate(criteria$DMAapp) == "acoudet"){
    #20 is the nm radius that we want for the acoustic buffer, but the acoustic positions are filled as group_size of 3 by default, which already gives a 4.79 buffer
    buffnm<-20-round(sqrt(3/(pi*egden)),2)
    #print(buffnm)
  } else {
    
  buffnm<-15
  
  }    
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
  
  ################
  print(dma15)
  
  dma15<-dma15 %>% 
    dplyr::select(cluster, lon, lat)
  
  IDclust<-split(dma15, dma15$cluster)
  
  IDclust<-lapply(IDclust, function(x) { x["cluster"] <- NULL; x })
  
  polyclust<-lapply(IDclust, Polygon)
  
  polyclust_<-lapply(seq_along(polyclust), function(i) Polygons(list(polyclust[[i]]), ID = names(IDclust)[i]))
  
  polyclust_sp<-SpatialPolygons(polyclust_, proj4string = CRS.latlon)
  
  polyclust_sp_df<-SpatialPolygonsDataFrame(polyclust_sp, data.frame(id = unique(dma15$cluster), row.names = unique(dma15$cluster)))
  
  print("new dma bounds")
  dmabounds<-polyclust_sp %>%
    fortify() %>%
    mutate(LAT = round(lat, 2), LON = round(long, 2))%>%
    dplyr::select(id,order,LAT,LON)%>%
    dplyr::rename("ID" = "id", "VERTEX" = "order")
  
  kmlcoord<-dmabounds#%>%
  #filter(Vertex != 5)
  
  ###############
  
  #############
  ## dma name #
  #############
  
  ##port/landmark reference
  dmaname<-data.frame(port = c("Bay of Fundy Canada","Portland ME","Portsmouth NH","Boston MA",
                               "Providence RI","New York NY","Atlantic City NJ", "Virginia Beach VA",
                               "Martha's Vineyard MA", "Nantucket MA", "Cape Cod MA", "Cape Cod Bay", "Hyannis MA", "Chatham MA"),
                      lon = c(-66.9317,-70.2500,-70.7333,-71.0833,-71.4000,-73.9667,-74.4167,-75.9595,-70.6167,-70.0833,-69.9778,-70.27,-70.27, -69.973),
                      lat = c(44.7533,43.6667,43.0833,42.3500,41.8333,40.7833,39.3500,36.8469, 41.4000,41.2833,41.8830,41.80,41.65, 41.686),
                      cardinal = NA)
  
  dmadist<-dmaname
  print("598")
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
  print(dmanamedf)
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
  print("with trigsize")
  print(dmanamedf)
  
  ## this section determines observer_organization for dma input into the dmainfo table in Oracle. 
  ## For cases where sightings from multiple organizations are considered together, this code picks the organization that has the most sightings that contribute.
  
  if (isolate(criteria$triggrptrue) == TRUE & isolate(criteria$DMAapp) == "vissig"){
    
    obs_org2<-left_join(dmasightID,egsas, by = "sightID")%>%
      group_by(GROUP_SIZE)%>%
      mutate(rank = rank(GROUP_SIZE, ties.method = "first"))%>%
      filter(rank == 1)%>%
      ungroup()%>%
      distinct(OBSERVER_ORG)
    #print(obs_org2)
    
    if (nrow(obs_org2) != 1){
      obs_org2<-obs_org2%>%
        filter(OBSERVER_ORG != 0)}
    #print(obs_org2)
    
    if (nrow(obs_org2) != 1){
      obs_org2<-obs_org2%>%
        slice(1)}
    #print(obs_org2)
    
    dmanamedf<-dmanamedf%>%
      mutate(TRIGGERORG = obs_org2$OBSERVER_ORG)
    
  } else if (isolate(criteria$triggrptrue) == TRUE & isolate(criteria$DMAapp) == "acoudet"){
    dmanamedf<-dmanamedf%>%
      mutate(TRIGGERORG = 82) #82 is Robots4Whales
  } else {
    dmanamedf<-dmanamedf%>%
      mutate(TRIGGERORG = 1) #1 is NEFSC
  }
  #print(dmanamedf)
  } 
} # end of 4

if (4 %in% egsas$ACTION_NEW | (5 %in% egsas$ACTION_NEW)){
  print("4 or 5")
  if(!exists("polyclust_sp")){
    polyclust_sp<-SpatialPolygons(list(fakedma))
  } 
  
  #the below is to put a bandaid on sightings trigger extensions not having black core ring to plot. 
  #add it to the list
  if(!exists("polycoorddf_sp")){
    polycoorddf_sp<-SpatialPolygons(list(fakedma))
  }
  
  if(!exists("extpolycoorddf_sp")){
    extpolycoorddf_sp<-SpatialPolygons(list(fakedma))
  }
  ##############
  ##join dmanamedf with ext dmas
  ##ext IDs should be 1 + max(ID) of new dmas
  if(exists("dmanamedf") & exists("extdfname")){
    
    extdfname$ID<-as.character(extdfname$ID)
    extdfname$NAME<-as.character(extdfname$NAME)
    extdfname$TRIGGERDATE<-ymd_hms(extdfname$TRIGGERDATE)

    alldmas<-rbind(dmanamedf,extdfname)
    
  } else if (!exists("dmanamedf") & exists("extdfname")){
    alldmas<-extdfname
  } else if (exists("dmanamedf") & !exists("extdfname")){
    alldmas<-dmanamedf
  }  

  alldmas_trig<-alldmas%>%
    filter(TRIGGER_GROUPSIZE > 2)%>%
    group_by(ID)%>%
    slice_max(TRIGGERORG)%>%
    ungroup()
  
  print(alldmas_trig)
  
  alldmas<-alldmas_trig%>%
    mutate(ID = dense_rank(ID))
  print(alldmas)
  
  print("do bounds exist?")
  if(exists("dmabounds") & exists("extdfbounds")){
    alldmabounds<-rbind(dmabounds,extdfbounds)
  } else if (!exists("dmabounds") & exists("extdfbounds")){
    alldmabounds<-extdfbounds
  } else if (exists("dmabounds") & !exists("extdfbounds")){
    alldmabounds<-dmabounds
  } 
  
  print(alldmabounds)
  alldmabounds<-alldmabounds%>%
    right_join(alldmas_trig, by = "ID")%>%
    mutate(ID = dense_rank(ID))%>%
    dplyr::select(ID, VERTEX, LAT, LON)
  print(alldmabounds)
  
  ##for database (excludes the 5th point to close the polygon)
  dmacoord<-alldmabounds%>%
    mutate("Lat (Degree Minutes)" = paste( trunc(LAT), formatC(round((LAT %% 1)*60,0), width = 2,flag = 0), "N", sep = " "),
           "Lon (Degree Minutes)" = paste( formatC(abs(trunc(LON)), width = 3,flag = 0), formatC(round((abs(LON) %% 1)*60,0), width = 2,flag = 0), "W", sep = " "))%>%
    dplyr::rename("Lat (Decimal Degrees)" = LAT, "Lon (Decimal Degrees)" = LON)%>%
    filter(VERTEX != 5)
  #print(dmacoord)
  
  if (exists("kmlcoord")){
    print("enter kml land")
  
  kmlcoord<-kmlcoord%>%
    dplyr::select(-VERTEX)
  
    if(nrow(kmlcoord) > 0){
  ##KML for new dmas only
  CRS.gearth <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # gearth = google earth
  print("764")
  coordinates(kmlcoord)<-~LON+LAT
  proj4string(kmlcoord)<-CRS.latlon
  dmabounds_kml.tr<- spTransform(kmlcoord, CRS.gearth)
  
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
    }
  }
  
  if (criteria$DMAapp == "acoudet"){
    pzone_ = "ASZ_"
  } else {
    pzone_ = c("DMA_")}
  dma_date<-paste0(pzone_,year(egsas$DateTime[1]),"_",strftime(egsas$DateTime[1], "%m"),"_",strftime(egsas$DateTime[1], "%d"))
  ###############
  ##buttons
  print("enable")
  enable("dmaup")

  ###############
  dmanameout<-alldmas%>%
    dplyr::rename("GROUP_SIZE" = "TRIGGER_GROUPSIZE")
  print(dmanameout)
  dmanameout$TRIGGERDATE<-as.character(dmanameout$TRIGGERDATE)
  dmanameout$GROUP_SIZE<-sprintf("%.0f",round(as.numeric(dmanameout$GROUP_SIZE), digits = 0))
  dmanameout$TRIGGERORG<-sprintf("%.0f",round(as.numeric(dmanameout$TRIGGERORG), digits = 0))
  
  #don't display group size for acoustics
  if (isolate(criteria$DMAapp) == 'acoudet'){
    dmanameout<-dmanameout%>%
      dplyr::select(-GROUP_SIZE)
  }
  
  output$dmanameout<-renderTable({dmanameout})
  output$dmacoord<-renderTable({dmacoord})

print("a&d 864")
#print(egsas)

if ("ID" %in% colnames(egsas)){
  if (isolate(criteria$DMAapp) == "acoudet") {
    egsastab<-egsas %>% 
      dplyr::select(ID,PLATFORM,DateTime,GROUP_SIZE,LATITUDE,LONGITUDE,ID_RELIABILITY,ACTION_NEW)%>%
      mutate(CATEGORY = 7)
  } else {
    egsastab<-egsas %>% 
      dplyr::select(ID,DateTime,GROUP_SIZE,LATITUDE,LONGITUDE,ID_RELIABILITY,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION_NEW)
  }
  
} else {
  if (isolate(criteria$DMAapp) == "acoudet") {
    egsastab<-egsas %>% 
      dplyr::select(PLATFORM,DateTime,GROUP_SIZE,LATITUDE,LONGITUDE,ID_RELIABILITY,ACTION_NEW)%>%
      mutate(CATEGORY = 7)
  } else {
    egsastab<-egsas %>% 
      dplyr::select(DateTime,GROUP_SIZE,LATITUDE,LONGITUDE,ID_RELIABILITY,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION_NEW)
  }
}
     
  
  if (isolate(criteria$loc) == 'Network'){
    
    #The shapes that get plotted are independent of the names
    #print("label testing")
    #print(fortify(benigndma))

    sasdma<-sasdma%>%
      addPolygons(data = benigndma, weight = 2, color = "yellow") %>%
      addPolygons(data = extensiondma, weight = 2, color = "orange") %>%
      addPolygons(data = benignapz, weight = 2, color = "yellow", dashArray = "4 8") %>%
      addPolygons(data = extensionapz, weight = 2, color = "orange", dashArray = "4 8")%>%
      addLegend(title = "Dynamic Management: solid border = DMA, dashed border = Acoustic", colors = c("yellow","orange","blue"), labels = c("Active zone","Active zone eligible for extension","Potential zone"), opacity = 0.4, position = "topleft")
  }
  
  ##display core areas for visual sightings
  if (isolate(criteria$DMAapp) == 'vissig' | isolate(criteria$DMAapp) == 'rwsurv'){
  
    sasdma<-sasdma%>%
      addPolygons(data = polyclust_sp, weight = 2, color = "blue")%>%
      addPolygons(data = polycoorddf_sp, weight = 2, color = "black")%>%
      addPolygons(data = extpolycoorddf_sp, weight = 2, color = "black")
    
  } else {
    
    sasdma<- sasdma%>%
      addPolygons(data = polyclust_sp, weight = 2, color = "blue", dashArray = "4 8")
  } 
  
} else { ##4 in egsas$action_new
 
  if ("ID" %in% colnames(egsas)){
    if (isolate(criteria$DMAapp) == "acoudet") {
      egsastab<-egsas %>% 
        dplyr::select(ID,PLATFORM,DateTime,GROUP_SIZE,LATITUDE,LONGITUDE,ID_RELIABILITY,ACTION_NEW)%>%
        mutate(CATEGORY = 7)
    } else {
      egsastab<-egsas %>% 
        dplyr::select(ID,DateTime,GROUP_SIZE,LATITUDE,LONGITUDE,ID_RELIABILITY,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION_NEW)
    }
    
  } else {
    if (isolate(criteria$DMAapp) == "acoudet") {
      egsastab<-egsas %>% 
        dplyr::select(PLATFORM,DateTime,GROUP_SIZE,LATITUDE,LONGITUDE,ID_RELIABILITY,ACTION_NEW)%>%
        mutate(CATEGORY = 7)
    } else {
      egsastab<-egsas %>% 
        dplyr::select(DateTime,GROUP_SIZE,LATITUDE,LONGITUDE,ID_RELIABILITY,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION_NEW)}}

  if (isolate(criteria$loc) == 'Network'){
    sasdma<-sasdma%>%
      addPolygons(data = benigndma, weight = 2, color = "yellow") %>%
      addPolygons(data = extensiondma, weight = 2, color = "orange")%>%
      addPolygons(data = benignapz, weight = 2, color = "yellow") %>%
      addPolygons(data = extensionapz, weight = 2, color = "orange")
  } 

}

###################
## LEAFLET FINAL ##
###################

##visual
if (isolate(criteria$DMAapp) == 'vissig' | isolate(criteria$DMAapp) == 'rwsurv'){
  
  sasdma<-sasdma%>%
    addCircleMarkers(lng = ~egsas$LONGITUDE, lat = ~egsas$LATITUDE, radius = 5, stroke = FALSE, fillOpacity = 0.5 , color = "black", popup = paste0(egsas$DateTime,", Group Size:", egsas$GROUP_SIZE))%>%
    addLegend(colors = c("green","red","black"), labels = c("Shipping Lanes","SMA","Core area of right whale sightings"), opacity = 0.4, position = "topleft")
    


##acoustic
} else if (isolate(criteria$DMAapp) == "acoudet") {
  
  egsas_dma<-egsas%>%filter(ACTION_NEW == 4 | ACTION_NEW == 5)
  egsas_notdma<-egsas%>%filter(ACTION_NEW != 4 & ACTION_NEW != 5)
  
  sasdma<-sasdma%>%
    addCircleMarkers(lng = ~egsas_notdma$LONGITUDE, lat = ~egsas_notdma$LATITUDE, radius = 5, stroke = FALSE, fillOpacity = 0.5 , color = "grey", popup = egsas_notdma$DateTime)%>%
    addCircleMarkers(lng = ~egsas_dma$LONGITUDE, lat = ~egsas_dma$LATITUDE, radius = 5, stroke = FALSE, fillOpacity = 0.5 , color = "black", popup = egsas_dma$DateTime)%>%
    addLegend(colors = c("green","red","black","grey"), labels = c("Shipping Lanes","SMA","Right whale acoustic detection - trigger", "Other right whale acoustic detection"), opacity = 0.4, position = "topleft")
  
}

####################
egsastab$GROUP_SIZE<-sprintf("%.0f",round(egsastab$GROUP_SIZE, digits = 0))
egsastab$CATEGORY<-as.numeric(egsastab$CATEGORY)
egsastab$CATEGORY<-sprintf("%.0f",round(egsastab$CATEGORY, digits = 0))
egsastab$ID_RELIABILITY<-as.numeric(egsastab$ID_RELIABILITY)
egsastab$ID_RELIABILITY<-sprintf("%.0f",round(egsastab$ID_RELIABILITY, digits = 0))
egsastab$ACTION_NEW<-as.numeric(egsastab$ACTION_NEW)

### egsas table for output

if (isolate(criteria$DMAapp) == 'acoudet'){
  egsastab<-egsastab%>%
    dplyr::select(-GROUP_SIZE)%>%
    mutate(ACTION_NEW=replace(ACTION_NEW, ACTION_NEW==4, 24),
           ACTION_NEW=replace(ACTION_NEW, ACTION_NEW==5, 25))
}

##############

sas_react$egsastab<-egsastab

################
## On network ##
################

if (isolate(criteria$loc) == 'Network'){
  ##########
  ###sas on network
  egsastabout<-sas_react$egsastab%>%
    left_join(actioncodedf, by = c("ACTION_NEW" = "ID"))%>%
    dplyr::rename("ACTION_NEW_TRANSLATION" = "ACTION")
  #print(str(egsastabout))
  egsastabout$ACTION_NEW<-sprintf("%.0f",round(egsastabout$ACTION_NEW, digits = 0))
  
  ##########
  ##dmas on network
  dma_react$egsas = egsas
  dma_react$sasdma = sasdma
  dma_react$alldmas = alldmas
  
  if (exists("extdfname")){
    dma_react$extdfname = extdfname
  } else {
    dma_react$extdfname = ""
  }
  
  dma_react$dmacoord = dmacoord
  dma_react$dmanameout = dmanameout
  
  #############
  ## Outputs ##
  #############
  
  output$egsastabout<-renderTable({egsastabout},  striped = TRUE)
  output$sasdma = renderLeaflet({print(dma_react$sasdma)})

}#ends network path for Oracle uploads
