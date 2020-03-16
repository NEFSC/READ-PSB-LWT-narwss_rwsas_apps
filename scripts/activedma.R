#################
## ACTIVE DMAs ##
#################

#################
## declare function
#################

querytoshape<-function(x){ #dmaquery = x

vector5<-x%>%
  filter(VERTEX == 1)%>%
  mutate(VERTEX=replace(VERTEX, VERTEX==1, 5))

DMADF<-rbind(x, vector5)

DMADF<-DMADF%>%
  arrange(ID, VERTEX)%>%
  dplyr::select(ID,LON,LAT,-VERTEX)

idDMA<-split(DMADF, DMADF$ID)
idDMA<-lapply(idDMA, function(x) { x["ID"] <- NULL; x })

DMAcoord<-lapply(idDMA, Polygon)
DMAcoord_<-lapply(seq_along(DMAcoord), function(i) Polygons(list(Polygon(DMAcoord[[i]], hole=as.logical(NA))), ID = names(idDMA)[i]))
SpatialPolygons(DMAcoord_)
}

#################
##action code dataframe to join with results of dma evaluation later
actioncode<-"select *
            from rightwhalesight.action"
actioncodedf<-sqlQuery(cnxn, actioncode)
actioncodedf$ID<-as.numeric(actioncodedf$ID)

#################
#query all relevant DMAs
##the to_date(to_char) for the expdate is necessary, otherwise, the modayr seconds default to 00:00:00 and dmas that expire on the same day are still included
activedmasql<-paste0("select rightwhalesight.dmainfo.name, to_char(rightwhalesight.dmainfo.expdate, 'YYYY-MM-DD') as expdate, ID, to_char((rightwhalesight.dmainfo.expdate - 7), 'YYYY-MM-DD') as ext
                  from rightwhalesight.dmainfo
                  where to_date('",MODAYR,"', 'YYYY-MM-DD') < to_date(to_char(EXPDATE, 'YYYY-MM-DD'),'YYYY-MM-DD') 
                    and to_date('",MODAYR,"', 'YYYY-MM-DD') > to_date(to_char(TRIGGERDATE, 'YYYY-MM-DD'),'YYYY-MM-DD')
                     and cancelled is null;")

actdma<-sqlQuery(cnxn,activedmasql)

print(actdma)
actdma<-actdma%>%
  group_by(NAME)%>%
  arrange(EXPDATE)%>%
  top_n(n = 1, EXPDATE)%>% #selects for later dma if there are two technically active because of an extension
  ungroup()

print(actdma)

##do we have ANY dmas?
if (nrow(actdma) == 0){
  
  benigndma<-SpatialPolygons(list(fakedma))
  extensiondma<-SpatialPolygons(list(fakedma))
  dmanamesexp<-"None"
  

} else {

  ############
  ## report ##
  ############
  actdma$EXPDATE<-lubridate::ymd(actdma$EXPDATE)
  actdma$EXPDATE<-format(actdma$EXPDATE, format = "%d %B %Y")
  print(actdma)
  repdma<-actdma%>%  
    mutate(sentence = paste(NAME, "expires on", EXPDATE))
  print(repdma)
  dmalist<-as.list(repdma$sentence)
  dmanamesexp<-do.call("paste", c(dmalist, sep = ", "))
  
####################
## Extend or not? ##
####################
  
  ##dma bounds
  actdma_boundssql<-paste0("select rightwhalesight.dmacoords.ID, vertex, lat, lon
                  from rightwhalesight.dmacoords, rightwhalesight.dmainfo
                  where to_date('",MODAYR,"', 'YYYY-MM-DD') < EXPDATE
                    and to_date('",MODAYR,"', 'YYYY-MM-DD') > TRIGGERDATE
                    and rightwhalesight.dmacoords.ID = RIGHTWHALESIGHT.DMAINFO.ID;")
  
  
  actdma_bounds<-sqlQuery(cnxn,actdma_boundssql)
  
  actdmadf<-inner_join(actdma,actdma_bounds,by = "ID")
    
## dmas not up for extension
##nothing happens = noth
actdmadf$EXPDATE<-ymd(actdmadf$EXPDATE)
actdmadf$EXT<-ymd(actdmadf$EXT)
  
dmanoth<-actdmadf%>%
  filter(EXT > MODAYR)%>%
  dplyr::select(ID,VERTEX,LAT,LON)

###########
##dmas up for extension

dmaext<-actdmadf%>%
  filter(EXT <= MODAYR)%>%
  dplyr::select(ID,VERTEX,LAT,LON)

############
#evaluate benign DMAs
    if (nrow(dmanoth) == 0){
      
      benigndma<-SpatialPolygons(list(fakedma))
      
    } else {
      
      benigndma<-querytoshape(dmanoth)
      
    }


############
#evaluate extension triggers DMAs
    if (nrow(dmaext) == 0){
  
      extensiondma<-SpatialPolygons(list(fakedma))
  
    } else {

      ##all polys together
      extensiondma<-querytoshape(dmaext)
   
  ######################
  
  ##distinct polys for extension
  IDlist<-as.list(unique(dmaext$ID))
  names(IDlist)<-unique(dmaext$ID)
  
  
  for (i in names(IDlist)){
    a<-dmaext%>%
      filter(ID == i)
    
    b<-querytoshape(a)
    
    if(exists("extdma_name") == FALSE & exists("extdma_list") == FALSE){
      extdma_name<-list(a)
      extdma_list<-list(b)
      
    } else if (length(extdma_name) > 0 & length(extdma_list) > 0){
      extdma_name<-list.append(extdma_name,a)
      extdma_list<-list.append(extdma_list,b) #rlist::list.append
    }
  }
  
  names(extdma_name)<-names(IDlist)
  names(extdma_list)<-names(IDlist)
  
  ## declare projection
  extdma.sp<-extdma_list
  print("ext dma")
  print(extdma.sp)
  #made this a loop because I could not figure out how to apply it over a list 3/21
  for (i in names(IDlist)){
    proj4string(extdma.sp[[i]])<-CRS.latlon
  }
  
  ##change projection
  extdma.tr<-lapply(extdma.sp, function (x) {spTransform(x, CRS.new)})
  
}

}


##change projection
extensiondma.sp<-extensiondma
##declare what kind of projection thy are in
proj4string(extensiondma.sp)<-CRS.latlon
##change projection
extensiondma.tr<-spTransform(extensiondma.sp, CRS.new)

print("ext dma")
print(extensiondma)

##change projection
benigndma.sp<-benigndma
##declare what kind of projection thy are in
proj4string(benigndma.sp)<-CRS.latlon
##change projection
benigndma.tr<-spTransform(benigndma.sp, CRS.new)  

print("benign dma")
print(benigndma)




