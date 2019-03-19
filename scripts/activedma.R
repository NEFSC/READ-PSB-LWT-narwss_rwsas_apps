#################
## ACTIVE DMAs ##
#################

cnxn <- odbcConnect(server, uid=sasuid, pwd=saspswd,believeNRows=FALSE)

dmanamessql<-paste0("select rightwhalesight.dmainfo.name, trunc(rightwhalesight.dmainfo.expdate) as expdate, ID
                  from rightwhalesight.dmainfo
                  where to_timestamp('",MODAYR,"', 'YYYY-MM-DD HH24:MI:SS') < EXPDATE
                    and to_timestamp('",MODAYR,"', 'YYYY-MM-DD HH24:MI:SS') > STARTDATE;")

dmanamesquery<-sqlQuery(cnxn,dmanamessql)

print(dmanamesquery) 

###########
##extension

dma_ext<-paste0("select rightwhalesight.dmainfo.name, trunc(rightwhalesight.dmainfo.expdate) as expdate, ID
                  from rightwhalesight.dmainfo
                  where to_timestamp('",MODAYR,"', 'YYYY-MM-DD HH24:MI:SS') < EXPDATE
                    and to_timestamp('",MODAYR,"', 'YYYY-MM-DD HH24:MI:SS') > EXPDATE - 7;")

dma_extquery<-sqlQuery(cnxn,dma_ext)

print(dma_extquery)   

if (nrow(dmanamesquery) == 0){
  fakedma<-data.frame(
   long = c(-71,-71,-71,-71,-71),
   lat = c(42,42,42,42,42))
  
  fakedma<-Polygons(list(Polygon(fakedma, hole=as.logical(NA))), ID = 1)
  
  activedma<-SpatialPolygons(list(fakedma))
  dmanamesexp<-"None"
  
} else {
  
  ##DMA names and expiration
  dmanamesquery$EXPDATE<-as.Date(dmanamesquery$EXPDATE)
  ##this is used for extension criteria
  ##will need to figure out how to evaluate over a list
  expext<-dmanamesquery
  ##this is used for report sentence
  dmanamesquery$EXPDATE<-format(dmanamesquery$EXPDATE, format = "%d %B %Y")
  
  dmanamesdistinct<-dmanamesquery%>%
    group_by(NAME)%>%
    arrange(EXPDATE)%>%
    top_n(n = 1, EXPDATE)%>%
    mutate(sentence = paste(NAME, "expires on", EXPDATE))

  dmalist<-as.list(dmanamesdistinct$sentence)
  dmanamesexp<-do.call("paste", c(dmalist, sep = ", "))

  ##dma bounds
dmasql<-paste0("select rightwhalesight.dmacoords.ID, vertex, lat, lon
                  from rightwhalesight.dmacoords, rightwhalesight.dmainfo
                  where to_timestamp('",MODAYR,"', 'YYYY-MM-DD HH24:MI:SS') < EXPDATE and
                        to_timestamp('",MODAYR,"', 'YYYY-MM-DD HH24:MI:SS') > STARTDATE and
                        rightwhalesight.dmacoords.ID = RIGHTWHALESIGHT.DMAINFO.ID;")


dmaquery<-sqlQuery(cnxn,dmasql)

vector5<-dmaquery%>%
  filter(VERTEX == 1)%>%
  mutate(VERTEX=replace(VERTEX, VERTEX==1, 5))

DMADF<-rbind(dmaquery, vector5)

DMADF<-DMADF%>%
  arrange(ID, VERTEX)%>%
  dplyr::select(ID,LON,LAT,-VERTEX)

idDMA<-split(DMADF, DMADF$ID)
idDMA<-lapply(idDMA, function(x) { x["ID"] <- NULL; x })

DMAcoord<-lapply(idDMA, Polygon)
DMAcoord_<-lapply(seq_along(DMAcoord), function(i) Polygons(list(Polygon(DMAcoord[[i]], hole=as.logical(NA))), ID = names(idDMA)[i]))
activedma<-SpatialPolygons(DMAcoord_)}

##change projection
activedma.sp<-activedma

##declare what kind of projection thy are in
proj4string(activedma.sp)<-CRS.latlon
##change projection
activedma.tr<-spTransform(activedma.sp, CRS.new)
