##########
#extension dma
#########
library(rlist)
###########
##dmas up for extension

dma_ext<-paste0("select rightwhalesight.dmainfo.name, trunc(rightwhalesight.dmainfo.expdate) as expdate, ID
                from rightwhalesight.dmainfo
                where to_timestamp('",MODAYR,"', 'YYYY-MM-DD HH24:MI:SS') < EXPDATE
                and to_timestamp('",MODAYR,"', 'YYYY-MM-DD HH24:MI:SS') > EXPDATE - 8;")

dma_extquery<-sqlQuery(cnxn,dma_ext)

print(dma_extquery)

dma_extdistinct<-dma_extquery%>%
  group_by(NAME)%>%
  arrange(EXPDATE)%>%
  top_n(n = 1, EXPDATE)%>% #selects for later dma if there are two technically active because of an extension
  ungroup()

print(dma_extdistinct)

##are there active DMAs that could be extended?
if (nrow(dma_extdistinct) == 0){
  EXT = FALSE
} else {
  
  ##dma bounds
  dma_extsql<-paste0("select rightwhalesight.dmacoords.ID, vertex, lat, lon
                  from rightwhalesight.dmacoords, rightwhalesight.dmainfo
                  where to_timestamp('",MODAYR,"', 'YYYY-MM-DD HH24:MI:SS') < EXPDATE and
                        to_timestamp('",MODAYR,"', 'YYYY-MM-DD HH24:MI:SS') > EXPDATE - 8 and
                        rightwhalesight.dmacoords.ID = RIGHTWHALESIGHT.DMAINFO.ID;")
  dma_extsql<-sqlQuery(cnxn,dma_extsql)
  
  print(dma_extsql)
  
  
}


extdf<-inner_join(dma_extdistinct,dma_extsql,by = "ID")

IDlist<-as.list(unique(extdf$ID))
names(IDlist)<-unique(extdf$ID)



for (i in names(IDlist)){
  a<-extdf%>%
    filter(ID == i)
  print(a)
  
  b<-a%>%
    dplyr::select(-NAME, -EXPDATE)
  
  c<-querytoshape(b)
  print(c) 
   
  if(exists("ext_list") == FALSE & exists("ext_shapelist") == FALSE){
     ext_list<-list(a)
     ext_shapelist<-list(c)
     
   } else if (length(ext_list) > 0 & length(ext_shapelist) > 0){
     ext_list<-list.append(ext_list,a)
     ext_shapelist<-list.append(ext_shapelist,c) #rlist::list.append
   }
}

names(ext_list)<-names(IDlist)
names(ext_shapelist)<-names(IDlist)
  