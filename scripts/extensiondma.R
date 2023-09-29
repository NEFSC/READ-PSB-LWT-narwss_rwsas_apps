
## Extensions

## dmas/slow_zones up for extension ----

dma_ext <-
  paste0(
    "select rightwhalesight.dmainfo.name, to_char(rightwhalesight.dmainfo.expdate, 'YYYY-MM-DD') as expdate, ID,  to_char((rightwhalesight.dmainfo.expdate - 8), 'YYYY-MM-DD') as ext
                from rightwhalesight.dmainfo
                where to_date('",
    MODAYR,
    "', 'YYYY-MM-DD') < EXPDATE
                  and to_date('",
    MODAYR,
    "', 'YYYY-MM-DD') >= to_date(to_char((rightwhalesight.dmainfo.expdate - 8), 'YYYY-MM-DD'), 'YYYY-MM-DD')", sep = ""
  )

#dma_extquery <- sqlQuery(cnxn, dma_ext)
dma_extquery_q <-dbSendQuery(cnxn, dma_ext)
dma_extquery <- fetch(dma_extquery_q) #HJF sqlQuery 6/14 replace 20230626

print(dma_extquery)

dma_extdistinct <- dma_extquery %>%
  group_by(NAME) %>%
  arrange(EXPDATE) %>%
  top_n(n = 1, EXPDATE) %>% #selects for later dma if there are two technically active because of an extension
  ungroup()

print(dma_extdistinct)

## are there active DMAs/Slow Zones that could be extended? ----
if (nrow(dma_extdistinct) == 0) {
  ext_list <- list()
  ext_shapelist <- list()
  
} else {
  ##dma bounds
  dma_extsql <-
    paste0(
      "select rightwhalesight.dmacoords.ID, vertex, lat, lon
                  from rightwhalesight.dmacoords, rightwhalesight.dmainfo
                  where to_date('",
      MODAYR,
      "', 'YYYY-MM-DD') < EXPDATE
                    and to_date('",
      MODAYR,
      "', 'YYYY-MM-DD') >= to_date(to_char((rightwhalesight.dmainfo.expdate - 8), 'YYYY-MM-DD'), 'YYYY-MM-DD')
                    and rightwhalesight.dmacoords.ID = RIGHTWHALESIGHT.DMAINFO.ID", sep = ""
    )
  #dma_extsql <- sqlQuery(cnxn, dma_extsql)
  dma_extsql_q <- dbSendQuery(cnxn, dma_extsql)
  dma_extsql <- fetch(dma_extsql_q) #HJF 20230626 7/14 sqlQuery replace
  
  print(dma_extsql)
  
  extdf <- inner_join(dma_extdistinct, dma_extsql, by = "ID")
  
  IDlist <- as.list(unique(extdf$ID))
  names(IDlist) <- unique(extdf$ID)
  
  
  for (i in names(IDlist)) {
    a <- extdf %>%
      filter(ID == i)
    print(a)
    
    b <- a %>%
      dplyr::select(-NAME,-EXPDATE)
    
    c <- querytoshape(b)
    print(c)
    
    if (exists("ext_list") == FALSE &
        exists("ext_shapelist") == FALSE) {
      ext_list <- list(a)
      ext_shapelist <- list(c)
      
    } else if (length(ext_list) > 0 & length(ext_shapelist) > 0) {
      ext_list <- list.append(ext_list, a)
      ext_shapelist <- list.append(ext_shapelist, c) #rlist::list.append
    }
  }
  
  names(ext_list) <- names(IDlist)
  names(ext_shapelist) <- names(IDlist)
}
