## ACTIVE Slow zones ----

#queries for active dmas/acoustic protection zones and their bounds. Also identifies if zones are within time period where they could be extended.
##a lot of variables are named with "dma" even if they refer to both kinds of protection zones (acoustic vs. visual) because the original code for DMAs was modifed in 2020 to accomodate the new acoustic protection zone program

## declare function ----

print("begin active slowzone")
querytoshape <- function(x) {
  #dmaquery = x

  vector5 <- x %>%
    filter(VERTEX == 1) %>%
    mutate(VERTEX = replace(VERTEX, VERTEX == 1, 5))

  DMADF <- rbind(x, vector5)

  DMADF <- DMADF %>%
    arrange(ID, VERTEX) %>%
    dplyr::select(ID, LON, LAT, -VERTEX)

  idDMA <- split(DMADF, DMADF$ID)
  idDMA <- lapply(idDMA, function(x) {
    x["ID"] <- NULL
    x
  })

  DMAcoord <- lapply(idDMA, Polygon)
  DMAcoord_ <-
    lapply(seq_along(DMAcoord), function(i)
      Polygons(list(Polygon(
        DMAcoord[[i]], hole = as.logical(NA)
      )), ID = names(idDMA)[i]))
  SpatialPolygons(DMAcoord_)
}

if (isolate(criteria$loc) == 'Network') {

  ##action code dataframe to join with results of trigger analysis later
  actioncode <- "select * from action"
  #actioncodedf <- sqlQuery(cnxn, actioncode)
  actioncodedf_q <- dbSendQuery(cnxn, actioncode)
  actioncodedf<-fetch(actioncodedf_q) #HJF 3/14 sqlQuery replace 20230626
  actioncodedf$ID <- as.numeric(actioncodedf$ID)
  
  #query all relevant DMAs & APZs
  ##the to_date(to_char) for the expdate is necessary, otherwise, the modayr seconds default to 00:00:00 and dmas that expire on the same day are still included
  activedmasql <-
    paste0(
      "select dmainfo.name, to_char(dmainfo.expdate, 'YYYY-MM-DD') as expdate, ID, to_char((dmainfo.expdate - 7), 'YYYY-MM-DD') as ext, dmainfo.triggertype
                  from dmainfo
                  where to_date('",
      MODAYR,
      "', 'YYYY-MM-DD') < to_date(to_char(EXPDATE, 'YYYY-MM-DD'),'YYYY-MM-DD')
                    and to_date('",
      MODAYR,
      "', 'YYYY-MM-DD') > to_date(to_char(TRIGGERDATE, 'YYYY-MM-DD'),'YYYY-MM-DD')
                     and (cancelled not like 'cancel%' or cancelled is null)", sep=""
    )

  #actdma <- sqlQuery(cnxn, activedmasql)
  actdma_q <- dbSendQuery(cnxn, activedmasql)
  actdma<-fetch(actdma_q) #HJF replace 4/14 sqlQuery 20230626
  #print("actdma")
  #print(actdma)

} else {
  print("else")
  actioncodedf <- data.frame(
    ID = c(1, 2, 4, 5),
    ACTION = c(
      "ONLY 1 OR 2",
      "IN EXISTING PROTECTION ZONE",
      "DMA",
      "DMA EXTENSION"
    )
  )

  dmacsv <-
    read.csv(
      './Aerial and SLOW zone data/DMAINFO export 05Aug2021.csv',
      #'./example_data/example_data_slowzones.csv',
      header = T,
      stringsAsFactors = F
    )
  dmacsv$EXPDATE <- as.Date(dmacsv$EXPDATE)
  dmacsv$TRIGGERDATE <- dmy_hms(dmacsv$TRIGGERDATE) #20231002 HJF

  actdma <- dmacsv %>%
    filter(EXPDATE > MODAYR & TRIGGERDATE < MODAYR) %>%
    dplyr::select(NAME, EXPDATE, ID, TRIGGERTYPE) %>%
    #distinct(NAME, EXPDATE, ID, TRIGGERTYPE) %>%
    mutate(EXT = EXPDATE - days(7))
}
#print("actdma")
#print(actdma)


actdma <- actdma %>%
  group_by(NAME, TRIGGERTYPE) %>%  #20251213 added trigger type to group_by for CCB issues
  arrange(EXPDATE) %>%
  top_n(n = 1, EXPDATE) %>% #selects for later dma if there are two technically active because of an extension
  ungroup()
print("actdma")
print(actdma)

##do we have ANY dmas?
if (nrow(actdma) == 0) {
  benigndma <- SpatialPolygons(list(fakeslowzone))
  extensiondma <- SpatialPolygons(list(fakeslowzone))
  benignapz <- SpatialPolygons(list(fakeslowzone))
  extensionapz <- SpatialPolygons(list(fakeslowzone))
  dmanamesexp <- "None"

} else {

  ## report ----

  repdma <- actdma %>%
    mutate(
      EXPDATE = format(EXPDATE, format = "%d %B %Y"),
      sentence = paste(NAME, "expires on", EXPDATE)
    )
  print("repdma")
  print(repdma)
  dmalist <- as.list(repdma$sentence)
  dmanamesexp <-
    stringi::stri_replace_last(do.call("paste", c(dmalist, sep = ", ")), fixed = ",", ", and")

  ## Extend or not? ----

  if (isolate(criteria$loc) == 'Network') {

    ##dma/apz bounds
    actdma_boundssql <- paste0(
      "select dmacoords.ID, vertex, lat, lon
                     from dmainfo
                     left outer join dmacoords on dmainfo.ID = dmacoords.ID
                     where to_date('",
      MODAYR,
      "', 'YYYY-MM-DD') < to_date(to_char(EXPDATE, 'YYYY-MM-DD'),'YYYY-MM-DD')
                     and to_date('",
      MODAYR,
      "', 'YYYY-MM-DD') > to_date(to_char(TRIGGERDATE, 'YYYY-MM-DD'),'YYYY-MM-DD')
                     and (cancelled not like 'cancel%' or cancelled is null)", sep =""
    )

    #actdma_bounds <- sqlQuery(cnxn, actdma_boundssql)
    actdma_bounds_q <- dbSendQuery(cnxn, actdma_boundssql)
    actdma_bounds<-fetch(actdma_bounds_q) #HJF 5/14 sqlQuery replace 20230626
    print(actdma_bounds)

    actdmadf <- actdma %>%
      left_join(actdma_bounds, by = "ID")

  } else {
    actdma_bounds <-
      read.csv(
        './Aerial and SLOW zone data/DMACOORDS export 05Aug2021.csv',
        #'./example_data/example_data_slowzones.csv',
        header = T,
        stringsAsFactors = F
      )

    actdmadf <- actdma_bounds %>%
      dplyr::select(ID, VERTEX, LAT, LON) %>%
      right_join(actdma, by = "ID")

    print("second else")
  }
  actdmadf$EXT <- ymd(actdmadf$EXT)
  actdmadf$ID <- as.numeric(actdmadf$ID)
  actdmadf$VERTEX <- as.numeric(actdmadf$VERTEX)
  actdmadf$LAT <- as.numeric(actdmadf$LAT)
  actdmadf$LON <- as.numeric(actdmadf$LON)
  print(actdmadf)

  ## Categorizing current protection zones ----

  ## DMA ## For visual sightings
  ## dmas not up for extension, nothing happens = noth
  dmanoth <- actdmadf %>%
    filter(EXT > MODAYR & TRIGGERTYPE == "v") %>%
    dplyr::select(ID, VERTEX, LAT, LON)

  ##dmas up for extension
  dmaext <- actdmadf %>%
    filter(EXT <= MODAYR & TRIGGERTYPE == "v") %>%
    dplyr::select(ID, VERTEX, LAT, LON)

  ## Acoustic Protection Zones (APZ) ----
  ##apz not up for extension, nothing (noth) happens
  apznoth <- actdmadf %>%
    filter(EXT > MODAYR & TRIGGERTYPE == "a") %>%
    dplyr::select(ID, VERTEX, LAT, LON)
  #print("apznoth")
  #print(apznoth)
  
  ## apz up for extension
  apzext <- actdmadf %>%
    filter(EXT <= MODAYR & TRIGGERTYPE == "a") %>%
    dplyr::select(ID, VERTEX, LAT, LON)
  #print("apzext")
  #print(apzext)

  #evaluate DMAs ----
  #benign

  if (nrow(dmanoth) == 0) {
    benigndma <- SpatialPolygons(list(fakeslowzone))
  } else {
    benigndma <- querytoshape(dmanoth)
  }
  #print("benigndma")
  #print(benigndma)
  #evaluate extension triggers DMAs

  if (nrow(dmaext) == 0) {
    extensiondma <- SpatialPolygons(list(fakeslowzone))
  } else {
    ##all polys together
    extensiondma <- querytoshape(dmaext)
    #print("extensiondma")
    #print(extensiondma)

    ##change projection, extension
    ##distinct polys for DMA extension
    IDlist <- as.list(unique(dmaext$ID))
    names(IDlist) <- unique(dmaext$ID)

    for (i in names(IDlist)) {
      a <- dmaext %>%
        filter(ID == i)

      b <- querytoshape(a)

      if (exists("extdma_name") == FALSE &
          exists("extdma_list") == FALSE) {
        extdma_name <- list(a)
        extdma_list <- list(b)

      } else if (length(extdma_name) > 0 & length(extdma_list) > 0) {
        extdma_name <- list.append(extdma_name, a)
        extdma_list <- list.append(extdma_list, b) #rlist::list.append
      }
    }

    names(extdma_name) <- names(IDlist)
    names(extdma_list) <- names(IDlist)

    ## declare projection
    extdma.sp <- extdma_list
    print("ext dma")
    print(extdma.sp)
    #made this a loop because I could not figure out how to apply it over a list 3/21
    for (i in names(IDlist)) {
      proj4string(extdma.sp[[i]]) <- CRS.latlon
    }
    
    #NEEDS 2025 CLEANUP/APPLY COORDINATES!! Similar function should be in action and slow zone
    #attempting same thing but to make sf objects via st_as_sf or # Convert each SpatialPolygons object in the list to an sf object 
    extdma.sp <- lapply(extdma.sp, st_as_sf)
    #for (i in names(IDlist)) {
     # extdma.sp[[i]] <- st_as_sf(extdma.sp[[i]], coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) #was CRS.latlon
    #}
    
    ##change projection
    extdma.tr <-
      lapply(extdma.sp, function (x) {
      st_transform(x, crs = 26919) #was CRS.new added crs = 
      })
  }

  # evaluate APZs ----
  #benign

  if (nrow(apznoth) == 0) {
    benignapz <- SpatialPolygons(list(fakeslowzone))
  } else {
    benignapz <- querytoshape(apznoth)
  }

  #evaluate extension triggers APZs

  if (nrow(apzext) == 0) {
    extensionapz <- SpatialPolygons(list(fakeslowzone))
  } else {
    ##all polys together
    extensionapz <- querytoshape(apzext)

    ##distinct polys for APZ extension
    IDlist <- as.list(unique(apzext$ID))
    names(IDlist) <- unique(apzext$ID)

    for (i in names(IDlist)) {
      a <- apzext %>%
        filter(ID == i)

      b <- querytoshape(a)

      if (exists("extapz_name") == FALSE &
          exists("extapz_list") == FALSE) {
        extapz_name <- list(a)
        extapz_list <- list(b)

      } else if (length(extapz_name) > 0 & length(extapz_list) > 0) {
        extapz_name <- list.append(extapz_name, a)
        extapz_list <- list.append(extapz_list, b) #rlist::list.append
      }
    }

    names(extapz_name) <- names(IDlist)
    names(extapz_list) <- names(IDlist)

    ## declare projection
    extapz.sp <- extapz_list
    print("ext apz")
    print(extapz.sp)
    #made this a loop because I could not figure out how to apply it over a list 3/21
    for (i in names(IDlist)) {
      proj4string(extapz.sp[[i]]) <- CRS.latlon
    }
    #update 20251119 for sf objects, getting rid of rgdal ##this makesa spatail object which is not desired
    #extapz.tr <- lapply(extapz.sp, function(p) {
     # sp_obj <- SpatialPolygons(list(p), proj4string = CRS.latlon)
      #st_as_sf(sp_obj)
    #})
    extapz.sp <- lapply(extapz.sp, st_as_sf)
    ## NEED TO APPLY COORDINATES SOMEWHERE
    #for (i in names(IDlist)) {
     # extapz.sp[[i]] <- st_as_sf(extapz.sp[[i]], coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) #changed from CRS.latlon
    #}

    ##change projection
    extapz.tr <-
     lapply(extapz.sp, function (x) {
      st_transform(x, crs = 26919) #changed from CRS.new
      })
  }

} # 73

##change projection ----

#DMA

#benigndma.sp <- benigndma #can delete?
##declare what kind of projection they are in
#proj4string(benigndma.sp) <- CRS.latlon
benigndma.sp <- st_as_sf(benigndma) #251120 update to sf/ditch rgdal
st_crs(benigndma.sp) <- CRS.latlon
##change projection
benigndma.tr <- sf::st_transform(benigndma.sp, CRS.new)

#extensiondma.sp <- extensiondma can delete?
##declare what kind of projection they are in
#proj4string(extensiondma.sp) <- CRS.latlon
extensiondma.sp <- st_as_sf(extensiondma) #251120 update to sf/ditch rgdal
st_crs(extensiondma.sp) <- CRS.latlon
##change projection
extensiondma.tr <- sf::st_transform(extensiondma.sp, CRS.new)

#APZ

#benignapz.sp <- benignapz
##declare what kind of projection they are in
#proj4string(benignapz.sp) <- CRS.latlon
benignapz.sp <- st_as_sf(benignapz) #251120 update to sf/ditch rgdal
st_crs(benignapz.sp) <- CRS.latlon
##change projection
benignapz.tr <- sf::st_transform(benignapz.sp, CRS.new)

#extensionapz.sp <- extensionapz
##declare what kind of projection they are in
#proj4string(extensionapz.sp) <- CRS.latlon
extensionapz.sp <- st_as_sf(extensionapz) #251120 update to sf/ditch rgdal
st_crs(extensionapz.sp) <- CRS.latlon
##change projection
extensionapz.tr <- sf::st_transform(extensionapz.sp, CRS.new)

# ## ACTIVE SLOW zones ----
# #queries for active dmas/acoustic protection zones and their bounds. Also identifies if zones are within time period where they could be extended.
# ##a lot of variables are named with "dma" even if they refer to both kinds of protection zones (acoustic vs. visual) because the original code for DMAs was modifed in 2020 to accommodate the new acoustic protection zone program
# 
# ## declare function ----
# 
# print("beg activedma")
# querytoshape <- function(x) {
#   #dmaquery = x
#   
#   vector5 <- x %>%
#     filter(VERTEX == 1) %>%
#     mutate(VERTEX = replace(VERTEX, VERTEX == 1, 5))
#   
#   DMADF <- rbind(x, vector5)
#   
#   DMADF <- DMADF %>%
#     arrange(ID, VERTEX) %>%
#     dplyr::select(ID, LON, LAT, -VERTEX)
#   
#   idDMA <- split(DMADF, DMADF$ID)
#   idDMA <- lapply(idDMA, function(x) {
#     x["ID"] <- NULL
#     x
#   })
#   
#   DMAcoord <- lapply(idDMA, Polygon)
#   DMAcoord_ <-
#     lapply(seq_along(DMAcoord), function(i)
#       Polygons(list(Polygon(
#         DMAcoord[[i]], hole = as.logical(NA)
#       )), ID = names(idDMA)[i]))
#   SpatialPolygons(DMAcoord_)
# }
# 
# if (isolate(criteria$loc) == 'Network') {
# 
#   ##action code dataframe to join with results ofvtrigger analysis later
#   actioncode <- "select *
#             from action"
#   actioncodedf <- sqlQuery(cnxn, actioncode)
#   actioncodedf$ID <- as.numeric(actioncodedf$ID)
#   
#   #query all relevant DMAs & APZs
#   ##the to_date(to_char) for the expdate is necessary, otherwise, the modayr seconds default to 00:00:00 and dmas that expire on the same day are still included
#   activedmasql <-
#     paste0(
#       "select dmainfo.name, to_char(dmainfo.expdate, 'YYYY-MM-DD') as expdate, ID, to_char((dmainfo.expdate - 7), 'YYYY-MM-DD') as ext, dmainfo.triggertype
#                   from dmainfo
#                   where to_date('",
#       MODAYR,
#       "', 'YYYY-MM-DD') < to_date(to_char(EXPDATE, 'YYYY-MM-DD'),'YYYY-MM-DD')
#                     and to_date('",
#       MODAYR,
#       "', 'YYYY-MM-DD') > to_date(to_char(TRIGGERDATE, 'YYYY-MM-DD'),'YYYY-MM-DD')
#                      and (cancelled not like 'cancel%' or cancelled is null);"
#     )
#   
#   actdma <- sqlQuery(cnxn, activedmasql)
#   print(actdma)
#   
# } else {
#   print("else")
#   actioncodedf <- data.frame(
#     ID = c(1, 2, 4, 5),
#     ACTION = c(
#       "ONLY 1 OR 2",
#       "IN EXISTING PROTECTION ZONE",
#       "DMA",
#       "DMA EXTENSION"
#     )
#   )
#   
#   dmacsv <-
#     read.csv(
#       './example_data/example_data_slowzones.csv',
#       header = T,
#       stringsAsFactors = F
#     )
#   dmacsv$EXPDATE <- as.Date(dmacsv$EXPDATE)
#   dmacsv$TRIGGERDATE <- dmy_hms(dmacsv$TRIGGERDATE)
#   
#   actdma <- dmacsv %>%
#     filter(EXPDATE > MODAYR & TRIGGERDATE < MODAYR) %>%
#     distinct(NAME, EXPDATE, ID, TRIGGERTYPE) %>%
#     mutate(EXT = EXPDATE - days(7))
#   
# }
# 
# print(actdma)
# 
# actdma <- actdma %>%
#   group_by(NAME) %>%
#   arrange(EXPDATE) %>%
#   top_n(n = 1, EXPDATE) %>% #selects for later dma if there are two technically active because of an extension
#   ungroup()
# 
# print(actdma)
# 
# ##do we have ANY dmas?
# if (nrow(actdma) == 0) {
#   benigndma <- SpatialPolygons(list(fakeslowzone))
#   extensiondma <- SpatialPolygons(list(fakeslowzone))
#   benignapz <- SpatialPolygons(list(fakeslowzone))
#   extensionapz <- SpatialPolygons(list(fakeslowzone))
#   dmanamesexp <- "None"
#   
# } else {
# 
#   ## report ----
#   
#   repdma <- actdma %>%
#     mutate(
#       EXPDATE = format(EXPDATE, format = "%d %B %Y"),
#       sentence = paste(NAME, "expires on", EXPDATE)
#     )
#   print(repdma)
#   dmalist <- as.list(repdma$sentence)
#   dmanamesexp <-
#     stringi::stri_replace_last(do.call("paste", c(dmalist, sep = ", ")), fixed = ",", ", and")
#   
#   ## Extend or not? ----
# 
#   if (isolate(criteria$loc) == 'Network') {
# 
#     ##dma/apz bounds
#     actdma_boundssql <- paste0(
#       "select dmacoords.ID, vertex, lat, lon
#                      from dmainfo
#                      left outer join dmacoords on dmainfo.ID = dmacoords.ID
#                      where to_date('",
#       MODAYR,
#       "', 'YYYY-MM-DD') < to_date(to_char(EXPDATE, 'YYYY-MM-DD'),'YYYY-MM-DD')
#                      and to_date('",
#       MODAYR,
#       "', 'YYYY-MM-DD') > to_date(to_char(TRIGGERDATE, 'YYYY-MM-DD'),'YYYY-MM-DD')
#                      and (cancelled not like 'cancel%' or cancelled is null);"
#     )
#     
#     actdma_bounds <- sqlQuery(cnxn, actdma_boundssql)
#     print(actdma_bounds)
#     
#     actdmadf <- actdma %>%
#       left_join(actdma_bounds, by = "ID")
#     
#   } else {
#     actdma_bounds <-
#       read.csv(
#         './example_data/example_data_slowzones.csv',
#         header = T,
#         stringsAsFactors = F
#       )
#     
#     actdmadf <- actdma_bounds %>%
#       dplyr::select(ID, VERTEX, LAT, LON) %>%
#       right_join(actdma, by = "ID")
#     
#     print("second else")
#   }
#   actdmadf$EXT <- ymd(actdmadf$EXT)
#   actdmadf$ID <- as.numeric(actdmadf$ID)
#   actdmadf$VERTEX <- as.numeric(actdmadf$VERTEX)
#   actdmadf$LAT <- as.numeric(actdmadf$LAT)
#   actdmadf$LON <- as.numeric(actdmadf$LON)
#   print(actdmadf)
# 
#   ## Categorizing current protection zones ----
#   
#   ## DMA ## For visual sightings
#   ## dmas not up for extension, nothing happens = noth
#   dmanoth <- actdmadf %>%
#     filter(EXT > MODAYR & TRIGGERTYPE == "v") %>%
#     dplyr::select(ID, VERTEX, LAT, LON)
#   
#   ##dmas up for extension
#   dmaext <- actdmadf %>%
#     filter(EXT <= MODAYR & TRIGGERTYPE == "v") %>%
#     dplyr::select(ID, VERTEX, LAT, LON)
#   
#   ## Acoustic Protection Zones (APZ) ----
#   ##apz not up for extension, nothing (noth) happens
#   apznoth <- actdmadf %>%
#     filter(EXT > MODAYR & TRIGGERTYPE == "a") %>%
#     dplyr::select(ID, VERTEX, LAT, LON)
#   print("apznoth")
#   print(apznoth)
#   ## apz up for extension
#   apzext <- actdmadf %>%
#     filter(EXT <= MODAYR & TRIGGERTYPE == "a") %>%
#     dplyr::select(ID, VERTEX, LAT, LON)
#   print("apzext")
#   print(apzext)
# 
#   #evaluate DMAs ----
#   #benign
#   
#   if (nrow(dmanoth) == 0) {
#     benigndma <- SpatialPolygons(list(fakeslowzone))
#   } else {
#     benigndma <- querytoshape(dmanoth)
#   }
#   
#   print(benigndma)
#   #evaluate extension triggers DMAs
#   
#   if (nrow(dmaext) == 0) {
#     extensiondma <- SpatialPolygons(list(fakeslowzone))
#   } else {
#     ##all polys together
#     extensiondma <- querytoshape(dmaext)
#     print(extensiondma)
# 
#     ##change projection, extension
#     ##distinct polys for DMA extension
#     IDlist <- as.list(unique(dmaext$ID))
#     names(IDlist) <- unique(dmaext$ID)
#     
#     for (i in names(IDlist)) {
#       a <- dmaext %>%
#         filter(ID == i)
#       
#       b <- querytoshape(a)
#       
#       if (exists("extdma_name") == FALSE &
#           exists("extdma_list") == FALSE) {
#         extdma_name <- list(a)
#         extdma_list <- list(b)
#         
#       } else if (length(extdma_name) > 0 & length(extdma_list) > 0) {
#         extdma_name <- list.append(extdma_name, a)
#         extdma_list <- list.append(extdma_list, b) #rlist::list.append
#       }
#     }
#     
#     names(extdma_name) <- names(IDlist)
#     names(extdma_list) <- names(IDlist)
#     
#     ## declare projection
#     extdma.sp <- extdma_list
#     print("ext dma")
#     print(extdma.sp)
#     #made this a loop because I could not figure out how to apply it over a list 3/21
#     for (i in names(IDlist)) {
#       proj4string(extdma.sp[[i]]) <- CRS.latlon
#     }
#     
#     ##change projection
#     extdma.tr <-
#       lapply(extdma.sp, function (x) {
#         sf::st_transform(x, CRS.new)
#       })
#   }
#   
#   # evaluate APZs ----
#   #benign
#   
#   if (nrow(apznoth) == 0) {
#     benignapz <- SpatialPolygons(list(fakeslowzone))
#   } else {
#     benignapz <- querytoshape(apznoth)
#   }
#   
#   #evaluate extension triggers APZs
#   
#   if (nrow(apzext) == 0) {
#     extensionapz <- SpatialPolygons(list(fakeslowzone))
#   } else {
#     ##all polys together
#     extensionapz <- querytoshape(apzext)
#     
#     ##distinct polys for APZ extension
#     IDlist <- as.list(unique(apzext$ID))
#     names(IDlist) <- unique(apzext$ID)
#     
#     for (i in names(IDlist)) {
#       a <- apzext %>%
#         filter(ID == i)
#       
#       b <- querytoshape(a)
#       
#       if (exists("extapz_name") == FALSE &
#           exists("extapz_list") == FALSE) {
#         extapz_name <- list(a)
#         extapz_list <- list(b)
#         
#       } else if (length(extapz_name) > 0 & length(extapz_list) > 0) {
#         extapz_name <- list.append(extapz_name, a)
#         extapz_list <- list.append(extapz_list, b) #rlist::list.append
#       }
#     }
#     
#     names(extapz_name) <- names(IDlist)
#     names(extapz_list) <- names(IDlist)
#     
#     ## declare projection
#     extapz.sp <- extapz_list
#     print("ext apz")
#     print(extapz.sp)
#     #made this a loop because I could not figure out how to apply it over a list 3/21
#     for (i in names(IDlist)) {
#       proj4string(extapz.sp[[i]]) <- CRS.latlon
#     }
#     
#     ##change projection
#     extapz.tr <-
#       lapply(extapz.sp, function (x) {
#         sf::st_transform(x, CRS.new)
#       })
#   }
#   
# } # 73
# 
# ##change projection ----
# 
# #DMA
# 
# benigndma.sp <- benigndma
# ##declare what kind of projection thy are in
# proj4string(benigndma.sp) <- CRS.latlon
# ##change projection
# benigndma.tr <- sf::st_transform(benigndma.sp, CRS.new)
# 
# extensiondma.sp <- extensiondma
# ##declare what kind of projection thy are in
# proj4string(extensiondma.sp) <- CRS.latlon
# ##change projection
# extensiondma.tr <- sf::st_transform(extensiondma.sp, CRS.new)
# 
# #APZ
# 
# benignapz.sp <- benignapz
# ##declare what kind of projection thy are in
# proj4string(benignapz.sp) <- CRS.latlon
# ##change projection
# benignapz.tr <- sf::st_transform(benignapz.sp, CRS.new)
# 
# extensionapz.sp <- extensionapz
# ##declare what kind of projection thy are in
# proj4string(extensionapz.sp) <- CRS.latlon
# ##change projection
# extensionapz.tr <- sf::st_transform(extensionapz.sp, CRS.new)
