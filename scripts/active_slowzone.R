## ACTIVE DMAs and acoustic protection zones ----

#queries for active dmas/acoustic protection zones and their bounds. Also identifies if zones are within time period where they could be extended.
##a lot of variables are named with "dma" even if they refer to both kinds of protection zones (acoustic vs. visual) because the original code for DMAs was modifed in 2020 to accomodate the new acoustic protection zone program

## declare function ----

print("beg activedma")
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

  ##action code dataframe to join with results ofvtrigger analysis later
  actioncode <- "select *
            from action"
  actioncodedf <- sqlQuery(cnxn, actioncode)
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
                     and (cancelled not like 'cancel%' or cancelled is null);"
    )
  
  actdma <- sqlQuery(cnxn, activedmasql)
  print(actdma)
  
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
      header = T,
      stringsAsFactors = F
    )
  dmacsv$EXPDATE <- as.Date(dmacsv$EXPDATE)
  dmacsv$TRIGGERDATE <- dmy(dmacsv$TRIGGERDATE)
  
  actdma <- dmacsv %>%
    filter(EXPDATE > MODAYR & TRIGGERDATE < MODAYR) %>%
    dplyr::select(NAME, EXPDATE, ID, TRIGGERTYPE) %>%
    mutate(EXT = EXPDATE - days(7))
  
}

print(actdma)

actdma <- actdma %>%
  group_by(NAME) %>%
  arrange(EXPDATE) %>%
  top_n(n = 1, EXPDATE) %>% #selects for later dma if there are two technically active because of an extension
  ungroup()

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
                     and (cancelled not like 'cancel%' or cancelled is null);"
    )
    
    actdma_bounds <- sqlQuery(cnxn, actdma_boundssql)
    print(actdma_bounds)
    
    actdmadf <- actdma %>%
      left_join(actdma_bounds, by = "ID")
    
  } else {
    actdma_bounds <-
      read.csv(
        './Aerial and SLOW zone data/DMACOORDS export 05Aug2021.csv',
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
  print("apznoth")
  print(apznoth)
  ## apz up for extension
  apzext <- actdmadf %>%
    filter(EXT <= MODAYR & TRIGGERTYPE == "a") %>%
    dplyr::select(ID, VERTEX, LAT, LON)
  print("apzext")
  print(apzext)

  #evaluate DMAs ----
  #benign
  
  if (nrow(dmanoth) == 0) {
    benigndma <- SpatialPolygons(list(fakeslowzone))
  } else {
    benigndma <- querytoshape(dmanoth)
  }
  
  print(benigndma)
  #evaluate extension triggers DMAs
  
  if (nrow(dmaext) == 0) {
    extensiondma <- SpatialPolygons(list(fakeslowzone))
  } else {
    ##all polys together
    extensiondma <- querytoshape(dmaext)
    print(extensiondma)

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
    
    ##change projection
    extdma.tr <-
      lapply(extdma.sp, function (x) {
        spTransform(x, CRS.new)
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
    
    ##change projection
    extapz.tr <-
      lapply(extapz.sp, function (x) {
        spTransform(x, CRS.new)
      })
  }
  
} # 73

##change projection ----

#DMA

benigndma.sp <- benigndma
##declare what kind of projection thy are in
proj4string(benigndma.sp) <- CRS.latlon
##change projection
benigndma.tr <- spTransform(benigndma.sp, CRS.new)

extensiondma.sp <- extensiondma
##declare what kind of projection thy are in
proj4string(extensiondma.sp) <- CRS.latlon
##change projection
extensiondma.tr <- spTransform(extensiondma.sp, CRS.new)

#APZ

benignapz.sp <- benignapz
##declare what kind of projection thy are in
proj4string(benignapz.sp) <- CRS.latlon
##change projection
benignapz.tr <- spTransform(benignapz.sp, CRS.new)

extensionapz.sp <- extensionapz
##declare what kind of projection thy are in
proj4string(extensionapz.sp) <- CRS.latlon
##change projection
extensionapz.tr <- spTransform(extensionapz.sp, CRS.new)
