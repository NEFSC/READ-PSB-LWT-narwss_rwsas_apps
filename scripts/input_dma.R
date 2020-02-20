observeEvent(input$dmaup,{
  disable("dmaup")
  print("dma button pressed")
  
  dmareportmap<-fitBounds(dma_react$sasdma,min(dma_react$dmacoord$`Lon (Decimal Degrees)`)+0.5, min(dma_react$dmacoord$`Lat (Decimal Degrees)`)-0.5, max(dma_react$dmacoord$`Lon (Decimal Degrees)`)-0.5, max(dma_react$dmacoord$`Lat (Decimal Degrees)`)+0.5)
  htmlwidgets::saveWidget(dmareportmap, "temp.html", selfcontained = FALSE)
  webshot::webshot("temp.html", file = paste0(date_formats$date1,"_dmamap.png"))#,cliprect = bounds)
  
  ###################  
  ##dma info upload##
  ###################
  
  maxidsql<-"SELECT max(ID)
  FROM RIGHTWHALESIGHT.DMAINFO"
  
  maxid<-sqlQuery(cnxn,maxidsql)
  maxid<-as.integer(maxid)
  print(maxid)
  
  trig<-unique(date(dma_react$alldmas$TRIGGERDATE))
  print(trig)
  trig<-force_tz(trig, tzone = "America/New_York")
  triggerdateletter<-format(trig, "%B %d, %Y")
  ##expiration date
  exp<-trig
  hour(exp)<-0
  minute(exp)<-0
  second(exp)<-01
  exp <- exp + days(16)
  
  expletter<-format(exp, "%H:%M:%S %Z %B %d, %Y")
  
  ###
  alldmas<-dma_react$alldmas
  alldmas$ID<-as.numeric(alldmas$ID) #a number to add to
  print(alldmas)
  #### this is where new dmas and extensions need to be together in alldmas
  dmainfo<-alldmas%>%
    dplyr::mutate(OLDID = ID,
           ID = ID + maxid,
           EXPDATE = paste0("to_timestamp('",exp,"', 'YYYY-MM-DD HH24:MI:SS')"),
           TRIGGERDATE = paste0("to_timestamp('",TRIGGERDATE,"', 'YYYY-MM-DD HH24:MI:SS')"),
           STARTDATE = paste0("to_timestamp('",ymd_hms(Sys.time()),"', 'YYYY-MM-DD HH24:MI:SS')"))%>%
    dplyr::select(OLDID,ID,NAME,EXPDATE,TRIGGERDATE,INITOREXT,TRIGGERORG,STARTDATE,TRIGGER_GROUPSIZE)
  print("past dmainfo")
  ##################################
  ##list of dmas being extended
  
  if (dma_react$extdfname != ""){
    extended_dmainfo<-as.list(dma_react$extdfname$ID)
    
    for (i in extended_dmainfo){
      #print(i)
      sqlQuery(cnxn, paste0("UPDATE DMAINFO
                            SET CANCELLED = 'extended'
                            WHERE ID = ",i,";"))}
    }
  ##################################
  
  if (criteria$DMAapp == "acoudet"){
    dmainfo<-dmainfo%>%
      mutate(TRIGGERTYPE = 'a')
  } else {
    dmainfo<-dmainfo%>%
      mutate(TRIGGERTYPE = 'v')
  }
  
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
    
    sqlQuery(cnxn, paste0("INSERT INTO DMAINFO(ID, NAME, EXPDATE, TRIGGERDATE, INITOREXT, TRIGGERORG, STARTDATE, TRIGGERGROUPSIZE, TRIGGERTYPE)
                          VALUES(", dmainfovalues,");"))
  }
  
  ####################
  ##dma coord upload##
  ####################
  dma_react$dmacoord$ID<-as.numeric(dma_react$dmacoord$ID)
  dmacoordinsert<-left_join(dmainfo,dma_react$dmacoord, by = c("OLDID" = "ID"))%>%
    dplyr::select(ID,VERTEX,`Lat (Decimal Degrees)`,`Lon (Decimal Degrees)`)%>%
    mutate(ROWNUMBER = 999999)
  
  for (i in 1:nrow(dmacoordinsert)){
    
    dmacoordvalues <- paste0(apply(dmacoordinsert[i,], 1, function(x) paste0("'", paste0(x, collapse = "', '"), "'")), collapse = ", ")
    sqlQuery(cnxn, paste0("INSERT INTO DMACOORDS(ID, VERTEX, LAT, LON, ROWNUMBER)
                          VALUES(", dmacoordvalues,");"))
  }
  
  print("dma end")

  enable("dmareport")
  if("i" %in% dma_react$dmanameout$INITOREXT){enable("kml")}
  enable("dmaletter")
  
  output$dmareport<-downloadHandler(
    filename = paste0(date_formats$day1,date_formats$month1,date_formats$year1,"_PotentialDMA_Report.pdf"),
    content = function(file) {
      
      if (criteria$loc == 'Network'){
        tempReport<-file.path("./scripts/DMAReport.Rmd")
      } else if (criteria$loc == 'Local'){
        tempReport<-file.path(paste0(inputpath,"/DMAReport.Rmd"))
      } 
      
      print(tempReport)
      file.copy("DMAReport.Rmd", tempReport, overwrite = TRUE)
      params<-list(dmanameselect = dmanameselect, date1 = date_formats$date1, expletter = expletter, egsastab = sas_react$egsastab, dmanameout = dma_react$dmanameout, dmacoord = dma_react$dmacoord)
      
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
  print(dma_react$alldmas)
  letterdate<-format(Sys.Date(), '%B %d, %Y')
  
  if (criteria$DMAapp != "acoudet"){
    
    numberword<-dma_react$alldmas%>%
      dplyr::select(-ID,-NAME)%>%
      distinct(INITOREXT, TRIGGER_GROUPSIZE, TRIGGERDATE, TRIGGERORG)%>%
      mutate(triggerword = numbers2words(TRIGGER_GROUPSIZE))
    print(numberword)
    triggerword<-as.list(numberword$triggerword)
    triggerword<-do.call("paste", c(triggerword, sep = ", "))
    triggerword<-sub(",([^,]*)$", " and\\1", triggerword)  
    
    letterdirect<-direction(dmanameselect)
    
    dmalandmark<-dma_react$alldmas%>%
      mutate(landmark = dplyr::case_when(
        grepl('Bay of Fundy Canada',NAME) ~ 'Bay of Fundy Canada',
        grepl('Portland ME',NAME) ~ 'Portland ME',
        grepl('Portsmouth NH',NAME) ~ 'Portsmouth NH',
        grepl('Boston MA',NAME) ~ 'Boston MA',
        grepl('Providence RI',NAME) ~ 'Providence RI',
        grepl('New York NY',NAME) ~ 'New York NY',
        grepl('Atlantic City NJ',NAME) ~ 'Atlantic City NJ',
        grepl('Virginia Beach VA',NAME) ~ 'Virginia Beach VA',
        grepl("Martha's Vineyard MA",NAME) ~ "Martha's Vineyard MA",
        grepl('Nantucket MA',NAME) ~ 'Nantucket MA',
        grepl('Cape Cod MA',NAME) ~ 'Cape Cod MA',
        grepl('Cape Cod Bay',NAME) ~ 'Cape Cod Bay'
      ))
    
    landmark_ls<-as.list(unique(dmalandmark$landmark))
    landmark<-do.call("paste", c(landmark_ls, sep = ", "))
    landmark<-sub(",([^,]*)$", " and\\1", landmark)  
    
  } else if (criteria$DMAapp == "acoudet"){
    
    triggerword<-''
    letterdirect<-'acoustically'
    landmark<-''
    
  }
  
  letterbounds<-left_join(dma_react$alldmas,dma_react$dmacoord, by = "ID")
  print(letterbounds)
  #initial/extension
  ie<-as.list(unique(letterbounds$INITOREXT))
  
  neworextlet<-NULL
  
  if ('e' %in% ie & 'i' %in% ie){
    neworextlet<-"Since whales were detected both in a region where there are no protections in place, as well as within a region where the protections are due to expire in a week or less,
    we recommend a DMA be initiated/extended at the following bounds:"
  } else if ('i' %in% ie){
    neworextlet<-"Since no protections are in place in this region at this time, we recommend a DMA be initiated that is bounded by the following:"
  } else if ('e' %in% ie){
    neworextlet<-"Since the current protections in this region are due to expire in a week or less, we recommend an extension of the DMA(s) that is/are bounded by the following:"
  }  
  
  if(1 %in% letterbounds$ID){
    DMA1<-letterbounds%>%filter(ID == 1)
    title1<-unique(DMA1$NAME)
    print(typeof(title1))
    NLat1<-unique(DMA1$`Lat (Degree Minutes)`[DMA1$`Lat (Decimal Degrees)` == max(DMA1$`Lat (Decimal Degrees)`)])
    SLat1<-unique(DMA1$`Lat (Degree Minutes)`[DMA1$`Lat (Decimal Degrees)` == min(DMA1$`Lat (Decimal Degrees)`)])
    WLon1<-unique(DMA1$`Lon (Degree Minutes)`[DMA1$`Lon (Decimal Degrees)` == max(DMA1$`Lon (Decimal Degrees)`)])
    ELon1<-unique(DMA1$`Lon (Degree Minutes)`[DMA1$`Lon (Decimal Degrees)` == min(DMA1$`Lon (Decimal Degrees)`)])
    
    print(paste(title1,NLat1,SLat1,WLon1,ELon1))}
  
  if(2 %in% letterbounds$ID){
    DMA2<-letterbounds%>%filter(ID == 2)
    title2<-unique(DMA2$NAME)
    title2<-as.character(title2)
    print(typeof(title2))
    NLat2<-unique(DMA2$`Lat (Degree Minutes)`[DMA2$`Lat (Decimal Degrees)` == max(DMA2$`Lat (Decimal Degrees)`)])
    SLat2<-unique(DMA2$`Lat (Degree Minutes)`[DMA2$`Lat (Decimal Degrees)` == min(DMA2$`Lat (Decimal Degrees)`)])
    WLon2<-unique(DMA2$`Lon (Degree Minutes)`[DMA2$`Lon (Decimal Degrees)` == max(DMA2$`Lon (Decimal Degrees)`)])
    ELon2<-unique(DMA2$`Lon (Degree Minutes)`[DMA2$`Lon (Decimal Degrees)` == min(DMA2$`Lon (Decimal Degrees)`)])
    
    print(paste(title2,NLat2,SLat2,WLon2,ELon2))
  } else {
    title2 = ""
    title2<-as.character(title2)
    NLat2 = ""
    SLat2 = ""
    WLon2 = ""
    ELon2 = ""
  }
  
  if(3 %in% letterbounds$ID){
    DMA3<-letterbounds%>%filter(ID == 3)
    title3<-unique(DMA3$NAME)
    title3<-as.character(title3)
    NLat3<-unique(DMA3$`Lat (Degree Minutes)`[DMA3$`Lat (Decimal Degrees)` == max(DMA3$`Lat (Decimal Degrees)`)])
    SLat3<-unique(DMA3$`Lat (Degree Minutes)`[DMA3$`Lat (Decimal Degrees)` == min(DMA3$`Lat (Decimal Degrees)`)])
    WLon3<-unique(DMA3$`Lon (Degree Minutes)`[DMA3$`Lon (Decimal Degrees)` == max(DMA3$`Lon (Decimal Degrees)`)])
    ELon3<-unique(DMA3$`Lon (Degree Minutes)`[DMA3$`Lon (Decimal Degrees)` == min(DMA3$`Lon (Decimal Degrees)`)])
    
    print(paste(title3,NLat3,SLat3,WLon3,ELon3))
  } else {
    title3 = ""
    title3<-as.character(title3)
    NLat3 = ""
    SLat3 = ""
    WLon3 = ""
    ELon3 = ""
  }
  
  if(4 %in% letterbounds$ID){
    DMA4<-letterbounds%>%filter(ID == 4)
    title4<-unique(DMA4$NAME)
    title4<-as.character(title4)
    NLat4<-unique(DMA4$`Lat (Degree Minutes)`[DMA4$`Lat (Decimal Degrees)` == max(DMA4$`Lat (Decimal Degrees)`)])
    SLat4<-unique(DMA4$`Lat (Degree Minutes)`[DMA4$`Lat (Decimal Degrees)` == min(DMA4$`Lat (Decimal Degrees)`)])
    WLon4<-unique(DMA4$`Lon (Degree Minutes)`[DMA4$`Lon (Decimal Degrees)` == max(DMA4$`Lon (Decimal Degrees)`)])
    ELon4<-unique(DMA4$`Lon (Degree Minutes)`[DMA4$`Lon (Decimal Degrees)` == min(DMA4$`Lon (Decimal Degrees)`)])
    
    print(paste(title4,NLat4,SLat4,WLon4,ELon4))
  } else {
    title4 = ""
    title4<-as.character(title4)
    NLat4 = ""
    SLat4 = ""
    WLon4 = ""
    ELon4 = ""
  }
  
  
  output$dmaletter <- downloadHandler(
    
    filename = function() {
      paste0("DMA ",date_formats$year1,date_formats$month2,date_formats$day1," ",dmanameselect,".pdf")},
    
    content = function(file) {
      
      if (criteria$loc == 'Network'){
        tempReport<-file.path("./scripts/DMALetter.Rmd")
      } else if (criteria$loc == 'Local'){
        tempReport<-file.path(paste0(inputpath,"/DMALetter.Rmd"))
      }        
      
      file.copy("DMALetter.Rmd", tempReport, overwrite = TRUE)
      
      ##choose group that saw the most to be the trigger org
      if (criteria$triggrptrue == TRUE){
        if (criteria$DMAapp == "acoudet"){

          acou_obs<-dma_react$egsas%>%
            distinct(PLATFORM_NAME,INSTITUTION,URL)%>%
            mutate(URL = replace(URL, grepl('Woods Ho', INSTITUTION) == TRUE, 'robots4whales.whoi.edu'))
          print(str(acou_obs))
          
          platform_name_ls<-as.list(unique(acou_obs$PLATFORM_NAME))
          platform_obs<-do.call("paste", c(platform_name_ls, sep = ", "))
          platform_obs<-sub(",([^,]*)$", " and\\1", platform_obs)
          
          institution_ls<-as.list(unique(acou_obs$INSTITUTION))
          institution_obs<-do.call("paste", c(institution_ls, sep = ", "))
          institution_obs<-sub(",([^,]*)$", " and\\1", institution_obs)
          
          url_ls<-as.list(unique(acou_obs$URL))
          url_obs<-do.call("paste", c(url_ls, sep = ", "))
          url_obs<-sub(",([^,]*)$", " and\\1", url_obs)
          print(url_obs)
          
          #Stellwagen Slocum glider operated by the Woods Hole Oceanographic Institution (see robots4whales.whoi.edu for more information)
          
          observer<- paste0(platform_obs," operated by the ",institution_obs," (see ",url_obs," for more information)")
          observer<-as.character(observer)
          print(observer)
          
        } else {
          observer<-input$triggrp
          print(observer)
        }
      } else {
        observer<-"NOAA North Atlantic Right Whale Sighting Survey"
      }
      
      params<-list(letterdate = letterdate, date1 = date_formats$date1, triggerdateletter = triggerdateletter, triggerword = triggerword, 
                   letterdirect = letterdirect, landmark = landmark, observer = observer, neworextlet = neworextlet, 
                   title1 = title1, NLat1 = NLat1, SLat1 = SLat1, WLon1 = WLon1, ELon1 = ELon1,
                   title2 = title2, NLat2 = NLat2, SLat2 = SLat2, WLon2 = WLon2, ELon2 = ELon2,
                   title3 = title3, NLat3 = NLat3, SLat3 = SLat3, WLon3 = WLon3, ELon3 = ELon3,
                   title4 = title4, NLat4 = NLat4, SLat4 = SLat4, WLon4 = WLon4, ELon4 = ELon4,
                   expletter = expletter)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )})
  
  })#inputdma