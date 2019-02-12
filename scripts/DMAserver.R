
disable("dmaup")
disable("dmareport")
disable("kml")

loc<-"Network"
    ###########
    
observeEvent(input$query,{
  
      dmaevaldate<-input$sasdate    
      print(dmaevaldate)
      
      fish <- odbcConnect("fish", uid="RIGHTWHALESIGHT", pwd="m1les0ch5+??",believeNRows=FALSE)
      
      datesql<-paste0("select SIGHTDATE,GROUPSIZE,LAT,LON,SPECIES_CERT,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION,OBSERVER_PEOPLE,OBSERVER_PLATFORM,OBSERVER_ORG,REPORTER_PEOPLE,REPORTER_PLATFORM,REPORTER_ORG,WHALEALERT,OBSERVER_COMMENTS
                from rightwhalesight.sas
                where trunc(sightdate) = to_date('",dmaevaldate,"','YYYY-MM-DD')
                      and LAT > 36.5;") 
      
      dailyeg<-sqlQuery(fish,datesql)
      
      dailyeg$SIGHTDATE<-ymd_hms(dailyeg$SIGHTDATE, tz = "America/New_York")
      dailyeg$LAT<-sprintf("%.5f",round(dailyeg$LAT, digits = 5))
      dailyeg$LON<-sprintf("%.5f",round(dailyeg$LON, digits = 5))
      
      dailyeg[] <- lapply(dailyeg, as.character)
      
      dailyeg<-dailyeg%>%
        mutate(Select = TRUE)%>%
        dplyr::select(Select, everything())
      
      dailyeghot<-rhandsontable(dailyeg)%>%
        hot_col("SIGHTDATE", width = 150)
        
      output$dailyeghot = renderRHandsontable({dailyeghot})

})
      
observeEvent(input$eval,{
      
      dmaevaldate<-input$sasdate
      
      egtable = hot_to_r(input$dailyeghot)

      egtable<-egtable%>%
        filter(Select == TRUE)%>%
        dplyr::select(-Select)

      egtable$LAT<-as.numeric(egtable$LAT)
      egtable$LON<-as.numeric(egtable$LON)
      ###############
      ##SMA evaluation
      ########
      smapath<-"./SMA ind shp"
      MODA<-unique(format(dmaevaldate, "%m-%d"))
      MODAYR<-unique(dmaevaldate, "%m-%d")
      ##egtable & egsas kept seperate like this for now, need to investigate more about how these tables are used between dma and narwss apps 11/20/2018 lmc
      egtable<-egtable%>%
        dplyr::rename("DateTime" = "SIGHTDATE","LATITUDE" = "LAT", "LONGITUDE" = "LON", "GROUP_SIZE" = "GROUPSIZE", "ID_RELIABILITY" = "SPECIES_CERT")
      
      egsas<-egtable %>% 
        dplyr::select(DateTime,GROUP_SIZE,LATITUDE,LONGITUDE,ID_RELIABILITY,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION,OBSERVER_COMMENTS,OBSERVER_PEOPLE,OBSERVER_PLATFORM,OBSERVER_ORG)
      print(egsas)
      egsas$DateTime<-strftime(egsas$DateTime,'%Y-%m-%d %H:%M:%S')
      
      source('./scripts/oracleaccess.R', local = TRUE)$value
      source('./scripts/sma.R', local = TRUE)$value
      source('./scripts/activedma.R', local = TRUE)$value
      
      if(nrow(egtable) == 0){
        ##if no Eg:
        output$error1<-renderText({"No right whales were reported for this day"})
        }
    else {
      
        #####
      ##egtable for SAS
        output$error1<-renderText({""})
        
        month1<-month.abb[month(as.character(egsas$DateTime[1]))]
        day1<-day(egsas$DateTime[1])
        year1<-year(egsas$DateTime[1])
        date1<-paste0(day1,' ',month1,' ',year1)
     
      source('./scripts/action & dma.R', local = TRUE)$value
      
      
      #############
      
      }
})  #24
    

  