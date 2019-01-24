
disable("dmaup")
disable("kml")
    
    ###########
    
observeEvent(input$eval,{
  
      dmaevaldate<-input$sasdate    
      print(dmaevaldate)
      
      fish <- odbcConnect("fish", uid="RIGHTWHALESIGHT", pwd="m1les0ch5+??",believeNRows=FALSE)
      
      datesql<-paste0("select SIGHTDATE,GROUPSIZE,LAT,LON,SPECIES_CERT,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,DUPLICATE,CATEGORY,ACTION,OBSERVER_COMMENTS,OBSERVER_PEOPLE,EDITOR_COMMENTS,REPORTER_PEOPLE,OBSERVER_PLATFORM,REPORTER_PLATFORM,RID,REVIEWED,ID,OBSERVER_ORG,OOD,REPORTER_ORG,WHALEALERT
                from rightwhalesight.sas
                where trunc(sightdate) = to_date('",dmaevaldate,"','YYYY-MM-DD');")
      
      dailyeg<-sqlQuery(fish,datesql)
      print(dailyeg)
      
      ###############
      ##SMA evaluation
      ########
      smapath<-"//net/mmi/Fieldwrk/Aerials/Shiny/NARWSS_shinyapp/SMA ind shp"
      MODA<-unique(format(dmaevaldate, "%m-%d"))
      ##egtable & egsas kept seperate like this for now, need to investigate more about how these tables are used between dma and narwss apps 11/20/2018 lmc
      egtable<-dailyeg%>%
        dplyr::rename("DateTime" = "SIGHTDATE","LATITUDE" = "LAT", "LONGITUDE" = "LON", "GROUP_SIZE" = "GROUPSIZE", "ID_RELIABILITY" = "SPECIES_CERT")
      
      egsas<-egtable %>% 
        dplyr::select(DateTime,GROUP_SIZE,LATITUDE,LONGITUDE,ID_RELIABILITY,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION,OBSERVER_COMMENTS,OBSERVER_PEOPLE,OBSERVER_PLATFORM,OBSERVER_ORG)
      print(egsas)
      egsas$DateTime<-strftime(egsas$DateTime,'%Y-%m-%d %H:%M:%S')
      
      source('//net/mmi/Fieldwrk/Aerials/2018/Flights/edit_data/Shiny 2018/scripts/sma_2018.R', local = TRUE)$value
      source('//net/mmi/Fieldwrk/Aerials/2018/Flights/edit_data/Shiny 2018/scripts/activedma_2018.R', local = TRUE)$value
      
      if(nrow(egtable) == 0){
        ##if no Eg:
        output$error1<-renderText({"No right whales were reported for this day"})
        }
      else {
      
        #####
      ##egtable for SAS
        output$error1<-renderText({""})
      
     
      source('//net/mmi/Fieldwrk/Aerials/2018/Flights/edit_data/Shiny 2018/scripts/action & dma_2018.R', local = TRUE)$value
      
      
      #############
      
      }
})  #7
      

  