disable("eval")
disable("dmaup")
disable("dmareport")
disable("dmaletter")
disable("kml")

loc<-"Network"
source('./scripts/reactive.R', local = TRUE)$value
    ###########
    
observeEvent(input$query,{
      #clear anything that happened before
      blank<-data.frame()
      output$dmanameout<-renderTable({blank})
      output$dmacoord<-renderTable({blank})
      output$sasdma = renderLeaflet({print(blank)})
      output$egsastabout<-renderTable({blank},  striped = TRUE)
      dmainfovalues<-""
      dmacoordvalues<-""
      egvalues<-""
  
      dmaevaldate<-input$sasdate    
      print(dmaevaldate)
      
      source('./scripts/oracleaccess.R', local = TRUE)$value
      
      ######################
      ## VISUAL SIGHTINGS ##
      ######################
      
      #and LAT > 36.5

    if (input$sig_acou == 'Visual Sightings'){ 
      
      datesql<-paste0("select rightwhalesight.sas.ID, SIGHTDATE,GROUPSIZE,LAT,LON,SPECIES_CERT,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,rightwhalesight.action.action,OBSERVER_PEOPLE,OBSERVER_PLATFORM,OBSERVER_ORG,REPORTER_PEOPLE,REPORTER_PLATFORM,REPORTER_ORG,WHALEALERT,OBSERVER_COMMENTS
                from rightwhalesight.sas,rightwhalesight.action
                where trunc(sightdate) = to_date('",dmaevaldate,"','YYYY-MM-DD')
                      and SPECIES_CERT = 3
                      and rightwhalesight.sas.action = rightwhalesight.action.ID   
                order by ID;") 
      
      dailyeg<-sqlQuery(cnxn,datesql)
      #########################
      ## Acoustic Detections ##
      #########################           
    } else if (input$sig_acou == 'Acoustic Detections'){
      
      datesql<-paste0("select *
                from rightwhalesight.acoustic_detections
                where trunc(DATETIME_ET) = to_date('",dmaevaldate,"','YYYY-MM-DD')
                order by DATETIME_ET;") 
      
      dailyeg<-sqlQuery(cnxn,datesql)
      
    }
      
      if(nrow(dailyeg) == 0){
        ##if no Eg:
        output$error1<-renderText({"No right whales were reported for this day"})
        disable("eval")
        output$dailyeghot = renderRHandsontable({blank})
      }
      else if (nrow(dailyeg) > 0 & input$sig_acou == 'Visual Sightings'){
        output$error1<-renderText({""})  
        
        dailyeg$SIGHTDATE<-ymd_hms(dailyeg$SIGHTDATE, tz = "America/New_York")
        dailyeg$LAT<-sprintf("%.5f",round(dailyeg$LAT, digits = 5))
        dailyeg$LON<-sprintf("%.5f",round(dailyeg$LON, digits = 5))
        
        dailyeg[] <- lapply(dailyeg, as.character)
        
        dailyeg<-dailyeg%>%
          mutate(Select = TRUE)%>%
          dplyr::select(Select, everything())
        
        dailyeghot<-rhandsontable(dailyeg,readOnly = TRUE)%>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
          hot_cols(columnSorting = TRUE)%>%
          hot_col("SIGHTDATE", width = 150)%>%
          hot_col("Select", readOnly = FALSE)
        
        output$dailyeghot = renderRHandsontable({dailyeghot})
        enable("eval")
        
      } else if (nrow(dailyeg) > 0 & input$sig_acou == 'Acoustic Detections'){
        
        output$error1<-renderText({""})  
        
        dailyeg$DATETIME_ET<-ymd_hms(dailyeg$DATETIME_ET, tz = "America/New_York")

        dailyeg$LAT<-sprintf("%.5f",round(dailyeg$LAT, digits = 5))
        dailyeg$LON<-sprintf("%.5f",round(dailyeg$LON, digits = 5))
        dailyeg[] <- lapply(dailyeg, as.character)
        
        dailyeg<-dailyeg%>%
          mutate(Select = TRUE)%>%
          dplyr::select(Select, everything())
        dailyeghot<-rhandsontable(dailyeg,readOnly = TRUE)%>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
          hot_cols(columnSorting = TRUE)%>%
          hot_col("DATETIME_ET", width = 150)%>%
          hot_col("Select", readOnly = FALSE)
        
        output$dailyeghot = renderRHandsontable({dailyeghot})
        enable("eval")
        
      }
      
})
      
observeEvent(input$eval,{
  
  blank<-data.frame()
  output$dmanameout<-renderTable({blank})
  output$dmacoord<-renderTable({blank})
  output$sasdma = renderLeaflet({print(blank)})
  output$egsastabout<-renderTable({blank},  striped = TRUE)
  dmainfovalues<-""
  dmacoordvalues<-""
  egvalues<-""

      dmaevaldate<-input$sasdate
      print(dmaevaldate)
      egtable = hot_to_r(input$dailyeghot)
      print(egtable)
      egtable<-egtable%>%
        filter(Select == TRUE)%>%
        dplyr::select(-Select)
      
      print(egtable)
      
      egtable$LAT<-as.numeric(egtable$LAT)
      egtable$LON<-as.numeric(egtable$LON)
      ###############
      ##SMA evaluation
      ########
      smapath<-"./SMA ind shp"
      MODA<-unique(format(dmaevaldate, "%m-%d"))
      MODAYR<-unique(dmaevaldate, "%m-%d")

      if (input$sig_acou == 'Visual Sightings'){
      ##egtable & egsas kept seperate like this for now, need to investigate more about how these tables are used between dma and narwss apps 11/20/2018 lmc
        egtable<-egtable%>%
          dplyr::rename("DateTime" = "SIGHTDATE","LATITUDE" = "LAT", "LONGITUDE" = "LON", "GROUP_SIZE" = "GROUPSIZE", "ID_RELIABILITY" = "SPECIES_CERT")
      
        egsas<-egtable %>% 
          dplyr::select(ID,DateTime,GROUP_SIZE,LATITUDE,LONGITUDE,ID_RELIABILITY,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION,OBSERVER_COMMENTS,OBSERVER_PEOPLE,OBSERVER_PLATFORM,OBSERVER_ORG)
        
        egsas$DateTime<-strftime(egsas$DateTime,'%Y-%m-%d %H:%M:%S')
        
        criteria$DMAapp<-"vissig"
        
        } else if (input$sig_acou == 'Acoustic Detections'){
        
        egtable<-egtable%>%
          dplyr::rename("DateTime" = "DATETIME_ET", "LATITUDE" = "LAT", "LONGITUDE" = "LON")%>%
          mutate(ACTION_NEW = NA, GROUP_SIZE = 3, ID_RELIABILITY = 3)
        
        egsas<-egtable
        
        egsas$DateTime<-strftime(egsas$DateTime,'%Y-%m-%d %H:%M:%S')
        
        criteria$DMAapp<-"acoudet"
        
        }
      
      ##############
      ## FAKE DMA ##
      ##############
      fakedma<-data.frame(
        long = c(-71,-71,-71,-71,-71),
        lat = c(42,42,42,42,42))
      
      fakedma<-Polygons(list(Polygon(fakedma, hole=as.logical(NA))), ID = 1)
      ##############
      
      source('./scripts/sma.R', local = TRUE)$value
      source('./scripts/activedma.R', local = TRUE)$value
      
      #####
      ##egtable for SAS
        output$error1<-renderText({""})
        
        date_formats$month1<-month.abb[month(as.character(egsas$DateTime[1]))]
        date_formats$month2<-format.Date(egsas$DateTime[1], "%m")
        date_formats$day1<-format.Date(egsas$DateTime[1], "%d")
        date_formats$year1<-year(egsas$DateTime[1])
        date_formats$date1<-paste0(date_formats$day1,' ',date_formats$month1,' ',date_formats$year1)
        
        #this is for the user input for the observer in the dma letter
        criteria$triggrptrue<-TRUE
        print(criteria$DMAapp)
        
      source('./scripts/action & dma.R', local = TRUE)$value
      
      #############
      
      
})  #24

source('./scripts/oracleaccess.R', local = TRUE)$value
source('./scripts/input_sas.R', local = TRUE)$value
source('./scripts/input_dma.R', local = TRUE)$value
    

  