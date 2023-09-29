disable("eval")
disable("dmaup")
disable("dmareport")
disable("dmaletter")
disable("kml")

if (file.exists('./scripts/oracleaccess.R') == TRUE) {
  source('./scripts/reactive.R', local = TRUE)$value
  source('./scripts/oracleaccess.R', local = TRUE)$value
  #need criteria$loc for 'action & slowzone' even though you can only do trigger analysis on the server. It separates it from the NARWSS app.
  criteria$loc <- 'Network'
  
  observeEvent(input$query, {
    disable("eval")
    disable("dmaup")
    disable("dmareport")
    disable("dmaletter")
    disable("kml")
    #clear anything that happened before
    blank <- data.frame()
    output$dmanameout <- renderTable({
      blank
    })
    output$dmacoord <- renderTable({
      blank
    })
    output$sasdma = renderLeaflet({
      print(blank)
    })
    output$egsastabout <- renderTable({
      blank
    },  striped = TRUE)
    dmainfovalues <- ""
    dmacoordvalues <- ""
    egvalues <- ""
    
    dmaevaldate <- input$sasdate
    print(dmaevaldate)
    
    
    ## VISUAL SIGHTINGS ----
    
    
    if (input$sig_acou == 'Visual Sightings') {
      datesql <-
        paste0(
          "select rightwhalesight.saswmjoin2.ID, SIGHTDATE,GROUPSIZE,LAT,LON,SPECIES_CERT,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,rightwhalesight.action.action,OBSERVER_PEOPLE,OBSERVER_PLATFORM,OBSERVER_ORG,REPORTER_PEOPLE,REPORTER_PLATFORM,REPORTER_ORG,WHALEALERT,OBSERVER_COMMENTS
                from rightwhalesight.saswmjoin2,rightwhalesight.action
                where trunc(sightdate) = to_date('",
          dmaevaldate,
          "','YYYY-MM-DD')
                and rightwhalesight.saswmjoin2.action = rightwhalesight.action.ID
                and SPECIES_CERT = 3
                order by ID" , sep =""
        )
      
      #dailyeg <- sqlQuery(cnxn, datesql)
      dailyeg_q <- dbSendQuery(cnxn, datesql)
      dailyeg<-fetch(dailyeg_q) #HJF sqlQuery replace 13/14 20230626
      
      ## ACOUSTIC DETECTIONS ----
      
    } else if (input$sig_acou == 'Acoustic Detections') {
      source('./scripts/Acoustic_datapull.R', local = TRUE)$value
      
      datesql <- paste0(
        "select *
                from rightwhalesight.acoustic_detections
                where trunc(DATETIME_ET) = to_date('",
        dmaevaldate,
        "','YYYY-MM-DD')
                order by DATETIME_ET", sep="" #HJF added 20230626 and might be why trigger server gets further than rest
      )
      #dailyeg <- sqlQuery(cnxn, datesql)
      dailyeg_q <- dbSendQuery(cnxn, datesql)
      dailyeg <- fetch(dailyeg_q) #HJF sqlQuery replace 14/14 20230626
      
    }
    
    if (nrow(dailyeg) == 0) {
      ##if no Eg:
      output$error1 <-
        renderText({
          "No right whales were reported for this day"
        })
      disable("eval")
      output$dailyeghot = renderRHandsontable({
        blank
      })
    }
    else if (nrow(dailyeg) > 0 &
             input$sig_acou == 'Visual Sightings') {
      output$error1 <- renderText({
        ""
      })
      
      dailyeg$SIGHTDATE <-
        ymd_hms(dailyeg$SIGHTDATE, tz = "America/New_York")
      dailyeg$LAT <- sprintf("%.5f", round(dailyeg$LAT, digits = 5))
      dailyeg$LON <- sprintf("%.5f", round(dailyeg$LON, digits = 5))
      
      dailyeg[] <- lapply(dailyeg, as.character)
      
      dailyeg <- dailyeg %>%
        mutate(Select = TRUE) %>%
        dplyr::select(Select, everything())
      
      dailyeghot <- rhandsontable(dailyeg, readOnly = TRUE) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_cols(columnSorting = TRUE) %>%
        hot_col("SIGHTDATE", width = 150) %>%
        hot_col("Select", readOnly = FALSE)
      
      output$dailyeghot = renderRHandsontable({
        dailyeghot
      })
      enable("eval")
      
    } else if (nrow(dailyeg) > 0 &
               input$sig_acou == 'Acoustic Detections') {
      output$error1 <- renderText({
        ""
      })
      
      dailyeg$DATETIME_ET <-
        ymd_hms(dailyeg$DATETIME_ET, tz = "America/New_York")
      
      dailyeg$LAT <- sprintf("%.5f", round(dailyeg$LAT, digits = 5))
      dailyeg$LON <- sprintf("%.5f", round(dailyeg$LON, digits = 5))
      dailyeg[] <- lapply(dailyeg, as.character)
      
      dailyeg <- dailyeg %>%
        mutate(Select = TRUE) %>%
        dplyr::select(Select, everything())
      dailyeghot <- rhandsontable(dailyeg, readOnly = TRUE) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_cols(columnSorting = TRUE) %>%
        hot_col("DATETIME_ET", width = 150) %>%
        hot_col("Select", readOnly = FALSE)
      
      output$dailyeghot = renderRHandsontable({
        dailyeghot
      })
      enable("eval")
      
    }
    
  })
  
  observeEvent(input$eval, {
    blank <- data.frame()
    output$dmanameout <- renderTable({
      blank
    })
    output$dmacoord <- renderTable({
      blank
    })
    output$sasdma = renderLeaflet({
      print(blank)
    })
    output$egsastabout <- renderTable({
      blank
    },  striped = TRUE)
    dmainfovalues <- ""
    dmacoordvalues <- ""
    egvalues <- ""
    
    dmaevaldate <- input$sasdate
    print(dmaevaldate)
    egtable = hot_to_r(input$dailyeghot)
    print(egtable)
    egtable <- egtable %>%
      filter(Select == TRUE) %>%
      dplyr::select(-Select)
    
    print(egtable)
    
    egtable$LAT <- as.numeric(egtable$LAT)
    egtable$LON <- as.numeric(egtable$LON)
    
    ## SMA evaluation ----
    
    smapath <- "./SMA ind shp"
    MODA <- unique(format(dmaevaldate, "%m-%d"))
    MODAYR <- unique(dmaevaldate, "%m-%d")
    
    if (input$sig_acou == 'Visual Sightings') {
      ##egtable & egsas kept separate like this for now, need to investigate more about how these tables are used between trigger and narwss apps 11/20/2018 lmc
      egtable <- egtable %>%
        dplyr::rename(
          "DateTime" = "SIGHTDATE",
          "LATITUDE" = "LAT",
          "LONGITUDE" = "LON",
          "GROUP_SIZE" = "GROUPSIZE",
          "ID_RELIABILITY" = "SPECIES_CERT"
        )
      
      egsas <- egtable %>%
        dplyr::select(
          ID,
          DateTime,
          GROUP_SIZE,
          LATITUDE,
          LONGITUDE,
          ID_RELIABILITY,
          MOMCALF,
          FEEDING,
          DEAD,
          SAG,
          ENTANGLED,
          CATEGORY,
          ACTION,
          OBSERVER_COMMENTS,
          OBSERVER_PEOPLE,
          OBSERVER_PLATFORM,
          OBSERVER_ORG
        )
      
      egsas$DateTime <-
        strftime(egsas$DateTime, '%Y-%m-%d %H:%M:%S')
      
      criteria$DMAapp <- "vissig"
      
    } else if (input$sig_acou == 'Acoustic Detections') {
      egtable <- egtable %>%
        dplyr::rename(
          "DateTime" = "DATETIME_ET",
          "LATITUDE" = "LAT",
          "LONGITUDE" = "LON"
        ) %>%
        mutate(
          ACTION_NEW = NA,
          GROUP_SIZE = 3,
          ID_RELIABILITY = 3
        )
      
      egsas <- egtable
      
      egsas$DateTime <-
        strftime(egsas$DateTime, '%Y-%m-%d %H:%M:%S')
      
      criteria$DMAapp <- "acoudet"
      
    }
    
    ## FAKE slowzone ----
    
    fakeslowzone <- data.frame(long = c(-71,-71,-71,-71,-71),
                               lat = c(42, 42, 42, 42, 42))
    
    fakeslowzone <-
      Polygons(list(Polygon(fakeslowzone, hole = as.logical(NA))), ID = 1)
    
    source('./scripts/sma.R', local = TRUE)$value
    source('./scripts/active_slowzone.R', local = TRUE)$value
    
    ##egtable for SAS
    output$error1 <- renderText({
      ""
    })
    
    date_formats$month1 <-
      month.abb[month(as.character(egsas$DateTime[1]))]
    date_formats$month2 <- format.Date(egsas$DateTime[1], "%m")
    date_formats$day1 <- format.Date(egsas$DateTime[1], "%d")
    date_formats$year1 <- year(egsas$DateTime[1])
    date_formats$date1 <-
      paste0(date_formats$day1,
             ' ',
             date_formats$month1,
             ' ',
             date_formats$year1)
    
    #this is for the user input for the observer in the slow zone letter
    criteria$triggrptrue <- TRUE
    print(criteria$DMAapp)
    
    source('./scripts/action & slowzone.R', local = TRUE)$value
    
  })
  
  source('./scripts/input_sas.R', local = TRUE)$value
  source('./scripts/input_slowzone.R', local = TRUE)$value
  
} else {
  disable("query")
}
