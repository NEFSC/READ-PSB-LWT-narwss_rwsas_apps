
disable("edittable")
disable("save")
disable("report")
disable("sas")
disable("dmaup")
disable("dmareport")
disable("kml")
disable("dmaletter")

source('./scripts/reactive.R', local = TRUE)$value
criteria$triggrptrue <- FALSE
criteria$DMAapp<-"rwsurv"

observeEvent(input$rawupload,{
  
    survey_date=input$sd
    
    yr<-substr(survey_date,1,2)
    
    if (input$filepathway == 'Network'){
      path<-paste0('//net/mmi/Fieldwrk/Aerials/20',yr,'/Flights/edit_data/')
    } else if (input$filepathway == 'Local'){
      path<-input$filepathinput
      print(path)
      print(typeof(path))
    }
    
    rawed<-input$rawedits
    vis = 0:35
    beau = seq(0, 12, by = 0.1)
    cloud = c(1:4,9)
    glare = 0:3
    qual = c("e","g","m","p","x")
    ahead = 0:360
    obspos = c("L","C","R")
    ang = c(0:89,89.1,89.2,89.3,89.4,89.5,89.6,89.7,89.8,89.9,90)
    cue = c(1:5,8,9)
    ######
    #Files
    #####
    if (input$sd == "") {
        output$error<-renderText({"Enter a survey date"})
    } else if (!file.exists(paste0(path,survey_date,'/',survey_date,'.gps'))) { 
        output$error2<-renderText({"Uh oh! Those files can't be found! Double check your connection to the network, the local network pathway, your data, and/or your survey date entry."})         
    } else if (rawed == "Yes" && !file.exists(paste0(path,survey_date,'/','effsig_',survey_date,'.csv'))){
        output$error2<-renderText({"No initial eff/sig edits were saved."})    
    } else {
      output$error2<-renderText({""})
      if (rawed == "Yes"){
        eff_sig<-as.data.frame(read.csv(paste0(path,survey_date,'/','effsig_',survey_date,'.csv'),header=TRUE, stringsAsFactors = FALSE))
      } else if (rawed == "No"){
        
########################        

        #path<-"//net/mmi/Fieldwrk/Aerials/2019/Flights/edit_data/"
        #survey_date<-199996
        
        ## gps ##
        
        gps<-as.data.frame(read.csv(paste0(path,survey_date,'/',survey_date,'.gps'), header=FALSE, stringsAsFactors = FALSE))
        names(gps)<-c('DATETIME_UTC','LATITUDE','LONGITUDE','SPEED','HEADING','ALTITUDE','T1')
        
        ## effort ##
        
        eff_list<-list.files(paste0(path,survey_date,'/'), "*\\.eff")
        eff_files<-lapply(eff_list, function (x) read.csv(paste0(path,survey_date,'/',x), header=FALSE, stringsAsFactors = FALSE))
        eff_all<-do.call(rbind, eff_files)
        names(eff_all)<-c('Trans','Line','ABE','DATETIME_UTC','BEAUFORT','VISIBILTY_NM','CLOUD_CODE','GLARE_L','GLARE_R',
                       'QUALITY_L','QUALITY_R','EFFORT_COMMENTS')#,'EDIT1','EDIT2','EDIT3','T1') ##T1=trash
      
        ## sightings ##
        
        sig_list<-list.files(paste0(path,survey_date,'/'), "*\\.sig")
        
        sig_files<-lapply(sig_list, function (x) 
          
          if (file.info(paste0(path,survey_date,'/',x))$size > 0) {
          read.csv(paste0(path,survey_date,'/',x), header=FALSE, stringsAsFactors = FALSE)
            
        } else if (file.info(paste0(path,survey_date,'/',x))$size == 0)  {
          data.frame(V1=NA,V2=NA,V3=NA,V4=NA,V5=NA,V6=NA,V7=NA,V8=NA,V9=NA,V10=NA,V11=NA,V12=NA,V13=NA,V14=NA,V15=NA,V16=NA,V17=NA,V18=NA,V19=NA,V20=NA,V21=NA)
        })
        
        sig_all<-do.call(rbind, sig_files)
        names(sig_all)<-c('Trans','Line','SIGHTING_NUMBER','DATETIME_UTC','OBSERVER','ANGLE','SPCODE','GROUP_SIZE','CUE',
                       'ACTUAL_HEADING','T1','T2','SIGHTING_COMMENTS','OBS_POSITION','LATITUDE','LONGITUDE','T3','CALVES',
                       'T4','B1_FINAL_CODE','T5') ##T1=trash
        sig_all$LATITUDE<-as.double(sig_all$LATITUDE)
        sig_all$LONGITUDE<-as.double(sig_all$LONGITUDE)
      
      #############
      #Drop Columns
      #############
        
      eff<-eff_all%>%
        filter(ABE != "E")%>%  ##remove rows with "E" from eff
        dplyr::select(-Trans,-Line,-ABE)
      
      sig<-sig_all%>%
        filter(!is.na(Trans))%>% ##remove rows with no sig data
        dplyr::select(-Trans,-Line,-T1,-T2,-T3,-T4,-T5)

      gps<-gps%>%
        dplyr::select(-T1,-ALTITUDE)
      
      ######
      eff$BEAUFORT<-as.numeric(eff$BEAUFORT)
      sig$ACTUAL_HEADING<-as.numeric(sig$ACTUAL_HEADING)
      
      ##format DateTime
      gps$DATETIME_UTC<-dmy_hms(gps$DATETIME_UTC)
      eff$DATETIME_UTC<-dmy_hms(eff$DATETIME_UTC)
      sig$DATETIME_UTC<-dmy_hms(sig$DATETIME_UTC)
      
      ###############
      ###############
      #DATETIME link to position, spead, and heading data
      #package data.table documentation
      ######
      
      setDT(eff)[,  LATITUDE := setDT(gps)[eff, LATITUDE, on = "DATETIME_UTC", roll = "nearest"]]
      setDT(eff)[,  LONGITUDE := setDT(gps)[eff, LONGITUDE, on = "DATETIME_UTC", roll = "nearest"]]
      setDT(eff)[,  SPEED := setDT(gps)[eff, SPEED, on = "DATETIME_UTC", roll = "nearest"]]
      setDT(eff)[,  HEADING := setDT(gps)[eff, HEADING, on = "DATETIME_UTC", roll = "nearest"]]
      
      setDT(sig)[,  SPEED := setDT(gps)[sig, SPEED, on = "DATETIME_UTC", roll = "nearest"]]
      setDT(sig)[,  HEADING := setDT(gps)[sig, HEADING, on = "DATETIME_UTC", roll = "nearest"]]
      
      #merge effort and sighting files
      ######
      ##merge and add columns
      ##the below came out of an error thrown 8/3/2019 where eff$LONGITUDE were characters for some reason
      eff$LATITUDE<-as.double(eff$LATITUDE)
      eff$LONGITUDE<-as.double(eff$LONGITUDE)
      
      eff_sig<-merge(eff, sig, by=c("DATETIME_UTC","LATITUDE","LONGITUDE","SPEED","HEADING"), all=TRUE)
      eff_sig<-eff_sig%>%
        filter(!is.na(DATETIME_UTC))
      eff_sig<-data.frame(eff_sig,
                          ALTITUDE = NA,
                          B2_FINAL_CODE = NA,
                          B3_FINAL_CODE = NA,
                          B4_FINAL_CODE = NA,
                          B5_FINAL_CODE = NA,
                          PHOTOS = NA,
                          EDIT1 = NA,
                          EDIT2 = NA,
                          EDIT3 = NA)
      
      #eff_sig<-cbind(eff_sig, ALTITUDE, B2_FINAL_CODE, B3_FINAL_CODE, B4_FINAL_CODE, B5_FINAL_CODE, PHOTOS, EDIT1, EDIT2, EDIT3)
      
      ##reorder for editing ease
      eff_sig <- eff_sig %>% dplyr::select(DATETIME_UTC, LATITUDE, LONGITUDE, ALTITUDE, HEADING, SPEED, VISIBILTY_NM, BEAUFORT,
                                           CLOUD_CODE, GLARE_L, GLARE_R, QUALITY_L, QUALITY_R, EFFORT_COMMENTS, SIGHTING_NUMBER, 
                                           SPCODE, GROUP_SIZE, CALVES, ACTUAL_HEADING, OBSERVER, OBS_POSITION, ANGLE, CUE, 
                                           B1_FINAL_CODE, B2_FINAL_CODE, B3_FINAL_CODE, B4_FINAL_CODE, B5_FINAL_CODE, 
                                           PHOTOS, SIGHTING_COMMENTS, EDIT1, EDIT2, EDIT3)
      #####
      ##format for rhandsontable
      eff_sig$LATITUDE<-sprintf("%.5f",round(eff_sig$LATITUDE, digits = 5))
      eff_sig$LONGITUDE<-sprintf("%.5f",round(eff_sig$LONGITUDE, digits = 5))
      ##round lat/lon for show
    }  
    ######
      
      eff_sig$DATETIME_UTC<-as.character.Date(eff_sig$DATETIME_UTC)
      eff_sig$LATITUDE<-as.character(eff_sig$LATITUDE)
      eff_sig$LONGITUDE<-as.character(eff_sig$LONGITUDE)
      eff_sig$ALTITUDE<-as.character(eff_sig$ALTITUDE)
      eff_sig$VISIBILTY_NM<-as.character(eff_sig$VISIBILTY_NM)
      eff_sig$BEAUFORT<-as.character(eff_sig$BEAUFORT)
      eff_sig$CLOUD_CODE<-as.character(eff_sig$CLOUD_CODE)
      eff_sig$GLARE_L<-as.character(eff_sig$GLARE_L)
      eff_sig$GLARE_R<-as.character(eff_sig$GLARE_R)
      eff_sig$QUALITY_L<-as.character(eff_sig$QUALITY_L)
      eff_sig$QUALITY_R<-as.character(eff_sig$QUALITY_R)
      eff_sig$EFFORT_COMMENTS<-as.character(eff_sig$EFFORT_COMMENTS)
      eff_sig$SIGHTING_NUMBER<-as.character(eff_sig$SIGHTING_NUMBER)
      eff_sig$SPCODE<-as.character(eff_sig$SPCODE)
      eff_sig$GROUP_SIZE<-as.character(eff_sig$GROUP_SIZE)
      eff_sig$CALVES<-as.character(eff_sig$CALVES)
      eff_sig$ACTUAL_HEADING<-as.character(eff_sig$ACTUAL_HEADING)
      eff_sig$OBSERVER<-as.character(eff_sig$OBSERVER)
      eff_sig$OBS_POSITION<-as.character(eff_sig$OBS_POSITION)
      eff_sig$ANGLE<-as.character(eff_sig$ANGLE)
      eff_sig$CUE<-as.character(eff_sig$CUE)
      eff_sig$B1_FINAL_CODE<-as.character(eff_sig$B1_FINAL_CODE)
      eff_sig$B2_FINAL_CODE<-as.character(eff_sig$B2_FINAL_CODE)
      eff_sig$B3_FINAL_CODE<-as.character(eff_sig$B3_FINAL_CODE)
      eff_sig$B4_FINAL_CODE<-as.character(eff_sig$B4_FINAL_CODE)
      eff_sig$B5_FINAL_CODE<-as.character(eff_sig$B5_FINAL_CODE)
      eff_sig$PHOTOS<-as.character(eff_sig$PHOTOS)
      eff_sig$SIGHTING_COMMENTS<-as.character(eff_sig$SIGHTING_COMMENTS)
      eff_sig$EDIT1<-as.character(eff_sig$EDIT1)
      eff_sig$EDIT2<-as.character(eff_sig$EDIT2)
      eff_sig$EDIT3<-as.character(eff_sig$EDIT3)
      
      eff_sig[is.na(eff_sig)] <- ""
      ##hot formatting
      #####
      handsES<-rhandsontable(eff_sig,readOnly = TRUE)%>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
        hot_cols(columnSorting = TRUE)%>%
        hot_col("DATETIME_UTC", readOnly = FALSE, width = 150)%>%
        ##format only allows up to 4 decimal places
        hot_col("LATITUDE", format = "00.00000")%>% 
        hot_col("LONGITUDE", format = "00.00000")%>%
        hot_col("ALTITUDE", format = "0",readOnly = FALSE)%>%
        hot_col("HEADING", format = "000")%>%
        hot_col("SPEED", format = "000")%>%
        hot_col("VISIBILTY_NM",format = "0",readOnly = FALSE, type = "dropdown", source = vis)%>%
        hot_col("BEAUFORT",format = "0.0",readOnly = FALSE, type = "dropdown", source = beau)%>%
        hot_col("CLOUD_CODE",format = "0",readOnly = FALSE, type = "dropdown", source = cloud)%>%
        hot_col("GLARE_L",format = "0",readOnly = FALSE, type = "dropdown", source = glare)%>%
        hot_col("GLARE_R",format = "0",readOnly = FALSE, type = "dropdown", source = glare)%>%
        hot_col("QUALITY_L",readOnly = FALSE, type = "dropdown", source = qual)%>%
        hot_col("QUALITY_R",readOnly = FALSE, type = "dropdown", source = qual)%>%
        hot_col("SIGHTING_NUMBER",format = "0",readOnly = FALSE)%>%
        hot_col("SPCODE",readOnly = FALSE)%>%
        hot_col("GROUP_SIZE",format = "0",readOnly = FALSE)%>%
        hot_col("CALVES",readOnly = FALSE)%>%
        hot_col("ACTUAL_HEADING", readOnly = FALSE, format = "000", type = "dropdown", source = ahead)%>%
        hot_col("OBSERVER",readOnly = FALSE)%>%
        hot_col("OBS_POSITION",readOnly = FALSE, type = "dropdown", source = obspos)%>%
        hot_col("ANGLE",readOnly = FALSE, type = "dropdown", source = ang)%>%
        hot_col("CUE",readOnly = FALSE, type = "dropdown", source = cue)%>%
        hot_col("B1_FINAL_CODE",format = "00",readOnly = FALSE)%>%
        hot_col("B2_FINAL_CODE",format = "00",readOnly = FALSE)%>%
        hot_col("B3_FINAL_CODE",format = "00",readOnly = FALSE)%>%
        hot_col("B4_FINAL_CODE",format = "00",readOnly = FALSE)%>%
        hot_col("B5_FINAL_CODE",format = "00",readOnly = FALSE)%>%
        hot_col("PHOTOS",format = "0",readOnly = FALSE)%>%
        hot_col("EFFORT_COMMENTS",readOnly = FALSE)%>%
        hot_col("SIGHTING_COMMENTS",readOnly = FALSE)%>%
        hot_col("EDIT1",readOnly = FALSE)%>%
        hot_col("EDIT2",readOnly = FALSE)%>%
        hot_col("EDIT3",readOnly = FALSE)
      
      output$handsES = renderRHandsontable({handsES})
      enable("edittable")  
    } 
  })
  
  observeEvent(input$edittable,{
    ###########
    if (input$sd == "") {
      output$error<-renderText({"You must edit your raw eff and sig files first"})
    } else {
      
      output$error<-renderText({""})
      eff_sig2 = hot_to_r(input$handsES)
      
      survey_date<-input$sd
      yr<-substr(survey_date,1,2)
      
      if (input$filepathway == 'Network'){
        path<-paste0('//net/mmi/Fieldwrk/Aerials/20',yr,'/Flights/edit_data/')
        criteria$loc<-"Network"
      } else if (input$filepathway == 'Local'){
        path<-input$filepathinput
        criteria$loc<-"Local"
      }
      
      write.csv(eff_sig2, paste0(path,survey_date,'/','effsig_',survey_date,'.csv'), na = '', row.names = FALSE)
      ######
      ##reformat out of hot
      eff_sig2$DATETIME_UTC<-ymd_hms(eff_sig2$DATETIME_UTC)
      eff_sig2$ALTITUDE<-as.numeric(eff_sig2$ALTITUDE)
      eff_sig2$VISIBILTY_NM<-as.numeric(eff_sig2$VISIBILTY_NM)
      eff_sig2$BEAUFORT<-as.numeric(eff_sig2$BEAUFORT)
      eff_sig2$CLOUD_CODE<-as.numeric(eff_sig2$CLOUD_CODE)
      eff_sig2$GLARE_L<-as.numeric(eff_sig2$GLARE_L)
      eff_sig2$GLARE_R<-as.numeric(eff_sig2$GLARE_R)
      eff_sig2$SIGHTING_NUMBER<-as.numeric(eff_sig2$SIGHTING_NUMBER)
      eff_sig2$GROUP_SIZE<-as.numeric(eff_sig2$GROUP_SIZE)
      eff_sig2$CALVES<-as.numeric(eff_sig2$CALVES)
      eff_sig2$ACTUAL_HEADING<-as.numeric(eff_sig2$ACTUAL_HEADING)
      eff_sig2$ANGLE<-as.numeric(eff_sig2$ANGLE)
      eff_sig2$CUE<-as.numeric(eff_sig2$CUE)
      eff_sig2$B1_FINAL_CODE<-as.character(eff_sig2$B1_FINAL_CODE)
      eff_sig2$B2_FINAL_CODE<-as.character(eff_sig2$B2_FINAL_CODE)
      eff_sig2$B3_FINAL_CODE<-as.character(eff_sig2$B3_FINAL_CODE)
      eff_sig2$B4_FINAL_CODE<-as.character(eff_sig2$B4_FINAL_CODE)
      eff_sig2$B5_FINAL_CODE<-as.character(eff_sig2$B5_FINAL_CODE)
      eff_sig2$PHOTOS<-as.numeric(eff_sig2$PHOTOS)
      
      eff_sig2$SPCODE[eff_sig2$SPCODE == ''] <- NA
      
      gps2<-as.data.frame(read.csv(paste0(path,survey_date,'/',survey_date,'.gps'), header=FALSE, stringsAsFactors = FALSE))
      names(gps2)<-c('DATETIME_UTC','LATITUDE','LONGITUDE','SPEED','HEADING','ALTITUDE','T1')
      gps2$DATETIME_UTC<-dmy_hms(gps2$DATETIME_UTC)
      gps2$T1<-NULL
      gps2$ALTITUDE<-NULL
      
      #DATETIME link to position, spead, and heading data
      #package data.table documentation
      ######
      setDT(eff_sig2)[,  LATITUDE := setDT(gps2)[eff_sig2, LATITUDE, on = "DATETIME_UTC", roll = "nearest"]]
      setDT(eff_sig2)[,  LONGITUDE := setDT(gps2)[eff_sig2, LONGITUDE, on = "DATETIME_UTC", roll = "nearest"]]
      setDT(eff_sig2)[,  SPEED := setDT(gps2)[eff_sig2, SPEED, on = "DATETIME_UTC", roll = "nearest"]]
      setDT(eff_sig2)[,  HEADING := setDT(gps2)[eff_sig2, HEADING, on = "DATETIME_UTC", roll = "nearest"]]
      
      #############
      #GPS filter
      #############
      ##get bin list using seq for every 8 seconds
      gpsbin <- cut(gps2$DATETIME_UTC, breaks = c(seq(from = gps2$DATETIME_UTC[1], to = gps2$DATETIME_UTC[nrow(gps2)], by = 8)))
      ##bind bin list to gps
      gpsplus<-cbind(gps2, gpsbin)
      ##order by DateTime and rank
      gpsrank<- gpsplus %>% arrange(DATETIME_UTC, gpsbin) %>% group_by(gpsbin) %>% mutate(rank=rank(DATETIME_UTC, ties.method = "first"))
      ##select for 1st in the bin
      gpsfil<-gpsrank %>% filter(rank == 1)
      
      #############
      
      f<-merge(eff_sig2, gpsfil, by=c("DATETIME_UTC","LATITUDE","LONGITUDE","SPEED","HEADING"), all=TRUE)
      f<-f[order(f$DATETIME_UTC, -f$EFFORT_COMMENTS),]
      ##########
      #ALTITUDE#
      ##########
      
      f$ALTITUDE = na.locf(f$ALTITUDE, na.rm = FALSE)
      f$ALTITUDE[which(is.na(f$ALTITUDE))]<-1000
      
      ##
      #SIGHTING_NUMBER
      #####
      ##
      #Id reliability
      #####
      ID_RELIABILITY = NULL
      
      for (i in 1:nrow(f)) 
        if (!is.na(f$SPCODE[i]) && grepl('psi',f$SIGHTING_COMMENTS[i])) {
          ID_RELIABILITY[i] = 2
        } else if (!is.na(f$SPCODE[i]) && !grepl('psi',f$SIGHTING_COMMENTS[i])) {
          ID_RELIABILITY[i] = 3
        } else {
          ID_RELIABILITY[i] = NA
        }
      
      f<-cbind(f,ID_RELIABILITY)
      
      #####
      #Photos
      #####
      
      PH = NULL
      
      for (i in 1:nrow(f)) 
        if (!is.na(f$SPCODE[i]) & grepl('no ph',f$SIGHTING_COMMENTS[i])) {
          PH[i] = 1
        } else if (!is.na(f$SPCODE[i]) && (grepl('thru',f$SIGHTING_COMMENTS[i]) | (grepl('ph',f$SIGHTING_COMMENTS[i])))) {
          PH[i] = 2
        } else if (!is.na(f$SPCODE[i])) {
          PH[i] = 1
        } else {
          PH[i] = NA
        }
      
      f$PHOTOS<-PH
      #####
      #True swim direction
      #####
      
      n=360
      
      for (i in 1:nrow(f)) 
        if (!is.na(f$ACTUAL_HEADING[i]) && (f$ACTUAL_HEADING[[i]] + round(f$HEADING[[i]], digits = 0) <= n)) {
          f$ACTUAL_HEADING[[i]] = f$ACTUAL_HEADING[[i]] + round(f$HEADING[[i]], digits = 0)
        } else if (!is.na(f$ACTUAL_HEADING[i]) && (f$ACTUAL_HEADING[[i]] + round(f$HEADING[[i]], digits = 0)) >= n) {
          f$ACTUAL_HEADING[[i]] = f$ACTUAL_HEADING[[i]] + round(f$HEADING[[i]], digits = 0) - n
        } 
      
      #########
      #Weather codes
      #########
      #last observed carried forward
      f$QUALITY_L[which(f$QUALITY_L=='')]<-NA
      f$QUALITY_R[which(f$QUALITY_R=='')]<-NA
      
      f$BEAUFORT<-na.locf(f$BEAUFORT, na.rm = FALSE)
      f$VISIBILTY_NM<-na.locf(f$VISIBILTY_NM, na.rm = FALSE)
      f$CLOUD_CODE<-na.locf(f$CLOUD_CODE, na.rm = FALSE)
      f$GLARE_L<-na.locf(f$GLARE_L, na.rm = FALSE)
      f$GLARE_R<-na.locf(f$GLARE_R, na.rm = FALSE)
      f$QUALITY_L<-na.locf(f$QUALITY_L, na.rm = FALSE)
      f$QUALITY_R<-na.locf(f$QUALITY_R, na.rm = FALSE)
      
      #get rid of before on watch
      f<-subset(f, !is.na(BEAUFORT) | (is.na(BEAUFORT) & !is.na(SPCODE)))
      ################
      # Flight Codes #
      ################
      
      ###############
      # FLIGHT_TYPE #
      ###############
      FLIGHT_TYPE = NULL
      for (i in 1:nrow(f))
        if (grepl('beg cs', f$EFFORT_COMMENTS[i])) {
          FLIGHT_TYPE[i] = 31
        } else if (grepl('beg mrnm', f$EFFORT_COMMENTS[i])) {
          FLIGHT_TYPE[i] = 21
        } else if (grepl('beg rwmr', f$EFFORT_COMMENTS[i])) {
          FLIGHT_TYPE[i] = 20
        } else if (grepl('beg str', f$EFFORT_COMMENTS[i])) {
          FLIGHT_TYPE[i] = 12
        } else if (grepl('beg st', f$EFFORT_COMMENTS[i])) {
          FLIGHT_TYPE[i] = 11
        } else if (grepl('beg ha', f$EFFORT_COMMENTS[i])) {
          FLIGHT_TYPE[i] = 10
        } else if (grepl('beg di', f$EFFORT_COMMENTS[i])) {
          FLIGHT_TYPE[i] = 9
        } else if (grepl('beg br', f$EFFORT_COMMENTS[i])) {
          FLIGHT_TYPE[i] = 8
        } else if (grepl('beg ra', f$EFFORT_COMMENTS[i])) {
          FLIGHT_TYPE[i] = 7
        } else if (grepl('beg fi', f$EFFORT_COMMENTS[i])) {
          FLIGHT_TYPE[i] = 6
        } else {
          FLIGHT_TYPE[i] = NA
        }  
      
      f<-cbind(f, FLIGHT_TYPE)
      #########
      #LEGTYPE# 
      #########
      ##searches for ' '+legtype abbreviation+' ' OR ' '+legtype abbreviation ($ means it's the end of the string)
      LEGTYPE = NULL
      for (i in 1:nrow(f))
        if (grepl('break', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 4
        } else if (grepl(' ds | ds$', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 13
        } else if (grepl(' str | str$', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 12
        } else if (grepl(' st | st$', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 11
        } else if (grepl(' ha | ha$', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 10
        } else if (grepl(' di | di$', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 9
        } else if (grepl(' br | br$', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 8
        } else if (grepl(' ra | ra$', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 7
        } else if (grepl(' fi | fi$', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 6
        } else if (grepl(' cr | cr$', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 3
        } else if (grepl(' tr | tr$', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 1
        } else {
          LEGTYPE[i] = NA
        } 
      
      f<-cbind(f, LEGTYPE)
      ############
      # LEGSTAGE #
      ############
      LEGSTAGE = NULL
      for (i in 1:nrow(f))
        if (grepl('off watch', f$EFFORT_COMMENTS[i]) | grepl('no res', f$EFFORT_COMMENTS[i])) {
          LEGSTAGE[i] = 0
        } else if (grepl('beg ', f$EFFORT_COMMENTS[i])) {
          LEGSTAGE[i] = 1
        } else if (grepl('break', f$EFFORT_COMMENTS[i])) {
          LEGSTAGE[i] = 3
        } else if (grepl('res ', f$EFFORT_COMMENTS[i])) {
          LEGSTAGE[i] = 4
        } else if (grepl('end', f$EFFORT_COMMENTS[i])) {
          LEGSTAGE[i] = 5
        } else {
          LEGSTAGE[i] = NA
        } 
      
      f<-cbind(f, LEGSTAGE)
      ################
      # PSB_LEGSTAGE #
      ################
      
      PSB_LEGSTAGE = NULL
      for (i in 1:nrow(f))
        if (grepl('off watch', f$EFFORT_COMMENTS[i]) | grepl('no res', f$EFFORT_COMMENTS[i])) {
          PSB_LEGSTAGE[i] = 0
        } else if (grepl('beg ', f$EFFORT_COMMENTS[i])) {
          PSB_LEGSTAGE[i] = 1
        } else if (grepl('break', f$EFFORT_COMMENTS[i])) {
          PSB_LEGSTAGE[i] = 3
        } else if (grepl('res ', f$EFFORT_COMMENTS[i])) {
          PSB_LEGSTAGE[i] = 4
        } else if (grepl('end', f$EFFORT_COMMENTS[i])) {
          PSB_LEGSTAGE[i] = 5
        } else {
          PSB_LEGSTAGE[i] = NA
        } 
      
      f<-cbind(f, PSB_LEGSTAGE)
      
      #####
      #carry down flight codes
      f$FLIGHT_TYPE<-na.locf(f$FLIGHT_TYPE, na.rm = FALSE)
      f$LEGTYPE<-na.locf(f$LEGTYPE, na.rm = FALSE)
      f$LEGSTAGE<-na.locf(f$LEGSTAGE, na.rm = FALSE)
      f$PSB_LEGSTAGE<-na.locf(f$PSB_LEGSTAGE, na.rm = FALSE)
      
      ##FILL IN OFF EFFORT FLIGHT/LEG CODES
      
      naFT<-unique(f$FLIGHT_TYPE)
      naFT<-na.omit(naFT)
      
      f$FLIGHT_TYPE[which(is.na(f$FLIGHT_TYPE))]<-naFT
      f$LEGTYPE[which(is.na(f$LEGTYPE))]<-1
      f$LEGSTAGE[which(is.na(f$LEGSTAGE))]<-0
      f$PSB_LEGSTAGE[which(is.na(f$PSB_LEGSTAGE))]<-0
      
      ##alt obs, ap, fin est
      f$LEGSTAGE[which(grepl('alt obs', f$SIGHTING_COMMENTS) & f$LEGSTAGE != 0)]<-6
      f$PSB_LEGSTAGE[which(grepl('alt obs', f$SIGHTING_COMMENTS))]<-6
      f$PSB_LEGSTAGE[which(grepl('ap', f$SIGHTING_COMMENTS) & !grepl('no ap', f$SIGHTING_COMMENTS) & !grepl('unsure', f$SIGHTING_COMMENTS))]<-7
      f$PSB_LEGSTAGE[which(grepl('fin est', f$SIGHTING_COMMENTS))]<-9
      
      ##delete off watch lines without sightings or eff comments
      f<-f[!((f$LEGSTAGE == 0 & f$PSB_LEGSTAGE == 0) & (!grepl('off watch', f$EFFORT_COMMENTS) & is.na(f$SPCODE))),]
      
      ######
      #add event number column
      ######
      f[order(as.Date(f$DATETIME_UTC, format = dmy_hms)),]
      EVENT_NUMBER<-1:nrow(f)
      f<-cbind(EVENT_NUMBER, f)
      
      ######
      #Translate species code
      #############
      f$SPCODE[which(f$SPCODE=='cm')]<-'BASH'
      f$SPCODE[which(f$SPCODE=='blsh')]<-'BLSH'
      f$SPCODE[which(f$SPCODE=='bm')]<-'BLWH'
      f$SPCODE[which(f$SPCODE=='tt')]<-'BODO'
      f$SPCODE[which(f$SPCODE=='bh')]<-'BOWH'
      f$SPCODE[which(f$SPCODE=='cg-b')]<-'CG-B'
      f$SPCODE[which(f$SPCODE=='cg-c')]<-'CG-C'
      f$SPCODE[which(f$SPCODE=='cg-u')]<-'CG-U'
      f$SPCODE[which(f$SPCODE=='crsh')]<-'CRSH'
      f$SPCODE[which(f$SPCODE=='de-b')]<-'DE-B'
      f$SPCODE[which(f$SPCODE=='de-g')]<-'DE-G'
      f$SPCODE[which(f$SPCODE=='de-f')]<-'DE-F'
      f$SPCODE[which(f$SPCODE=='de-j')]<-'DE-J'
      f$SPCODE[which(f$SPCODE=='de-o')]<-'DE-O'
      f$SPCODE[which(f$SPCODE=='de-s')]<-'DE-S'
      f$SPCODE[which(f$SPCODE=='de-w')]<-'DE-W'
      f$SPCODE[which(f$SPCODE=='fe-h')]<-'FE-H'
      f$SPCODE[which(f$SPCODE=='fe-s')]<-'FE-S'
      f$SPCODE[which(f$SPCODE=='fg-u')]<-'FG-U'
      f$SPCODE[which(f$SPCODE=='bp')]<-'FIWH'
      f$SPCODE[which(f$SPCODE=='fv-c')]<-'FV-C'
      f$SPCODE[which(f$SPCODE=='fv-f')]<-'FV-F'
      f$SPCODE[which(f$SPCODE=='fv-g')]<-'FV-G'
      f$SPCODE[which(f$SPCODE=='fv-h')]<-'FV-H'
      f$SPCODE[which(f$SPCODE=='fv-l')]<-'FV-L'
      f$SPCODE[which(f$SPCODE=='fv-t')]<-'FV-T'
      f$SPCODE[which(f$SPCODE=='fv-u')]<-'FV-U'
      f$SPCODE[which(f$SPCODE=='gg')]<-'GRAM'
      f$SPCODE[which(f$SPCODE=='ch')]<-'GRTU'
      f$SPCODE[which(f$SPCODE=='pp')]<-'HAPO'
      f$SPCODE[which(f$SPCODE=='hhsh')]<-'HHSH'
      f$SPCODE[which(f$SPCODE=='mn')]<-'HUWH'
      f$SPCODE[which(f$SPCODE=='oo')]<-'KIWH'
      f$SPCODE[which(f$SPCODE=='dc')]<-'LETU'
      f$SPCODE[which(f$SPCODE=='cc')]<-'LOTU'
      f$SPCODE[which(f$SPCODE=='ba')]<-'MIWH'
      f$SPCODE[which(f$SPCODE=='mv-b')]<-'MV-B'
      f$SPCODE[which(f$SPCODE=='mv-c')]<-'MV-C'
      f$SPCODE[which(f$SPCODE=='mv-L')]<-'MV-L'
      f$SPCODE[which(f$SPCODE=='mv-o')]<-'MV-O'
      f$SPCODE[which(f$SPCODE=='mv-s')]<-'MV-S'
      f$SPCODE[which(f$SPCODE=='mv-t')]<-'MV-T'
      f$SPCODE[which(f$SPCODE=='mv-u')]<-'MV-U'
      f$SPCODE[which(f$SPCODE=='nv-l')]<-'NV-L'
      f$SPCODE[which(f$SPCODE=='nv-s')]<-'NV-S'
      f$SPCODE[which(f$SPCODE=='mm')]<-'OCSU'
      f$SPCODE[which(f$SPCODE=='gm')]<-'PIWH'
      f$SPCODE[which(f$SPCODE=='lk')]<-'RITU'
      f$SPCODE[which(f$SPCODE=='eg')]<-'RIWH'
      f$SPCODE[which(f$SPCODE=='rv-l')]<-'RV-L'
      f$SPCODE[which(f$SPCODE=='rv-s')]<-'RV-S'
      f$SPCODE[which(f$SPCODE=='dd')]<-'SADO'
      f$SPCODE[which(f$SPCODE=='scfi')]<-'SCFI'
      f$SPCODE[which(f$SPCODE=='bb')]<-'SEWH'
      f$SPCODE[which(f$SPCODE=='sb')]<-'SONO'
      f$SPCODE[which(f$SPCODE=='ss')]<-'SPDO'
      f$SPCODE[which(f$SPCODE=='pm')]<-'SPWH'
      f$SPCODE[which(f$SPCODE=='pc')]<-'SPWH'
      f$SPCODE[which(f$SPCODE=='sc')]<-'STDO'
      f$SPCODE[which(f$SPCODE=='tu')]<-'TUNS'
      f$SPCODE[which(f$SPCODE=='bw')]<-'UNBW'
      f$SPCODE[which(f$SPCODE=='cw')]<-'UNCW'
      f$SPCODE[which(f$SPCODE=='ud')]<-'UNDO'
      f$SPCODE[which(f$SPCODE=='lf')]<-'UNFI'
      f$SPCODE[which(f$SPCODE=='fs')]<-'UNFS'
      f$SPCODE[which(f$SPCODE=='ua')]<-'UNID'
      f$SPCODE[which(f$SPCODE=='lw')]<-'UNLW'
      f$SPCODE[which(f$SPCODE=='mw')]<-'UNMW'
      f$SPCODE[which(f$SPCODE=='unra')]<-'UNRA'
      f$SPCODE[which(f$SPCODE=='sl')]<-'UNSE'
      f$SPCODE[which(f$SPCODE=='sh')]<-'UNSH'
      f$SPCODE[which(f$SPCODE=='ut')]<-'UNTU'
      f$SPCODE[which(f$SPCODE=='uw')]<-'UNWH'
      f$SPCODE[which(f$SPCODE=='lr')]<-'WBDO'
      f$SPCODE[which(f$SPCODE=='whal')]<-'WHAL'
      f$SPCODE[which(f$SPCODE=='la')]<-'WSDO'
      f$SPCODE[which(f$SPCODECODE=='zoop')]<-'ZOOP'
      
      ########
      #Translate Cue
      ########
      f$CUE[which(f$CUE==1)]<-'ANIMAL'
      f$CUE[which(f$CUE==2)]<-'SPLASH'
      f$CUE[which(f$CUE==3)]<-'BLOW'
      f$CUE[which(f$CUE==4)]<-'FOOTPRINT'
      f$CUE[which(f$CUE==5)]<-'BIRDS'
      f$CUE[which(f$CUE==6)]<-'VESSEL/GEAR'
      f$CUE[which(f$CUE==7)]<-'WINDROWS'
      f$CUE[which(f$CUE==8)]<-'OTHER'
      f$CUE[which(f$CUE==9)]<-'DISTURBANCE'
      
      ######
      #Translate Behaviors
      ######
      #assoc. w/ birds
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='ab')]<-53
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='ab')]<-53
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='ab')]<-53
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='ab')]<-53
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='ab')]<-53
      #assoc. w/ cetaceans
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='ac')]<-51
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='ac')]<-51
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='ac')]<-51
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='ac')]<-51
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='ac')]<-51
      #
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='ap')]<-52
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='ap')]<-52
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='ap')]<-52
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='ap')]<-52
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='ap')]<-52
      #bubbles observed
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='bb')]<-58
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='bb')]<-58
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='bb')]<-58
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='bb')]<-58
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='bb')]<-58
      #body contact non-SAG
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='bc')]<-44
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='bc')]<-44
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='bc')]<-44
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='bc')]<-44
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='bc')]<-44
      #
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='bn')]<-26
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='bn')]<-26
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='bn')]<-26
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='bn')]<-26
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='bn')]<-26
      #breach
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='br')]<-13
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='br')]<-13
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='br')]<-13
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='br')]<-13
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='br')]<-13
      #
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='bv')]<-25
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='bv')]<-25
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='bv')]<-25
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='bv')]<-25
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='bv')]<-25
      #
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='cl')]<-38
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='cl')]<-38
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='cl')]<-38
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='cl')]<-38
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='cl')]<-38
      #defecation
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='de')]<-37
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='de')]<-37
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='de')]<-37
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='de')]<-37
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='de')]<-37
      #dive, fluke not raise
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='dn')]<-23
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='dn')]<-23
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='dn')]<-23
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='dn')]<-23
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='dn')]<-23
      #dive, fluke raised
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='dr')]<-24
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='dr')]<-24
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='dr')]<-24
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='dr')]<-24
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='dr')]<-24
      #dead, stranded
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='ds')]<-01
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='ds')]<-01
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='ds')]<-01
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='ds')]<-01
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='ds')]<-01
      #dead, in water
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='dw')]<-00
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='dw')]<-00
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='dw')]<-00
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='dw')]<-00
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='dw')]<-00
      #apparent feeding
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='fe')]<-54
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='fe')]<-54
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='fe')]<-54
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='fe')]<-54
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='fe')]<-54
      
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='fi')]<-71
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='fi')]<-71
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='fi')]<-71
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='fi')]<-71
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='fi')]<-71
      #
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='ha')]<-72
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='ha')]<-72
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='ha')]<-72
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='ha')]<-72
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='ha')]<-72
      #
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='hb')]<-76
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='hb')]<-76
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='hb')]<-76
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='hb')]<-76
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='hb')]<-76
      #
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='hr')]<-77
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='hr')]<-77
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='hr')]<-77
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='hr')]<-77
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='hr')]<-77
      #apparent influence by platform
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='in')]<-10
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='in')]<-10
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='in')]<-10
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='in')]<-10
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='in')]<-10
      #lobtail
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='lt')]<-20
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='lt')]<-20
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='lt')]<-20
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='lt')]<-20
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='lt')]<-20
      #milling
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='mi')]<-78
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='mi')]<-78
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='mi')]<-78
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='mi')]<-78
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='mi')]<-78
      #SAG
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='sa')]<-90
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='sa')]<-90
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='sa')]<-90
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='sa')]<-90
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='sa')]<-90
      #swim below surface
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='sb')]<-18
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='sb')]<-18
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='sb')]<-18
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='sb')]<-18
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='sb')]<-18
      #swim at surface
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='ss')]<-17
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='ss')]<-17
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='ss')]<-17
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='ss')]<-17
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='ss')]<-17
      #tangled in fishing gear
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='ta')]<-92
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='ta')]<-92
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='ta')]<-92
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='ta')]<-92
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='ta')]<-92
      #
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='tr')]<-34
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='tr')]<-34
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='tr')]<-34
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='tr')]<-34
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='tr')]<-34
      #visible injury
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='vi')]<-05
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='vi')]<-05
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='vi')]<-05
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='vi')]<-05
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='vi')]<-05
      #
      f$B1_FINAL_CODE[which(f$B1_FINAL_CODE=='zo')]<-63
      f$B2_FINAL_CODE[which(f$B2_FINAL_CODE=='zo')]<-63
      f$B3_FINAL_CODE[which(f$B3_FINAL_CODE=='zo')]<-63
      f$B4_FINAL_CODE[which(f$B4_FINAL_CODE=='zo')]<-63
      f$B5_FINAL_CODE[which(f$B5_FINAL_CODE=='zo')]<-63  
      
      ##########
      #Calf code when calf > 0
      f$CALVES[which(f$CALVES==0)]<-NA
      
      for (i in 1:nrow(f))
        if (!is.na(f$CALVES[i]) && (f$B1_FINAL_CODE[i]) == '') {
          f$B1_FINAL_CODE[i] = '40'
        } else if (!is.na(f$CALVES[i]) && (f$B2_FINAL_CODE[i] == '') && (f$B1_FINAL_CODE[i] != '40')){
          f$B2_FINAL_CODE[i] = '40'
        } else if (!is.na(f$CALVES[i]) && (f$B3_FINAL_CODE[i] == '') && (f$B1_FINAL_CODE[i] != '40') && (f$B2_FINAL_CODE[i] != '40')){
          f$B3_FINAL_CODE[i] = '40'
        } else if (!is.na(f$CALVES[i]) && (f$B4_FINAL_CODE[i] == '') && (f$B1_FINAL_CODE[i] != '40') && (f$B2_FINAL_CODE[i] != '40') && (f$B3_FINAL_CODE[i] != '40')){
          f$B4_FINAL_CODE[i] = '40'
        } else if (!is.na(f$CALVES[i]) && (f$B5_FINAL_CODE[i] == '') && (f$B1_FINAL_CODE[i] != '40') && (f$B2_FINAL_CODE[i] != '40') && (f$B3_FINAL_CODE[i] != '40') && (f$B4_FINAL_CODE[i] != '40')){
          f$B5_FINAL_CODE[i] = '40'
        } else if (!is.na(f$CALVES[i]) && (f$B5_FINAL_CODE[i] != '40') && (f$B1_FINAL_CODE[i] != '40') && (f$B2_FINAL_CODE[i] != '40') && (f$B3_FINAL_CODE[i] != '40') && (f$B4_FINAL_CODE[i] != '40')){
          output$calferror<-renderText({"Congrats! You have calves, but too many behaviors in a sighting to include code 40."})
        } 
      
      #####
      #add SST_C, drop and reorder columns
      #####
      
      f$DATETIME_UTC<-ymd_hms(f$DATETIME_UTC, tz = "GMT")
      
      ##reordered final
      rf <- f%>%
        mutate(SST_C = NA,
               PLANE = paste0("TWIN OTTER NOAA ",input$tn),
               DATETIME_ET = format(DATETIME_UTC, tz = "America/New_York"))%>%
        dplyr::select(PLANE,DATETIME_ET,EVENT_NUMBER,LATITUDE,LONGITUDE,FLIGHT_TYPE,LEGTYPE,LEGSTAGE,PSB_LEGSTAGE,
                                ALTITUDE,HEADING,SPEED,SST_C,VISIBILTY_NM,BEAUFORT,CLOUD_CODE,GLARE_L,GLARE_R,
                                QUALITY_L,QUALITY_R,SIGHTING_NUMBER,SPCODE,ID_RELIABILITY,GROUP_SIZE,CALVES,
                                ACTUAL_HEADING,OBSERVER,OBS_POSITION,ANGLE,CUE,B1_FINAL_CODE,B2_FINAL_CODE,
                                B3_FINAL_CODE,B4_FINAL_CODE,B5_FINAL_CODE,PHOTOS,EFFORT_COMMENTS,SIGHTING_COMMENTS,
                                EDIT1,EDIT2,EDIT3)
      rf[is.na(rf)] <- ""
      print(str(rf))
      rf$LONGITUDE<-as.numeric(rf$LONGITUDE)
      print(str(rf))
      maplat<-rf$LATITUDE
      maplon<-rf$LONGITUDE

      rf$LATITUDE<-sprintf("%.5f",round(rf$LATITUDE, digits = 5))
      rf$LONGITUDE<-sprintf("%.5f",round(rf$LONGITUDE, digits = 5))
      
      ftype<-unique(rf$FLIGHT_TYPE)
      print(ftype)
      
      rf[] <- lapply(rf, as.character)
      
      #####
      
      handsrf<-rhandsontable(rf,readOnly = TRUE)%>%
        hot_cols(columnSorting = TRUE)%>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
        hot_col("DATETIME_ET", width = 150)%>%
        hot_col("LATITUDE", format = "00.00000")%>%
        hot_col("LONGITUDE", format = "00.00000")%>%
        hot_col("FLIGHT_TYPE",format = "0",readOnly = FALSE)%>%
        hot_col("LEGTYPE",format = "0",readOnly = FALSE)%>%
        hot_col("LEGSTAGE",format = "0",readOnly = FALSE)%>%
        hot_col("PSB_LEGSTAGE",format = "0",readOnly = FALSE)%>%
        hot_col("HEADING", format = "000")%>%
        hot_col("SPEED", format = "000")%>%
        hot_col("ALTITUDE", format = "0", readOnly = FALSE)%>%
        hot_col("VISIBILTY_NM",format = "0",readOnly = FALSE)%>%
        hot_col("BEAUFORT",format = "0.0",readOnly = FALSE)%>%
        hot_col("CLOUD_CODE",format = "0",readOnly = FALSE)%>%
        hot_col("GLARE_L",format = "0",readOnly = FALSE)%>%
        hot_col("GLARE_R",format = "0",readOnly = FALSE)%>%
        hot_col("QUALITY_L",readOnly = FALSE)%>%
        hot_col("QUALITY_R",readOnly = FALSE)%>%
        hot_col("SIGHTING_NUMBER",format = "0",readOnly = FALSE)%>%
        hot_col("SPCODE",readOnly = FALSE)%>%
        hot_col("ID_RELIABILITY",format = "0",readOnly = FALSE)%>%
        hot_col("GROUP_SIZE",format = "0",readOnly = FALSE)%>%
        hot_col("CALVES",readOnly = FALSE)%>%
        hot_col("ACTUAL_HEADING", format = "000")%>%
        hot_col("OBSERVER",readOnly = FALSE)%>%
        hot_col("OBS_POSITION",readOnly = FALSE)%>%
        hot_col("ANGLE",readOnly = FALSE)%>%
        hot_col("CUE",readOnly = FALSE)%>%
        hot_col("B1_FINAL_CODE",format = "00",readOnly = FALSE)%>%
        hot_col("B2_FINAL_CODE",format = "00",readOnly = FALSE)%>%
        hot_col("B3_FINAL_CODE",format = "00",readOnly = FALSE)%>%
        hot_col("B4_FINAL_CODE",format = "00",readOnly = FALSE)%>%
        hot_col("B5_FINAL_CODE",format = "00",readOnly = FALSE)%>%
        hot_col("PHOTOS",format = "0",readOnly = FALSE)%>%
        hot_col("EFFORT_COMMENTS",readOnly = FALSE)%>%
        hot_col("SIGHTING_COMMENTS",readOnly = FALSE)%>%
        hot_col("EDIT1",readOnly = FALSE)%>%
        hot_col("EDIT2",readOnly = FALSE)%>%
        hot_col("EDIT3",readOnly = FALSE)
      
      #####
      ##output format
      
      sightings<-filter(rf, rf$SPCODE != '')
      sightings$LATITUDE<-as.numeric(sightings$LATITUDE)
      sightings$LONGITUDE<-as.numeric(sightings$LONGITUDE)
      
      factpal <- colorFactor(palette = "Set1", domain = sightings$SPCODE)
      egmap<-leaflet(data = rf) %>% 
        addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE) %>%
        addPolylines(lng=~maplon, lat = ~maplat, weight = 2, color = "black") %>%
        addCircleMarkers(lng = ~sightings$LONGITUDE, lat = ~sightings$LATITUDE, color = ~factpal(sightings$SPCODE), stroke = FALSE, fillOpacity = 2, radius = 5) %>%
        addLegend(pal = factpal, values = sightings$SPCODE, opacity = 1)%>%
        addWMSTiles(
          "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
          layers = c("1-degree grid", "5-degree grid"),
          options = WMSTileOptions(format = "image/png8", transparent = TRUE),
          attribution = NULL)
      
      output$egmap = renderLeaflet({print(egmap)})
      output$handsrf = renderRHandsontable({handsrf})
      enable("save")
    }
    
    ###########
    
    observeEvent(input$save,{
      final = hot_to_r(input$handsrf)
      ##########
      ##format/write f_ file
      ##########
      survey_date<-input$sd
      yr<-substr(survey_date,1,2)
      
      if (criteria$loc == 'Network'){
        path<-paste0('//net/mmi/Fieldwrk/Aerials/20',yr,'/Flights/edit_data/')
        enable("sas")
      } else if (criteria$loc == 'Local'){
        path<-input$filepathinput
      }
 
      final<-final%>%
        arrange(DATETIME_ET)
      
      final$LATITUDE<-as.numeric(final$LATITUDE)
      final$LONGITUDE<-as.numeric(final$LONGITUDE)
      final$B1_FINAL_CODE<-as.character(final$B1_FINAL_CODE)
      final$B2_FINAL_CODE<-as.character(final$B2_FINAL_CODE)
      final$B3_FINAL_CODE<-as.character(final$B3_FINAL_CODE)
      final$B4_FINAL_CODE<-as.character(final$B4_FINAL_CODE)
      final$B5_FINAL_CODE<-as.character(final$B5_FINAL_CODE)
      ##f_
      write.csv(final, paste0(path,survey_date,'/','f_',survey_date,'.csv'), na = '', row.names = FALSE)
      
      #####
      ##report formatting
      ###############
      
      ##beaufort
      minbft<-min(final$BEAUFORT)
      maxbft<-max(final$BEAUFORT)
      
      conditionsL<-final%>%
        distinct(QUALITY_L)%>%
        #as.data.frame()%>%
        dplyr::rename(QUALITY = QUALITY_L)
      conditionsR<-final%>%
        distinct(QUALITY_R)%>%
        as.data.frame()%>%
        dplyr::rename(QUALITY = QUALITY_R)
      
      cond<-rbind(conditionsL,conditionsR)
      cond<-cond%>%
        distinct
      
      ##DATETIME_ET for data export, but DateTime for all code that follows 9/2/20 LMC
      final<-final%>%
        arrange(DATETIME_ET, desc(EFFORT_COMMENTS))%>%
        dplyr::rename('DateTime' = 'DATETIME_ET')
      
      ##confsig = other species
      confsig<-filter(final, final$SPCODE != '' & !grepl('UN',final$SPCODE) & !grepl('-',final$SPCODE) & !is.na(final$SPCODE))
      #print(confsig)
      resight<- grepl("\\<s\\d", confsig$SIGHTING_COMMENTS) & confsig$LEGTYPE == 4
      print(resight)
      confsig<-confsig%>%
        filter(!resight)
      print(confsig)
      confsig$GROUP_SIZE<-as.integer(confsig$GROUP_SIZE)

      ## eg table that goes to make the egreport table using 'ap's and the egsas table that is the 'fin est's
      egtable<-NULL
      egtable<-final%>%
        filter(final$SPCODE == 'RIWH')%>%
        as.data.frame()
      print(egtable)
      #for and if loops for behavior
      
      for (i in 1:seq_along(nrow(egtable)))
        if (nrow(egtable) == 0) {
          
          egtable[1,] <- ''
          egtable$DateTime = 'No right whales sighted'
          disable("sas")
          
        } else {
          for (i in 1:nrow(egtable))
            if (egtable$B1_FINAL_CODE[i] == '00' | egtable$B1_FINAL_CODE[i] == '54' | egtable$B1_FINAL_CODE[i] == '90' | egtable$B1_FINAL_CODE[i] == '92'){
              egtable$B1_FINAL_CODE[i] = egtable$B1_FINAL_CODE[i] 
            } else {
              egtable$B1_FINAL_CODE[i] = ""
            }
          for (i in 1:nrow(egtable))
            if (egtable$B2_FINAL_CODE[i] == '00' | egtable$B2_FINAL_CODE[i] == '54' | egtable$B2_FINAL_CODE[i] == '90' | egtable$B2_FINAL_CODE[i] == '92'){
              egtable$B2_FINAL_CODE[i] = egtable$B2_FINAL_CODE[i] 
            } else {
              egtable$B2_FINAL_CODE[i] = ""
            }
          for (i in 1:nrow(egtable))
            if (egtable$B3_FINAL_CODE[i] == '00' | egtable$B3_FINAL_CODE[i] == '54' | egtable$B3_FINAL_CODE[i] == '90' | egtable$B3_FINAL_CODE[i] == '92'){
              egtable$B3_FINAL_CODE[i] = egtable$B3_FINAL_CODE[i]
            } else {
              egtable$B3_FINAL_CODE[i] = ""
            }
          for (i in 1:nrow(egtable))
            if (egtable$B4_FINAL_CODE[i] == '00' | egtable$B4_FINAL_CODE[i] == '54' | egtable$B4_FINAL_CODE[i] == '90' | egtable$B4_FINAL_CODE[i] == '92'){
              egtable$B4_FINAL_CODE[i] = egtable$B4_FINAL_CODE[i]
            } else {
              egtable$B4_FINAL_CODE[i] = ""
            }
          for (i in 1:nrow(egtable))
            if (egtable$B5_FINAL_CODE[i] == '00' | egtable$B5_FINAL_CODE[i] == '54' | egtable$B5_FINAL_CODE[i] == '90' | egtable$B5_FINAL_CODE[i] == '92'){
              egtable$B5_FINAL_CODE[i] = egtable$B5_FINAL_CODE[i]
            } else {
              egtable$B5_FINAL_CODE[i] = ""
            }
        }
      
      Behavior<-as.character(paste0(egtable$B1_FINAL_CODE," ",egtable$B2_FINAL_CODE," ",egtable$B3_FINAL_CODE," ",egtable$B4_FINAL_CODE," ",egtable$B5_FINAL_CODE))
      egtable<-cbind(egtable, Behavior)
      print(egtable)
      
      ## egrep is the aps and fin est no breaks without dupes and without new?
      egrep<-egtable%>%
        filter((PSB_LEGSTAGE == 7 | startsWith(SIGHTING_COMMENTS, "fin est no break") | grepl('No right whales', egtable$DateTime)))
      print(egrep)
      egrep<-egrep%>%
        filter(!grepl('new\\?', egrep$SIGHTING_COMMENTS) & !grepl('dup', egrep$SIGHTING_COMMENTS))
      print(egrep)
      ##eg table for report
      egreport<-subset(egrep, select=c("DateTime", "LATITUDE", "LONGITUDE", "GROUP_SIZE", "CALVES", "Behavior"))
      
      egreport$Behavior<-gsub("00","dead", egreport$Behavior)
      egreport$Behavior<-gsub("54","feeding", egreport$Behavior)
      egreport$Behavior<-gsub("90","SAG", egreport$Behavior)
      egreport$Behavior<-gsub("92","entangled", egreport$Behavior)
      
      names(egreport)<-c("Date/Time (ET)", "Latitude", "Longitude", "Number", "Calves", "Behavior")
      
      ##non eg table
      noneg<-filter(confsig, confsig$SPCODE != 'RIWH' & grepl('WH',confsig$SPCODE))
      
      ##must do it this way to get the right number of right whales
      noeg<-subset(noneg, select=c("SPCODE","GROUP_SIZE","LATITUDE","LONGITUDE"))
      yeseg<-subset(egrep, select=c("SPCODE","GROUP_SIZE","LATITUDE","LONGITUDE"))
      spectab<-rbind(noeg, yeseg)
      spectab<-spectab%>%
        filter(SPCODE != '')
      
      spectab$GROUP_SIZE <- as.numeric(spectab$GROUP_SIZE)
      spectab$LATITUDE <- as.numeric(spectab$LATITUDE)
      spectab$LONGITUDE <- as.numeric(spectab$LONGITUDE)
      
      netable<-spectab %>%
        dplyr::select("SPCODE","GROUP_SIZE")%>%
        group_by(SPCODE) %>%
        summarise(GROUP_SIZE = sum(GROUP_SIZE)) %>%
        as.data.frame()

      if (nrow(netable) == 0) {
        netable[1,]<-''
        netable$SPCODE = 'No large whales sighted'
      } else {
      
      netable$SPCODE[which(netable$SPCODE=='BASH')]<-'Basking Shark'
      netable$SPCODE[which(netable$SPCODE=='BEWH')]<-'Beaked Whale'
      netable$SPCODE[which(netable$SPCODE=='BLSH')]<-'Blue Shark'
      netable$SPCODE[which(netable$SPCODE=='BLWH')]<-'Blue Whale'
      netable$SPCODE[which(netable$SPCODE=='BODO')]<-'Bottlenosed Dolphin'
      netable$SPCODE[which(netable$SPCODE=='BOWH')]<-'Bowhead Whale'
      netable$SPCODE[which(netable$SPCODE=='FIWH')]<-'Fin Whale'
      netable$SPCODE[which(netable$SPCODE=='GRAM')]<-"Risso's Dolphin"
      netable$SPCODE[which(netable$SPCODE=='GRTU')]<-"Green Seaturtle"
      netable$SPCODE[which(netable$SPCODE=='HAPO')]<-'Harbor Porpoise'
      netable$SPCODE[which(netable$SPCODE=='HHSH')]<-'Hammerhead Shark'
      netable$SPCODE[which(netable$SPCODE=='HUWH')]<-'Humpback Whale'
      netable$SPCODE[which(netable$SPCODE=='KIWH')]<-'Killer Whale'
      netable$SPCODE[which(netable$SPCODE=='LETU')]<-'Leatherback Seaturtle'
      netable$SPCODE[which(netable$SPCODE=='LOTU')]<-'Loggerhead Seaturtle'
      netable$SPCODE[which(netable$SPCODE=='MIWH')]<-'Minke Whale'
      netable$SPCODE[which(netable$SPCODE=='NBWH')]<-'Northern Bottlenose Whale'
      netable$SPCODE[which(netable$SPCODE=='OCSU')]<-'Ocean Sunfish'
      netable$SPCODE[which(netable$SPCODE=='PIWH')]<-'Pilot Whale'
      netable$SPCODE[which(netable$SPCODE=='RIWH')]<-"Right Whale"
      netable$SPCODE[which(netable$SPCODE=='RITU')]<-"Kemp's Ridley Seaturtle"
      netable$SPCODE[which(netable$SPCODE=='SADO')]<-"Common Dolphin"
      netable$SPCODE[which(netable$SPCODE=='SEWH')]<-"Sei Whale"
      netable$SPCODE[which(netable$SPCODE=='SONO')]<-"Sonobuoy Deployed"
      netable$SPCODE[which(netable$SPCODE=='SPDO')]<-"Spinner Dolphin"
      netable$SPCODE[which(netable$SPCODE=='SPWH')]<-"Sperm Whale"
      netable$SPCODE[which(netable$SPCODE=='STDO')]<-"Striped Dolphin"
      netable$SPCODE[which(netable$SPCODE=='WBDO')]<-"White Beaked Dolphin"
      netable$SPCODE[which(netable$SPCODE=='WSDO')]<-"Atlantic White-Sided Dolphin"
      }
      
      names(netable)<-c("Species", "Total number")

      ##############################################
      ##Seasonal Management Area (SMA) evaluation ##
      ##############################################
      
      ##month day for sma evaluation
      MODAYR<-final$DateTime[1]
      MODAYR<-as.Date(MODAYR)
      print(MODAYR)
    
      MODA<-final%>%distinct(as.Date(DateTime))
      MODA<-unique(format(MODA, "%m-%d"))
      print(MODA)
      
      ##############
      ## FAKE DMA ##
      ##############
      fakedma<-data.frame(
        long = c(-71,-71,-71,-71,-71),
        lat = c(42,42,42,42,42))
      
      fakedma<-Polygons(list(Polygon(fakedma, hole=as.logical(NA))), ID = 1)
      ##############
      
      source('./scripts/sma.R', local = TRUE)$value
      
      if (input$filepathway == 'Network'){
      source('./scripts/oracleaccess.R', local = TRUE)$value
      source('./scripts/activedma.R', local = TRUE)$value}
      
      if(nrow(egtable) == 0 | egtable$DateTime[1] == 'No right whales sighted'){
        ##if no Eg:
        output$error1<-renderText({"No right whales were reported for this day"})
      }
      else {
        
        output$obspeeps_options<-renderUI({
          radioButtons("obspeeps","Who even are you?", choiceNames = list("Allison", "Christin", "Leah", "Pete", "Tim", "I don't know"), choiceValues = list(3,4,873,2,1,0), selected = "I don't know")})
        output$plane_options<-renderUI({
          radioButtons("plane","Which plane were you in?", choiceNames = list("NOAA46", "NOAA48", "NOAA56", "NOAA57", "An Otter"), choiceValues = list(183,178,284,49,18), selected = "An Otter")})  
        
        #####
        ##egtable for SAS
        output$error1<-renderText({""})
        
          #########################
          ##eg sightings for SAS ##
          #########################
        print(egrep)
          egsas<-egrep%>%
            dplyr::select(DateTime, GROUP_SIZE, CALVES, LATITUDE, LONGITUDE, ID_RELIABILITY, Behavior)%>%
            mutate(MOMCALF=NA, FEEDING=NA, DEAD=NA, SAG=NA, ENTANGLED=NA, CATEGORY='1', ACTION=NA)
          print(egsas)
          #####    
          for (i in 1:nrow(egsas))
            if (egsas$CALVES[i] > 0){
              egsas$MOMCALF[i] = '1'
            } else {
              egsas$MOMCALF[i] = '0'
            }
          for (i in 1:nrow(egsas))
            if (grepl('54', egsas$Behavior[i])){
              egsas$FEEDING[i] = '1'
            } else {
              egsas$FEEDING[i] = '0'
            }
          for (i in 1:nrow(egsas))
            if (grepl('00', egsas$Behavior[i])){
              egsas$DEAD[i] = '1'
            } else {
              egsas$DEAD[i] = '0'
            }
          for (i in 1:nrow(egsas))    
            if (grepl('90', egsas$Behavior[i])){
              egsas$SAG[i] = '1'
            } else {
              egsas$SAG[i] = '0'
            }
          for (i in 1:nrow(egsas))    
            if (grepl('92', egsas$Behavior[i])){
              egsas$ENTANGLED[i] = '1'
            } else {
              egsas$ENTANGLED[i] = '0'
            }

          egsas<-egsas%>%
            dplyr::select(-CALVES, -Behavior)
      
      source('./scripts/action & dma.R', local = TRUE)$value
      disable("dmaup")
        }
      
        
      ############
      ## REPORT ##
      ############
      
      date_formats$month1<-month.abb[month(as.character(final$DateTime[1]))]
      date_formats$month2<-format.Date(final$DateTime[1], "%m")
      date_formats$day1<-format.Date(final$DateTime[1], "%d")
      date_formats$year1<-year(final$DateTime[1])
      date_formats$date1<-paste0(date_formats$day1,' ',date_formats$month1,' ',date_formats$year1)

      leafpal <- colorFactor(palette = c("khaki1","cadetblue1","black","darkolivegreen3","royalblue1","firebrick2","mediumorchid3","antiquewhite3","chocolate4","darkorange","thistle1","darkolivegreen2"), 
                             domain = c("BEWH","BLWH","BOWH","FIWH","HUWH","KIWH","MIWH","NBWH","PIWH","RIWH","SEWH","SPWH"))
      
      ##leaflet map with US or Canadian shipping lanes and managament schemes
      if (nrow(sightings19) >= nrow(sightings20)){ #US
        
        reportleaf<-leaflet(data = spectab, options = leafletOptions(zoomControl = FALSE)) %>% 
          addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE) %>%
          addPolygons(data = smapresent.sp, weight = 2, color = "red") %>%
          addPolylines(data = NEUS_shiplane.sp, weight = 1, color = "green", fill = F)%>%
          addLegend(colors = c("green","yellow","red"), labels = c("Shipping Lanes","Dynamic Management Area","Seasonal Management Area"), opacity = 0.3)
      
      } else if (nrow(sightings19) < nrow(sightings20)){
        
        reportleaf<-leaflet(data = spectab, options = leafletOptions(zoomControl = FALSE)) %>% 
          addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE) %>%
          addPolygons(data = dyna_ship.sp, weight = 2, color = "green", fill = F) %>%
          addPolygons(data = GSL_shiplane.sp, weight = 2, color = "green")%>%
          addPolygons(data = spm.sp, weight = 2, color = "white")%>%
          addLegend(colors = c("green"), labels = c("Dynamic Shipping Section"), opacity = 0.3) 
      
      }
      
      #if on network, add in the current DMAs including the ones that are not up for extension (benign) and the ones eligible for extension
      #these are the same color for the flight report, but won't be for the Potential Protection Area report (if applicable)
      if (input$filepathway == 'Network'){ 
        reportleaf<-reportleaf %>%
          addPolygons(data = benigndma, weight = 2, color = "yellow") %>%
          addPolygons(data = extensiondma, weight = 2, color = "yellow")
      
        print("inside leaflet protection zones")
        print(benigndma)
        print(extensiondma)
        } 
      
      #Add tracklines and sightings
        reportleaf<-reportleaf %>%
          addPolylines(lng=~maplon, lat = ~maplat, weight = 2, color = "black") %>%
          addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, color = ~leafpal(SPCODE), stroke = FALSE, fillOpacity = 2, radius = 5) %>%
          addLegend(pal = leafpal, values = spectab$SPCODE, opacity = 0.9)%>%
          addWMSTiles(
            "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
            layers = c("1-degree grid", "5-degree grid"),
            options = WMSTileOptions(format = "image/png8", transparent = TRUE),
            attribution = NULL)
      
        #focus boundaries on the flight area
      reportmap<-fitBounds(reportleaf,min(final$LONGITUDE), min(final$LATITUDE), max(final$LONGITUDE), max(final$LATITUDE))

      print(getwd())
      unlink("./*surveymap.png")
      unlink("./scripts/*.log")

      enable("report")
      output$reportmap = renderLeaflet({print(reportmap)})
      output$netable<-renderTable({netable}, digits = 0)
      output$egreport<-renderTable({egreport})
      print("html1")
      htmlwidgets::saveWidget(reportmap, "temp.html", selfcontained = FALSE)
      print("html2")

        output$report<-downloadHandler(
          filename = paste0(date_formats$day1,date_formats$month1,date_formats$year1,"_NOAA_NERW_Aerial_Report.pdf"),
          content = function(file) {
            
              print(tempdir())
            
            if (ftype == 20){
              ftypesent<-"Only large whale sightings were recorded on this survey."
            } else if (ftype == 21){
              ftypesent<-"Only large whale sightings (excluding live minke whales) were recorded on this survey."
            } else {
              ftypesent<-""
            }
            
            rptnotes<-input$reportnotes
            
            if (criteria$loc == 'Network'){

              dmanamesexpsent<-paste0("Active right whale SLOW zone(s): ",dmanamesexp,".")
              webshotpath<-paste0(getwd(),"/surveymap.png")
              source('./scripts/oracleaccess.R', local = TRUE)$value
              source('./scripts/input_sas.R', local = TRUE)$value
              source('./scripts/input_dma.R', local = TRUE)$value
              
            } else if (criteria$loc == 'Local'){
              
              disable("dmaup")
              disable("dmareport")
              disable("kml")
              disable("dmaletter")
              
              dmanamesexpsent<-""
              webshotpath<-paste0(path,"surveymap.png")
              
            }
              
              webshot::webshot("temp.html", file = webshotpath)
              print("webshot")
              
              tempReport<-file.path("./scripts/FlightReport.Rmd")
              file.copy("FlightReport.Rmd", tempReport, overwrite = FALSE)
            
              params<-list(date1 = date_formats$date1, rptnotes = rptnotes, reportmap = reportmap, netable = netable, egreport = egreport, dmanamesexpsent = dmanamesexpsent, ftypesent = ftypesent, webshotpath = webshotpath)
              print(webshotpath)
              rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
                              )
                              
            })
    }) #OBSERVE EVENT SAVE   
  })  #OBSERVE EVENT EDITTABLE


  
