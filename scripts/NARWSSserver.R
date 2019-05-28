
disable("edittable")
disable("save")
disable("report")
disable("sas")
disable("dmaup")
disable("dmareport")
#disable("kml")

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
    
    mfyn<-input$multiflight
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
        output$error2<-renderText({"Uh oh! Those files can't be found! Double check your connection to the network, the local network pathway, and/or your survey date entry."})         
    } else if (rawed == "No" && mfyn == "Yes" && !file.exists(paste0(path,survey_date,'/',survey_date,' (2).eff'))){
        output$error2<-renderText({"Are you sure you had a two flight day?"})
    } else if (rawed == "No" && mfyn == "No" && file.exists(paste0(path,survey_date,'/',survey_date,' (2).eff'))){
        output$error2<-renderText({"Are you sure you had a single flight day?"})
    } else if (rawed == "Yes" && !file.exists(paste0(path,survey_date,'/','effsig_',survey_date,'.csv'))){
        output$error2<-renderText({"No initial eff/sig edits were saved."})    
    } else {
      
      if (rawed == "Yes"){
        eff_sig<-as.data.frame(read.csv(paste0(path,survey_date,'/','effsig_',survey_date,'.csv'),header=TRUE, stringsAsFactors = FALSE))
      } else if (rawed == "No"){
        
        if(mfyn == "Yes"){
        output$error2<-renderText({""})  
        ######
        ##this is the same as the else path except what is between the ###**###
        ###**###
        
        gps<-as.data.frame(read.csv(paste0(path,survey_date,'/',survey_date,'.gps'), header=FALSE, stringsAsFactors = FALSE))
        names(gps)<-c('DateTime','LATITUDE','LONGITUDE','SPEED','HEADING','ALTITUDE','T1')
        
        eff1<-as.data.frame(read.csv(paste0(path,survey_date,'/',survey_date,' (1).eff'), header=FALSE, stringsAsFactors = FALSE))
        eff2<-as.data.frame(read.csv(paste0(path,survey_date,'/',survey_date,' (2).eff'), header=FALSE, stringsAsFactors = FALSE))
        names(eff1)<-c('Trans','Line','ABE','DateTime','BEAUFORT','VISIBILTY','CLOUD_CODE','GLARE_L','GLARE_R',
                       'QUALITY_L','QUALITY_R','EFFORT_COMMENTS')#,'EDIT1','EDIT2','EDIT3','T1') ##T1=trash
        names(eff2)<-c('Trans','Line','ABE','DateTime','BEAUFORT','VISIBILTY','CLOUD_CODE','GLARE_L','GLARE_R',
                       'QUALITY_L','QUALITY_R','EFFORT_COMMENTS')#,'EDIT1','EDIT2','EDIT3','T1') ##T1=trash
        
        sig1name<-paste0(path,survey_date,'/',survey_date,' (1).sig')
        sig2name<-paste0(path,survey_date,'/',survey_date,' (2).sig')
        
          if (file.info(sig1name)$size == 0 & file.info(sig2name)$size > 0){
          
            sig1<-data.frame(Trans=NA,Line=NA,SIGHTING_NUMBER=NA,DateTime=NA,OBSERVER=NA,ANGLE=NA,SPCODE=NA,GROUP_SIZE=NA,CUE=NA,
                          ACTUAL_HEADING=NA,T1=NA,T2=NA,SIGHTING_COMMENTS=NA,OBS_POSITION=NA,LATITUDE=NA,LONGITUDE=NA,T3=NA,CALVES=NA,
                          T4=NA,B1_FINAL_CODE=NA,T5=NA)
            sig1$LATITUDE<-as.double(sig1$LATITUDE)
            sig1$LONGITUDE<-as.double(sig1$LONGITUDE)
          
            sig2<-as.data.frame(read.csv(sig2name, header=FALSE, stringsAsFactors = FALSE))
            names(sig2)<-c('Trans','Line','SIGHTING_NUMBER','DateTime','OBSERVER','ANGLE','SPCODE','GROUP_SIZE','CUE',
                         'ACTUAL_HEADING','T1','T2','SIGHTING_COMMENTS','OBS_POSITION','LATITUDE','LONGITUDE','T3','CALVES',
                         'T4','B1_FINAL_CODE','T5') ##T1=trash
          
          } else if (file.info(sig1name)$size > 0 & file.info(sig2name)$size == 0){
  
            sig1<-as.data.frame(read.csv(sig1name, header=FALSE, stringsAsFactors = FALSE))
            names(sig1)<-c('Trans','Line','SIGHTING_NUMBER','DateTime','OBSERVER','ANGLE','SPCODE','GROUP_SIZE','CUE',
                 'ACTUAL_HEADING','T1','T2','SIGHTING_COMMENTS','OBS_POSITION','LATITUDE','LONGITUDE','T3','CALVES',
                 'T4','B1_FINAL_CODE','T5') ##T1=trash
  
            sig2<-data.frame(Trans=NA,Line=NA,SIGHTING_NUMBER=NA,DateTime=NA,OBSERVER=NA,ANGLE=NA,SPCODE=NA,GROUP_SIZE=NA,CUE=NA,
                   ACTUAL_HEADING=NA,T1=NA,T2=NA,SIGHTING_COMMENTS=NA,OBS_POSITION=NA,LATITUDE=NA,LONGITUDE=NA,T3=NA,CALVES=NA,
                   T4=NA,B1_FINAL_CODE=NA,T5=NA)
            sig2$LATITUDE<-as.double(sig2$LATITUDE)
            sig2$LONGITUDE<-as.double(sig2$LONGITUDE)
  
          } else if (file.info(sig1name)$size == 0 & file.info(sig2name)$size == 0){
            
            sig1<-data.frame(Trans=NA,Line=NA,SIGHTING_NUMBER=NA,DateTime=NA,OBSERVER=NA,ANGLE=NA,SPCODE=NA,GROUP_SIZE=NA,CUE=NA,
                             ACTUAL_HEADING=NA,T1=NA,T2=NA,SIGHTING_COMMENTS=NA,OBS_POSITION=NA,LATITUDE=NA,LONGITUDE=NA,T3=NA,CALVES=NA,
                             T4=NA,B1_FINAL_CODE=NA,T5=NA)
            sig1$LATITUDE<-as.double(sig1$LATITUDE)
            sig1$LONGITUDE<-as.double(sig1$LONGITUDE)
            
            sig2<-data.frame(Trans=NA,Line=NA,SIGHTING_NUMBER=NA,DateTime=NA,OBSERVER=NA,ANGLE=NA,SPCODE=NA,GROUP_SIZE=NA,CUE=NA,
                   ACTUAL_HEADING=NA,T1=NA,T2=NA,SIGHTING_COMMENTS=NA,OBS_POSITION=NA,LATITUDE=NA,LONGITUDE=NA,T3=NA,CALVES=NA,
                   T4=NA,B1_FINAL_CODE=NA,T5=NA)
            sig2$LATITUDE<-as.double(sig2$LATITUDE)
            sig2$LONGITUDE<-as.double(sig2$LONGITUDE)
            
          } else {
          
            sig1<-as.data.frame(read.csv(sig1name, header=FALSE, stringsAsFactors = FALSE))
            sig2<-as.data.frame(read.csv(sig2name, header=FALSE, stringsAsFactors = FALSE))
            names(sig1)<-c('Trans','Line','SIGHTING_NUMBER','DateTime','OBSERVER','ANGLE','SPCODE','GROUP_SIZE','CUE',
                         'ACTUAL_HEADING','T1','T2','SIGHTING_COMMENTS','OBS_POSITION','LATITUDE','LONGITUDE','T3','CALVES',
                         'T4','B1_FINAL_CODE','T5') ##T1=trash
            names(sig2)<-c('Trans','Line','SIGHTING_NUMBER','DateTime','OBSERVER','ANGLE','SPCODE','GROUP_SIZE','CUE',
                         'ACTUAL_HEADING','T1','T2','SIGHTING_COMMENTS','OBS_POSITION','LATITUDE','LONGITUDE','T3','CALVES',
                         'T4','B1_FINAL_CODE','T5') ##T1=trash
          }
        
        eff<-merge(eff1, eff2, by=c('Trans','Line','ABE','DateTime','BEAUFORT','VISIBILTY','CLOUD_CODE','GLARE_L','GLARE_R',
                                    'QUALITY_L','QUALITY_R','EFFORT_COMMENTS'), all=TRUE)
        sig<-merge(sig1, sig2, by=c('Trans','Line','SIGHTING_NUMBER','DateTime','OBSERVER','ANGLE','SPCODE','GROUP_SIZE','CUE',
                                    'ACTUAL_HEADING','T1','T2','SIGHTING_COMMENTS','OBS_POSITION','LATITUDE','LONGITUDE','T3','CALVES',
                                    'T4','B1_FINAL_CODE','T5'), all=TRUE)
        ###**###
        } else if (mfyn == "No"){
        output$error2<-renderText({""})
        
        eff<-as.data.frame(read.csv(paste0(path,survey_date,'/',survey_date,'.eff'), header=FALSE, stringsAsFactors = FALSE))
        gps<-as.data.frame(read.csv(paste0(path,survey_date,'/',survey_date,'.gps'), header=FALSE, stringsAsFactors = FALSE))
        
        names(gps)<-c('DateTime','LATITUDE','LONGITUDE','SPEED','HEADING','ALTITUDE','T1')
        names(eff)<-c('Trans','Line','ABE','DateTime','BEAUFORT','VISIBILTY','CLOUD_CODE','GLARE_L','GLARE_R',
                      'QUALITY_L','QUALITY_R','EFFORT_COMMENTS')#,'EDIT1','EDIT2','EDIT3','T1') ##T1=trash
        signame<-paste0(path,survey_date,'/',survey_date,'.sig')
        
          if (file.info(signame)$size == 0){
            sig<-data.frame(Trans=NA,Line=NA,SIGHTING_NUMBER=NA,DateTime=NA,OBSERVER=NA,ANGLE=NA,SPCODE=NA,GROUP_SIZE=NA,CUE=NA,
                          ACTUAL_HEADING=NA,T1=NA,T2=NA,SIGHTING_COMMENTS=NA,OBS_POSITION=NA,LATITUDE=NA,LONGITUDE=NA,T3=NA,CALVES=NA,
                          T4=NA,B1_FINAL_CODE=NA,T5=NA)
            sig$LATITUDE<-as.double(sig$LATITUDE)
            sig$LONGITUDE<-as.double(sig$LONGITUDE)
          } else {
            sig<-as.data.frame(read.csv(signame, header=FALSE, stringsAsFactors = FALSE))
            names(sig)<-c('Trans','Line','SIGHTING_NUMBER','DateTime','OBSERVER','ANGLE','SPCODE','GROUP_SIZE','CUE',
                          'ACTUAL_HEADING','T1','T2','SIGHTING_COMMENTS','OBS_POSITION','LATITUDE','LONGITUDE','T3','CALVES',
                          'T4','B1_FINAL_CODE','T5') ##T1=trash
          }
        }
      
      ######
      eff$BEAUFORT<-as.numeric(eff$BEAUFORT)
      sig$ACTUAL_HEADING<-as.numeric(sig$ACTUAL_HEADING)
      ##delete rows with "E" from eff
      
      eff<-filter(eff, eff$ABE != "E")
      #effx<-eff[which(eff$ABE != "B" & eff$EFFORT_COMMENTS != ''),]
      #effz<-eff[which(eff$ABE == "B" & eff$EFFORT_COMMENTS != ''),]
      #eff<-merge(effx, effz, by=c('Trans','Line','ABE','DateTime','BEAUFORT','VISIBILTY','CLOUD_CODE','GLARE_L','GLARE_R',
      #                            'QUALITY_L','QUALITY_R','EFFORT_COMMENTS'), all=TRUE)
      
      ##format DateTime
      gps$DateTime<-dmy_hms(gps$DateTime)
      eff$DateTime<-dmy_hms(eff$DateTime)
      sig$DateTime<-dmy_hms(sig$DateTime)
      
      #############
      #Drop Columns
      ########
      eff$Trans<-NULL
      eff$Line<-NULL
      eff$ABE<-NULL
      eff$T1<-NULL
      
      sig$Trans<-NULL
      sig$Line<-NULL
      sig$T1<-NULL
      sig$T2<-NULL
      sig$T3<-NULL
      sig$T4<-NULL
      sig$T5<-NULL
      
      gps$T1<-NULL
      gps$ALTITUDE<-NULL
      ################
      ###############
      #DATETIME link to position, spead, and heading data
      #package data.table documentation
      ######
      
      setDT(eff)[,  LATITUDE := setDT(gps)[eff, LATITUDE, on = "DateTime", roll = "nearest"]]
      setDT(eff)[,  LONGITUDE := setDT(gps)[eff, LONGITUDE, on = "DateTime", roll = "nearest"]]
      setDT(eff)[,  SPEED := setDT(gps)[eff, SPEED, on = "DateTime", roll = "nearest"]]
      setDT(eff)[,  HEADING := setDT(gps)[eff, HEADING, on = "DateTime", roll = "nearest"]]
      
      setDT(sig)[,  SPEED := setDT(gps)[sig, SPEED, on = "DateTime", roll = "nearest"]]
      setDT(sig)[,  HEADING := setDT(gps)[sig, HEADING, on = "DateTime", roll = "nearest"]]
      #merge effort and sighting files
      
      ######
      ##merge and add columns
      eff_sig<-merge(eff, sig, by=c("DateTime","LATITUDE","LONGITUDE","SPEED","HEADING"), all=TRUE)
      eff_sig<-eff_sig%>%
        filter(!is.na(DateTime))
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
      eff_sig <- eff_sig %>% dplyr::select(DateTime, LATITUDE, LONGITUDE, ALTITUDE, HEADING, SPEED, VISIBILTY, BEAUFORT,
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
      
      eff_sig$DateTime<-as.character.Date(eff_sig$DateTime)
      eff_sig$LATITUDE<-as.character(eff_sig$LATITUDE)
      eff_sig$LONGITUDE<-as.character(eff_sig$LONGITUDE)
      eff_sig$ALTITUDE<-as.character(eff_sig$ALTITUDE)
      eff_sig$VISIBILTY<-as.character(eff_sig$VISIBILTY)
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
        hot_col("DateTime", readOnly = FALSE, width = 150)%>%
        ##format only allows up to 4 decimal places
        hot_col("LATITUDE", format = "00.00000")%>% 
        hot_col("LONGITUDE", format = "00.00000")%>%
        hot_col("ALTITUDE", format = "0",readOnly = FALSE)%>%
        hot_col("HEADING", format = "000")%>%
        hot_col("SPEED", format = "000")%>%
        hot_col("VISIBILTY",format = "0",readOnly = FALSE, type = "dropdown", source = vis)%>%
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
        loc<-"Network"
      } else if (input$filepathway == 'Local'){
        path<-input$filepathinput
        loc<-"Local"
      }
      
      write.csv(eff_sig2, paste0(path,survey_date,'/','effsig_',survey_date,'.csv'), na = '', row.names = FALSE)
      ######
      ##reformat out of hot
      eff_sig2$DateTime<-ymd_hms(eff_sig2$DateTime)
      eff_sig2$ALTITUDE<-as.numeric(eff_sig2$ALTITUDE)
      eff_sig2$VISIBILTY<-as.numeric(eff_sig2$VISIBILTY)
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
      names(gps2)<-c('DateTime','LATITUDE','LONGITUDE','SPEED','HEADING','ALTITUDE','T1')
      gps2$DateTime<-dmy_hms(gps2$DateTime)
      gps2$T1<-NULL
      gps2$ALTITUDE<-NULL
      
      #DATETIME link to position, spead, and heading data
      #package data.table documentation
      ######
      setDT(eff_sig2)[,  LATITUDE := setDT(gps2)[eff_sig2, LATITUDE, on = "DateTime", roll = "nearest"]]
      setDT(eff_sig2)[,  LONGITUDE := setDT(gps2)[eff_sig2, LONGITUDE, on = "DateTime", roll = "nearest"]]
      setDT(eff_sig2)[,  SPEED := setDT(gps2)[eff_sig2, SPEED, on = "DateTime", roll = "nearest"]]
      setDT(eff_sig2)[,  HEADING := setDT(gps2)[eff_sig2, HEADING, on = "DateTime", roll = "nearest"]]
      
      #############
      #GPS filter
      #############
      ##get bin list using seq for every 8 seconds
      gpsbin <- cut(gps2$DateTime, breaks = c(seq(from = gps2$DateTime[1], to = gps2$DateTime[nrow(gps2)], by = 8)))
      ##bind bin list to gps
      gpsplus<-cbind(gps2, gpsbin)
      ##order by DateTime and rank
      gpsrank<- gpsplus %>% arrange(DateTime, gpsbin) %>% group_by(gpsbin) %>% mutate(rank=rank(DateTime, ties.method = "first"))
      ##select for 1st in the bin
      gpsfil<-gpsrank %>% filter(rank == 1)
      
      #############
      
      f<-merge(eff_sig2, gpsfil, by=c("DateTime","LATITUDE","LONGITUDE","SPEED","HEADING"), all=TRUE)
      f<-f[order(f$DateTime, -f$EFFORT_COMMENTS),]
      ##
      #ALTITUDE
      #####
      
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
      f$VISIBILTY<-na.locf(f$VISIBILTY, na.rm = FALSE)
      f$CLOUD_CODE<-na.locf(f$CLOUD_CODE, na.rm = FALSE)
      f$GLARE_L<-na.locf(f$GLARE_L, na.rm = FALSE)
      f$GLARE_R<-na.locf(f$GLARE_R, na.rm = FALSE)
      f$QUALITY_L<-na.locf(f$QUALITY_L, na.rm = FALSE)
      f$QUALITY_R<-na.locf(f$QUALITY_R, na.rm = FALSE)
      
      #get rid of before on watch
      f<-subset(f, !is.na(BEAUFORT) | (is.na(BEAUFORT) & !is.na(SPCODE)))
      #####
      #Flight Codes
      #####
      #FLIGHT_TYPE
      #####
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
      ##########
      #LEGTYPE
      ######
      LEGTYPE = NULL
      for (i in 1:nrow(f))
        if (grepl('break', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 4
        } else if (grepl(' str', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 12
        } else if (grepl(' st', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 11
        } else if (grepl(' ha', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 10
        } else if (grepl(' di', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 9
        } else if (grepl(' br', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 8
        } else if (grepl(' ra', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 7
        } else if (grepl(' fi', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 6
        } else if (grepl(' cr', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 3
        } else if (grepl(' tr', f$EFFORT_COMMENTS[i])) {
          LEGTYPE[i] = 1
        } else {
          LEGTYPE[i] = NA
        } 
      
      f<-cbind(f, LEGTYPE)
      ########
      #LEGSTAGE
      ########
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
      #####
      #PSB_LEGSTAGE
      #######
      
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
      f[order(as.Date(f$DateTime, format = dmy_hms)),]
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
      #SST_C
      #####
      SST_C = NULL
      for (i in 1:nrow(f))
        SST_C[i] = NA
      
      f<-cbind(f,SST_C)
      
      ##drop rank columns
      f$gpsbin<-NULL
      f$rank<-NULL
      
      
      #####
      #reorder columns
      #####
      
      ##reordered final
      rf <- f %>% dplyr::select(DateTime,EVENT_NUMBER,LATITUDE,LONGITUDE,FLIGHT_TYPE,LEGTYPE,LEGSTAGE,PSB_LEGSTAGE,
                                ALTITUDE,HEADING,SPEED,SST_C,VISIBILTY,BEAUFORT,CLOUD_CODE,GLARE_L,GLARE_R,
                                QUALITY_L,QUALITY_R,SIGHTING_NUMBER,SPCODE,ID_RELIABILITY,GROUP_SIZE,CALVES,
                                ACTUAL_HEADING,OBSERVER,OBS_POSITION,ANGLE,CUE,B1_FINAL_CODE,B2_FINAL_CODE,
                                B3_FINAL_CODE,B4_FINAL_CODE,B5_FINAL_CODE,PHOTOS,EFFORT_COMMENTS,SIGHTING_COMMENTS,
                                EDIT1,EDIT2,EDIT3)
      rf[is.na(rf)] <- ""
      
      maplat<-rf$LATITUDE
      maplon<-rf$LONGITUDE
      rf$DateTime<-ymd_hms(rf$DateTime, tz = "GMT")
      rf$DateTime<-format(rf$DateTime, tz = "America/New_York")
      rf$LATITUDE<-sprintf("%.5f",round(rf$LATITUDE, digits = 5))
      rf$LONGITUDE<-sprintf("%.5f",round(rf$LONGITUDE, digits = 5))
      
      ftype<-unique(rf$FLIGHT_TYPE)
      print(ftype)
      
      rf[] <- lapply(rf, as.character)
      
      #####
      
      handsrf<-rhandsontable(rf,readOnly = TRUE)%>%
        hot_cols(columnSorting = TRUE)%>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)%>%
        hot_col("DateTime", width = 150)%>%
        hot_col("LATITUDE", format = "00.00000")%>%
        hot_col("LONGITUDE", format = "00.00000")%>%
        hot_col("FLIGHT_TYPE",format = "0",readOnly = FALSE)%>%
        hot_col("LEGTYPE",format = "0",readOnly = FALSE)%>%
        hot_col("LEGSTAGE",format = "0",readOnly = FALSE)%>%
        hot_col("PSB_LEGSTAGE",format = "0",readOnly = FALSE)%>%
        hot_col("HEADING", format = "000")%>%
        hot_col("SPEED", format = "000")%>%
        hot_col("ALTITUDE", format = "0", readOnly = FALSE)%>%
        hot_col("VISIBILTY",format = "0",readOnly = FALSE)%>%
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
      
      if (loc == 'Network'){
        path<-paste0('//net/mmi/Fieldwrk/Aerials/20',yr,'/Flights/edit_data/')
        enable("sas")
      } else if (loc == 'Local'){
        path<-input$filepathinput
      }
 
      final<-final[order(f$DateTime),]
      
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
      
      final<-final[order(final$DateTime, -final$EFFORT_COMMENTS),]
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
      egtable<-filter(final, final$SPCODE == 'RIWH')
      
      #for and if loops for behavior
      
      for (i in 1:seq_along(nrow(egtable)))
        if (nrow(egtable) == 0) {
          
          egtable[1,]<-''
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
      ##egrep is the aps and fin est no breaks without dupes and without new?
      #egrep<-filter(egtable, ((!grepl('new?', egtable$SIGHTING_COMMENTS) | !grepl('dup', egtable$SIGHTING_COMMENTS)) & (grepl('ap',egtable$SIGHTING_COMMENTS) | grepl('fin est no break', egtable$SIGHTING_COMMENTS) | grepl('No right whales', egtable$DateTime))))
      egrep<-egtable%>%
        filter((startsWith(SIGHTING_COMMENTS, "ap") | startsWith(SIGHTING_COMMENTS, "fin est no break") | grepl('No right whales', egtable$DateTime)))
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
      
      ###############
      ##SMA evaluation
      ########
      
      if (loc == 'Network'){
        smapath<-"./SMA ind shp"
      } else if (loc == 'Local'){
        smapath<-paste0(input$filepathinput,"SMA ind shp")
      }
      print(smapath)
      
      ###############
      ##Seasonal Management Area (SMA) evaluation
      ########
      
      ##month day for sma evaluation
      MODAYR<-final$DateTime[1]
      MODAYR<-as.Date(MODAYR)
      print(MODAYR)
    
      MODA<-final%>%distinct(as.Date(DateTime))
      MODA<-unique(format(MODA, "%m-%d"))
      print(MODA)
      
      source('./scripts/sma.R', local = TRUE)$value
      
      ##############
      ## FAKE DMA ##
      ##############
      fakedma<-data.frame(
        long = c(-71,-71,-71,-71,-71),
        lat = c(42,42,42,42,42))
      
      fakedma<-Polygons(list(Polygon(fakedma, hole=as.logical(NA))), ID = 1)
      ##############
      
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
        
          egsas<-egrep%>%
            dplyr::select(DateTime, GROUP_SIZE, CALVES, LATITUDE, LONGITUDE, ID_RELIABILITY, Behavior)
          
          MOMCALF<-NA
          FEEDING<-NA
          DEAD<-NA
          SAG<-NA
          ENTANGLED<-NA
          CATEGORY<-'1'
          ACTION<-NA
          
          egsas<-cbind(egsas, MOMCALF, FEEDING, DEAD, SAG, ENTANGLED, CATEGORY, ACTION)
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
      
      triggrptrue <- FALSE
      source('./scripts/action & dma.R', local = TRUE)$value
        }
      
        
      ############
      ## REPORT ##
      ############
      
      month1<-month.abb[month(as.character(final$DateTime[1]))]
      month2<-format.Date(final$DateTime[1], "%m")
      day1<-format.Date(final$DateTime[1], "%d")
      year1<-year(final$DateTime[1])
      date1<-paste0(day1,' ',month1,' ',year1)

      leafpal <- colorFactor(palette = c("khaki1","cadetblue1","black","darkolivegreen3","royalblue1","firebrick2","mediumorchid3","antiquewhite3","chocolate4","darkorange","thistle1","darkolivegreen2"), 
                             domain = c("BEWH","BLWH","BOWH","FIWH","HUWH","KIWH","MIWH","NBWH","PIWH","RIWH","SEWH","SPWH"))
      
      
      if(egtable$DateTime[1] == 'No right whales sighted'){
        
        reportleaf<-leaflet(data = spectab, options = leafletOptions(zoomControl = FALSE)) %>% 
          addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE) %>%
          addPolylines(lng=~maplon, lat = ~maplat, weight = 2, color = "black") %>%
          addPolygons(data = smapresent.sp, weight = 2, color = "red") %>%
          addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, color = ~leafpal(SPCODE), stroke = FALSE, fillOpacity = 2, radius = 5) %>%
          addLegend(colors = c("yellow","red"), labels = c("Dynamic Management Area","Seasonal Management Area"), opacity = 0.3)%>%
          addLegend(pal = leafpal, values = spectab$SPCODE, opacity = 1)%>%
          addWMSTiles(
            "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
            layers = c("1-degree grid", "5-degree grid"),
            options = WMSTileOptions(format = "image/png8", transparent = TRUE),
            attribution = NULL)
        
        if (input$filepathway == 'Network'){
          reportleaf<-reportleaf %>%
            addPolygons(data = benigndma, weight = 2, color = "yellow") %>%
            addPolygons(data = extensiondma, weight = 2, color = "yellow")
        } 
        
      } else {
        
        reportleaf<-leaflet(data = spectab, options = leafletOptions(zoomControl = FALSE)) %>% 
          addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE) %>%
          addPolylines(lng=~maplon, lat = ~maplat, weight = 2, color = "black") %>%
          addPolygons(data = smapresent.sp, weight = 2, color = "red") %>%
          addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, color = ~leafpal(SPCODE), stroke = FALSE, fillOpacity = 2, radius = 5) %>%
          addLegend(colors = c("yellow","red"), position = "topleft", labels = c("Dynamic Management Area","Seasonal Management Area"), opacity = 0.3)%>%
          addLegend(pal = leafpal, position = "topleft", values = spectab$SPCODE, opacity = 0.9)%>%
          addWMSTiles(
            "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
            layers = c("1-degree grid", "5-degree grid"),
            options = WMSTileOptions(format = "image/png8", transparent = TRUE),
            attribution = NULL)
        
        if (input$filepathway == 'Network'){
          reportleaf<-reportleaf %>%
            addPolygons(data = benigndma, weight = 2, color = "yellow") %>%
            addPolygons(data = extensiondma, weight = 2, color = "yellow")
        } 
        
      }
      
      reportmap<-fitBounds(reportleaf,min(final$LONGITUDE), min(final$LATITUDE), max(final$LONGITUDE), max(final$LATITUDE))
      print(getwd())
      
      enable("report")
      output$reportmap = renderLeaflet({print(reportmap)})
      output$netable<-renderTable({netable}, digits = 0)
      output$egreport<-renderTable({egreport})

      
      htmlwidgets::saveWidget(reportmap, "temp.html", selfcontained = FALSE)
      webshot::webshot("temp.html", file = paste0(date1,"_map.png"))
      print("webshot")
        
        output$report<-downloadHandler(
          filename = paste0(day1,month1,year1,"_NOAA_NERW_Aerial_Report.pdf"),
          content = function(file) {
            
              print(tempdir())
            
            if (ftype == 20){
              ftypesent<-"Only large whale sightings were recorded on this survey."
            } else if (ftype == 21){
              ftypesent<-"Only large whale sightings (excluding live minke whales) were recorded on this survey."
            }
            
            tempReport<-file.path("./scripts/FlightReport.Rmd")
            rptnotes<-input$reportnotes
            
            file.copy("FlightReport.Rmd", tempReport, overwrite = FALSE)
            
            if (loc == 'Network'){
              webshotpath<-paste0("//net/mmi/Fieldwrk/Aerials/Shiny/NARWSS_shinyapp/git/narwss_rwsas_apps/",date1,"_map.png")
              dmanamesexpsent<-paste0("Active Dynamic Management Area(s): ",dmanamesexp,".")
            } else if (loc == 'Local'){
              webshotpath<-paste0(path,date1,"_map.png")
              dmanamesexpsent<-""
              disable("dmaup")
              disable("kml")
              disable("dmaletter")
            }
            
              params<-list(date1 = date1, rptnotes = rptnotes, reportmap = reportmap, netable = netable, egreport = egreport, dmanamesexpsent = dmanamesexpsent, ftypesent = ftypesent, webshotpath = webshotpath)
              print(webshotpath)
              rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
                              
            )})
    }) #OBSERVE EVENT SAVE   
  })  #OBSERVE EVENT EDITTABLE
  
  
  