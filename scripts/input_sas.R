observeEvent(input$sas,{
  disable("sas")
  ######################
  ## upload to Oracle ##
  ######################  
  print("SAS button pressed")
  
  ################
  ##egsas upload##
  ################
  
  if(is.null(input$obspeeps)){
    output$error5<-renderText({"Decide who you are."})
  } else if(is.null(input$plane)){
    output$error5<-renderText({"Which plane were you in?"})
  } else {
    output$error5<-renderText({""})
    
    if ("ID" %in% colnames(sas_react$egsastab)){
      egsastab<-sas_react$egsastab%>%
        dplyr::select(-ID)
    } else {
      egsastab<-sas_react$egsastab
    }  
    
    egsastab<-egsastab%>%
      mutate(OBSERVER_PEOPLE = input$obspeeps,
             OBSERVER_PLATFORM = input$plane,
             ID = 99999, #SAS has a procedure to make this number chronological with the table, but it cannot be NULL
             OBSERVER_ORG = 1,
             OOD = 929 #929 means it came through this Shiny App
      )
    
    print(head(egsastab))
    
    for (i in 1:nrow(egsastab)){
      egvalues<-egsastab
      egvalues$DateTime<-paste0("'to_timestamp('",egvalues$DateTime,"', 'YYYY-MM-DD HH24:MI:SS')")
      egvalues <- paste0(sapply(egvalues[i,], function(x) paste0("", paste0(x, collapse = "', '"), "'")), collapse = ", '")
      egvalues <- gsub("'to_", "to_", egvalues)
      egvalues <- gsub("')'", "')", egvalues)
      sqlQuery(cnxn, paste0("INSERT INTO SAS(SIGHTDATE,GROUPSIZE,LAT,LON,SPECIES_CERT,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION,OBSERVER_PEOPLE,OBSERVER_PLATFORM,ID,OBSERVER_ORG,OOD)
                            VALUES(", egvalues,");"))
      
    }}
  
  enable("dmaup")
}) #input sas