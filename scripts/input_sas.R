observe({
  if (is.null(sas_react$egsastab) || nrow(sas_react$egsastab) == 0) {
    shinyjs::disable("sas")
  } else {
    shinyjs::enable("sas")
  }
})

observeEvent(input$sas, {
  disable("sas")
  
  ## upload to Oracle ----
  print("SAS button pressed")
  
  ## egsas upload ----
  
  if (is.null(input$obspeeps)) {
    output$error5 <- renderText({
      "Decide who you are."
    })
    enable("sas")
  } else if (is.null(input$plane)) {
    output$error5 <- renderText({
      "Which plane were you in?"
    })
  } else {
    output$error5 <- renderText({
      ""
    })
    
    shinyjs::disable("sas")
    updateActionButton(session, "sas", label = "Uploaded")
    
    shinyjs::removeClass("sas", "btn-danger")
    shinyjs::addClass("sas", "btn-warning")
    
    if ("ID" %in% colnames(sas_react$egsastab)) {
      egsastab <- sas_react$egsastab %>%
        dplyr::select(-ID)
    } else {
      egsastab <- sas_react$egsastab
    }
    
    egsastab <- egsastab %>%
      mutate(
        OBSERVER_PEOPLE = input$obspeeps,
        OBSERVER_PLATFORM = input$plane,
        ID = 99999,
        #SAS has a procedure to make this number chronological with the table, but it cannot be NULL
        OBSERVER_ORG = 1,
        OOD = 929 #929 means it came through this Shiny App
      )
    
    print(head(egsastab))
    
    for (i in 1:nrow(egsastab)) {
      egvalues <- egsastab
      egvalues$DateTime <-
        paste0("'to_timestamp('",
               egvalues$DateTime,
               "', 'YYYY-MM-DD HH24:MI:SS')")
      datetime_et_val <- paste0(
        "FROM_TZ(CAST(TO_TIMESTAMP('",
        egsastab$DateTime[i],
        "', 'YYYY-MM-DD HH24:MI:SS') AS TIMESTAMP(0)), 'America/New_York')"
      )
      
      egvalues <-
        paste0(sapply(egvalues[i,], function(x)
          paste0("", paste0(x, collapse = "', '"), "'")), collapse = ", '")
      egvalues <- gsub("'to_", "to_", egvalues)
      egvalues <- gsub("')'", "')", egvalues)
      
      dbExecute(
        cnxn,
        paste0(
          "INSERT INTO SAS(SIGHTDATE,GROUPSIZE,LAT,LON,SPECIES_CERT,MOMCALF,FEEDING,DEAD,SAG,ENTANGLED,CATEGORY,ACTION,OBSERVER_PEOPLE,OBSERVER_PLATFORM,ID,OBSERVER_ORG,OOD, DATETIME_ET)
                            VALUES(",
          egvalues,
          ",",
          datetime_et_val,
          ")", sep =""
        )
      )
      dbCommit(cnxn)  ####ADD SEP OR TAKE IT AWAY AND MOVE AROUND EXECUTE AND COMMIT AS NEEDED! HJF SqlQuery replace 8/14 20230626
    }
  }
  
  if (5 %in% egsastab$ACTION_NEW | 4 %in% egsastab$ACTION_NEW) {
    enable("dmaup")
  }
}) #input sas