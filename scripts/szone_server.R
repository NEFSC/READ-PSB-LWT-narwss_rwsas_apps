disable("eval")

if (file.exists('./scripts/oracleaccess.R') == TRUE) {
  source('./scripts/reactive.R', local = TRUE)$value
  source('./scripts/oracleaccess.R', local = TRUE)$value
  #need criteria$loc for action & dma even though you can only do DMA stuff on the server. It separates it from the NARWSS app.
  criteria$loc <- 'Network'
  
  #query button ----
  observeEvent(input$query, {
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
    MODA <- unique(format(dmaevaldate, "%m-%d"))
    MODAYR <- unique(dmaevaldate, "%m-%d")
    
    ## FAKE DMA ----
    fakeslowzone <- data.frame(long = c(-71, -71, -71, -71, -71),
                          lat = c(42, 42, 42, 42, 42))
    
    fakeslowzone <-
      Polygons(list(Polygon(fakeslowzone, hole = as.logical(NA))), ID = 1)
    ##
    egtable <- data.frame(LONGITUDE = -70)
    
    source('./scripts/sma.R', local = TRUE)$value
    source('./scripts/active_slowzone.R', local = TRUE)$value
    
    sasdma <-
      leaflet(data = smapresent.sp, options = leafletOptions(zoomControl = FALSE)) %>%
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE) %>%
      addPolygons(data = smapresent.sp,
                  weight = 2,
                  color = "red") %>%
      addPolylines(
        data = NEUS_shiplane.sp,
        weight = 1,
        color = "green",
        fill = F
      ) %>%
      addPolygons(data = benigndma,
                  weight = 2,
                  color = "yellow") %>%
      addPolygons(data = extensiondma,
                  weight = 2,
                  color = "orange") %>%
      addPolygons(
        data = benignapz,
        weight = 2,
        color = "yellow",
        dashArray = "4 8"
      ) %>%
      addPolygons(
        data = extensionapz,
        weight = 2,
        color = "orange",
        dashArray = "4 8"
      ) %>%
      addLegend(
        title = "Dynamic Management: solid border = DMA, dashed border = Acoustic",
        colors = c("yellow", "orange", "red"),
        labels = c("Active zone", "Active zone eligible for extension", "SMA"),
        opacity = 0.4,
        position = "topleft"
      )
    
    dma_react$sasdma = sasdma
    
    output$sasdma = renderLeaflet({
      print(dma_react$sasdma)
    })
    output$actdma <- renderTable({
      actdma
    },  striped = TRUE)
    output$actdma_bounds <-
      renderTable({
        actdma_bounds %>% arrange(ID, VERTEX)
      },  striped = TRUE)
    
  })
  
} else {
  disable("query")
}
