function(input, output, session) {
  
  leaflet.dat <- reactive({
    dat%>%
      dplyr::filter(source%in%c(input$select.markers))

  })
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addTiles()%>%
      addControl(html = markerLegendHTML(IconSet = IconSet), position = "bottomleft")%>%
      flyToBounds(lng1, lat1, lng2, lat2)#%>%
      #addPolygons(data = ngaricapes,weight = 1,color = "black", fillOpacity = 0.5,fillColor = "#7bbc63",group = "group",label=ngaricapes$Name)
  })    
  
  observe({
    
    leaflet.dat<-leaflet.dat()
   
    leafletProxy("map", data = leaflet.dat) %>%
      clearShapes() %>%
      clearMarkers() %>%
      addAwesomeMarkers(data=filter(leaflet.dat, source%in%c("stereo-bruv.image")),icon = icon.image, popup = popupImage(leaflet.dat$image, src = "remote"))%>%
      addAwesomeMarkers(data=filter(leaflet.dat, source%in%c("stereo-bruv.video")),icon = icon.video,popup = leaflet.dat$video)%>%   
      addAwesomeMarkers(data=filter(leaflet.dat, source%in%c("auv.video")),icon = icon.laptop, popup = leaflet.dat$video)
  })
  

  
}