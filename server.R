function(input, output, session) {
  
  # leaflet.dat <- reactive({
  #   dat%>%
  #     dplyr::filter(source%in%c(input$select.markers))
  # 
  # })
  
  output$map <- renderLeaflet({
    
    
    
    leaflet() %>% 
      addTiles(group = "OSM (default)")%>%
      addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
      addControl(html = markerLegendHTML(IconSet = IconSet), position = "bottomleft")%>%
      flyToBounds(lng1, lat1, lng2, lat2)%>%
      
      # State Marine Parks
      addPolygons(data = state.mp, weight = 1, color = "black", fillOpacity = 0.5, fillColor = "#7bbc63", group = "State marine parks", label=state.mp$Name)%>%
      
      # Commonwealth Marine Parks
      addPolygons(data = commonwealth.mp, weight = 1, color = "black", fillOpacity = 0.5, fillColor = ~factpal(IUCN), group = "Commonwealth marine parks", label=commonwealth.mp$ZoneName)%>%
      addLegend(pal = testpal, values = commonwealth.mp$IUCN, opacity = 1,
                title="IUCN Protected Area Category",
                position = "bottomright", group = "Commonwealth marine parks")%>%
      
      # stereo-BRUV Images
      # addAwesomeMarkers(data=filter(leaflet.dat, source%in%c("stereo-bruv.image")),icon = icon.image, popup = popupImage(leaflet.dat$image, src = "remote"), group = "stereo-BRUV imagery")%>%
      addAwesomeMarkers(data=filter(dat, source%in%c("stereo-bruv.image")),icon = icon.image,group = "stereo-BRUV images",
                            popup = dat$image,
                            popupOptions=c(closeButton = TRUE,
                                           minWidth = 0,
                                           maxWidth = 500
                            ))%>%
      # stereo-BRUV video
      addAwesomeMarkers(data=bruv.video,icon = icon.video, popup = bruv.video$bruv.video, group="stereo-BRUV videos",
                        popupOptions=c(closeButton = TRUE,
                        minWidth = 0,maxWidth = 500))%>%
      
      # AUV video
      addAwesomeMarkers(data=auv.video,icon = icon.laptop, popup = auv.video$auv.video,group="AUV 3D models",
                        popupOptions=c(closeButton = TRUE,
                                       minWidth = 0,maxWidth = 500))%>%
      
      addLayersControl(
        baseGroups = c("OSM (default)", "World Imagery"),
        overlayGroups = c("stereo-BRUV images",
                          "stereo-BRUV videos",
                          "AUV 3D models",
                          "State marine parks",
                          "Commonwealth marine parks"),
        options = layersControlOptions(collapsed = FALSE)
      )%>% hideGroup("State marine parks")%>%
      hideGroup("Commonwealth marine parks")
  })
  
  # observe({
  #   
  #   leaflet.dat<-leaflet.dat()
  #  
  #   leafletProxy("map", data = leaflet.dat) %>%
  #     clearShapes() %>%
  #     clearMarkers() %>%
  #     addPolygons(data = new.shp,weight = 1,color = "black", fillOpacity = 0.5,fillColor = "#7bbc63",group = "group",label=new.shp$Name)%>%
  #     # addAwesomeMarkers(data=filter(leaflet.dat, source%in%c("stereo-bruv.image")),icon = icon.image, popup = popupImage(leaflet.dat$image, src = "remote"))%>%
  #     
  #     addAwesomeMarkers(data=filter(leaflet.dat, source%in%c("stereo-bruv.image")),icon = icon.image,
  #                       popup = leaflet.dat$image,
  #                       popupOptions=c(closeButton = TRUE,
  #                                      minWidth = 0,
  #                                      maxWidth = 500
  #                       ))%>%
  #     
  #     addAwesomeMarkers(data=filter(leaflet.dat, source%in%c("stereo-bruv.video")),icon = icon.video,popup = leaflet.dat$video)%>%   
  #     addAwesomeMarkers(data=filter(leaflet.dat, source%in%c("auv.video")),icon = icon.laptop, popup = leaflet.dat$video)
  # })
  # 

  
}