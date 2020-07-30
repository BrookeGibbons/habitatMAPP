function(input, output, session) {
  
  # Working BG 28/07/20
  map.dat <- reactive({
    req(input$leaflet.marine.park)
    
    dat %>% 
      dplyr::filter(marine.park %in% input$leaflet.marine.park)
  })
  
  
  output$leaflet.map <- renderLeaflet({
    
    map.dat <- map.dat() # working  BG 28/07/20
    # map.dat <- dat
    
    lng1 <- min(map.dat$longitude)
    lat1 <- min(map.dat$latitude)
    lng2 <- max(map.dat$longitude)
    lat2 <- max(map.dat$latitude)
    
    leaflet() %>% 
      addTiles(group = "OSM (default)")%>%
      addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
      addControl(html = markerLegendHTML(IconSet = IconSet), position = "bottomleft")%>%
      flyToBounds(lng1, lat1, lng2, lat2)%>%
      
      # stereo-BRUV Images
      addAwesomeMarkers(data=filter(map.dat, source%in%c("stereo-bruv.image")),
                        icon = icon.image,
                        clusterOptions = markerClusterOptions(),
                        group = "stereo-BRUV images",
                        popup = map.dat$image,
                        popupOptions=c(closeButton = TRUE,
                                       minWidth = 0,
                                       maxWidth = 700 # changed from 500 BG 28/07
                        ))%>%
      
      # stereo-BRUV video
      addAwesomeMarkers(data=filter(map.dat, source%in%c("fish.video")),
                        icon = icon.video,
                        popup = map.dat$fish,
                        # clusterOptions = markerClusterOptions(),
                        group="stereo-BRUV videos",
                        popupOptions=c(closeButton = TRUE,
                                       minWidth = 0,maxWidth = 700))%>%
      
      # 3D models
      addAwesomeMarkers(data=filter(map.dat, source%in%c("3d.model")),
                        icon = icon.laptop,
                        popup = map.dat$auv,
                        # clusterOptions = markerClusterOptions(),
                        group="3D models",
                        popupOptions=c(closeButton = TRUE,
                                       minWidth = 0,maxWidth = 500))%>%
      
      # Ngari Capes Marine Parks
      addPolygons(data = ngari.mp, weight = 1, color = "black", 
                  fillOpacity = 0.5, fillColor = "#c1d72f", 
                  group = "State marine parks", label=ngari.mp$Name)%>%
      
      # State Marine Parks
      addPolygons(data = state.mp, weight = 1, color = "black", 
                  fillOpacity = 0.5, fillColor = ~state.pal(zone), 
                  group = "State marine parks", label=state.mp$COMMENTS)%>%
      
      # Add a legend
      addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
                title="State Zones",
                position = "bottomright", group = "State marine parks")%>%
      
      # Commonwealth Marine Parks
      addPolygons(data = commonwealth.mp, weight = 1, color = "black", 
                  fillOpacity = 0.5, fillColor = ~commonwealth.pal(zone), 
                  group = "Commonwealth marine parks", label=commonwealth.mp$ZoneName)%>%
      
      # Add a legend
      addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
                title="Commonwealth Zones",
                position = "bottomright", group = "Commonwealth marine parks")%>%
      
      # # Add a legend
      # addLegend(pal = testpal, values = commonwealth.mp$IUCN, opacity = 1,
      #           title="IUCN Protected Area Category",
      #           position = "bottomright", group = "State marine parks")%>%
      
      addLayersControl(
        baseGroups = c("OSM (default)", "World Imagery"),
        overlayGroups = c("stereo-BRUV images",
                          "stereo-BRUV videos",
                          "3D models",
                          "State marine parks",
                          "Commonwealth marine parks"), options = layersControlOptions(collapsed = FALSE))%>% 
      hideGroup("State marine parks")%>%
      hideGroup("Commonwealth marine parks")
    
    
    
    
  })
  
  
}