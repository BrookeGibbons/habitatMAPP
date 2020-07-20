function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    req(input$leaflet.select.marine.park)
    
    dat <- dat %>%
      filter(marine.park %in%c(input$leaflet.select.marine.park))
    
    lng1 <- min(dat$longitude)
    lat1 <- min(dat$latitude)
    lng2 <- max(dat$longitude)
    lat2 <- max(dat$latitude)
    
    leaflet() %>% 
      addTiles(group = "OSM (default)")%>%
      addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
      addControl(html = markerLegendHTML(IconSet = IconSet), position = "bottomleft")%>%
      flyToBounds(lng1, lat1, lng2, lat2)%>%
      
      # State Marine Parks
      addPolygons(data = state.mp, weight = 1, color = "black", 
                  fillOpacity = 0.5, fillColor = "#7bbc63", 
                  group = "State marine parks", label=state.mp$Name)%>%
      
      # Commonwealth Marine Parks
      addPolygons(data = commonwealth.mp, weight = 1, color = "black", 
                  fillOpacity = 0.5, fillColor = ~factpal(IUCN), 
                  group = "Commonwealth marine parks", label=commonwealth.mp$ZoneName)%>%
      
      # Add a legend
      addLegend(pal = testpal, values = commonwealth.mp$IUCN, opacity = 1,
                title="IUCN Protected Area Category",
                position = "bottomright", group = "Commonwealth marine parks")%>%
      # 
      # # Add a legend
      # addLegend(pal = testpal, values = commonwealth.mp$IUCN, opacity = 1,
      #           title="IUCN Protected Area Category",
      #           position = "bottomright", group = "State marine parks")%>%
      
      # stereo-BRUV Images
      addAwesomeMarkers(data=filter(dat, source%in%c("stereo-bruv.image")),
                        icon = icon.image,
                        group = "stereo-BRUV images",
                        popup = dat$image,
                        popupOptions=c(closeButton = TRUE,
                                           minWidth = 0,
                                           maxWidth = 500
                            ))%>%
      # stereo-BRUV video
      addAwesomeMarkers(data=filter(dat, source%in%c("bruv.video")),
                        icon = icon.video,
                        popup = dat$bruv.video,
                        group="stereo-BRUV videos",
                        popupOptions=c(closeButton = TRUE,
                        minWidth = 0,maxWidth = 500))%>%

      # AUV video
      addAwesomeMarkers(data=filter(dat, source%in%c("auv.video")),
                        icon = icon.laptop,
                        popup = dat$auv.video,
                        group="AUV 3D models",
                        popupOptions=c(closeButton = TRUE,
                                       minWidth = 0,maxWidth = 500))%>%
      
      addLayersControl(
        baseGroups = c("OSM (default)", "World Imagery"),
        overlayGroups = c("stereo-BRUV images",
                          "stereo-BRUV videos",
                          "AUV 3D models",
                          "State marine parks",
                          "Commonwealth marine parks"), options = layersControlOptions(collapsed = FALSE))%>% hideGroup("State marine parks")%>%hideGroup("Commonwealth marine parks")
  })

  
}