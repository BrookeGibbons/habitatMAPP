function(input, output, session) {
  
# Filter data to selected marine park (should make plotting faster) ----
  map.dat <- reactive({
    req(input$leaflet.marine.park)
    
    dat %>% 
      dplyr::filter(marine.park %in% input$leaflet.marine.park)
  })

# Create leaflet explore map ---- 
  output$imagery.leaflet <- renderLeaflet({
    
    map.dat <- map.dat() # call in filtered data
    
    habitat.highlights.popups <- filter(map.dat, source%in%c("habitat.highlights"))
    fish.highlights.popups <- filter(map.dat, source%in%c("fish.highlights"))
    threed.model.popups <- filter(map.dat, source%in%c("3d.model"))                                
    
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
      addAwesomeMarkers(data=filter(map.dat, source%in%c("image")),
                        icon = icon.video,
                        clusterOptions = markerClusterOptions(),
                        group = "stereo-BRUV images",
                        popup = map.dat$popup,
                        popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%

      # stereo-BRUV habitat videos
      addAwesomeMarkers(data=habitat.highlights.popups,
                        icon = icon.habitat,
                        popup = habitat.highlights.popups$popup,
                        # clusterOptions = markerClusterOptions(),
                        group="stereo-BRUV videos",
                        popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # stereo-BRUV fish videos
      addAwesomeMarkers(data=fish.highlights.popups,
                        icon = icon.fish,
                        popup = fish.highlights.popups$popup,
                        # clusterOptions = markerClusterOptions(),
                        group="stereo-BRUV videos",
                        popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%

      # 3D models
      addAwesomeMarkers(data=threed.model.popups,
                        icon = icon.models,
                        popup = threed.model.popups$popup,
                        # clusterOptions = markerClusterOptions(),
                        group="3D models",
                        popupOptions=c(closeButton = TRUE, minWidth = 0,maxWidth = 700))%>%
      
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