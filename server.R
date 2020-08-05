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
    image.popups <- filter(map.dat, source%in%c('image'))
    
    # Having this in the global.R script breaks now - make icons on server side 
    icon.habitat <- makeAwesomeIcon(icon = "image", library = "fa")
    icon.fish <- makeAwesomeIcon(icon = "video-camera", library = "fa", markerColor = "lightred", iconColor = "black")
    icon.models <- makeAwesomeIcon(icon = "laptop", library = "fa", markerColor = "orange", iconColor = "black")
    
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
      addAwesomeMarkers(data=image.popups,
                        icon = icon.habitat,
                        # clusterOptions = markerClusterOptions(),
                        group = "stereo-BRUV habitat",
                        popup = image.popups$popup,
                        popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # stereo-BRUV habitat videos
      addAwesomeMarkers(data=habitat.highlights.popups,
                        icon = icon.habitat,
                        popup = habitat.highlights.popups$popup,
                        # clusterOptions = markerClusterOptions(),
                        group="stereo-BRUV habitat",
                        popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # stereo-BRUV fish videos
      addAwesomeMarkers(data=fish.highlights.popups,
                        icon = icon.fish,
                        popup = fish.highlights.popups$popup,
                        # clusterOptions = markerClusterOptions(),
                        group="stereo-BRUV fish",
                        popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
    
      # 3D models
      addAwesomeMarkers(data=threed.model.popups,
                        icon = icon.models,
                        popup = threed.model.popups$popup,
                        # clusterOptions = markerClusterOptions(),
                        group="3D models",
                        popupOptions=c(closeButton = TRUE, minWidth = 0,maxWidth = 700)
                        )%>%
      
      
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
        overlayGroups = c("stereo-BRUV habitat",
                          "stereo-BRUV fish",
                          "3D models",
                          "State marine parks",
                          "Commonwealth marine parks"), options = layersControlOptions(collapsed = FALSE))%>% 
      hideGroup("State marine parks")%>%
      hideGroup("Commonwealth marine parks")
    
  })
  
  pie.data <- reactive({
    req(input$pie.marine.park, input$pie.method)
    
    pie.dat <- hab.data %>%
      dplyr::filter(marine.park%in%input$pie.marine.park)
    
    if (input$pie.method == "all") {
      pie.dat
      
    } else {
      pie.method <- input$pie.method
      filter(pie.dat, method == pie.method)
    }
  })
  
  output$pie.leaflet <- renderLeaflet({
    
    data<- pie.data() %>%
      dplyr::select(Consolidated,Macroalgae,Seagrasses,Sponges,Stony.corals,Turf.algae,Unconsolidated,Other)
  
    names(data) <- ga.capitalise(names(data))
    
    glimpse(data)
    
    #4eb570 green
    #d94c45 red
    #bd6539 brown
    #d67cc9 pink
    #78807a grey
    #faef52 yellow
    #d99445 orange
    
    broad.colors <- c("#8491B4B2","#1B9E77","#66A61E","#E64B35B2","#7570B3","#D95F02","#E6AB02","black")
    
    basemap <- leaflet(pie.data(), width = "100%", height = "800px") %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))%>%
      addTiles()
    
    # if (input$shapefile.pie==TRUE) {
    #   basemap<-basemap%>%
    #     addPolygons(data = new.shp,weight = 1,color = "black", fillOpacity = 0.5,fillColor = "#7bbc63",group = "group",label=new.shp$Name)
    # }
    
    basemap %>%
      addMinicharts(
        pie.data()$longitude, pie.data()$latitude,
        type = "pie",
        chartdata = data,
        colorPalette = broad.colors,
        width = 20, transitionTime = 0)
  })
  
}