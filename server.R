function(input, output, session) {
  
  
# Filter data to selected marine park (should make plotting faster) ----
  map.dat <- reactive({
    req(input$leaflet.marine.park)
    
    dat %>% 
      dplyr::filter(marine.park %in% input$leaflet.marine.park)
  })

# Create leaflet explore map ---- 
  output$imagery.leaflet <- renderLeaflet({
    # req(input$leaflet.zoom)
    
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
    
    # simulate building
    show_loading(elem = "leafletBusy")
    
    leaflet <- leaflet() %>% 
      addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
      addTiles(group = "Open Street Map")%>%
      addControl(html = markerLegendHTML(IconSet = IconSet), position = "bottomleft")%>%
      # flyToBounds(lng1, lat1, lng2, lat2)%>%
      fitBounds(lng1, lat1, lng2, lat2)%>%
      
      # stereo-BRUV Images
      addAwesomeMarkers(data=image.popups,
                        icon = icon.habitat,
                        clusterOptions = markerClusterOptions(iconCreateFunction =
                        JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(56,169,220,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                        group = "Habitat imagery",
                        popup = image.popups$popup,
                        popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # stereo-BRUV habitat videos
      addAwesomeMarkers(data=habitat.highlights.popups,
                        icon = icon.habitat,
                        popup = habitat.highlights.popups$popup,
                        clusterOptions = markerClusterOptions(iconCreateFunction =
                                                                JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(56,169,220,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                        group="Habitat imagery",
                        popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # stereo-BRUV fish videos
      addAwesomeMarkers(data=fish.highlights.popups,
                        icon = icon.fish,
                        popup = fish.highlights.popups$popup,
                        clusterOptions = markerClusterOptions(iconCreateFunction =
                                                                JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(255,137,121,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                        group="Fish highlights",
                        popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
    
      # 3D models
      addAwesomeMarkers(data=threed.model.popups,
                        icon = icon.models,
                        popup = threed.model.popups$popup,
                        clusterOptions = markerClusterOptions(iconCreateFunction =
                                                                JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(239,146,46,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                        group="3D models",
                        popupOptions=c(closeButton = TRUE, minWidth = 0,maxWidth = 700)
                        )%>%
      
      
      # Ngari Capes Marine Parks
      addPolygons(data = ngari.mp, weight = 1, color = "black", 
                  fillOpacity = 0.8, fillColor = "#7bbc63", 
                  group = "State Marine Parks", label=ngari.mp$Name)%>%
      
      # State Marine Parks
      addPolygons(data = state.mp, weight = 1, color = "black", 
                  fillOpacity = 0.8, fillColor = ~state.pal(zone), 
                  group = "State Marine Parks", label=state.mp$COMMENTS)%>%
      
      # Add a legend
      addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
                title="State Zones",
                position = "bottomright", group = "State Marine Parks")%>%
      
      # Commonwealth Marine Parks
      addPolygons(data = commonwealth.mp, weight = 1, color = "black", 
                  fillOpacity = 0.8, fillColor = ~commonwealth.pal(zone), 
                  group = "Australian Marine Parks", label=commonwealth.mp$ZoneName)%>%
      
      # Add a legend
      addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
                title="Australian Marine Park Zones",
                position = "bottomright", group = "Australian Marine Parks")%>%
      
      # # Add a legend
      # addLegend(pal = testpal, values = commonwealth.mp$IUCN, opacity = 1,
      #           title="IUCN Protected Area Category",
      #           position = "bottomright", group = "State marine parks")%>%
      
      addLayersControl(
        baseGroups = c("World Imagery","Open Street Map"),
        overlayGroups = c("Fish highlights",
                          "Habitat imagery",
                          "3D models",
                          "State Marine Parks",
                          "Australian Marine Parks"), options = layersControlOptions(collapsed = FALSE))%>% 
      hideGroup("State Marine Parks")%>%
      hideGroup("Australian Marine Parks")#%>%
      #hideGroup("Habitat imagery")
    
    
    # zoom.method(lng1, lat1, lng2, lat2) %>%
      # flyToBounds(lng1, lat1, lng2, lat2)%>%
      # fitBounds(lng1, lat1, lng2, lat2)%>%
        # 
        # if (input$leaflet.zoom%in%TRUE) {
        #   leaflet <- leaflet %>% flyToBounds(lng1, lat1, lng2, lat2)
        # } else {
        #   leaflet <- leaflet %>% fitBounds(lng1, lat1, lng2, lat2)
        # }
        # 
    
    Sys.sleep(5)
    hide_loading(elem = "leafletBusy")
    return(leaflet)
    # leaflet
    
  })
  
  # Habitat pie plot ----
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
        width = 20, transitionTime = 10)
  })
  
  
  bubble.data <- reactive({
    req(input$bubble.marine.park, input$bubble.method)
    
    bubble.dat <- hab.data %>%
      dplyr::filter(marine.park%in%input$bubble.marine.park)
    
    if (input$bubble.method == "all") {
      bubble.dat
      
    } else {
      bubble.method <- input$bubble.method
      filter(bubble.dat, method == bubble.method)
    }
    
  })
  
  
  # logos
  output$logos <- renderImage({
      return(list(
        src = "images/logos-stacked.png",
        width = 350,
        height = 600,
        contentType = "image/png",
        alt = "Face"
      ))
    
  }, deleteFile = FALSE)
  
  # Habitat bubble plot ----
  
  output$bubble.leaflet <- renderLeaflet({
    req(input$bubble.habitat)
    
    bubble.dat<-bubble.data()%>%
      dplyr::select(method, sample, longitude, latitude, Consolidated, Macroalgae, Seagrasses, Sponges, Stony.corals, Turf.algae, Unconsolidated, Other) # "biota.ascidians", "biota.crinoids", "biota.invertebrate.complex","biota.seagrasses",
    # Gather habitat to bubble plot easier
    
    habitat<-tidyr::gather(bubble.dat,"Consolidated","Macroalgae",
                    "Seagrasses","Sponges",
                    "Stony.corals","Turf.algae",
                    "Unconsolidated","Other",key="habitat.type",value="percent.cover")%>%glimpse()
    
    habitat<-habitat%>%
      dplyr::mutate(habitat.type=ga.capitalise(habitat.type))%>%
      dplyr::mutate(habitat.type=str_replace_all(.$habitat.type, c("[^[:alnum:]]"=" ")))
    
    habitat.bubble<-habitat%>%
      dplyr::filter(habitat.type==input$bubble.habitat)
    
    map <- leaflet(habitat.bubble) %>%
      addTiles()%>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    
    # if (input$shapefile.bubble==TRUE) {
    #   map<-map%>%
    #     addPolygons(data = new.shp,weight = 1,color = "black", fillOpacity = 0.5,fillColor = "#7bbc63",group = "group",label=new.shp$Name)
    # }
    
    overzero <- habitat.bubble%>%
      filter(percent.cover > 0)
    
    equalzero <- habitat.bubble%>%
      filter(percent.cover == 0)
    
    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~ latitude, lng = ~ longitude,
          radius = ~((percent.cover/max(percent.cover))*15), fillOpacity = 0.5, stroke = FALSE,
          label = ~as.character(percent.cover)
        )
    }
    if (nrow(equalzero)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero, lat = ~ latitude, lng = ~ longitude,
          radius = 2, fillOpacity = 0.5, color = "white",stroke = FALSE,
          label = ~as.character(percent.cover)
        )
    }
    map
  })
  
  
}