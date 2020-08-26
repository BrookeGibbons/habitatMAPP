function(input, output, session) {
  
  # Dropdown function -----
  create_dropdown <- function(input_name, choices, label) {
    if (!is.null(input[[input_name]]) && input[[input_name]] %in% choices) {
      selected <- input[[input_name]]
    } else {
      selected <- choices[1]
    }
    
    selectInput(
      inputId = input_name,
      label = label,
      choices = choices,
      selected = selected
    )
  }
  
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
    icon.fish <- makeAwesomeIcon(icon = "video", library = "fa", markerColor = "lightred", iconColor = "black")
    icon.models <- makeAwesomeIcon(icon = "laptop", library = "fa", markerColor = "orange", iconColor = "black")
    
    icon.habitat <- iconList(blue = makeIcon("images/marker_blue.png", iconWidth = 30, iconHeight =40))
    icon.fish <- iconList(blue = makeIcon("images/marker_red.png", iconWidth = 30, iconHeight =40))
    icon.models <- iconList(blue = makeIcon("images/marker_green.png", iconWidth = 30, iconHeight =40))

    
    lng1 <- min(map.dat$longitude)
    lat1 <- min(map.dat$latitude)
    lng2 <- max(map.dat$longitude)
    lat2 <- max(map.dat$latitude)
    
    # simulate building
    # show_loading(elem = "leafletBusy")
    
    leaflet <- leaflet() %>% 
      addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
      addTiles(group = "Open Street Map")%>%
      addControl(html = html_legend, position = "bottomleft")%>% # markerLegendHTML(IconSet = IconSet)
      # flyToBounds(lng1, lat1, lng2, lat2)%>%
      fitBounds(lng1, lat1, lng2, lat2)%>%
      
      # stereo-BRUV Images
      # addMarkers(data=image.popups,
      #                   icon = icon.habitat,
      #                   clusterOptions = markerClusterOptions(iconCreateFunction =
      #                   JS("
      #                                     function(cluster) {
      #                                        return new L.DivIcon({
      #                                          html: '<div style=\"background-color:rgba(56,169,220,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
      #                                          className: 'marker-cluster'
      #                                        });
      #                                      }")),
      #                   group = "Habitat imagery",
      #                   popup = image.popups$popup,
      #                   popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      # 
      # stereo-BRUV habitat videos
      addMarkers(data=habitat.highlights.popups,
                        icon = icon.habitat, 
                        popup = habitat.highlights.popups$popup,
                        clusterOptions = markerClusterOptions(iconCreateFunction =
                                                                JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(78,189,220,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                        group="Habitat imagery",
                        popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # stereo-BRUV fish videos
      addMarkers(data=fish.highlights.popups,
                        icon = icon.fish,
                        popup = fish.highlights.popups$popup,
                        clusterOptions = markerClusterOptions(iconCreateFunction =
                                                                JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(237,122,79,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                        group="Fish highlights",
                        popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
    
      # 3D models
      addMarkers(data=threed.model.popups,
                        icon = icon.models,
                        popup = threed.model.popups$popup,
                        clusterOptions = markerClusterOptions(iconCreateFunction =
                                                                JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(73,220,194,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
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
      
      addLayersControl(
        baseGroups = c("World Imagery","Open Street Map"),
        overlayGroups = c("Fish highlights",
                          "Habitat imagery",
                          "3D models",
                          "State Marine Parks",
                          "Australian Marine Parks"), options = layersControlOptions(collapsed = FALSE))%>% 
      hideGroup("State Marine Parks")%>%
      hideGroup("Australian Marine Parks")

    return(leaflet)
    
  })
  
  # Habitat pie plot ----
  pie.data <- reactive({
    req(input$pie.marine.park, input$pie.method)
    
    pie.dat <- hab.data #%>%
      #dplyr::filter(marine.park%in%input$pie.marine.park)
    
    if (input$pie.method == "all") {
      pie.dat
      
    } else {
      pie.method <- input$pie.method
      filter(pie.dat, method == pie.method)
    }
  })

  output$pie.leaflet <- renderLeaflet({
    
    coords<-pie.data()
    
    data<- coords %>%
      dplyr::select(Consolidated,Macroalgae,Seagrasses,Sponges,Stony.corals,Turf.algae,Unconsolidated,Other)
  
    names(data) <- ga.capitalise(names(data))
    
    #4eb570 green
    #d94c45 red
    #bd6539 brown
    #d67cc9 pink
    #78807a grey
    #faef52 yellow
    #d99445 orange
    
    leaflet(coords, width = "100%", height = "800px") %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))%>%
      # addTiles()%>%
      addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
      addTiles(group = "Open Street Map")%>%
      addMapPane("State Marine Parks", zIndex = 300) %>%
      addMapPane("Australian Marine Parks", zIndex = 300) %>%
      
      # Ngari Capes Marine Parks
      addPolygons(data = ngari.mp, weight = 1, color = "black", 
                  fillOpacity = 0.8, fillColor = "#7bbc63", 
                  group = "State Marine Parks", label=ngari.mp$Name,
                  options = pathOptions(pane = "State Marine Parks")) %>%
      
      # State Marine Parks
      addPolygons(data = state.mp, weight = 1, color = "black", 
                  fillOpacity = 0.8, fillColor = ~state.pal(zone), 
                  group = "State Marine Parks", label=state.mp$COMMENTS,
                  options = pathOptions(pane = "State Marine Parks")) %>%
      
      # Add a legend
      addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
                title="State Zones",
                position = "bottomright", group = "State Marine Parks")%>%
      
      # Commonwealth Marine Parks
      addPolygons(data = commonwealth.mp, weight = 1, color = "black", 
                  fillOpacity = 0.8, fillColor = ~commonwealth.pal(zone), 
                  group = "Australian Marine Parks", label=commonwealth.mp$ZoneName,
                  options = pathOptions(pane = "Australian Marine Parks")) %>%
      
      # Add a legend
      addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
                title="Australian Marine Park Zones",
                position = "bottomright", group = "Australian Marine Parks")%>%
      
      addLayersControl(baseGroups = c("World Imagery","Open Street Map"),
                       overlayGroups = c("Pie Charts",
                                         "State Marine Parks",
                                         "Australian Marine Parks"), options = layersControlOptions(collapsed = FALSE))%>% 
      hideGroup("State Marine Parks")%>%
      hideGroup("Australian Marine Parks")%>%
      addMinicharts(
        coords$longitude, coords$latitude,
        type = "pie",
        chartdata = data,
        colorPalette = broad.colors,
        width = 20, transitionTime = 10)
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
  
  # Habitat bubble data ----
  bubble.data <- reactive({
    req(input$bubble.marine.park) #, input$bubble.method
    
    bubble.dat <- hab.data %>%
      dplyr::filter(marine.park%in%input$bubble.marine.park)
    
    # if (input$bubble.method == "all") {
    #   bubble.dat
    #   
    # } else {
    #   bubble.method <- input$bubble.method
    #   filter(bubble.dat, method == bubble.method)
    # }
    
  })
  
  # Habitat bubble method dropdown ----
  output$bubble.method.dropdown <- renderUI({
    
    options <- hab.data %>%
      dplyr::filter(marine.park%in%input$bubble.marine.park)%>%
      distinct(method) %>%
      pull("method")
    
    create_dropdown("bubble.method.dropdown", options, NULL)
  })
  
  output$bubble.habitat.dropdown <- renderUI({
    
    dat <- hab.data%>%
      dplyr::filter(marine.park%in%input$bubble.marine.park)%>%
      dplyr::filter(method%in%input$bubble.method.dropdown)
    
    options <- tidyr::gather(dat,"Consolidated","Macroalgae","Macrophytes",
                             "Seagrasses","Sponges",
                             "Stony.corals","Turf.algae",
                             "Unconsolidated","Urchin.density","Other",
                             key="habitat.type",value="percent.cover")
    
    options.use<-options%>%
      dplyr::mutate(habitat.type=ga.capitalise(habitat.type))%>%
      dplyr::mutate(habitat.type=str_replace_all(.$habitat.type, c("[^[:alnum:]]"=" ")))%>%
      dplyr::filter(percent.cover>0)%>%
      distinct(habitat.type) %>%
      arrange()%>%
      pull("habitat.type")
    
    create_dropdown("bubble.habitat.dropdown", options.use, NULL)
  })
  
  # Habitat bubble plot ----
  output$bubble.leaflet <- renderLeaflet({
    req(input$bubble.habitat.dropdown)
    
    bubble.dat<-bubble.data()%>%
      dplyr::filter(method%in%c(input$bubble.method.dropdown))%>%
      dplyr::select(method, sample, longitude, latitude, Consolidated, Macroalgae, Seagrasses, Sponges, Stony.corals, Turf.algae, Unconsolidated, Other,Macrophytes,Urchin.density) # "biota.ascidians", "biota.crinoids", "biota.invertebrate.complex","biota.seagrasses",
    # Gather habitat to bubble plot easier
    
    habitat<-tidyr::gather(bubble.dat,"Consolidated","Macroalgae","Macrophytes",
                           "Seagrasses","Sponges",
                           "Stony.corals","Turf.algae",
                           "Unconsolidated","Urchin.density","Other",key="habitat.type",value="percent.cover")
    
    habitat<-habitat%>%
      dplyr::mutate(habitat.type=ga.capitalise(habitat.type))%>%
      dplyr::mutate(habitat.type=str_replace_all(.$habitat.type, c("[^[:alnum:]]"=" ")))
    
    unique(habitat$habitat.type)
    
    habitat.bubble<-habitat%>%
      dplyr::filter(habitat.type==input$bubble.habitat.dropdown)
    
    map <- leaflet(habitat.bubble) %>%
      addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
      addTiles(group = "Open Street Map")%>%
      addMapPane("State Marine Parks", zIndex = 300) %>%
      addMapPane("Australian Marine Parks", zIndex = 300) %>%
      
      # Ngari Capes Marine Parks
      addPolygons(data = ngari.mp, weight = 1, color = "black", 
                  fillOpacity = 0.8, fillColor = "#7bbc63", 
                  group = "State Marine Parks", label=ngari.mp$Name,
                  options = pathOptions(pane = "State Marine Parks")) %>%
      
      # State Marine Parks
      addPolygons(data = state.mp, weight = 1, color = "black", 
                  fillOpacity = 0.8, fillColor = ~state.pal(zone), 
                  group = "State Marine Parks", label=state.mp$COMMENTS,
                  options = pathOptions(pane = "State Marine Parks")) %>%
      
      # Add a legend
      addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
                title="State Zones",
                position = "bottomright", group = "State Marine Parks")%>%
      
      # Commonwealth Marine Parks
      addPolygons(data = commonwealth.mp, weight = 1, color = "black", 
                  fillOpacity = 0.8, fillColor = ~commonwealth.pal(zone), 
                  group = "Australian Marine Parks", label=commonwealth.mp$ZoneName,
                  options = pathOptions(pane = "Australian Marine Parks")) %>%
      
      # Add a legend
      addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
                title="Australian Marine Park Zones",
                position = "bottomright", group = "Australian Marine Parks")%>%
      
      addLayersControl(baseGroups = c("World Imagery","Open Street Map"),
                       overlayGroups = c("State Marine Parks",
                                         "Australian Marine Parks"), options = layersControlOptions(collapsed = FALSE))%>% 
      hideGroup("State Marine Parks")%>%
      hideGroup("Australian Marine Parks")%>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    
    overzero <- habitat.bubble%>%
      filter(percent.cover > 0)
    
    equalzero <- habitat.bubble%>%
      filter(percent.cover == 0)
    
    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~ latitude, lng = ~ longitude,
          radius = ~((percent.cover/max(percent.cover))*15), fillOpacity = 0.5, stroke = FALSE,
          color = "#1F67E0",
          label = ~as.character(percent.cover), group = "Bubbles"
        )
    }
    if (nrow(equalzero)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero, lat = ~ latitude, lng = ~ longitude,
          radius = 2, fillOpacity = 0.5, color = "white",stroke = FALSE,
          label = ~as.character(percent.cover), group = "Bubbles"
        )
    }
    
    if (input$bubble.marine.park%in%"Dongara") {
      map <- map %>%
        addCircleMarkers(
          data = trapping, lat = ~ latitude, lng = ~ longitude,
          stroke = FALSE, group = "Trap locations",fillOpacity = 1,
          radius = 4, color = "#67E01F")%>%
        addCircleMarkers(
          data = monitoring, lat = ~ latitude, lng = ~ longitude,
          stroke = FALSE, group = "Monitoring",fillOpacity = 1,
          radius = 4, color = "#E01F67")%>%
        
        # addPolygons(data = dongara.shp, weight = 1, color = "black",
        #             fillOpacity = 0.8, #fillColor = ~dongara.pal(seagrass.state),
        #             group = "Seagrass analysis", label=dongara.shp$seagrass.state) %>%
        
        addLayersControl(baseGroups = c("World Imagery","Open Street Map"),
                         overlayGroups = c("State Marine Parks",
                                           "Australian Marine Parks",
                                           "Trap locations",
                                           "Monitoring"#, "Seagrass analysis"
                                           ), options = layersControlOptions(collapsed = FALSE))%>% 
        hideGroup("State Marine Parks")%>%
        hideGroup("Australian Marine Parks")%>%
        hideGroup("Seagrass analysis")%>%
        addLegendCustom(colors = c("#67E01F", "#E01F67"), labels = c("Trapping", "Monitoring"), sizes = c(20, 20))
    }
    
    map
  })
  

# FISH ----
  # Top species ----
  output$top.species <- renderPlot({
    maxn.sum<-maxn%>%
      group_by(scientific)%>%
      dplyr::summarise(maxn=sum(maxn))%>%
      ungroup()%>%
      top_n(input$species.limit)
    
    length.sum<-length%>%
      group_by(scientific)%>%
      dplyr::summarise(number=sum(number))%>%
      ungroup()%>%
      top_n(input$species.limit)
    
    mass.sum<-mass%>%
      replace_na(list(mass.g=0))%>%
      group_by(scientific)%>%
      dplyr::summarise(mass.g=sum(mass.g))%>%
      ungroup()%>%
      dplyr::mutate(mass.kg=mass.g/1000)%>%
      top_n(input$species.limit)
    
    if (input$fish.metric%in%c("maxn")) {
    
    plot<-ggplot(maxn.sum, aes(x=reorder(scientific,maxn), y=maxn)) +   
        geom_bar(stat="identity",position=position_dodge())+
        coord_flip()+
        xlab("Species")+
        ylab(expression(Overall~abundance~(Sigma~MaxN)))+
        Theme1+
        theme(axis.text.y = element_text(face="italic"))+
        theme_collapse+
        scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
    } else if (input$fish.metric%in%c("length")){
      
      plot<-ggplot(length.sum, aes(x=reorder(scientific,number), y=number)) +   
        geom_bar(stat="identity",position=position_dodge())+
        coord_flip()+
        xlab("Species")+
        ylab(expression(Overall~number~measured~(Sigma~Number)))+
        Theme1+
        theme(axis.text.y = element_text(face="italic"))+
        theme_collapse+
        scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
    } else {
      plot<-ggplot(mass.sum, aes(x=reorder(scientific,mass.kg), y=mass.kg)) +   
        geom_bar(stat="identity",position=position_dodge())+
        coord_flip()+
        xlab("Species")+
        ylab(expression(Sum~of~mass~(KG)))+
        Theme1+
        theme(axis.text.y = element_text(face="italic"))+
        theme_collapse+
        scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
    }
    
    plot
    
  })
  
  # Species dropdown -----
  output$fish.species.dropdown <- renderUI({

    options <- maxn %>%
      dplyr::mutate(genus=ifelse(genus%in%c(NA,"NA","Unknown","NANA"),as.character(family),as.character(genus)))%>%
      dplyr::group_by(family,genus,species,scientific)%>%
      dplyr::summarise(n=sum(maxn))%>%
      arrange(-n)%>%
      distinct(scientific) %>%
      pull("scientific")
    
    create_dropdown("fish.species.dropdown", options, NULL)
  })
  
  # Marine park zones plot -----
  output$fish.zones <- renderPlot({
    
    maxn.per.sample<-maxn%>%
      dplyr::left_join(metadata.regions)%>%
      dplyr::filter(scientific%in%c(input$fish.species.dropdown))%>%
      dplyr::group_by(sample,zone)%>%
      dplyr::summarise(maxn=sum(maxn))
    
    mass.per.sample<-mass%>%
      # dplyr::filter(mass.g>0)%>%
      replace_na(list(mass.g=0))%>%
      dplyr::left_join(metadata.regions)%>%
      dplyr::filter(scientific%in%c(input$fish.species.dropdown))%>%
      dplyr::group_by(sample,zone)%>%
      dplyr::summarise(mass.g=sum(mass.g))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(mass.kg=mass.g/1000)
    
    length.data<-length%>%
      dplyr::left_join(metadata.regions)%>%
      dplyr::filter(scientific%in%c(input$fish.species.dropdown))
    
    scientific.name<-input$fish.species.dropdown
    
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                  gp=gpar(col="black", fontsize=13, fontface="italic")))
    

    if (input$fish.metric%in%c("maxn")) {
      
    plot <- ggplot(maxn.per.sample, aes(x = zone,y=maxn, fill = zone)) + 
      stat_summary(fun.y=mean, geom="bar",colour="black") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
      geom_hline(aes(yintercept=0))+
      xlab("Zone")+
      ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      annotation_custom(grob.sci)+ 
      Theme1+
      scale_fill_manual(values=c("#63BC78", "#7bbc63", "#fff7a3","#b9e6fb","#ccc1d6","#A463BC"))
    
    } else if (input$fish.metric%in%c("length")){
      plot <- ggplot(length.data,aes(x = factor(zone), y = length,  fill = zone, notch=FALSE, outlier.shape = NA),alpha=0.5) + 
        stat_boxplot(geom='errorbar')+
        geom_boxplot(outlier.color = NA, notch=FALSE)+
        stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
        scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
        xlab("Zone") + ylab("Length (mm)") +
        annotation_custom(grob.sci)+ 
        Theme1+
        scale_fill_manual(values=c("#63BC78", "#7bbc63", "#fff7a3","#b9e6fb","#ccc1d6","#A463BC"))
      
    } else {
      posn.d <- position_dodge(0.9)
      
      plot <- ggplot(mass.per.sample, aes(x = zone,y=mass.kg, fill = zone)) + 
        stat_summary(fun.y=mean, geom="bar",colour="black",position="dodge") +
        stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1,position=posn.d) +
        geom_hline(aes(yintercept=0))+
        xlab("Zone")+
        ylab("Average biomass per stereo-BRUV\nKG (+/- SE)")+
        scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
        annotation_custom(grob.sci)+ 
        Theme1+
        scale_fill_manual(values=c("#63BC78", "#7bbc63", "#fff7a3","#b9e6fb","#ccc1d6","#A463BC"))
      
    }
    
    plot
      
  # Status plots ----    
  })
  
  output$fish.status <- renderPlot({
    
    maxn.per.sample<-maxn%>%
      left_join(metadata.regions)%>%
      dplyr::filter(scientific%in%c(input$fish.species.dropdown))%>%
      group_by(sample,status)%>%
      summarise(maxn=sum(maxn))
    
    
    mass.per.sample<-mass%>%
      replace_na(list(mass.g=0))%>%
      dplyr::left_join(metadata.regions)%>%
      dplyr::filter(scientific%in%c(input$fish.species.dropdown))%>%
      dplyr::group_by(sample,status)%>%
      dplyr::summarise(mass.g=sum(mass.g))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(mass.kg=mass.g/1000)
    
    
    length.data<-length%>%
      dplyr::left_join(metadata.regions)%>%
      dplyr::filter(scientific%in%c(input$fish.species.dropdown))
    
    
    scientific.name<-input$fish.species.dropdown
    
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                  gp=gpar(col="black", fontsize=13, fontface="italic")))

    if (input$fish.metric%in%c("maxn")) {
      plot <- ggplot(maxn.per.sample, aes(x = status,y=maxn, fill = status)) + 
        stat_summary(fun.y=mean, geom="bar",colour="black") +
        stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
        geom_hline(aes(yintercept=0))+
        xlab("Status")+
        ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
        scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
        annotation_custom(grob.sci)+ 
        Theme1+
        scale_fill_manual(values = c("Fished" = "#BC637B", "No-take" = "#7bbc63"))
      
    } else if (input$fish.metric%in%c("length")){
      plot <-    ggplot(length.data,aes(x = length, fill=status), col = "black",alpha=0.5)+
        geom_histogram(alpha=0.5, position="identity",col="black")+
        xlab("Length (mm)") + ylab("Count") +
        scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
        annotation_custom(grob.sci)+ 
        Theme1+
        scale_fill_manual(values = c("Fished" = "#BC637B", "No-take" = "#7bbc63"))
      
    } else {
      posn.d <- position_dodge(0.9)
      
      plot <- ggplot(mass.per.sample, aes(x = status,y=mass.kg, fill = status)) + 
        stat_summary(fun.y=mean, geom="bar",colour="black",position="dodge") +
        stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1,position=posn.d) +
        geom_hline(aes(yintercept=0))+
        xlab("Status")+
        ylab("Average biomass per stereo-BRUV )\nKG (+/- SE)")+
        scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
        annotation_custom(grob.sci)+ 
        Theme1+
        scale_fill_manual(values = c("Fished" = "#BC637B", "No-take" = "#7bbc63"))
    }
    plot
  })
  
}