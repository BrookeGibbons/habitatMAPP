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