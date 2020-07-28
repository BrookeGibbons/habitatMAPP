library(shinydashboard)
library(leaflet)
library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(DT)
library(profvis)
library(dplyr)
library(mapview)
library(leafpop)
library(sf)
# library(gdal)
library(htmlwidgets)
library(GlobalArchive)

library(rgdal)
library(rgeos)
library(raster)
library(rmapshaper)
library(ggplot2)
library(stringr)
library(forcats)

# Load 2019 ningaloo metadata ----
ning.bruv.metadata<-read.csv("data/2019-08_Ningaloo_metadata.csv")

# Load 2014 Geographe bay metadata ----
gb.bruv.metadata<-read.csv("data/2014-12_Geographe.Bay_stereoBRUVs_Metadata.csv")

# Create dataframe for 2014 Geographe Bay BRUV images for plotting ----
gb.bruv.image<-gb.bruv.metadata%>%
  dplyr::mutate(image=paste0("https://marineecology.io/images/2014-12_BRUVs_Forward/",Sample,".jpg",sep=""))%>%
  ga.clean.names()%>%
  dplyr::mutate(source="stereo-bruv.image")%>%
  mutate(height='"230"')%>%mutate(width='"405"')%>%
  mutate(image=paste0('<iframe src=',image,' height=',height,' width=',width,'></iframe>'))%>%
  glimpse()

# Create dataframe for 2019 Ningaloo BRUV images for plotting ----
ning.bruv.image<-ning.bruv.metadata%>%
  dplyr::mutate(image=paste0("https://marineecology.io/images/2014-12_BRUVs_Forward/",sample,".jpg",sep=""))%>% # NEED TO UPDATE THIS
  ga.clean.names()%>%
  dplyr::mutate(source="stereo-bruv.image")%>%
  mutate(height='"230"')%>%mutate(width='"405"')%>%
  mutate(image=paste0('<iframe src=',image,' height=',height,' width=',width,'></iframe>'))%>%
  glimpse()

# Tempory video links ----
gb.bruv.video<-data.frame(c(-33.6249992,-33.6190304,-33.42207),
                          c(115.3109674,115.3875792,115.37193),
                          c("video 1", "drop2","video 3"),
                          c('<iframe width="300" height="200" src="https://www.youtube.com/embed/QFLGJPNairI?autoplay=1" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>',
                            '<video width="300" autoplay controls
  <source
    src="https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/videos/Compilations/test-video.mp4?raw=true"
    type="video/mp4">
                  </video>',
                            
                            '<video width="300" autoplay controls
  <source
    src="https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/videos/Compilations/test-video-2.mp4?raw=true"
    type="video/mp4">
                  </video>'
                            
                          ))

gb.auv.video<-data.frame(c(-33.477925),
                         c(115.2743343),
                         c("auv 1"),
                         c('<div class="sketchfab-embed-wrapper">
    <iframe title="A 3D model" width="400" height="300" src="https://sketchfab.com/models/2f5bb1e3fd824d65a2d090a1f78f3d9a/embed?autostart=1&amp;preload=1&amp;ui_controls=1&amp;ui_infos=1&amp;ui_inspector=1&amp;ui_stop=1&amp;ui_watermark=1&amp;ui_watermark_link=1" frameborder="0" allow="autoplay; fullscreen; vr" mozallowfullscreen="true" webkitallowfullscreen="true"></iframe>
    <p style="font-size: 13px; font-weight: normal; margin: 5px; color: #4A4A4A;">
        <a href="https://sketchfab.com/3d-models/15fps-2f5bb1e3fd824d65a2d090a1f78f3d9a?utm_medium=embed&utm_source=website&utm_campaign=share-popup" target="_blank" style="font-weight: bold; color: #1CAAD9;">15fps</a>
        by <a href="https://sketchfab.com/KyeAdams?utm_medium=embed&utm_source=website&utm_campaign=share-popup" target="_blank" style="font-weight: bold; color: #1CAAD9;">KyeAdams</a>
        on <a href="https://sketchfab.com?utm_medium=embed&utm_source=website&utm_campaign=share-popup" target="_blank" style="font-weight: bold; color: #1CAAD9;">Sketchfab</a>
    </p>
</div>'
                         ))

names(gb.bruv.video)<-c("latitude","longitude","sample","bruv.video")
names(gb.auv.video)<-c("latitude","longitude","sample","auv.video")

gb.bruv.video<-gb.bruv.video%>%
  dplyr::mutate(source="bruv.video")

gb.auv.video<-gb.auv.video%>%
  dplyr::mutate(source="auv.video")

# Merge data together for leaflet map ----
gb.dat<-bind_rows(gb.bruv.image,gb.bruv.video,gb.auv.video)%>%
  dplyr::select(latitude,longitude,image,bruv.video,auv.video,source)%>%
  dplyr::mutate(marine.park="Geographe Bay")%>%
  glimpse()

ning.dat<-bind_rows(ning.bruv.image)%>%
  dplyr::select(latitude,longitude,image,source)%>% # ,bruv.video,auv.video,source
  dplyr::mutate(marine.park="Ningaloo")%>%
  glimpse()

sw.dat<-bind_rows(ning.bruv.image)%>%
  dplyr::select(latitude,longitude,image,source)%>% # ,bruv.video,auv.video,source
  dplyr::mutate(marine.park="South-west Corner")%>%
  glimpse()

dat<-bind_rows(gb.dat,ning.dat, sw.dat)

# Make icon for images and videos----
icon.image <- makeAwesomeIcon(icon = "image", library = "fa")
icon.video <- makeAwesomeIcon(icon = "video-camera", library = "fa", markerColor = "lightred", iconColor = "black")
icon.laptop <- makeAwesomeIcon(icon = "laptop", library = "fa", markerColor = "orange", iconColor = "black")

# Legend function ----
markerLegendHTML <- function(IconSet) {
  # container div:
  legendHtml <- "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:10px; margin: 0;'> Marker Legend </h4>"
  
  n <- 1
  # add each icon for font-awesome icons icons:
  for (Icon in IconSet) {
    if (Icon[["library"]] == "fa") {
      legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 45px'>",
                          "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                          "<i style='margin-left: 3px; margin-top: 11px; 'class= 'fa fa-",Icon[["icon"]]," fa-inverse'></i>",
                          "</div>",
                          "<p style='position: relative; top: 15px; display: inline-block; ' >", names(IconSet)[n] ,"</p>",
                          "</div>")    
    }
    n<- n + 1
  }
  paste0(legendHtml, "</div>")
}

IconSet <- awesomeIconList(
  "stereo-BRUV video"   = makeAwesomeIcon(icon = "video-camera", library = "fa", markerColor = "lightred"),
  "stereo-BRUV image" = makeAwesomeIcon(icon = "image", library = "fa"),
  "AUV 3D models" = makeAwesomeIcon(icon = "laptop", library = "fa", markerColor = "orange")
)


# Spatial files ----

# state.mp <- readOGR("data/spatial/test1.shp")
# state.mp <- state.mp[state.mp$Name %in% c("East Geographe Bay Sanctuary Zone", "Busselton Jetty Sanctuary Zone","Central Geographe Bay Sanctuary Zone","Eagle Bay Sanctuary Zone","Cape Naturaliste Sanctuary Zone","Injidup Sanctuary Zone","Wyadup Sanctuary Zone","Yallingup Sanctuary Zone"), ]

# Spatial files
state.mp <- readOGR("data/spatial/test1.shp")
# wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

commonwealth.mp <- readOGR("data/spatial/AustraliaNetworkMarineParks.shp")

# commonwealth.mp$ZoneIUCN <- str_replace_all(commonwealth.mp$ZoneIUCN,c("IA"="Ia"))

commonwealth.mp$ZoneIUCN <- fct_collapse(commonwealth.mp$ZoneIUCN,
                                         Ia = c("Ia", "IA"),
                                         II = "II",
                                         IV = "IV",
                                         VI = "VI")

commonwealth.mp$IUCN <- as.factor(commonwealth.mp$ZoneIUCN)

unique(commonwealth.mp$IUCN)
factpal <- colorFactor(c("#f6c1d9", "#7bbc63",
                         "#fff7a3","#6daee0"), commonwealth.mp$IUCN)

testpal<-colorFactor(c("#f6c1d9", "#7bbc63",
                       "#fff7a3","#6daee0"),
                     levels=c("Ia", "II", "IV", "VI"))

#f6c1d9 - pink 1A, 1a
#7bbc63 - green IUCN II
#fff7a3 - yellow IUCN IV
#b9e6fb - light blue VI
#6daee0 - dark blue VI
