rm(list=ls()) # Clear memory

# Load libraries ----
library(leaflet)
library(dplyr)
library(mapview)
library(leafpop)
library(sf)
# library(gdal)
library(htmlwidgets)

# Set working directory ----
work.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of script
dir()

setwd(work.dir)

# Load metadata ----
metadata.2014<-read.csv("data/2014-12_Geographe.Bay_stereoBRUVs_Metadata.csv")

# Create dataframe for plotting ----
forwards.2014<-metadata.2014%>%
  mutate(image=paste0("https://marineecology.io/images/2014-12_BRUVs_Forward/",Sample,".jpg",sep=""))%>%
  # filter(Sample=="1")%>%
  glimpse()

forwards.2014<-forwards.2014%>% 
  mutate(height='"225"')%>%mutate(width='"400"')%>%
  mutate(image=paste0('<iframe src=',image,' height=',height,' width=',width,'></iframe>'))

unique(forwards$image)
unique(forwards.2014$image)

# Testing out video links ----
video.dot<-data.frame(c(-33.6249992,-33.6190304),
                c(115.3109674,115.3875792),
                c("video 1", "drop2"),
                c('<iframe width="400" height="300" src="https://www.youtube.com/embed/QFLGJPNairI" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>',
  #                 '<video width="300" controls
  # <source
  #   src="https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/videos/Compilations/test-video.mp4?raw=true"
  #   type="video/mp4">
  #                 </video>',
                  '<div class="sketchfab-embed-wrapper">
    <iframe title="A 3D model" width="400" height="300" src="https://sketchfab.com/models/2f5bb1e3fd824d65a2d090a1f78f3d9a/embed?autostart=1&amp;preload=1&amp;ui_controls=1&amp;ui_infos=1&amp;ui_inspector=1&amp;ui_stop=1&amp;ui_watermark=1&amp;ui_watermark_link=1" frameborder="0" allow="autoplay; fullscreen; vr" mozallowfullscreen="true" webkitallowfullscreen="true"></iframe>
    <p style="font-size: 13px; font-weight: normal; margin: 5px; color: #4A4A4A;">
        <a href="https://sketchfab.com/3d-models/15fps-2f5bb1e3fd824d65a2d090a1f78f3d9a?utm_medium=embed&utm_source=website&utm_campaign=share-popup" target="_blank" style="font-weight: bold; color: #1CAAD9;">15fps</a>
        by <a href="https://sketchfab.com/KyeAdams?utm_medium=embed&utm_source=website&utm_campaign=share-popup" target="_blank" style="font-weight: bold; color: #1CAAD9;">KyeAdams</a>
        on <a href="https://sketchfab.com?utm_medium=embed&utm_source=website&utm_campaign=share-popup" target="_blank" style="font-weight: bold; color: #1CAAD9;">Sketchfab</a>
    </p>
</div>
'
                  ))

auv.dot<-data.frame(c(-33.477925),
                    c(115.2743343),
                    c("auv 1"),
                    c('<video width="300" controls <source
    src="https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/videos/test-auv.mp4?raw=true"
    type="video/mp4"></video>'
                    ))
 
names(video.dot)<-c("lat","lon","point","video")
names(auv.dot)<-c("lat","lon","point","video")

# Make icon for images and videos----
icon.image <- makeAwesomeIcon(icon = "image", library = "fa")
icon.video <- makeAwesomeIcon(icon = "video-camera", library = "fa", markerColor = "lightred", iconColor = "black")
icon.laptop <- makeAwesomeIcon(icon = "laptop", library = "fa", markerColor = "orange", iconColor = "black")

# Create map ----
m<-leaflet(data = forwards.2014) %>%
  # addProviderTiles('Esri.WorldImagery') %>%
  addTiles()%>%
  # addCircleMarkers(popup = popupImage(forwards.2014$image, src = "remote"))%>%
  addAwesomeMarkers(data=forwards.2014,icon = icon.image,
                    popup = forwards.2014$image,
                    popupOptions=c(closeButton = TRUE,
                                   minWidth = 0,
                                   maxWidth = 500
                    ))%>%
  # addAwesomeMarkers(icon = icon.image, popup = popupImage(forwards.2014$image, src = "remote"))%>%
  addAwesomeMarkers(data=video.dot,icon = icon.video,
                    popup = video.dot$video,
                    popupOptions=c(closeButton = TRUE,
                      minWidth = 000,
                      maxWidth = 400
                    )
                    )%>%
  addAwesomeMarkers(data=auv.dot,icon = icon.laptop,
                    popup = auv.dot$video)
m


# Spatial files
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

commonwealth.marineparks <- readOGR(dsn="data/spatial/AustraliaNetworkMarineParks.shp")

# commonwealth.marineparks<-st_as_sf(commonwealth.marineparks)%>%
#   filter(NetName=="South-west")%>%
#   filter(ResName=="Geographe")%>%
#   dplyr::select(ZoneName,geometry)#%>%
#   #rmapshaper::ms_simplify() # to test later to test shiny speed

ngaricapes <- readOGR(dsn="data/spatial/test1.shp")



# ngari<-st_as_sf(ngari)%>%
#   filter(Name=="Ngari Capes")

# Below from habitapp
ngaricapes.id <- fortify(ngaricapes)
class(ngaricapes.id)


ngaricapes@data$id <- 0:(dim(ngaricapes@data)[1]-1) # add id field

new.shp<-readOGR("data/spatial/test1.shp")

ngaricapes.mp <- plyr::join(x = ngaricapes.id,y = ngaricapes@data, by="id")
 
plot(ngaricapes)
proj4string(commonwealth.marineparks)<-CRS(wgs.84)
proj4string(ngaricapes)<-CRS(wgs.84)

map<-m%>%addPolygons(data = new.shp,weight = 1,color = "black", fillOpacity = 0.5,fillColor = "#7bbc63",group = "group",label=new.shp$Name)



setwd(work.dir)
saveWidget(m, file="test.map.html")

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
  "stereo-BRUV video"   = makeAwesomeIcon(icon = "video-camera", library = "fa", markerColor = "lightred", iconColor = "black"),
  "stereo-BRUV image" = makeAwesomeIcon(icon = "image", library = "fa"),
  "AUV photogrammetry" = makeAwesomeIcon(icon = "laptop", library = "fa", markerColor = "orange", iconColor = "black")
)

map.with.legend<-m%>%
  addControl(html = markerLegendHTML(IconSet = IconSet), position = "bottomleft")

map.with.legend

