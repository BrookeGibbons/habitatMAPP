library(shinydashboard)
library(leaflet)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(DT)
library(profvis)
library(leaflet)
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


# rgeos::gSimplify on your spatial data
# rmapshaper::ms_simplify works better than rgeos::gSimplify for simplifying polyons

# Load metadata ----
bruv.metadata<-read.csv("data/2014-12_Geographe.Bay_stereoBRUVs_Metadata.csv")

# Create dataframe for plotting ----
bruv.image<-bruv.metadata%>%
  dplyr::mutate(image=paste0("https://marineecology.io/images/2014-12_BRUVs_Forward/",Sample,".jpg",sep=""))%>%
  ga.clean.names()%>%
  dplyr::mutate(source="stereo-bruv.image")%>%
  glimpse()

# Tempory video links ----
bruv.video<-data.frame(c(-33.6249992,-33.6190304),
                      c(115.3109674,115.3875792),
                      c("video 1", "drop2"),
                      c('<iframe width="300" height="200" src="https://www.youtube.com/embed/QFLGJPNairI" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>',
                        '<video width="300" controls
  <source
    src="https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/videos/Compilations/test-video.mp4?raw=true"
    type="video/mp4">
                  </video>'
                      ))

auv.video<-data.frame(c(-33.477925),
                    c(115.2743343),
                    c("auv 1"),
                    c('<video width="300" controls <source
    src="https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/videos/test-auv.mp4?raw=true"
    type="video/mp4"></video>'
                    ))

names(bruv.video)<-c("latitude","longitude","sample","video")
names(auv.video)<-c("latitude","longitude","sample","video")

bruv.video<-bruv.video%>%
  dplyr::mutate(source="stereo-bruv.video")

auv.video<-auv.video%>%
  dplyr::mutate(source="auv.video")

# Merge data together for leaflet map ----
dat<-bind_rows(bruv.image,bruv.video,auv.video)%>%
  dplyr::select(latitude,longitude,image,video,source)%>%
  glimpse()

# Make icon for images and videos----
icon.image <- makeAwesomeIcon(icon = "image", library = "fa")
icon.video <- makeAwesomeIcon(icon = "video-camera", library = "fa", markerColor = "lightred", iconColor = "black")
icon.laptop <- makeAwesomeIcon(icon = "laptop", library = "fa", markerColor = "orange", iconColor = "black")

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

lng1<-min(dat$longitude)
lat1<-min(dat$latitude)
lng2<-max(dat$longitude)
lat2<-max(dat$latitude)

# Spatial files
# wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# commonwealth.marineparks <- readOGR(dsn="data/spatial/AustraliaNetworkMarineParks.shp")
# 
# commonwealth.marineparks<-st_as_sf(commonwealth.marineparks)%>%
#   filter(NetName=="South-west")%>%
#   filter(ResName=="Geographe")%>%
#   dplyr::select(ZoneName,geometry)#%>%
#   #rmapshaper::ms_simplify() # to test later to test shiny speed

# ngaricapes <- readOGR(dsn="data/spatial/test1.shp")

# ngari<-st_as_sf(ngari)%>%
#   filter(Name=="Ngari Capes")

# Below from habitapp
# ngaricapes.id <- fortify(ngaricapes)
# class(ngaricapes.id)
# 
# ngaricapes@data$id <- 0:(dim(ngaricapes@data)[1]-1) # add id field
# 
# ngaricapes.mp <- plyr::join(x = ngaricapes.id,y = ngaricapes@data, by="id")
# 
# plot(ngaricapes)
# # proj4string(commonwealth.marineparks)<-CRS(wgs.84)
# proj4string(ngaricapes)<-CRS(wgs.84)
