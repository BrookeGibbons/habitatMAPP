# Load libraries ----
library(dplyr)
library(DT)
library(forcats)
library(ggplot2)
library(GlobalArchive)
library(htmlwidgets)
library(leaflet)
library(leafpop)
library(mapview)
library(profvis)
library(raster)
library(rgdal)
library(rgeos)
library(rmapshaper)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(sf)
library(stringr)

# Bring in data ----
# Load 2019 ningaloo metadata ----
ning.bruv.metadata <- read.csv("data/2019-08_Ningaloo_metadata.csv") %>%
  mutate(sample = as.character(sample))

# Load 2014 geographe bay metadata ----
gb.bruv.metadata <- read.csv("data/2014-12_Geographe.Bay_stereoBRUVs_Metadata.csv")

# Load 2020 south west metadata ----
sw.bruv.metadata <- read.csv("data/2020-06_south-west_stereoBRUVs_metadata.csv")

# Create dataframe for 2014 Geographe Bay BRUV images for plotting ----
gb.bruv.image <- gb.bruv.metadata %>%
  dplyr::mutate(image=paste0("https://marineecology.io/images/2014-12_BRUVs_Forward/",Sample,".jpg",sep="")) %>%
  ga.clean.names() %>%
  dplyr::mutate(source="stereo-bruv.image") %>%
  mutate(height='"230"') %>% mutate(width='"405"') %>%
  mutate(image=paste0('<iframe src=',image,' height=',height,' width=',width,'></iframe>')) %>%
  glimpse()

# Create dataframe for 2019 Ningaloo BRUV images for plotting ----
ning.bruv.image <- ning.bruv.metadata %>%
  dplyr::mutate(image=paste0("https://marineecology.io/images/habitatmapp/ningaloo/",sample,".jpg",sep="")) %>% 
  ga.clean.names() %>%
  dplyr::mutate(source="stereo-bruv.image") %>%
  mutate(height='"365"')%>%mutate(width='"645"')%>%
  mutate(image=paste0('<iframe src=',image,' height=',height,' width=',width,'></iframe>')) %>%
  glimpse()

# Create dataframe for 2019 Ningaloo BRUV images for plotting ----
sw.bruv.image <- sw.bruv.metadata %>%
  ga.clean.names() %>%
  dplyr::mutate(image=paste0("https://marineecology.io/images/habitatmapp/sw/",sample,".jpg",sep="")) %>% # NEED TO UPDATE THIS
  ga.clean.names() %>%
  dplyr::mutate(source="stereo-bruv.image") %>%
  mutate(height='"365"')%>%mutate(width='"645"') %>%
  mutate(image=paste0('<iframe src=',image,' height=',height,' width=',width,'></iframe>')) %>%
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
                            
                            '<video width="645" autoplay controls
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
gb.dat <- bind_rows(gb.bruv.image,gb.bruv.video,gb.auv.video) %>%
  dplyr::select(latitude,longitude,image,bruv.video,auv.video,source) %>%
  dplyr::mutate(marine.park="Geographe Bay") %>%
  glimpse()

ning.dat <- bind_rows(ning.bruv.image) %>%
  dplyr::select(latitude,longitude,image,source) %>% # ,bruv.video,auv.video,source
  dplyr::mutate(marine.park="Ningaloo") %>%
  glimpse()

sw.dat <- bind_rows(sw.bruv.image) %>%
  dplyr::select(latitude,longitude,image,source) %>% # ,bruv.video,auv.video,source
  dplyr::mutate(marine.park="South-west Corner") %>%
  glimpse()

dat <- bind_rows(gb.dat,ning.dat, sw.dat)

# Make icon for images and videos----
icon.image <- makeAwesomeIcon(icon = "image", library = "fa")
icon.video <- makeAwesomeIcon(icon = "video-camera", library = "fa", markerColor = "lightred", iconColor = "black")
icon.laptop <- makeAwesomeIcon(icon = "laptop", library = "fa", markerColor = "orange", iconColor = "black")

IconSet <- awesomeIconList(
  "stereo-BRUV video"   = makeAwesomeIcon(icon = "video-camera", library = "fa", markerColor = "lightred"),
  "stereo-BRUV image" = makeAwesomeIcon(icon = "image", library = "fa"),
  "AUV 3D models" = makeAwesomeIcon(icon = "laptop", library = "fa", markerColor = "orange")
)

# Spatial files ----
# State marine parks ----
ngari.mp <- readOGR("data/spatial/test1.shp") 
state.mp <- readOGR("data/spatial/WA_MPA_2018.shp")

# filter out unassigned and unclassified
state.mp <- state.mp[!state.mp$ZONE_TYPE %in% c("Unassigned (IUCN IA)","Unassigned (IUCN II)","Unassigned (IUCN III)","Unassigned (IUCN IV)","Unassigned (IUCN VI)","MMA (Unclassified) (IUCN VI)","MP (Unclassified) (IUCN VI)"), ]

# remove all alphanumeric to rename zone type
state.mp$zone<-str_replace_all(state.mp$ZONE_TYPE, c("[^[:alnum:]]"=" "))
state.mp$zone<-str_replace_all(state.mp$zone, c("Conservation Area  IUCN IA "="Conservation",
                                                          "General Use  IUCN II "="General Use",
                                                          "General Use Area  IUCN VI "="General Use",
                                                          "General Use Zone  IUCN II "="General Use",
                                                          "Recreation Area  IUCN II "="Recreation",
                                                          "Recreation Zone  IUCN II "="Recreation",
                                                          "Sanctuary Area  IUCN VI "="Sanctuary",
                                                          "Sanctuary Zone  IUCN IA "="Sanctuary",
                                                          "Special Purpose Zone  Aquaculture   IUCN VI " ="Special Purpose",
                                                          "Special Purpose Zone  Benthic Protection   IUCN IV "="Special Purpose",
                                                          "Special Purpose Zone  Dugong Protection   IUCN IV "="Special Purpose", 
                                                          "Special Purpose Zone  Habitat Protection   IUCN IV " ="Special Purpose",
                                                          "Special Purpose Zone  Pearling   IUCN VI "  ="Special Purpose",    
                                                          "Special Purpose Zone  Puerulus   IUCN IA "  ="Special Purpose", 
                                                          "Special Purpose Zone  Scientific Reference   IUCN II "="Special Purpose",
                                                          "Special Purpose Zone  Scientific Reference   IUCN VI "="Special Purpose",
                                                          "Special Purpose Zone  Seagrass Protection   IUCN IV "="Special Purpose", 
                                                          "Special Purpose Zone  Shore Based Activities   IUCN II "="Special Purpose",
                                                          "Special Purpose Zone  Wildlife Conservation   IUCN VI "="Special Purpose",
                                                          "Special Purpose Zone  Wildlife Viewing and Protection   IUCN IV "="Special Purpose",
                                                          "Special Purpose Zone 1  Shore based Activities   IUCN II "="Special Purpose",       
                                                          "Special Purpose Zone 2  Shore based Activities   IUCN II "="Special Purpose",       
                                                          "Special Purpose Zone 3  Shore based Activities   IUCN II " ="Special Purpose",      
                                                          "Special Purpose Zone 3  Shore based Activities   IUCN VI " ="Special Purpose",      
                                                          "Special Purpose Zone 4  Shore based Activities   IUCN II "="Special Purpose"))

# Commonwealth marine parks ----
commonwealth.mp <- readOGR("data/spatial/AustraliaNetworkMarineParks.shp")
commonwealth.mp$zone<-str_replace_all(commonwealth.mp$ZoneName, c("[^[:alnum:]]"=" "))
commonwealth.mp$zone<-str_replace_all(commonwealth.mp$zone, c(" Zone"="",
                                                                      "Habitat Protection  Lord Howe "="Habitat Protection",
                                                                      "Habitat Protection  Reefs "="Habitat Protection",
                                                                      "Marine National Park"="National Park",
                                                                      "Special Purpose  Mining Exclusion "="Special Purpose",
                                                                      "Special Purpose  Norfolk "="Special Purpose",
                                                                      "Special Purpose  Trawl "="Special Purpose"))

# Create factors for legends and plotting ----
# State marine parks ----
state.mp$zone <- as.factor(state.mp$zone)
state.mp$zone<-fct_relevel(state.mp$zone, "Conservation", "Sanctuary", "Special Purpose", "Recreation", "General Use")

state.pal <- colorFactor(c("#bfaf02", # conservation
                           "#c1d72f", # sanctuary
                           "#ccc1d6", # special purpose
                           "#f4eb0a", # recreational
                           '#b5e2ed' # general use
), state.mp$zone)

# Commonwealth marine parks ----
commonwealth.mp$zone <- as.factor(commonwealth.mp$zone)
commonwealth.mp$zone<-fct_relevel(commonwealth.mp$zone, "Sanctuary", "National Park", "Recreational Use", "Habitat Protection", "Multiple Use", "Special Purpose")

commonwealth.pal <- colorFactor(c("#f6c1d9", # Sanctuary
                         "#7bbc63", # National Park
                         "#fdb930", # Recreational Use
                         "#fff7a3", # Habitat Protection
                         '#b9e6fb', # Multiple Use
                         '#6daee0'# Special Purpose
), commonwealth.mp$zone)

# To plot using IUCN ----
# commonwealth.mp$ZoneIUCN <- str_replace_all(commonwealth.mp$ZoneIUCN,c("IA"="Ia"))

# commonwealth.mp$ZoneIUCN <- fct_collapse(commonwealth.mp$ZoneIUCN,
#                                          Ia = c("Ia", "IA"),
#                                          II = "II",
#                                          IV = "IV",
#                                          VI = "VI")
# commonwealth.mp$IUCN <- as.factor(commonwealth.mp$ZoneIUCN)
# unique(commonwealth.mp$IUCN)
# factpal <- colorFactor(c("#f6c1d9", "#7bbc63",
#                          "#fff7a3","#6daee0"), commonwealth.mp$IUCN) # use when only displaying IUCN
# testpal<-colorFactor(c("#f6c1d9", "#7bbc63",
#                        "#fff7a3","#6daee0"),
#                      levels=c("Ia", "II", "IV", "VI"))

## Colours for plotting ----
# Commonwealth colours
#f6c1d9 - pink 1A, 1a
#7bbc63 - green IUCN II
#fff7a3 - yellow IUCN IV
#b9e6fb - light blue VI
#6daee0 - dark blue VI

# State colours
#bfaf02 - conservation
#c1d72f - sanctuary
#ccc1d6 - special purpose
#f4eb0a - recreational 
#b5e2ed - general use

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
