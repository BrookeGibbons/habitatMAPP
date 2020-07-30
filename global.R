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
  dplyr::mutate(source = "image") %>%
  mutate(height='"365"') %>% mutate(width='"645"') %>%
  mutate(popup=paste0('<iframe src=',image,' height=',height,' width=',width,'></iframe>')) %>%
  dplyr::select(latitude, longitude, popup, source) %>%
  dplyr::mutate(marine.park = "Geographe Bay")

# Create dataframe for 2019 Ningaloo BRUV images for plotting ----
ning.bruv.image <- ning.bruv.metadata %>%
  dplyr::mutate(image=paste0("https://marineecology.io/images/habitatmapp/ningaloo/",sample,".jpg",sep="")) %>% 
  ga.clean.names() %>%
  dplyr::mutate(source = "image") %>%
  mutate(height='"365"')%>%mutate(width='"645"')%>%
  mutate(popup=paste0('<iframe src=',image,' height=',height,' width=',width,'></iframe>')) %>%
  dplyr::select(latitude, longitude, popup, source) %>% # ,bruv.video,auv.video,source
  dplyr::mutate(marine.park = "Ningaloo")

# Create dataframe for 2019 Ningaloo BRUV images for plotting ----
sw.bruv.image <- sw.bruv.metadata %>%
  ga.clean.names() %>%
  dplyr::mutate(image=paste0("https://marineecology.io/images/habitatmapp/sw/",sample,".jpg",sep="")) %>% # NEED TO UPDATE THIS
  ga.clean.names() %>%
  dplyr::mutate(source = "image") %>%
  mutate(height='"365"')%>%mutate(width='"645"') %>%
  mutate(popup=paste0('<iframe src=',image,' height=',height,' width=',width,'></iframe>')) %>%
  dplyr::select(latitude, longitude, popup, source) %>% # ,bruv.video,auv.video,source
  dplyr::mutate(marine.park = "South-west Corner")

# Fish and AUV video links ----
fish.and.models <- read.csv("data/zone-midpoints.csv", na.strings=c("NA","NaN", " ",""))

fish <- fish.and.models %>% 
  dplyr::filter(!is.na(fish)) %>% 
  dplyr::mutate(source = "fish.video")%>%
  dplyr::mutate(popup = paste("<center><h4>Fish observed in the ",
                             marine.park,
                             " Marine Park, in the ",
                             zone,
                             " Zone.</h4></center>","<br/>",
                             fish, sep = ""))

models <- fish.and.models %>% 
  dplyr::filter(!is.na(auv)) %>% 
  dplyr::mutate(source = "3d.model") %>%
  dplyr::mutate(popup = paste(auv, sep = ""))

# Merge data together for leaflet map ----
dat <- bind_rows(gb.bruv.image, ning.bruv.image, sw.bruv.image, fish, models)

# Make icon for images and videos----
icon.image <- makeAwesomeIcon(icon = "image", library = "fa")
icon.video <- makeAwesomeIcon(icon = "video-camera", library = "fa", markerColor = "lightred", iconColor = "black")
icon.laptop <- makeAwesomeIcon(icon = "laptop", library = "fa", markerColor = "orange", iconColor = "black")

IconSet <- awesomeIconList(
  "stereo-BRUV video"   = makeAwesomeIcon(icon = "video-camera", library = "fa", markerColor = "lightred"),
  "stereo-BRUV image" = makeAwesomeIcon(icon = "image", library = "fa"),
  "3D models" = makeAwesomeIcon(icon = "laptop", library = "fa", markerColor = "orange")
)

# Use this to only have one plot function for markers - can't use a filter to hide them individually :(
# get.color <- function(dat) {
#   sapply(dat$source, function(source) {
#     if(source %in% "3d.model") {
#       "orange"
#     } else if(source %in% "fish.video") {
#       "lightred"
#     } else { # stereo-bruv.image
#       "green"
#     } })
# }

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


# HOW TO ADD ICONS BASED ON COLUMN ------
# Make a list of icons. We'll index into it based on name.
# oceanIcons <- iconList(
#   ship = makeIcon("ferry-18.png", "ferry-18@2x.png", 18, 18),
#   pirate = makeIcon("danger-24.png", "danger-24@2x.png", 24, 24)
# )
# 
# # Some fake data
# df <- sp::SpatialPointsDataFrame(
#   cbind(
#     (runif(20) - .5) * 10 - 90.620130,  # lng
#     (runif(20) - .5) * 3.8 + 25.638077  # lat
#   ),
#   data.frame(type = factor(
#     ifelse(runif(20) > 0.75, "pirate", "ship"),
#     c("ship", "pirate")
#   ))
# )
# 
# leaflet(df) %>% addTiles() %>%
#   # Select from oceanIcons based on df$type
#   addMarkers(icon = ~oceanIcons[type])

# first 20 quakes
# df.20 <- quakes[1:20,]
# 
