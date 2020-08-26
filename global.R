# Load libraries ----
# install.packages("shinybusy")
# profvis(runApp())
library(dplyr)
library(DT)
library(forcats)
library(fst)
library(ggplot2)
library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
library(grid)
library(htmlwidgets)
library(leaflet)
library(leaflet.minicharts)
library(leafpop)
library(mapview)
library(profvis)
library(raster)
library(readr)
library(rgdal)
library(rgeos)
library(rmapshaper)
library(shiny)
library(shinyBS)
library(shinybusy)
library(shinydashboard)
# library(shinydashboardPlus)
library(shinythemes)
library(shinyjs)
library(sf)
library(stringr)
library(tidyr)

# Bring in data ----
# Load 2019 ningaloo metadata ----
ning.bruv.metadata <- read.csv("data/2019-08_Ningaloo_metadata.csv") %>%
  mutate(sample = as.character(sample))

# Load 2014 geographe bay metadata ----
gb.bruv.metadata <- read.csv("data/2014-12_Geographe.Bay_stereoBRUVs_Metadata.csv")

# Load 2020 south west metadata ----
sw.bruv.metadata <- read.csv("data/2020-06_south-west_stereoBRUVs_metadata.csv")

# Create dataframe for 2014 Geographe Bay BRUV images for plotting ----
# gb.bruv.image <- gb.bruv.metadata %>%
#   dplyr::mutate(image=paste0("https://marineecology.io/images/2014-12_BRUVs_Forward/",Sample,".jpg",sep="")) %>%
#   ga.clean.names() %>%
#   dplyr::mutate(source = "image") %>%
#   mutate(height='"365"') %>% mutate(width='"645"') %>%
#   mutate(popup=paste0('<iframe src=',image,' height=',height,' width=',width,'></iframe>')) %>%
#   dplyr::select(latitude, longitude, popup, source) %>%
#   dplyr::mutate(marine.park = "Geographe Bay")

gb.bruv.video <- gb.bruv.metadata %>%
  ga.clean.names() %>%
  # dplyr::mutate(sample=as.numeric(sample))%>% # for testing only
  # dplyr::filter(sample<61)%>% # for testing only
  dplyr::mutate(sample=as.character(sample))%>% 
  dplyr::mutate(source = "habitat.highlights") %>%
  dplyr::mutate(popup=paste0('<video width="645" autoplay controls>
  <source src="https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/videos/',sample,'.mp4?raw=true" type="video/mp4">
</video>')) %>%
  dplyr::select(latitude, longitude, popup, source) %>%
  dplyr::mutate(marine.park = "Geographe Bay")

# https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/videos/Compilations/test-video-2.mp4?raw=true # this link works

# Create dataframe for 2019 Ningaloo BRUV images for plotting ----
ning.bruv.image <- ning.bruv.metadata %>%
  dplyr::mutate(image=paste0("https://marineecology.io/images/habitatmapp/ningaloo/",sample,".jpg",sep="")) %>%
  ga.clean.names() %>%
  dplyr::mutate(source = "image") %>%
  mutate(height='"365"')%>%mutate(width='"645"')%>%
  mutate(popup=paste0('<iframe src=',image,' height=',height,' width=',width,'></iframe>')) %>%
  dplyr::select(latitude, longitude, popup, source) %>% # ,bruv.video,auv.video,source
  dplyr::mutate(marine.park = "Ningaloo")

ning.bruv.video <- ning.bruv.metadata %>%
  ga.clean.names() %>%
  dplyr::mutate(source = "habitat.highlights") %>%
  dplyr::mutate(popup=paste0('<video width="645" autoplay controls>
  <source src="https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/videos/ningaloo/',sample,'.mp4?raw=true" type="video/mp4">
</video>')) %>%
  dplyr::select(latitude, longitude, popup, source) %>% # ,bruv.video,auv.video,source
  dplyr::mutate(marine.park = "Ningaloo")
# 
# # Create dataframe for 2019 Ningaloo BRUV images for plotting ----
sw.bruv.image <- sw.bruv.metadata %>%
  ga.clean.names() %>%
  dplyr::mutate(image=paste0("01",sample,"",sep="")) %>% # NEED TO UPDATE THIS
  dplyr::mutate(source = "image") %>%
  dplyr::mutate(popup=paste0('<video width="645" autoplay controls>
  <source src="https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/videos/south-west/',sample,'.mp4?raw=true" type="video/mp4">
</video>')) %>%
  dplyr::select(latitude, longitude, popup, source) %>% # ,bruv.video,auv.video,source
  dplyr::mutate(marine.park = "South-west Corner")

# Fish hihglights and 3D model links ----
fish <- read.csv("data/zone-midpoints.csv", na.strings=c("NA","NaN", " ","")) %>%
  dplyr::filter(!is.na(fish)) %>%
  dplyr::mutate(source = "fish.highlights")%>%
  dplyr::mutate(popup = paste("<center><h4>Fish observed in the ",
                              marine.park,
                              " Marine Park, in the ",
                              zone,
                              " Zone.</h4></center>","<br/>",
                              fish, sep = ""))

models <- read.csv("data/3Dmodels.csv", na.strings=c("NA","NaN", " ","")) %>% 
  dplyr::mutate(source = "3d.model")

# Merge data together for leaflet map ----
dat <- bind_rows(models, gb.bruv.video, sw.bruv.image, fish, ning.bruv.video) # fish, gb.bruv.image, ning.bruv.image, sw.bruv.image, 

# Lobster trapping sites -----
trapping <- read.csv("data/dongara/trap.locations.csv")%>%
  dplyr::select(X,Y)%>%
  dplyr::rename(longitude=X,latitude=Y)

monitoring <- read.csv("data/dongara/all.monitoring.sites.csv")%>%
  dplyr::select(X,Y)%>%
  dplyr::rename(longitude=X,latitude=Y)

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
dongara.shp <-  readOGR("data/spatial/Raster_to_Poly.shp") 

unique(dongara.shp$Desc_)

dongara.shp$seagrass.state<-fct_relevel(dongara.shp$Desc_, "Conservation (no-take)", "Sanctuary (no-take)", "Recreation", "General Use", "Special Purpose")

dongara.pal <- colorFactor(c("#f6c1d9", # Seagrass Gained
                                  "#7bbc63", # Seagrass Degraded
                                  "#fdb930", # Seagrass Lost
                                  "#fff7a3", # Stable Seagrass
                                  '#b9e6fb', # Stable Partial Seagrass
                                  '#ccc1d6',# Stable Sand
                                  'black' # Noise
), dongara.shp$seagrass.state)


ngari.mp <- readOGR("data/spatial/test1.shp") 
state.mp <- readOGR("data/spatial/WA_MPA_2018.shp")

# filter out unassigned and unclassified
state.mp <- state.mp[!state.mp$ZONE_TYPE %in% c("Unassigned (IUCN IA)","Unassigned (IUCN II)","Unassigned (IUCN III)","Unassigned (IUCN IV)","Unassigned (IUCN VI)","MMA (Unclassified) (IUCN VI)","MP (Unclassified) (IUCN VI)"), ]

# remove all alphanumeric to rename zone type
state.mp$zone<-str_replace_all(state.mp$ZONE_TYPE, c("[^[:alnum:]]"=" "))
state.mp$zone<-str_replace_all(state.mp$zone, c("Conservation Area  IUCN IA "="Conservation (no-take)",
                                                "General Use  IUCN II "="General Use",
                                                "General Use Area  IUCN VI "="General Use",
                                                "General Use Zone  IUCN II "="General Use",
                                                "Recreation Area  IUCN II "="Recreation",
                                                "Recreation Zone  IUCN II "="Recreation",
                                                "Sanctuary Area  IUCN VI "="Sanctuary (no-take)",
                                                "Sanctuary Zone  IUCN IA "="Sanctuary (no-take)",
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

# unique(state.mp$zone)


# Commonwealth marine parks ----
commonwealth.mp <- readOGR("data/spatial/AustraliaNetworkMarineParks.shp")
commonwealth.mp$zone<-str_replace_all(commonwealth.mp$ZoneName, c("[^[:alnum:]]"=" "))
commonwealth.mp$zone<-str_replace_all(commonwealth.mp$zone, c(" Zone"="",
                                                              "Habitat Protection  Lord Howe " = "Habitat Protection",
                                                              "Habitat Protection  Reefs " = "Habitat Protection",
                                                              "Marine National Park" = "National Park",
                                                              "National Park" = "National Park (no-take)",
                                                              "Special Purpose  Mining Exclusion " = "Special Purpose",
                                                              "Special Purpose  Norfolk " = "Special Purpose",
                                                              "Special Purpose  Trawl " = "Special Purpose",
                                                              "Sanctuary" = "Sanctuary (no-take)"))
unique(commonwealth.mp$zone)


# Create factors for legends and plotting ----
# State marine parks ----
state.mp$zone <- as.factor(state.mp$zone)
state.mp$zone<-fct_relevel(state.mp$zone, "Conservation (no-take)", "Sanctuary (no-take)", "Recreation", "General Use", "Special Purpose")

state.pal <- colorFactor(c("#bfaf02", # conservation
                           "#7bbc63", # sanctuary = National Park
                           "#fdb930", # recreation
                           "#b9e6fb", # general use
                           '#ccc1d6' # special purpose
), state.mp$zone)

# Commonwealth marine parks ----
commonwealth.mp$zone <- as.factor(commonwealth.mp$zone)
commonwealth.mp$zone<-fct_relevel(commonwealth.mp$zone, "Sanctuary (no-take)", "National Park (no-take)", "Recreational Use", "Habitat Protection", "Multiple Use", "Special Purpose")

commonwealth.pal <- colorFactor(c("#f6c1d9", # Sanctuary
                                  "#7bbc63", # National Park
                                  "#fdb930", # Recreational Use
                                  "#fff7a3", # Habitat Protection
                                  '#b9e6fb', # Multiple Use
                                  '#ccc1d6'# Special Purpose
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

# Fish data for plots ----
master<-read_csv("data/fish/australia.life.history_200824.csv")%>%
  ga.clean.names()%>%
  dplyr::select(family,genus,species,australian.common.name)

family.common.names<-read_csv("data/fish/family.common.names.csv")%>%
  ga.clean.names()%>%
  distinct()%>%
  dplyr::mutate(family.common.name=paste("An unknown",australian.family.common.name,sep=" "))%>%
  dplyr::select(family,family.common.name)

lumped.common.names<-read_csv("data/fish/lumped.common.names.csv")%>%
  ga.clean.names()%>%
  distinct()

maxn <- read_csv("data/fish/2014-12_Geographe.Bay_stereoBRUVs.complete.maxn.csv",col_types = cols(.default = "c"))%>%
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  dplyr::select(-c(latitude,longitude,status))%>%
  left_join(master)%>%
  left_join(family.common.names)%>%
  left_join(lumped.common.names)%>%
  dplyr::mutate(common.name=if_else(is.na(australian.common.name),family.common.name,australian.common.name))%>%
  dplyr::mutate(common.name=if_else(!is.na(lumped.common.name),lumped.common.name,common.name))%>%
  dplyr::mutate(scientific=paste(genus," ",species," (",common.name,")",sep=""))

metadata.regions<-read_csv("data/fish/metadata.regions.csv",col_types = cols(.default = "c"))%>%
  mutate(zone=str_replace_all(.$zone,c("Sanctuary"="Sanctuary (no-take)","Fished"="Outside Marine Park")))%>%
  mutate(zone=as.factor(zone))%>%
  mutate(zone=fct_relevel(zone, "Sanctuary (no-take)", "National Park (no-take)","Habitat Protection","Multiple Use","Special Purpose"))%>%
  mutate(status=fct_relevel(status, "No-take", "Fished"))

length <- read_csv("data/fish/2014-12_Geographe.Bay_stereoBRUVs.complete.length.csv",col_types = cols(.default = "c"))%>%
  dplyr::mutate(number=as.numeric(number),length=as.numeric(length))%>%
  dplyr::select(-c(latitude,longitude,status))%>%
  left_join(master)%>%
  left_join(family.common.names)%>%
  left_join(lumped.common.names)%>%
  dplyr::mutate(common.name=if_else(is.na(australian.common.name),family.common.name,australian.common.name))%>%
  dplyr::mutate(common.name=if_else(!is.na(lumped.common.name),lumped.common.name,common.name))%>%
  dplyr::mutate(scientific=paste(genus," ",species," (",common.name,")",sep=""))

mass <- read_csv("data/fish/2014-12_Geographe.Bay_stereoBRUVs.complete.mass.csv",col_types = cols(.default = "c"))%>%
  dplyr::mutate(number=as.numeric(number),mass.g=as.numeric(mass.g))%>%
  dplyr::select(-c(latitude,longitude,status))%>%
  left_join(master)%>%
  left_join(family.common.names)%>%
  left_join(lumped.common.names)%>%
  dplyr::mutate(common.name=if_else(is.na(australian.common.name),family.common.name,australian.common.name))%>%
  dplyr::mutate(common.name=if_else(!is.na(lumped.common.name),lumped.common.name,common.name))%>%
  dplyr::mutate(scientific=paste(genus," ",species," (",common.name,")",sep=""))


# Habitat data for plotting ----
geo.hab.data <- fst::read_fst("data/annotations/geographe/southwest.broad.fst") %>%
  as.data.frame() %>%
  dplyr::filter(!campaignid%in%c("2007-03_Capes_MF_stereoBRUVs"))%>%
  dplyr::mutate(method=str_replace_all(.$campaignid, c("2007-03_Capes_MF_stereoBRUVs"="stereo-BRUV",
                                                       "2014-12_Geographe_Bay_stereoBRUVs"="stereo-BRUV")))%>%
  dplyr::mutate(marine.park="Geographe Bay")%>%
  dplyr::mutate(Macrophytes=Seagrasses+Macroalgae)

dongara.hab.data<-read_csv("data/dongara/towed_broad.percent.cover.csv")

hab.data<-bind_rows(geo.hab.data,dongara.hab.data)%>%
  tidyr::replace_na(list(Macroalgae=0,
                         Turf.algae=0,
                         Stony.corals=0,
                         Unconsolidated=0,
                         Seagrasses=0,
                         Sponges=0,
                         Consolidated=0,
                         Macrophytes=0,
                         Urchin.density=0,
                         Other=0))

broad.colors <- c("#8491B4B2","#1B9E77","#66A61E","#E64B35B2","#7570B3","#D95F02","#E6AB02","black")

# Make icon for images and videos----
icon.habitat <- makeAwesomeIcon(icon = "image", library = "fa")
icon.fish <- makeAwesomeIcon(icon = "video", library = "fa", markerColor = "lightred", iconColor = "black")
icon.models <- makeAwesomeIcon(icon = "laptop", library = "fa", markerColor = "orange", iconColor = "black")

IconSet <- awesomeIconList(
  "Fish highlights"   = makeAwesomeIcon(icon = "video", library = "fa", markerColor = "lightred"),
  "Habitat imagery" = makeAwesomeIcon(icon = "image", library = "fa"),
  "3D models" = makeAwesomeIcon(icon = "laptop", library = "fa", markerColor = "orange")
)

testIcons <- iconList(blue = makeIcon("images/marker_red.png", iconWidth = 24, iconHeight =32),
                       green = makeIcon("images/marker_green.png", iconWidth = 24, iconHeight =32),
                       orange = makeIcon("images/marker_blue.png", iconWidth = 24, iconHeight =32))

html_legend <- "<div style='width: auto; height: 45px'> <div style='position: relative; display: inline-block; width: 36px; height: 45px' <img src='images/marker_red.png'> </div> <p style='position: relative; top: 15px; display: inline-block; ' > BRUV </p> </div>
<div style='width: auto; height: 45px'> <div style='position: relative; display: inline-block; width: 36px; height: 45px' <img src='images/marker_red.png'> </div> <p style='position: relative; top: 15px; display: inline-block; ' > BRUV </p> </div>
<div style='width: auto; height: 45px'> <div style='position: relative; display: inline-block; width: 36px; height: 45px' <img src='images/marker_red.png'> </div> <p style='position: relative; top: 15px; display: inline-block; ' > BRUV </p> </div>"

html_legend <- "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:10px; margin: 0;'> Marker Legend </h4><br/>

<img src='https://github.com/BrookeGibbons/habitatMAPP/blob/master/images/marker_red.png?raw=true'
style='width:30px;height:35px;'> Fish highlights <br/> 

<img src='https://github.com/BrookeGibbons/habitatMAPP/blob/master/images/marker_blue.png?raw=true'
style='width:30px;height:35px;'> Habitat imagery<br/> 

<img src='https://github.com/BrookeGibbons/habitatMAPP/blob/master/images/marker_green.png?raw=true'  
style='width:30px;height:35px;'> 3D models"

# Legend function ----
markerLegendHTML <- function(IconSet) {
  # container div:
  legendHtml <- "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:10px; margin: 0;'> Marker Legend </h4>"
  
  n <- 1
  # add each icon for font-awesome icons icons:
  for (Icon in IconSet) {
    # if (Icon[["library"]] == "fa") {
      legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 45px'>",
                          "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                          "<i style='margin-left: 3px; margin-top: 11px; 'class= 'fa fa-",Icon[["icon"]]," fa-inverse'></i>",
                          "</div>",
                          "<p style='position: relative; top: 15px; display: inline-block; ' >", names(IconSet)[n] ,"</p>",
                          "</div>")    
    # }
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



dbHeader <- dashboardHeader()
dbHeader$children[[2]]$children <-  tags$a(href='http://mycompanyishere.com',
                                           tags$img(src='https://www.nespmarine.edu.au/sites/default/themes/nespmarine/logo.png',height='60',width='200'))

dbHeader <- dashboardHeader(title = "marine mapper",
                            tags$li(a(href = 'https://marineecology.io/',
                                      img(src = 'https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/MEG-white.png?raw=true',
                                          title = "Marine Ecology Group", height = "50px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"),
                            tags$li(a(href = 'https://www.nespmarine.edu.au/',
                                      img(src = 'https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/mbh-logo-white-cropped.png?raw=true',
                                          title = "Marine Biodiversity Hub", height = "50px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))


# Theme for plotting ----
Theme1 <-    theme_bw()+
  theme( # use theme_get() to see available options
    panel.grid = element_blank(), 
    panel.border = element_blank(), 
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=12),
    legend.title = element_blank(),
    #legend.position = "top",
    text=element_text(size=12),
    strip.text.y = element_text(size = 12,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=12),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12),
    axis.text.y=element_text(size=12),
    axis.text.x=element_text(size=12),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.title = element_text(color="black", size=12, face="bold.italic"))

theme_collapse<-theme(      ## the commented values are from theme_grey
  panel.grid.major=element_line(colour = "white"), ## element_line(colour = "white")
  panel.grid.minor=element_line(colour = "white", size = 0.25), 
  plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

# functions for summarising data on plots ----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)

addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
}

change.over.time<- raster(x = "data/spatial/Change Over Time.tif")


reclass_df <- c(10100.5,	10101.5,	1,
                10102.5,	10103.5,	1,
                10300.5,	10301.5,	1,
                20201.5,	20202.5,	2,
                30101.5,	30102.5,	3,
                30102.5,	30103.5,	3,
                30200.5,	30201.5,	3,
                30202.5,	30203.5,	3,
                30302.5,	30303.5,	3,
                10201.5,	10202.5,	4,
                10301.5,	10302.5,	4,
                30201.5,	30202.5,	4,
                30301.5,	30302.5,	4,
                20102.5,	20103.5,	5,
                20200.5,	20201.5,	5,
                20202.5,	20203.5,	5,
                20300.5,	20301.5,	5,
                20302.5,	20303.5,	5,
                30100.5,	30101.5,	5,
                30300.5,	30301.5,	5,
                10101.5,	10102.5,	6,
                10202.5,	10203.5,	6,
                10302.5,	10303.5,	6,
                10200.5,	10201.5,	7,
                20100.5,	20101.5,	7,
                20101.5,	20102.5,	7,
                20301.5,	20302.5,	7,
                10103.5,	10104.5,	NA,
                10203.5,	10204.5,	NA,
                10303.5,	10304.5,	NA,
                10400.5,	10401.5,	NA,
                20103.5, 20104.5,	NA,
                20401.5,	20402.5,	NA,
                30203.5,	30204.5,	NA,
                30303.5,	30304.5,	NA,
                30400.5,	30401.5,	NA,
                30401.5,	30402.5,	NA,
                30402.5,	30403.5,	NA,
                30403.5,	30404.5,	NA,
                40100.5,	40101.5,	NA,
                40101.5,	40102.5,	NA,
                40102.5,	40103.5,	NA,
                40103.5,	40104.5,	NA,
                40200.5,	40201.5,	NA,
                40201.5,	40202.5,	NA,
                40202.5,	40203.5,	NA,
                40203.5,	40204.5,	NA,
                40300.5,	40301.5,	NA,
                40301.5,	40302.5,	NA,
                40302.5,	40303.5,	NA,
                40303.5,	40304.5,	NA,
                40400.5,	40401.5,	NA,
                40401.5,	40402.5,	NA,
                40402.5,	40403.5,	NA,
                40403.5,	40404.5,	NA,
                30103.5,30104.5,NA,
                20403.4,20404.5,NA,
                20400.5,20401.5,NA,
                20203.5,20204.5,NA,
                10403.5,10404.5,NA,
                10402.5,10403.5,NA,
                10401.5,10402.5,NA
                
                
)

reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)
reclass_m

raster_classified <- reclassify(change.over.time, reclass_m)
raster_classified[is.na(raster_classified)] <- 0
summary(raster_classified)

raster.colors <- c(#"transparent",
  "#499C32", # 1, stable seagrass, DARK GREEN
  "#e7ed6f", # 2, stable sand, PALE YELLOW
  "#5CC140", # 3, stable partial seagrass, LIGHT GREEN
  "#C1405C", # 4, Seagrass lost, PALE RED
  "#6eed5a", # 5, Seagrass gained, NEON GREEN
  "#ED905A", # 6, Seagrass Degraded, PALE ORANGE
  "#5AB7ED"#,
  #"transparent"
)
at <- seq(0.5, 7.5, 1)
cb <- colorBin(palette = raster.colors, bins = at, domain = at,na.color = "transparent")

addLegendRaster <- function(map, colors, labels, sizes, opacity = 1){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
}
