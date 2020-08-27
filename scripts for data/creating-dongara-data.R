library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(GlobalArchive)
library(raster)
library(leaflet)

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

trapping <- read.csv("data/dongara/trap.locations.csv")%>%
  dplyr::select(X,Y)%>%
  dplyr::rename(longitude=X,latitude=Y)

monitoring <- read.csv("data/dongara/all.monitoring.sites.csv")%>%
  dplyr::select(X,Y)%>%
  dplyr::rename(longitude=X,latitude=Y)

leaflet() %>% 
  addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
  addRasterImage(raster_classified, colors = cb,maxBytes = 6 * 1024 * 1024) %>%
  addLegendRaster(colors = c("#499C32", # 1, stable seagrass, DARK GREEN
                             "#e7ed6f", # 2, stable sand, PALE YELLOW
                             "#5CC140", # 3, stable partial seagrass, LIGHT GREEN
                             "#C1405C", # 4, Seagrass lost, PALE RED
                             "#6eed5a", # 5, Seagrass gained, NEON GREEN
                             "#ED905A", # 6, Seagrass Degraded, PALE ORANGE
                             "#5AB7ED"), # noise 
                  labels = c("Stable Seagrass",
                             "Stable Sand",
                             "Stable Partial Seagrass",
                             'Seagrass Lost',
                             "Seagrass Gained",
                             "Seagrass Degraded",
                             "Noise"), sizes = c(20, 20,20, 20,20, 20,20))%>%
  addCircleMarkers(
    data = trapping, lat = ~ latitude, lng = ~ longitude,
    stroke = FALSE, group = "Trap locations",fillOpacity = 1,
    radius = 4, color = "#67E01F")%>%
  addCircleMarkers(
    data = monitoring, lat = ~ latitude, lng = ~ longitude,
    stroke = FALSE, group = "Monitoring",fillOpacity = 1,
    radius = 4, color = "#E01F67")%>%
  addLegendCustom(colors = c("#67E01F", "#E01F67"), labels = c("Trapping", "Monitoring"), sizes = c(20, 20))

# setwd("C:/GitHub/habitatMAPP/data/dongara")
setwd("/srv/shiny-server/marinemapper/data/dongara")
dir()

metadata<-read_csv("Image_Location_Data.csv")%>%
  ga.clean.names()%>%
  dplyr::rename(sample=image_name)

urchin<-read_csv("Urchin_Density_Data.csv")%>%
  dplyr::select(image_name,Urchins_per_m2)%>%
  ga.clean.names()%>%
  dplyr::rename(sample=image_name)%>%
  left_join(metadata,.)%>%
  mutate(Urchin.density=as.numeric(urchins_per_m2))%>%
  dplyr::select(sample,Urchin.density)%>%
  tidyr::replace_na(list(Urchin.density=0))

write.csv(urchin, "urchin.denisty.dongara.csv",row.names = FALSE)

names(urchin)




hab <- read.csv("BenthoBoxPointTags.csv") %>%
  ga.clean.names() %>% # Function Brooke & Tim wrote to tidy column names
  dplyr::select(image.name,image.source.dataset,point.x.from.top.left.corner.,point.y.from.top.left.corner.,display.name) %>% 
  dplyr::rename(habitat = display.name, point.x = point.x.from.top.left.corner., point.y = point.y.from.top.left.corner.)%>%
  dplyr::filter(!habitat%in%c(NA, "")) %>%
  mutate(habitat=paste("hab.",habitat))%>%
  rename(sample=image.name)%>%
  distinct()%>%
  dplyr::glimpse()


habitat<-hab%>%
  separate(sample, c("before", "sample"), "images&files=")%>%
  dplyr::select(-c(before))

names(habitat)
unique(habitat$sample)
unique(habitat$campaignid)

# CREATE catami point score------
unique(habitat$habitat)%>%sort()

point.score <- habitat %>%
  distinct()%>%
  dplyr::filter(!habitat%in%c("", NA, "hab: Substrate: Open Water", "hab: Substrate: Unknown")) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(sample) %>%
  spread(key = habitat, value = count, fill=0) %>%
  dplyr::select(-c(point.x, point.y, image.source.dataset)) %>%
  ungroup()%>%
  dplyr::group_by(sample) %>%
  dplyr::summarise_all((sum)) %>%
  ungroup()

percent.cover <- point.score%>%
  dplyr::mutate(total.sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample) %>%
  mutate_at(vars(starts_with("hab.")),funs(./total.sum*100))%>%
  mutate_at(vars(starts_with("hab.")),funs(round(.,digits=2)))%>%
  dplyr::select(-total.sum) %>%
  left_join(metadata) %>%
  ga.clean.names()%>%
  glimpse()

names(percent.cover)
names(percent.cover)<-str_replace_all(names(percent.cover),c("hab."=""))

# Write final habitat data----
write.csv(percent.cover,paste(study,"raw.percent.cover.csv",sep="_"),row.names = FALSE)

# Make broad categories -----
# Minimize the number of habitat categories for plotting ----
names(percent.cover)

percent.cover<-ga.clean.names(percent.cover)

names(percent.cover)

# Posodonia
# 0 = Posodonia sp.
# 1 - 25 = Unknown 1
# 26 - 50 = Unknown 2
# 51 - 75 = Unknown 3
# 76 - 100 = Unknown 4
# 
# Amphibolis
# 0 = Amphibolis sp.
# 1 - 25 = Complex 1
# 26 - 50 = Complex 2
# 51 - 75 = Complex 3
# 76 - 100 = Complex 4
# 
# Strap like leaves
# 0 = Strap like leaves
# 1 - 25 = Unknown 5
# 26 - 50 = Unknown 6
# 51 - 75 = Unknown 7
# 76 - 100 = Unknown 8

test<-read.csv("towed_raw.percent.cover.csv")
names(test)

# test<-test%>%glimpse()%>%
  # mutate(macroalgae=macroalgae.articulated.calcareous+macroalgae.articulated.calcareous.green)

broad.hab <- test%>%ungroup()%>%
  # Macroalgae
  dplyr::mutate(Macroalgae=(macroalgae.articulated.calcareous+
           macroalgae.articulated.calcareous.green+
           macroalgae.articulated.calcareous.green.articulated.calcareous.green+
           macroalgae.articulated.calcareous.red+
           macroalgae.articulated.calcareous.red.articulated.calcareous.red+
           macroalgae.drift.algae+
           macroalgae.encrusting.brown+
           macroalgae.encrusting.green+
           macroalgae.encrusting.red+
           macroalgae.encrusting.red.calcareous+
           macroalgae.erect.coarse.branching+
           macroalgae.erect.coarse.branching.brown+
           macroalgae.erect.coarse.branching.brown.drift+
           macroalgae.erect.coarse.branching.brown.sargassum.spp+
           macroalgae.erect.coarse.branching.green+
           macroalgae.erect.coarse.branching.green.caulerpa.spp+
           macroalgae.erect.coarse.branching.red+
           macroalgae.erect.fine.branching+
           macroalgae.erect.fine.branching.brown+
           macroalgae.erect.fine.branching.brown.brown.understory.algae+
           macroalgae.erect.fine.branching.green+
           macroalgae.erect.fine.branching.green.caulerpa.spp+
           macroalgae.erect.fine.branching.red+
           macroalgae.erect.fine.branching.red.foliose+
           macroalgae.filamentous.filiform.brown+
           macroalgae.filamentous.filiform.green+
           macroalgae.filamentous.filiform.red+
           macroalgae.filamentous.filiform.turfing.algae+
           macroalgae.globose.saccate+
           macroalgae.globose.saccate.brown+
           macroalgae.globose.saccate.green+
           macroalgae.laminate.brown+
           macroalgae.laminate.green+
           macroalgae.large.canopy.forming+
           macroalgae.large.canopy.forming.brown+
           macroalgae.large.canopy.forming.brown.ecklonia.radiata))%>%
  # Turfing algae
  # mutate(Turf.algae=
  #          biota.macroalgae.filamentous.and.filiform.turfing.algae)%>%
  # Sand
  mutate(Unconsolidated=substrate.unconsolidated.soft.pebble.gravel
         +substrate.unconsolidated.soft.pebble.gravel.biologenic
         +substrate.unconsolidated.soft.pebble.gravel.biologenic.rhodoliths
         +substrate.unconsolidated.soft.pebble.gravel.gravel.2.10mm.
         +substrate.unconsolidated.soft.pebble.gravel.pebble.10.64mm.
         +substrate.unconsolidated.soft.sand.mud.2mm.coarse.sand.with.shell.fragments.
         +substrate.unconsolidated.soft.sand.mud.2mm.fine.sand.no.shell.fragments.
         +substrate.unconsolidated.soft.sand.mud.2mm.fine.sand.no.shell.fragments.)%>%
  # Seagrasses
  mutate(Seagrasses= 
           seagrasses+seagrasses.elliptical.leaves+
           seagrasses.elliptical.leaves.halophila.sp.caab.63600902.+
           seagrasses.elliptical.leaves.halophila.sp.caab.63600902.epiphytes.algae+
           seagrasses.elliptical.leaves.halophila.sp.caab.63600902.epiphytes.other+
           seagrasses.strap.like.leaves+seagrasses.strap.like.leaves.amphibolis.sp.caab.63600903.+
           seagrasses.strap.like.leaves.amphibolis.sp.caab.63600903.epiphytes.algae+
           seagrasses.strap.like.leaves.amphibolis.sp.caab.63600903.epiphytes.other+
           seagrasses.strap.like.leaves.posidonia.sp.caab.63600903.+
           seagrasses.strap.like.leaves.posidonia.sp.caab.63600903.epiphytes.algae+
           seagrasses.strap.like.leaves.rupia.sp.caab.63600903.+
           seagrasses.strap.like.leaves.zostera.sp.caab.63600903.+
           sea.spiders+fishes.eels
           )%>%
  # Rock
  mutate(Consolidated=
           substrate.consolidated.hard.
           +substrate.consolidated.hard.boulders
           +substrate.consolidated.hard.cobbles
           +substrate.consolidated.hard.rock)%>%
  
  # Sponges
  mutate(Sponges=sponges
  +sponges.crusts+sponges.crusts.encrusting+sponges.crusts.encrusting.bryozoa.sponge.matrix+sponges.crusts.encrusting.encrusting.yellow.2+sponges.massive.forms+sponges.massive.forms.simple.massive.black.oscula.papillate)%>%
  
  # stony corals
  mutate(Stony.corals=cnidaria.corals+cnidaria.corals.stony.corals.encrusting)%>%
  
  # Macrophytes
  
  mutate(Macrophytes=Seagrasses+Macroalgae)%>%
  
  # dplyr::rename(Other=biota.unknown.sp10)%>%
  mutate(Other=unscorable+
           molluscs.gastropods+
           fishes.bony.fishes+
           bioturbation.unknown.origin.pogostick+
           bryozoa+
           bryozoa.bryozoa.sponge.matrix+
           echinoderms.sea.stars+
           echinoderms.sea.urchins+
           echinoderms.sea.urchins.regular.urchins)%>%
  
  dplyr::select(c(sample,y,x,Macroalgae,Unconsolidated,Seagrasses,Sponges,Consolidated,Other,Macrophytes,Stony.corals))%>%
  
  dplyr::rename(latitude=y,longitude=x)%>%
  mutate(method="Towed",marine.park="Dongara")%>%
  filter(!is.na(latitude))%>%
  left_join(.,urchin)

plot(metadata$x, metadata$y)

# Save broad habitat types ----
write.csv(broad.hab,paste(study,"broad.percent.cover.csv",sep="_"),row.names = FALSE)
