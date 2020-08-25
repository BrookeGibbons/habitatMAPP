library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(GlobalArchive)

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
