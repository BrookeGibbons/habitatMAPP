library(dplyr)
library(readr)
library(GlobalArchive)
install.packages("life.cycle")
install.packages("installr")
library(installr)
updateR()

setwd("C:/GitHub/habitatMAPP/data/dongara")
dir()

metadata<-read_csv("Image_Location_Data.csv")%>%
  ga.clean.names()

urchin<-read_csv("Urchin_Density_Data.csv")%>%
  dplyr::select(image_name,Urchins_per_m2)%>%
  ga.clean.names()%>%
  left_join(metadata,.)%>%
  mutate(urchin.density.m2=as.numeric(urchins_per_m2))%>%
  select(x,y,urchin.density.m2)%>%
  replace_na(list(urchin.density.m2=0))

write.csv(urchin, "urchin.denisty.dongara.csv",row.names = FALSE)

names(urchin)


library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(GlobalArchive)

# Study name----
rm(list=ls()) #clear memory

study <- "towed"
# Set sub directories----
habitat <- read.csv("BenthoBoxPointTags.csv") %>%
  ga.clean.names() %>% # Function Brooke & Tim wrote to tidy column names
  dplyr::select(image.name,image.source.dataset,point.x.from.top.left.corner.,point.y.from.top.left.corner.,display.name) %>% # Select columns to keep
  dplyr::mutate(image.name=str_replace_all(image.name,c("https://uwa-auv.s3-ap-southeast-2.amazonaws.com/"="",".jpg"="","/Images"="","2014_TowedVideo/"="",".jpeg"=""))) %>% # Remove url from sample names
  dplyr::rename(habitat = display.name, point.x = point.x.from.top.left.corner., point.y = point.y.from.top.left.corner.)%>%
  #tidyr::separate(image.name, into = c("campaignid","sample"),sep="/")%>%
  dplyr::filter(!habitat%in%c(NA, "")) %>%
  rename(sample=image.name)%>%
  #mutate(id = paste(campaignid, sample, sep = ".")) %>%
  mutate(habitat=paste("hab:",habitat))%>%
  dplyr::glimpse()

names(habitat)
unique(habitat$sample)
unique(habitat$campaignid)



# Metadata ----
setwd(metadata.dir)
dir()

metadata<-read.csv("2014_GB_Towed_Video_Metadata_dd.csv")%>%
  dplyr::filter(!is.na(latitude))%>%
  dplyr::mutate(sample=str_replace_all(.$image_name,c(".jpeg"="")))%>%
  dplyr::select(sample,latitude,longitude)%>%
  glimpse()

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
  dplyr::summarise_all(funs(sum)) %>%
  ungroup()

percent.cover <- point.score%>%
  dplyr::mutate(total.sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample) %>%
  mutate_at(vars(starts_with("hab: ")),funs(./total.sum*100))%>%
  mutate_at(vars(starts_with("hab: ")),funs(round(.,digits=2)))%>%
  dplyr::select(-total.sum) %>%
  left_join(metadata) %>%
  glimpse()

names(percent.cover)<-str_replace_all(names(percent.cover),c("hab: "=""))

# Write final habitat data----
# join starting with relief - as this is most likely to have the most samples with habitat data
setwd(raw.dir)
dir()

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


broad.hab <- percent.cover%>%
  # Macroalgae
  mutate(Macroalgae=
           biota.macroalgae.encrusting.brown+
           biota.macroalgae.encrusting.red.calcareous+
           biota.macroalgae.erect.course.branching.brown+
           biota.macroalgae.erect.course.branching.brown.other.sp.+
           biota.macroalgae.erect.course.branching.red+
           biota.macroalgae.erect.fine.branching.red+
           biota.macroalgae.globose.saccate.brown)%>%
  # Turfing algae
  mutate(Turf.algae=
           biota.macroalgae.filamentous.and.filiform.turfing.algae)%>%
  # Sand
  mutate(Unconsolidated=
           substrate.unconsolidated.sand.mud.coarse.sand.)%>%
  # Seagrasses
  mutate(Seagrasses= 
           biota.seagrasses.strap.like.leaves+
           biota.seagrasses.strap.like.leaves.amphibolis.sp.+
           biota.seagrasses.strap.like.leaves.posidonia.sp.+
           biota.seagrasses.strap.like.leaves.thalassodendrum.sp.+
           
           biota.unknown.sp1+
           biota.unknown.sp2+
           biota.unknown.sp3+
           biota.unknown.sp4+
           
           biota.unknown.sp5+
           biota.unknown.sp6+
           biota.unknown.sp7+
           biota.unknown.sp8+
           
           biota.invertebrate.complex.complex.1+
           biota.invertebrate.complex.complex.2+
           biota.invertebrate.complex.complex.3+
           biota.invertebrate.complex.complex.4)%>%
  # Rock
  mutate(Consolidated=
           substrate.consolidated+
           substrate.consolidated.rock.turf.mat)%>%
  # Sponges
  rename(Sponges=biota.sponges,Other=biota.unknown.sp10)%>%
  
  dplyr::select(c(sample,latitude,longitude,Macroalgae,Turf.algae,Unconsolidated,Seagrasses,Sponges,Consolidated,Other))

# Save broad habitat types ----
setwd(raw.dir)
dir()

write.csv(broad.hab,paste(study,"broad.percent.cover.csv",sep="_"),row.names = FALSE)

# Detailed
dat.detailed<-percent.cover%>%
  # Rename seagrass categories
  # total seagrass
  dplyr::mutate(Strap.like.leaves=
                  biota.seagrasses.strap.like.leaves+
                  biota.seagrasses.strap.like.leaves.amphibolis.sp.+
                  biota.seagrasses.strap.like.leaves.posidonia.sp.+
                  biota.seagrasses.strap.like.leaves.thalassodendrum.sp.+
                  
                  biota.unknown.sp1+
                  biota.unknown.sp2+
                  biota.unknown.sp3+
                  biota.unknown.sp4+
                  
                  biota.unknown.sp5+
                  biota.unknown.sp6+
                  biota.unknown.sp7+
                  biota.unknown.sp8+
                  
                  biota.invertebrate.complex.complex.1+
                  biota.invertebrate.complex.complex.2+
                  biota.invertebrate.complex.complex.3+
                  biota.invertebrate.complex.complex.4)%>%
  
  # total seagrass with epiphytes
  dplyr::mutate(Strap.like.leaves.with.epiphytes=
                  biota.unknown.sp1+
                  biota.unknown.sp2+
                  biota.unknown.sp3+
                  biota.unknown.sp4+
                  
                  biota.unknown.sp5+
                  biota.unknown.sp6+
                  biota.unknown.sp7+
                  biota.unknown.sp8+
                  
                  biota.invertebrate.complex.complex.1+
                  biota.invertebrate.complex.complex.2+
                  biota.invertebrate.complex.complex.3+
                  biota.invertebrate.complex.complex.4)%>%
  
  # Posidonia
  dplyr::mutate(Posidonia=
                  biota.seagrasses.strap.like.leaves.posidonia.sp.+
                  biota.unknown.sp1+
                  biota.unknown.sp2+
                  biota.unknown.sp3+
                  biota.unknown.sp4)%>%
  dplyr::mutate(Posidonia.with.epiphytes=
                  biota.unknown.sp1+
                  biota.unknown.sp2+
                  biota.unknown.sp3+
                  biota.unknown.sp4)%>%
  
  # Amphibolis
  dplyr::mutate(Amphibolis=
                  biota.seagrasses.strap.like.leaves.amphibolis.sp.+
                  biota.invertebrate.complex.complex.1+
                  biota.invertebrate.complex.complex.2+
                  biota.invertebrate.complex.complex.3+
                  biota.invertebrate.complex.complex.4)%>%
  dplyr::mutate(Amphibolis.with.epiphytes=
                  biota.invertebrate.complex.complex.1+
                  biota.invertebrate.complex.complex.2+
                  biota.invertebrate.complex.complex.3+
                  biota.invertebrate.complex.complex.4)%>%
  # Zostera
  dplyr::mutate(Thalassodendrum=
                  biota.seagrasses.strap.like.leaves.thalassodendrum.sp.)%>%
  
  # total Macroalgae
  dplyr::mutate(Macroalgae=
                  biota.macroalgae.encrusting.brown+
                  biota.macroalgae.encrusting.red.calcareous+
                  biota.macroalgae.erect.course.branching.brown+
                  biota.macroalgae.erect.course.branching.brown.other.sp.+
                  biota.macroalgae.erect.course.branching.red+
                  biota.macroalgae.erect.fine.branching.red+
                  biota.macroalgae.globose.saccate.brown)%>%
  #total turf macroalgae
  dplyr::mutate(Turf.algae=
                  biota.macroalgae.filamentous.and.filiform.turfing.algae)%>%
  
  # total erect course
  dplyr::mutate(Erect.coarse.branching=
                  biota.macroalgae.erect.course.branching.brown+
                  biota.macroalgae.erect.course.branching.brown.other.sp.+
                  biota.macroalgae.erect.course.branching.red)%>%
  # total fine course
  dplyr::mutate(Erect.fine.branching=
                  biota.macroalgae.erect.fine.branching.red)%>%
  
  dplyr::select(sample,latitude,longitude,
                Strap.like.leaves,Strap.like.leaves.with.epiphytes,
                Amphibolis,Amphibolis.with.epiphytes,
                Posidonia,Posidonia.with.epiphytes,
                Thalassodendrum,
                Macroalgae,
                Turf.algae,
                Erect.coarse.branching,
                Erect.fine.branching)

# Save detailed habitat types ----
setwd(raw.dir)
dir()

write.csv(dat.detailed,paste(study,"detailed.percent.cover.csv",sep="_"),row.names = FALSE)


