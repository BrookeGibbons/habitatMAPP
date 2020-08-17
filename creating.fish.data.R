library(dplyr)
library(readr)

maxn <- read_csv("data/fish/2014-12_Geographe.Bay_stereoBRUVs.complete.maxn.csv",col_types = cols(.default = "c"))%>%
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  glimpse()

length<-read_csv("data/fish/2014-12_Geographe.Bay_stereoBRUVs.complete.length.csv",col_types = cols(.default = "c"))%>%
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::mutate(length=as.numeric(length))%>%
  glimpse()

metadata.raw<-read_csv("data/fish/2014-12_Geographe.Bay_stereoBRUVs.checked.metadata.csv",col_types = cols(.default = "c"))%>%
  mutate(latitude=as.numeric(latitude), longitude=as.numeric(longitude))%>%
  glimpse()

metadata<-read_csv("data/fish/2014-12_Geographe.Bay_stereoBRUVs.checked.metadata.csv",col_types = cols(.default = "c"))%>%
  mutate(latitude=as.numeric(latitude), longitude=as.numeric(longitude))%>%
  glimpse()

wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
coordinates(metadata) <- c('longitude', 'latitude')
proj4string(metadata)<-CRS(wgs.84)
  
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
# proj4string(commonwealth.mp)<-CRS(wgs.84)
commonwealth.mp <- spTransform(commonwealth.mp, CRS(wgs.84))

# add in commonwealth reserves
metadata.commonwealth.marineparks <- over(metadata, commonwealth.mp)
  
metadata.2<-metadata.raw%>%
    bind_cols(metadata.commonwealth.marineparks)
  
  # add in state reserves


ngari.mp <- readOGR("data/spatial/test1.shp")
proj4string(ngari.mp) <- CRS(wgs.84)

metadata.wa.marineparks <- over(metadata, ngari.mp)%>%
  # dplyr::rename(zone=Name)%>%
  mutate(state.zone=ifelse(!is.na(Name), "Sanctuary",NA))%>%
  bind_cols(metadata.raw,.)
  
  metadata.regions<-metadata.2%>%
    left_join(metadata.wa.marineparks)%>%
    mutate(zone=ifelse(is.na(zone),state.zone,zone))%>%
    dplyr::mutate(status=str_replace_all(.$zone,c("[^[:alnum:]]"=".",
                                                  "National.Park..no.take."="No-take",
                                                  "Sanctuary"="No-take",
                                                  "Special.Purpose"="Fished",
                                                  "Multiple.Use"="Fished",
                                                  "Habitat.Protection"="Fished")))%>%
    replace_na(list(zone="Fished",status="Fished"))%>%
    dplyr::select(sample,latitude,longitude,status,zone)%>%
    glimpse()

  
unique(metadata.regions$zone)
unique(metadata.regions$status)

write.csv(metadata.regions,"metadata.regions.csv",row.names = FALSE)
