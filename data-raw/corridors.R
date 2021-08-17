# prepare corridors

requireNamespace("readxl", quietly = TRUE)
requireNamespace("fs", quietly = TRUE)
requireNamespace("tigris", quietly = TRUE)
requireNamespace("janitor", quietly = TRUE)

library(dplyr)
library(fs)
library(sf)
library(tigris)
library(tidycensus)
library(janitor)

## corridor shapes ---------
library(rgdal)
corridors <- readOGR('data-raw/Corridorsv1', 'Export_Output',
                   verbose=FALSE
)
corridors<-st_as_sf(corridors)
corridors<-corridors%>%
  select(Name)%>%
  st_transform(4326)

## corridor tract sets ---------------
###grab corridor geometries
corridor_tracts <- tigris::tracts(
  state = "MD",
  county = "Baltimore City",
  class = "sf")%>%
  mutate(GEOID=paste0(STATEFP,COUNTYFP,TRACTCE))%>%
  filter(GEOID=="24510010200"|GEOID=="24510040100"|GEOID=="24510040200"|GEOID=="24510090300"
         |GEOID=="24510090400"|GEOID=="24510090500"|GEOID=="24510090800"|GEOID=="24510120201"
         |GEOID=="24510120300"|GEOID=="24510120400"|GEOID=="24510140200"|GEOID=="24510140300"
         |GEOID=="24510150100"|GEOID=="24510150800"|GEOID=="24510150900"|GEOID=="24510151000"
         |GEOID=="24510151100"|GEOID=="24510160100"|GEOID=="24510160200"|GEOID=="24510170100"
         |GEOID=="24510170200"|GEOID=="24510170300"|GEOID=="24510180100"|GEOID=="24510180200"
         |GEOID=="24510180300"|GEOID=="24510190100"|GEOID=="24510190200"|GEOID=="24510190300"
         |GEOID=="24510200600"|GEOID=="24510200701"|GEOID=="24510200702"|GEOID=="24510210100"
         |GEOID=="24510210200"|GEOID=="24510260501"|GEOID=="24510260604"|GEOID=="24510260700"
         |GEOID=="24510260800"|GEOID=="24510260900"|GEOID=="24510261000"|GEOID=="24510261100")%>%
  select(GEOID)
library(rgdal)
library(readxl)
corridor_geo <- read_xlsx('./data-raw/Baltimore_corridor_crosswalk.xlsx')
corridor_tracts<-merge(corridor_tracts, corridor_geo, by.x='GEOID', by.y='tract_string')
corridor_tracts<-corridor_tracts%>%
  select(Corridor_Name, geometry)%>%
  group_by(Corridor_Name)%>%
  summarise(geometry=st_union(geometry))%>%
  select(Corridor_Name)
corridor_tracts <- corridor_tracts %>% 
  st_transform(4326)




##Uncomment below if you want to get geographies from tigris.
#corridor_tracts <- tigris::tracts(
#  state = "MD",
#  county = "Baltimore City",
#  class = "sf")%>%
#  mutate(GEOID=paste0(STATEFP,COUNTYFP,TRACTCE))%>%
#  filter(GEOID=="24510260501"|GEOID=="24510200701"|GEOID=="24510200600"|
#           GEOID=="24510200702"|GEOID=="24510200800"|GEOID=="24510260700"|
#           GEOID=="24510090800"|GEOID=="24510120400"|GEOID=="24510120300"|GEOID=="24510120500"|
#           GEOID=="24510090400"|GEOID=="24510090500"|GEOID=="24510120201"|GEOID=="24510180200"|
#           GEOID=="24510190100"|GEOID=="24510190200"|GEOID=="24510180300"|GEOID=="24510190300"|
#           GEOID=="24510151000"|GEOID=="24510150800"|GEOID=="24510151100"|GEOID=="24510110200"|
#           GEOID=="24510170100"|GEOID=="24510040100"|GEOID=="24510040200"|GEOID=="24510170200"|
#           GEOID=="24510150100"|GEOID=="24510140200"|GEOID=="24510160100"|GEOID=="24510140300"|
#           GEOID=="24510170300"|GEOID=="24510210100"|GEOID=="24510210200")%>%
#  select(GEOID)

#library(readxl)
#Corridor_Walkshed_Tracts_v2 <- read_excel("data-raw/Corridor_Walkshed_Tracts_v2.xlsx", 
#                                          sheet = "Corridor Tracts")

#corridor_tracts<-merge(corridor_tracts,Corridor_Walkshed_Tracts_v2, by.x='GEOID',by.y='CT2010')
#corridor_tracts<-corridor_tracts%>%select(Commercial_Corridor_Number,geometry)
#corridor_tracts<-corridor_tracts%>%
#  group_by(Commercial_Corridor_Number)%>%
#  summarise(geometry=st_union(geometry))%>%
#  rename(corridor_number="Commercial_Corridor_Number")%>%
#  select(corridor_number)

#corridor_tracts <- corridor_tracts %>% 
#  st_transform(4326)

# corridor_tracts %>% ggplot() + geom_sf(aes(col = corridor_number))

usethis::use_data(corridors, corridor_tracts, overwrite = TRUE)
