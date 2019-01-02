# R 3.5.1 

devtools::install_github("r-spatial/mapedit")
devtools::install_github("bhaskarvk/leaflet.extras")

#Proof of Concept 1 | Draw on Blank Map
library(mapview)
library(mapedit)
library(tidyverse)
library(sf)

what_we_created <- mapview() %>%
  editMap()

mapview(what_we_created$finished)

#Proof of Concept 2 | Edit and Delete Existing Features
library(sf)

# simplified border for purpose of exercise
border <- st_as_sfc(
  "LINESTRING(-109.050197582692 31.3535554844322, -109.050197582692 31.3535554844322, -111.071681957692 31.3723176640684, -111.071681957692 31.3723176640684, -114.807033520192 32.509681296831, -114.807033520192 32.509681296831, -114.741115551442 32.750242384668, -114.741115551442 32.750242384668, -117.158107738942 32.5652527715121, -117.158107738942 32.5652527715121)"
) %>%
  st_set_crs(4326)

# plot quickly for visual inspection
plot(border)

library(mapview)
library(mapedit)

new_borders <- mapview(border) %>%
  editMap("border")

# clean up before we move on
rm(list=ls())








## Now apply to sites that selected too many line features
library(mapview)
library(mapedit)
library(tidyverse)
library(sf)
library(leaflet)

WQS <- st_read('data/WQS2018_BRRO_albers_mini.shp')

probSites_xl <- read_csv('data/probSites_mini.csv')

probSites_sf <- st_as_sf(probSites_xl, 
                         coords = c("LongitudeD", "LatitudeDD"), # for point data
                         remove = F, # don't remove these lat/lon cols from df
                         crs = 4269) %>% # add projection, needs to be geographic for now bc entering lat/lng, 
  st_transform( st_crs(WQS))# project to Albers equal area for snapping

testList <- readRDS('data/testList.RDS')


# First let user choose which stream segment to keep if buffer snapped to more than one

#import my working function
snapCheck <- function(successDataFrame){
  successDataFrame %>%
    group_by(`Point Unique Identifier`) %>%
    filter(n()>1)
}

tooMany <- snapCheck(testList[['sf_output']] ) %>%
  st_transform(4326)# project to WGS84 for plotting


siteWithTooMany <- filter(probSites_sf, StationID == unique(tooMany$`Point Unique Identifier`)) %>%
  st_transform(4326)# project to WGS84 for plotting

# mapedit tooMany


fixedSites <- mapview(tooMany) %>%
  editMap('tooMany')
# works but man editing the feature and deleting all those vertices would be a pain!


#fixedSites <- mapview(tooMany) +
#  mapview(siteWithTooMany) %>%
#  editMap("tooMany")

# doesn't like mapview way of adding layers +


# Try with leaflet addMarkers
fixedSites <- mapview(tooMany) %>%
  addMarkers(data=siteWithTooMany,~LongitudeD,~LatitudeDD,#~geometry[[1]][1],~geometry[[1]][1], 
             popup = siteWithTooMany$StationID) %>%
  editMap('tooMany')
# Still doesn't like it



# Make leaflet map to get desired results

fixedSites <- leaflet(tooMany) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap,group='Nat Geo World Map') %>%
  addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery') %>%
  addProviderTiles(providers$OpenStreetMap,group='Open Street Map') %>%
  addPolylines(data=tooMany, group='WQS',
               color = ~colorNumeric(c("red", "green", "blue",'yellow'),OBJECTID)(OBJECTID),
               popup=popupTable(tooMany,zcol=c('Point Unique Identifier','Buffer Distance',
                                               "WATER_NAME","BASIN","WQS_COMMEN","SEC","CLASS",
                                               "SPSTDS",'SECTION_DE','Basin_Code','PWS','Trout',
                                               'Edit_Date','StreamType','Tier_III'))) %>%
  addMarkers(data=siteWithTooMany,~LongitudeD,~LatitudeDD,#~geometry[[1]][1],~geometry[[1]][1], 
             popup = siteWithTooMany$StationID, group='point') %>%
  addLayersControl(baseGroups=c('Nat Geo World Map','Esri World Imagery','Open Street Map'),
                   overlayGroups = c('WQS','point'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft') %>%
  editMap("tooMany")

# the look I want but cannot edit any layers now...


# Marc's code: can't edit features though
fixedSites <- mapview (tooMany)%>%
  
  editMap("siteWithTooMany")




# Emma's Conclusion:
# I love the idea of building in a feature (function) to package that quickly builds this interactive
# map of sites with too many snaps and lets users delete the undesired snaps, but mapedit just
# isn't there yet as a package
