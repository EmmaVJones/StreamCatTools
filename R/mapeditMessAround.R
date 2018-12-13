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

tooMany <- snapCheck(testList[['sf_output']] )

siteWithTooMany <- filter(probSites_sf, StationID == unique(tooMany$`Point Unique Identifier`))

# mapedit tooMany
fixedSites <- mapview(tooMany) +
  mapview(siteWithTooMany) %>%
  editMap("tooMany")
