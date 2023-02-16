##SJV Warehouse Map App V1
##Created by Mike McCarthy, Radical Research LLC
##Inspired by Graham Brady and Susan Phillips at Pitzer College and their code
##located here: https://docs.google.com/document/d/16Op4GgmK0A_0mUHAf9qqXzT_aekbdLb_ZFtBaZKfj6w/edit
##First created May, 2022
##Last modified February, 2023
##This script acquires and tidy parcel data for the app

rm(list =ls()) # clear environment
'%ni%' <- Negate('%in%') ## not in operator
gc()
##Libraries used in data acquisition
#library(RCurl)

##Libraries used in data processing and visualization
library(tidyverse)
library(janitor)
library(readxl)
##spatial libraries and visualization annotation
library(leaflet)
library(sf)
library(htmltools)

##set working, data, and app directories
wd <- getwd()

###set data, app, and export subdirectories
app_dir <- paste0(wd, '/SanJoaquinWarehouses')
#warehouse_dir <- paste0(wd, '/Warehouse_data')
output_dir <- paste0(wd, '/exports_other')
#crest_dir <- paste0(warehouse_dir, '/CREST_tables.gdb')
parcel_dir <- paste0(wd, '/Parcels')
APNIndex_dir <- paste0(wd, '/APNBookIndex')
zoning_dir <- paste0(wd, '/Zoning')
tracy_dir <- paste0(wd, '/TracyZoning')
#SBD_parcel_dir <- paste0(warehouse_dir, '/SBD_Parcel')

#calEJScreen_dir <- paste0(wd, '/calenviroscreen40')



gc()
##Set minimum size for analysis in thousand sq.ft. for non-warehouse classified
sq_ft_threshold_WH <- 28000
sq_ft_threshold_maybeWH <- 150000

##Try to import LA County data
sf::st_layers(dsn = parcel_dir)
sjv_parcels <- sf::st_read(dsn = parcel_dir, quiet = TRUE, type = 3) %>%
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>% 
  filter(SHAPE_AREA >= sq_ft_threshold_maybeWH) %>% 
  st_make_valid()

sjv_apn <- sf::st_read(dsn = APNIndex_dir, quiet = TRUE, type = 3) %>%
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84")
sjv_zoning <- sf::st_read(dsn = zoning_dir, quiet = TRUE, type = 3) %>%
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") 

cities <- c('LATHROP', 'STOCKTON', 'TRACY', 'MANTECA',
            'RIPON')

zones <- sjv_zoning %>% 
  st_set_geometry(value = NULL) %>% 
  group_by(ZOCOLABEL, ZNDESCRIPT) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  mutate(WH = str_detect(ZNDESCRIPT, 'WAREHOUSE')) %>% 
  mutate(Ind = str_detect(ZNDESCRIPT, 'INDUSTRIAL')) %>% 
  mutate(truck = str_detect(ZNDESCRIPT, 'TRUCK')) %>% 
  mutate(ag = str_detect(ZNDESCRIPT, 'AGRICULTURE')) %>% 
  mutate(pubF = str_detect(ZNDESCRIPT, 'PUBLIC FACILITIES')) %>% 
  mutate(sum = WH+truck+Ind + pubF) %>% 
  filter(ag == FALSE) %>% 
  filter(ZNDESCRIPT %in% cities | sum > 0) %>% 
  filter(ZNDESCRIPT != 'GENERAL INDUSTRIAL') %>% 
  filter(ZNDESCRIPT != 'TRACY') %>% 
  left_join(sjv_zoning) %>% 
  st_as_sf() %>% 
  st_make_valid()


whZ <- zones %>% 
  st_join(sjv_parcels) %>% 
  st_set_geometry(value = NULL) %>% 
  left_join(sjv_parcels) %>% 
  st_as_sf()

leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(provider = providers$CartoDB.Positron,
                   group = 'Basemap') %>% 
  addProviderTiles(provider = providers$Esri.WorldImagery,
                   group = 'Imagery') %>% 
  addLayersControl(baseGroups = c('BaseMap', 'Imagery')) %>% 
  addPolygons(data = whZ,
              label = ~htmlEscape(paste(ZNDESCRIPT)),
              weight = 1, 
              fillOpacity = 0.4,
              color= 'red')

gc()

tracy_zones <- sf::st_read(dsn = tracy_dir, layer = 'TracyZoning') %>% 
  st_transform(crs = 4326) %>% 
  filter(ZONING_COD %in% c('M-1', 'M-2', 'NEI', 'CRSP'))

tracy_zone_codes <- tracy_zones %>% 
  st_set_geometry(value = NULL) %>% 
  group_by(ZONING_TYP, ZONING_COD) %>% 
  summarize(count = n())

tracy_parcels <- tracy_zones %>% 
  st_join(sjv_parcels) %>% 
  st_set_geometry(value = NULL) %>% 
  left_join(sjv_parcels) %>% 
  st_as_sf()



leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(provider = providers$CartoDB.Positron,
                   group = 'Basemap') %>% 
  addProviderTiles(provider = providers$Esri.WorldImagery,
                   group = 'Imagery') %>% 
  addLayersControl(baseGroups = c('BaseMap', 'Imagery')) %>% 
  #addPolygons(data = whZ,
  #            label = ~htmlEscape(paste(APN, ZNDESCRIPT)),
  #            weight = 1, 
  #            fillOpacity = 0.4,
  #            color= 'red') %>% 
  addPolygons(data = tracy_zones,
              label = ~htmlEscape(ZONING_COD))


setwd(app_dir)
save.image('.RData')




