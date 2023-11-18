##SJV Warehouse Map App V1
##Created by Mike McCarthy, Radical Research LLC
##First created February 2023
##Last modified November 2023
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

## Import Stanislaus County data - 
##https://open-data-stancounty-gis.hub.arcgis.com/datasets/stancounty-gis::parcels-for-download/about
## pulled September 2023

stanislausParcels <- sf::st_read(dsn = 'StanislausParcels.geojson') |> 
  st_transform(crs = 4326) |>
  st_zm(drop = TRUE, what = 'ZM')
  #filter(SHAPE_Area >= sq_ft_threshold_WH)

stanislaus_zoning <- sf::st_read(dsn = 'Zoning_AGOL_stanislaus.geojson') |> 
  st_transform(crs = 4326)

zoning_types_stsls <- stanislaus_zoning |> 
  st_set_geometry(value = NULL) |> 
  select(Zone_Descr, Zone_Label, Shape__Area) |> 
  group_by(Zone_Descr, Zone_Label) |> 
  summarize(count = n(), area = sum(Shape__Area))

plannedZones_stsls <- stanislaus_zoning |> 
  filter(Zone_Descr %in% c('Planned Industrial')) |> 
  mutate(Zone_Label == 'P-I') 

IndZones_stsls <- stanislaus_zoning |> 
  filter(Zone_Label %in% c('M', 'L-M', 'SCP-IBP',
                           'SCP-PI')) |> 
  bind_rows(plannedZones_stsls)

cities1 <- stanislaus_zoning |> 
  filter(Zone_Label %in% c('CERES', 'HUGHSON', 'MODESTO',
                           'NEWMAN', 'OAKDALE', 'PATTERSON',
                           'RIVERBANK', 'TURLOCK', 'WATERFORD')) |> 
  select(Zone_Descr, Zone_Label, geometry)

zonePal <- colorFactor(palette = 'Dark2', domain = IndZones_stsls$Zone_Descr)

##Try to import SJV County data
sf::st_layers(dsn = parcel_dir)
sjv_parcels <- sf::st_read(dsn = parcel_dir, quiet = TRUE, type = 3) %>%
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") |> 
  filter(SHAPE_AREA >= sq_ft_threshold_maybeWH) |> 
  st_make_valid()

sjv_apn <- sf::st_read(dsn = APNIndex_dir, quiet = TRUE, type = 3) %>%
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84")
sjv_zoning <- sf::st_read(dsn = zoning_dir, quiet = TRUE, type = 3) %>%
  st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") 

city_names <- c('LATHROP', 'STOCKTON', 'TRACY', 'MANTECA',
            'RIPON')

zones <- sjv_zoning |> 
  st_set_geometry(value = NULL) |> 
  group_by(ZOCOLABEL, ZNDESCRIPT)  |> 
  summarize(count = n(), .groups = 'drop') |> 
  mutate(WH = str_detect(ZNDESCRIPT, 'WAREHOUSE')) |> 
  mutate(Ind = str_detect(ZNDESCRIPT, 'INDUSTRIAL')) |> 
  mutate(truck = str_detect(ZNDESCRIPT, 'TRUCK')) |> 
  mutate(ag = str_detect(ZNDESCRIPT, 'AGRICULTURE')) |> 
  mutate(pubF = str_detect(ZNDESCRIPT, 'PUBLIC FACILITIES')) |> 
  mutate(sum = WH+truck+Ind) |> 
  filter(ag == FALSE) |> 
  filter( sum > 0) |> 
 # filter(ZNDESCRIPT != 'GENERAL INDUSTRIAL') |> 
 # filter(ZNDESCRIPT != 'TRACY') |> 
  left_join(sjv_zoning) |> 
  st_as_sf() |> 
  st_make_valid()

zones1 <- colorFactor(palette = 'Set2', domain = zones$ZNDESCRIPT)

sf_use_s2(FALSE)

cities2 <- sjv_zoning |> 
  filter(ZNDESCRIPT %in% city_names) |> 
  select(ZNCODE, ZNLABEL, geometry) |> 
  rename(Zone_Descr = ZNCODE, Zone_Label = ZNLABEL) |> 
  bind_rows(cities1)

whZ <-sjv_parcels |>
  st_intersection(zones) |> 
  select(APN_CHR, ZNDESCRIPT, geometry) |> 
  mutate(category = 'Existing Industrial') |> 
  rename(class = ZNDESCRIPT,
         APN = APN_CHR) |> 
  st_transform(crs= 4326)#|> 
  #filter()

types <- st_geometry_type(whZ)
types_df <- data.frame(types)
str(types_df)
whZ_2 <- bind_cols(whZ, types_df)

whZ_2 <- whZ |> 
  filter(types %in% c('POLYGON', 'MULTIPOLYGON'
                      #, 'GEOMETRYCOLLECTION' 
                      )) 

#leaflet() |> 
 # addTiles() |> 
  #addPolygons(data = whZ_2)

  #st_set_geometry(value = NULL) |> 
  #left_join(sjv_parcels) |> 
  #st_as_sf()

gc()

tracy_zones <- sf::st_read(dsn = tracy_dir, layer = 'TracyZoning') |> 
  st_transform(crs = 4326) |> 
  filter(ZONING_COD %in% c('M-1', 'M-2', 'NEI', 'CRSP'))

tracy_zone_codes <- tracy_zones |> 
  st_set_geometry(value = NULL) |> 
  group_by(ZONING_TYP, ZONING_COD) |> 
  summarize(count = n())

tracy_parcels <-sjv_parcels |>
  st_intersection(tracy_zones) |>
  select(APN_CHR, ZONING_COD, geometry) |> 
  mutate(category = 'Existing Industrial') |> 
  rename(class = ZONING_COD,
         APN = APN_CHR) |> 
  st_transform(crs = 4326)
  #st_set_geometry(value = NULL) |> 
  #left_join(sjv_parcels) |> 
  #st_as_sf()

source('CEQA_plannedWH.R')

uninc_Industrial_stsls <- stanislausParcels |> 
  st_intersection(IndZones_stsls)  |>
  select(APN, Zone_Descr) |> 
  mutate(category = 'Existing Industrial') |> 
  rename(class = Zone_Descr) |> 
  st_transform(crs = 4326)
  
#  st_filter(stanislausParcels)

names(tracy_parcels)
names(uninc_Industrial_stsls)
names(whZ_2)

CEQA_WH2 <- CEQA_WH |> 
  mutate(category = 'Planned or approved',
         class = 'TBD') |> 
  rename(APN = ProjectName,
         geometry = geom) |> 
  st_transform(crs = 4326)

str(tracy_parcels)
str(CEQA_WH2)
str(uninc_Industrial_stsls)
str(whZ_2)

leaflet() |> 
  addTiles() |> 
  addPolygons(data = tracy_parcels) |> 
  addPolygons(data = CEQA_WH2) |> 
  addPolygons(data =uninc_Industrial_stsls) |> 
  addPolygons(data = whZ_2)

centralWH <- bind_rows(tracy_parcels, uninc_Industrial_stsls, whZ_2, CEQA_WH2) 

whPal <- colorFactor(palette = c('red', 'black'),
                     domain = centralWH$category)

cities2 <- cities2 |> 
  filter(Zone_Label != 'TRACY')

leaflet() |> 
  addProviderTiles(provider = providers$CartoDB.Positron,
                   group = 'Basemap') |> 
  addProviderTiles(provider = providers$Esri.WorldImagery,
                   group = 'Imagery') |> 
  addLayersControl(baseGroups = c('BaseMap', 'Imagery')) |> 
  addPolygons(data = centralWH,
              color = ~whPal(category),
              fillOpacity = 0.7,
              weight = 1) |> 
  addLegend(data = centralWH,
            pal = whPal,
            values = ~category,
            title = 'Category') |> 
  addPolygons(data =cities2,
              color = 'lightblue',
              fillOpacity = 0.3,
              weight = 1) |> 
  addLegend(data = cities2,
            colors = 'lightblue',
            labels = 'Cities - in progress')


rm(ls = IndZones_stsls, sjv_parcels, sjv_apn, sjv_zoning, stanislaus_zoning, stanislausParcels,
   tracy_zone_codes, tracy_zones)
rm(ls = CentralValleyPlanned, cities1, whZ, zones, zoning_types_stsls, uninc_Industrial_stsls,
   tracy_parcels, CEQA_WH, CEQA_WH2, plannedZones_stsls)

gc()
setwd(app_dir)
save.image('.RData')




