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
app_dir <- paste0(wd, '/WarehouseCENTRAL')
#warehouse_dir <- paste0(wd, '/Warehouse_data')
output_dir <- paste0(wd, '/exports_other')
#crest_dir <- paste0(warehouse_dir, '/CREST_tables.gdb')
parcel_dir <- paste0(wd, '/Parcels')
APNIndex_dir <- paste0(wd, '/APNBookIndex')
zoning_dir <- paste0(wd, '/Zoning')
tracy_dir <- paste0(wd, '/TracyZoning')
stockton_dir <- paste0(wd, '/StocktonZoning')
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
  bind_rows(plannedZones_stsls) |> 
  filter(Zone_Descr %in% c('Industrial', 'Planned Industrial', 
                           'Limited Industrial', 'Industrial UT'))

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

ironMtnTracy <- sjv_parcels |> 
  filter(APN_CHR %in% c('21306048', '21216020')) |> 
  select(APN_CHR, geometry) |> 
  mutate(category = 'Existing Industrial',
         ZONING_COD = 'M-1') |> 
  rename(class = ZONING_COD,
         APN = APN_CHR) |> 
  st_transform(crs = 4326)

tracy_parcels2 <- sjv_parcels |>
  st_intersection(tracy_zones) |>
  select(APN_CHR, ZONING_COD, geometry) |> 
  mutate(category = 'Existing Industrial') |> 
  rename(class = ZONING_COD,
         APN = APN_CHR) |> 
  st_transform(crs = 4326)
  #st_set_geometry(value = NULL) |> 
  #left_join(sjv_parcels) |> 
  #st_as_sf()

tracy_parcels <- bind_rows(tracy_parcels2, ironMtnTracy)

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

# Stockton industrial zones and parcels

stockton_zones <- sf::st_read(dsn = stockton_dir) |> 
  st_transform(crs = 4326) |>
  filter(ZONE %in% c('IG', 'IL', 'PT', '(IG)', '(IL)'))

zone_cats <- stockton_zones |> 
  st_set_geometry(value = NULL) |> 
  select(ZONE) |> 
  distinct()

#stocktonPal <- colorFactor(palette = 'Set2', domain = stockton_parcels$class)

#leaflet() |> 
#  addProviderTiles(provider = providers$Esri.WorldImagery) |> 
#  addPolygons(data = stockton_parcels,
#              fillColor = ~stocktonPal(class),
#              color = 'black',
#              label = ~APN,
#              fillOpacity = 0.8,
#              weight = 0.3) |> 
#  addLegend(data = stockton_parcels,
#            title = 'Zoning Category',
#            pal = stocktonPal,
#            values = ~class)

unusedStocktonParcels <- c('11736008', '11736009', '11736007', '11736023', '11736012', 
                            '13306006', '13310002', '14520001', '14520010',
                           '16203003', '16330008', '16326023', '16326020', '17702029',
                           '17702017', '17702039', '17523003', '17705025', '17705031',
                           '17705009', '17710046', 
                           '17711004', '17710003', '20102001', #Stockton Commerce Center APN
                           '17922039', 
                           '17920043', '18110027', '18110028', #Sanchez-Hogan Annexation Project
                           #'17922041', #'17922003', 
                       #    '17907004', '17923001', '17923011', '17923010', '17907017',
                           '18111004',  '18111020', '18111016', '18111006', '18111018', 
                           '19302007')

stockton_parcels <- sjv_parcels |>
  st_intersection(stockton_zones) |>
  select(APN_CHR, ZONE, geometry) |> 
  mutate(category = 'Existing Industrial') |> 
  rename(class = ZONE,
         APN = APN_CHR) |> 
  st_transform(crs = 4326) |> 
  filter(APN %ni% unusedStocktonParcels)

stockton_area1 <- as.numeric(units::set_units(st_area(stockton_parcels), acres))

stockton_parcels2 <- stockton_parcels |> 
  bind_cols(stockton_area1) |> 
  st_set_geometry(value = NULL) |> 
  summarize(acres = sum(...5))

stockton_UnusedIndparcels <- sjv_parcels |>
  st_intersection(stockton_zones) |>
  select(APN_CHR, ZONE, geometry) |> 
  mutate(category = 'Existing Industrial') |> 
  rename(class = ZONE,
         APN = APN_CHR) |> 
  st_transform(crs = 4326) |> 
  filter(APN %in% unusedStocktonParcels)

stockton_area <- as.numeric(units::set_units(st_area(stockton_UnusedIndparcels), acres))

stockton_UnusedIndparcels$area <- stockton_area

areaUnused <- stockton_UnusedIndparcels |> 
  st_set_geometry(value = NULL) |> 
  summarize(Acres = sum(area))

str(tracy_parcels)
str(CEQA_WH2)
str(uninc_Industrial_stsls)
str(whZ_2)
str(stockton_parcels)

## Lathrop and Manteca Parcels
source('FinalGCSWLathropMantecaProject.R')

LathMant <- LathMant_final |> 
  select(APN_CHR, geometry) |> 
  mutate(class = 'IZ', category = 'Existing Industrial') |> 
  rename(APN = APN_CHR)

# Bind all the cities to unincorporated county parcels                   
centralWH_raw <- bind_rows(tracy_parcels2, uninc_Industrial_stsls, whZ_2, CEQA_WH2, stockton_parcels,
                       LathMant)


notWarehouse_uninc <- c('17728039', '17728037', '17728018',
                        '13207010', '14326015', '10121003', '10121004', '003014005')
vacantPlanned <- c('17922019', '17922018', '17922017', '17922016', #MariposaIndustrialPark
                   '17922013', '17922012', '17922011', '17922010',#, #MariposaIndustrialPark
                     '17922041', '17922003', 
                     '17907004', '17923001', '17923011', '17923010', '17907017'
)

notWarehouses <- c('25311031', '25311016', '25310014', '25310003',  '25311022', #Airport
                   '20909045', '20909025', '20908023', '20908024', #Auto Auction
                   '20945044', '20945045', '20945050', '20945051', '20906065', 
                   '25312041', '25311029', '25311027', '25311028', '25311003',
                   '25311006', '25311007', '25311009', '25311008', '25311009',
                   '25311014', '25311034', '25312053', '25312052', '25312036', 
                   '25312048', '25312049', '25312047', '25321003', '25321033', 
                   '25301009', '25321018', '25321028', '25321025', #industrial facilities next to airport)
                   '24847028', '24613003',
                   '25207003', '25207004', '25215012', '25215008', '25201009', '25201007', '25215015', #parcels next to defense logistics industry
                   '21223006', '21223003', '21223005', #water treatment facility
                   '21224010', '21224011', '21225012', '21225011', '21221009', #random small businesses)
                   '21216016', '21216005', '21305009', '21307001', '21305008', #Agricultural North but zoned Industrial
                   '20922028', '20922027', '20946034', '20946031', '20940002', '20940010', 
                   '20908026', '20908037', '20908038', '20948007', 
                   '20916001', '20916013', '20916012',#Ag but zoned industrial west)
                   '22809006', '22809010', '22801005',#, #Ag and road southeast 1
                   '22114015', '22114027', '22114008', '22105014', '22125035', '22125026', #Ag and road southeast 1
                   '24103026', '24103019', '24103045', '24103046', '24103024', # Lathrop South
                   '19813044', '24103011', '24103034', '24140044', '24140033', '24140031', '24140029', 
                   '24128012', '24131007', '24131008', '24131046',
                   '19813019', '19813020', '19813035', '19813060', '19813059', 
                   '19818005', '19815006', '19801034', '19815004', '19815008', '19823001',
                   '19813054', '19813055', '19813056', '19813057', '19815001',
                   '19823012', '19817040', '19817008', '19816035', '19817014', '19817029', '19817042', #Lathrop South
                   '19818009', '19818008', '19818006', '19814011', '19804014',
                   '19817003', '19812005', '19810018', '19809017',
                   '19804023', '19820014', '19820021', '19820013' # Lathrop Central
)   

centralWH <- centralWH_raw |> 
  filter(APN %ni% notWarehouse_uninc) |> 
  filter(APN %ni% vacantPlanned) |> 
  filter(APN %ni% notWarehouses)# | 

footprint <- as.numeric(units::set_units(st_area(centralWH), ft^2))

centralWH$footprint <- round(footprint, -2)

centralWH2 <- centralWH |> 
  filter(footprint > sq_ft_threshold_WH)

whPal <- colorFactor(palette = c('red', 'black'),
                     domain = centralWH$category)

cities3 <- cities2 |> 
  filter(Zone_Label %ni% c('TRACY', 'STOCKTON', 'LATHROP', 'MANTECA'))

leaflet() |> 
  addProviderTiles(provider = providers$CartoDB.Positron,
                   group = 'Basemap') |> 
  addProviderTiles(provider = providers$Esri.WorldImagery,
                   group = 'Imagery') |> 
  addLayersControl(baseGroups = c('BaseMap', 'Imagery')) |> 
  addPolygons(data = centralWH2,
              color = ~whPal(category),
              fillOpacity = 0.5,
              weight = 1,
              label = ~APN) |> 
  addLegend(data = centralWH2,
            pal = whPal,
            values = ~category,
            title = 'Category') |> 
  addPolygons(data =cities3,
              color = 'lightblue',
              fillOpacity = 0.3,
              weight = 1) |> 
  addLegend(data = cities3,
            colors = 'lightblue',
            labels = 'Cities - in progress') |> 
  addPolylines(data = cities2,
              color = 'darkred',
              weight = 2,
              fillOpacity = 0) 


rm(ls = IndZones_stsls, sjv_parcels, sjv_apn, sjv_zoning, stanislaus_zoning, stanislausParcels,
   tracy_zone_codes, tracy_zones)
rm(ls = CentralValleyPlanned, cities1, whZ, zones, zoning_types_stsls, uninc_Industrial_stsls,
   tracy_parcels, CEQA_WH, CEQA_WH2, plannedZones_stsls)
rm(ls = stockton_parcels, stockton_zones)

gc()
setwd(app_dir)
save.image('.RData')
setwd(wd)



