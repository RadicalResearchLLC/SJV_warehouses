## Code initially created by Gabriela Camacho and Sia Were in Pitzer college course EA078
## Modified slightly by Mike McCarthy to work with Warehouse CENTRAL dataset
## First created November 2023
## Last modified December 2023
## 

#Creating one IZ polygon for Lathrop 
LathropIndustrial <- rbind(
  c(-121.299766, 37.779972),
  c(-121.303933, 37.785550),
  c(-121.295855, 37.809201),
  c(-121.292454, 37.809940),
  c(-121.291178, 37.811619),
  c(-121.275193, 37.811753),
  c(-121.275246, 37.84269), 
  c(-121.284471, 37.842715), 
  c(-121.282384, 37.851933),
  c(-121.275475, 37.851865), 
  c(-121.275558, 37.855558), 
  c(-121.281375, 37.855659),
  c(-121.277846, 37.868393), 
  c(-121.274806, 37.868342), 
  c(-121.274526, 37.857554), 
  c(-121.264925, 37.859626), 
  c(-121.261612, 37.798111),
  c(-121.270371, 37.79083),
  c(-121.279046, 37.78699),
  c(-121.299738, 37.780107),
  c(-121.299766, 37.779972))
LI_polyg <- st_sf(name = 'LathropIndustrial', geom = st_sfc(st_polygon(list(LathropIndustrial))), crs = 4326)


#SJV2cities.geojson <- sf::st_read(dsn = 'SJV2cities.geojson')

Lathropparcels <- sjv_parcels |> 
  st_filter(LI_polyg)
leaflet() |>
  addProviderTiles(provider = providers$CartoDB.Positron, group = 'Basemap') |> 
  addProviderTiles(provider = providers$Esri.WorldImagery, group = 'Imagery') |> 
  addLayersControl(baseGroups = c('Basemap', 'Imagery'))|> 
  addPolygons(data = Lathropparcels, label= ~APN_CHR)

#Me exporting the table to confirm if they are warehouses 
#apn_column <- Lathropparcels$APN
#write.csv(apn_column, file = "APN_Column.csv", row.names = FALSE)

#This filter is selecting the parcels that are warehouses 
LathropWHFilter <- Lathropparcels |>
  filter(APN_CHR %in% c(19332008,19332012,19332018,19333028,19333032,19338001,19338004,19603001,19603002,
                        19603023, 19603025,19603026, 19603027,19603028,19603029,19603032,19801015,
                        19801034,19801044,19801047, 19802001,19803001,19804001,
                        19804007,19804014,19804020,19804023,19806002,19806004,19806005,19806011,19806012,
                        19806014,19806017,19806019, 19809017, 19810018,
                        19810023,19812004,19812005,19812009,19812012,19812013,19812014,19812015,19813019,19813020,19813030,19813035,19813038,
                        19813042, 19813044,19813054,19813055,19813056,19813057,19813059,19813060,19813061,19813062,19813063,19813065,19813066,
                        19814003,19814004,19814011,19814013,19814017, 19814018, 19815001,19815004,19815006,19815008,
                        19816003,19816026,19818005,19818006,19818008,19818009,19819004,19819006,19819019,19819020,19819021,
                        19819030,19819031,19819032,19819033,19821018,19821019,19822002,19822003,19822009, 19822010,19822012,
                        19823001,19823012,19823015,19823016,19823017,19823018,19823019,19824049,19825060,24103005,24103011,24103016,
                        24103018, 24103019,24103020,24103021,24103022, 24103024,24103026,24103029,24103033,
                        24103034,24103045,24103046,24103047,
                        24128012,24131007,24131008,24131046,
                        24140004,24140006,24140007,24140029,24140031,24140033,24140036,
                        24140037,24140042,24140043,24140044))

#This displays the selected parcels and edits the visuals through pop-ups, borders, color, and opacity 
leaflet() |>
  addProviderTiles(provider = providers$CartoDB.Positron, group = 'Basemap') |> 
  addProviderTiles(provider = providers$Esri.WorldImagery, group = 'Imagery') |> 
  addLayersControl(baseGroups = c('Basemap', 'Imagery'))|> 
  addPolygons(data = LathropWHFilter, label= ~APN_CHR, weight = 1, color = 'darkred', fillOpacity = 0.5)

#Second IZ polygon - don't include - no warehouses yet just industrial zoning
LathropIndustrial2 <- rbind(
  c(-121.315649, 37.840631),
  c(-121.316422, 37.838666),
  c(-121.314791, 37.835073),
  c(-121.310450, 37.832747),
  c(-121.309871, 37.830124),
  c(-121.289248, 37.830050),
  c(-121.287642, 37.835257),
  c(-121.286184, 37.840863),
  c(-121.302186, 37.840951),
  c(-121.315649, 37.840631))
LI_polyg2 <- st_sf(name = 'LathropIndustrial2', geom = st_sfc(st_polygon(list(LathropIndustrial2))), crs = 4326) 


LathropWHFilter2<- Lathropparcels |>
  filter(APN_CHR %in% c(19202064,19202063,19202062,19202061,19202006,19202008,19202009,
                        19202065,19202066,19202038,19202022,19202052,19202048,19202019,
                        19202018,19202068,19202069,19202016,19202036,19202070,19202014))



LathropIZ3 <-rbind(LathropWHFilter,LathropWHFilter2)

leaflet() %>% 
  addTiles() %>%
  addProviderTiles(provider = providers$CartoDB.PositronNoLabels) %>% 
  addPolygons(data = LathropIZ3,
              color = 'darkred',
              fillOpacity = 0.6,
              weight = 1)
              #label = ~name)

sf_use_s2(FALSE)

##parcel distinction 
Lathropparcels <- sjv_parcels |> 
  st_filter(LathropIZ3)

##Combining polygons 
leaflet() |>
  addProviderTiles(provider = providers$CartoDB.Positron, group = 'Basemap') |> 
  addProviderTiles(provider = providers$Esri.WorldImagery, group = 'Imagery') |> 
  addLayersControl(baseGroups = c('Basemap', 'Imagery'))|>
  addPolygons(data = LathropIZ3,
              color = 'darkred',
              fillOpacity = 0.6,
              weight = 1,
              label = ~APN_CHR) |>
  addLegend(data = LathropIZ3,
          colors = 'darkred',
          labels = 'Industrial Zone')



##City boundaries 
LathropManteca.geojson <- cities2 |> 
  filter(Zone_Label %in% c('LATHROP', 'MANTECA'))
#lathropManteca <- sf::st_read(dsn = 'City_Boundaries.geojson') |>
#  filter(CITY %in% c('Lathrop', 'Manteca')) |>
#  st_transform(crs = 4326)


##Manteca polygons (same process)
MantecaIndustrial <- rbind(
  c(-121.264215, 37.855566),
  c(-121.255583, 37.855474),
  c(-121.254911, 37.844295),
  c(-121.254622, 37.840987),
  c(-121.257669, 37.840965),
  c(-121.257473, 37.837455),
  c(-121.258871, 37.837367),
  c(-121.259703, 37.837341),
  c(-121.259502, 37.833762),
  c(-121.262610, 37.833760),
  c(-121.264215, 37.855566))
MI_polyg <- st_sf(name = 'MantecaIndustrial', geom = st_sfc(st_polygon(list(MantecaIndustrial))), crs = 4326)
MantecaIndustrial2 <- rbind(
  c(-121.214391, 37.792927),
  c(-121.213275, 37.792927),
  c(-121.201645, 37.786823),
  c(-121.201645, 37.792419),
  c(-121.201645, 37.794657),
  c(-121.197483, 37.793504),
  c(-121.194052, 37.793794),
  c(-121.190592, 37.793826),
  c(-121.189494, 37.789356),
  c(-121.189738, 37.785431),
  c(-121.193279, 37.783598),
  c(-121.200605, 37.783501),
  c(-121.206751, 37.783534),
  c(-121.211309, 37.783694),
  c(-121.213344, 37.784563),
  c(-121.213344, 37.785496),
  c(-121.213426, 37.787297),
  c(-121.215827, 37.787715),
  c(-121.215949, 37.789323),
  c(-121.214118, 37.789645),
  c(-121.213751, 37.790996),
  c(-121.215909, 37.791253),
  c(-121.215949, 37.793022),
  c(-121.214810, 37.792315),
  c(-121.214199, 37.792958),
  c(-121.214565, 37.793859),
  c(-121.214391, 37.792927))
MI_polyg2 <- st_sf(name = 'MantecaIndustrial2', geom = st_sfc(st_polygon(list(MantecaIndustrial2))), crs = 4326)
MantecaIndustrial <- rbind(
  c(-121.195322, 37.783102),
  c(-121.189159, 37.783224),
  c(-121.191497, 37.780811),
  c(-121.195322, 37.783102),
  c(-121.195322, 37.783102))
MI_polyg3 <- st_sf(name = 'MantecaIndustrial', geom = st_sfc(st_polygon(list(MantecaIndustrial))), crs = 4326)
MantecaIndustrial <- rbind(
  c(-121.188761, 37.779572),
  c(-121.186131, 37.778705),
  c(-121.183327, 37.777439),
  c(-121.182700, 37.776957),
  c(-121.183467, 37.776076),
  c(-121.188900, 37.779146),
  c(-121.188761, 37.779572))
MI_polyg4 <- st_sf(name = 'MantecaIndustrial', geom = st_sfc(st_polygon(list(MantecaIndustrial))), crs = 4326)
MantecaIndustrial <- rbind(
  c(-121.261720, 37.802756),
  c(-121.261413, 37.800968),
  c(-121.256959, 37.801102),
  c(-121.256921, 37.800078),
  c(-121.255032, 37.800048),
  c(-121.255220, 37.797488),
  c(-121.263608, 37.797447),
  c(-121.262579, 37.799990),
  c(-121.261720, 37.802756))
MI_polyg5 <- st_sf(name = 'MantecaIndustrial', geom = st_sfc(st_polygon(list(MantecaIndustrial))), crs = 4326)
MantecaIndustrial <- rbind(
  c(-121.169854, 37.767499),
  c(-121.163581, 37.763850),
  c(-121.169902, 37.763872),
  c(-121.169854, 37.767499))
MI_polyg6 <- st_sf(name = 'MantecaIndustrial', geom = st_sfc(st_polygon(list(MantecaIndustrial))), crs = 4326)
MantecaIndustrial <- rbind(
  c(-121.169761, 37.761588),
  c(-121.166786, 37.761573),
  c(-121.166768, 37.759324),
  c(-121.163849, 37.759280),
  c(-121.160727, 37.759221),
  c(-121.157623, 37.760258),
  c(-121.156422, 37.759499),
  c(-121.156200, 37.757761),
  c(-121.156194, 37.757686),
  c(-121.160565, 37.757595),
  c(-121.160735, 37.756039),
  c(-121.163851, 37.756002),
  c(-121.163934, 37.757540),
  c(-121.166762, 37.757574),
  c(-121.169751, 37.757588),
  c(-121.169761, 37.761588))
MI_polyg7 <- st_sf(name = 'MantecaIndustrial', geom = st_sfc(st_polygon(list(MantecaIndustrial))), crs = 4326)

leaflet() |>
  addProviderTiles(provider = providers$CartoDB.Positron, group = 'Basemap') |>
  addProviderTiles(provider = providers$Esri.WorldImagery, group = 'Imagery') |>
  addLayersControl(baseGroups = c('Basemap', 'Imagery'))|>
  addPolygons(data = MI_polyg)
#We used a filter

Mantecaparcels2 <- sjv_parcels |>
  st_filter(MI_polyg)

threeMoreManteca <- c('19811014', '19811012', '19811013')
manteca3 <- sjv_parcels |> 
  filter(APN %in% threeMoreManteca) 



#We change the appearance, thus making it more salient and uniform
leaflet() |>
  addProviderTiles(provider = providers$CartoDB.Positron, group = 'Basemap') |>
  addProviderTiles(provider = providers$Esri.WorldImagery, group = 'Imagery') |>
  addLayersControl(baseGroups = c('Basemap', 'Imagery'))|>
  addPolygons(data = Mantecaparcels2, label= ~APN_CHR, weight = 1, color = 'darkred', fillOpacity = 0.5)
##Update this line of code with the name of your warehouse polygon

#plannedWarehouses <- rbind(MantecaIndustrial, MantecaIndustrial2)
MantecaIZ <- rbind(MI_polyg, MI_polyg2, MI_polyg3, MI_polyg4, MI_polyg5, MI_polyg6, MI_polyg7)
leaflet() %>%
  addTiles() %>%
  addProviderTiles(provider = providers$CartoDB.PositronNoLabels) %>%
  addPolygons(data = MantecaIZ,
              color = 'darkred',
              fillOpacity = 0.6,
              weight = 1,
              label = ~name) #%>%
#setView(lng = -117.396398, lat = 34.590419, zoom = 14)

##Combining all of our polygons 
sf_use_s2(FALSE)
Mantecaparcels <- sjv_parcels |>
  st_filter(MantecaIZ) |> 
  bind_rows(manteca3)


LathMant_combo <- bind_rows(LathropIZ3, Mantecaparcels)
## Lathrop and Manteca Parcels

vacantPlanned <- c('17922019', '17922018', '17922017', '17922016', #MariposaIndustrialPark
                   '17922013', '17922012', '17922011', '17922010'#, #MariposaIndustrialPark
                   #  '17922041', '17922003', 
                   #  '17907004', '17923001', '17923011', '17923010', '17907017'
)
notWarehouses <- c('25311031', '25311016', '25310014', '25310003',  '25311022', #Airport
                   '20909045', '20909025', '20908023', '20908024', #Auto Auction
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

notInCities <- c('19333028', '19338001', '19338004', '19332008', '19332012', '19332018',
                 '22814001', '22814002', '19820016', '19803027', '19333032',
                 '22814003', '22814004'  )

LathMant_final <- LathMant_combo |> 
  filter(APN %ni% vacantPlanned) |> 
  filter(APN %ni% notWarehouses) |> 
  filter(APN %ni% notInCities)

leaflet() |>
  addProviderTiles(provider = providers$CartoDB.Positron, group = 'Basemap') |> 
  addProviderTiles(provider = providers$Esri.WorldImagery, group = 'Imagery') |> 
  addLayersControl(baseGroups = c('Basemap', 'Imagery'))|>
  # addPolygons(data =lathropManteca,
  #              color = 'lightblue',
  #              fillOpacity = 0.3,
  #             weight = 1) |> 
  #  addLegend(data = lathropManteca,
  #            colors = 'lightblue',
  #           labels = 'City Boundary') |>
  addPolygons(data = LathMant_final,
              color = 'darkred',
              fillOpacity = 0.6,
              weight = 1,
              label = ~APN) |>
  addLegend(data = LathMant_final,
            colors = 'darkred',
            labels = 'Warehoues in Industrial Zone (IZ)') |> 
  addPolylines(data = LathropManteca.geojson,
               color = 'black',
               weight = 2)           

rm(ls = LathropIZ3, LathropIndustrial, LathropIndustrial2,
   Lathropparcels, LathropWHFilter, LI_polyg)  
rm(ls = LathMant_combo, LathropManteca.geojson, Lathropresidential, 
   LI_polyg2, LI_polyres1, manteca3, MantecaIndustrial,MantecaIndustrial2, MantecaResidential1)
rm(ls= MI_polyg, MI_polyg2, MI_polyg3, MI_polyg4, MI_polyg5, MI_polyg6, MI_polyg7)
rm(ls = Lathropresidentialparcels, LathropWHFilter2, MantecaIZ, Mantecaparcels, Mantecaparcels2, Mantecaparcels3)
