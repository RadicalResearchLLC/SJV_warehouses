#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(htmltools)
library(leaflet)
library(sf)
library(DT)
library(tidyverse)
library(shinycssloaders)
library(markdown)

sf_use_s2(FALSE)
deploy_date <- 'March 4, 2024'
version <- 'Warehouse CENTRAL v1.04, last updated'

# Define UI for application that draws a histogram
ui <- fluidPage(title = 'Warehouse CENTRAL',

    # Application title
titlePanel(
  fluidRow(column(1),
    column(3,
       div(style = 'height:60px; font-size: 30px;', 'Warehouse CENTRAL')),
    column(3, shiny::img(height = 60, src = 'Logo_Redford.jpg')),
              # column(2, shiny::img(height = 38, src = 'Logo.png')),
    column(3, shiny::img(height = 60, src = 'RNOW.png'))
    )
),

tabsetPanel(
  tabPanel('Dashboard',
             # Display slider bar selections, checkbox, and summary text
    fluidRow(column(3, align = 'center', h3('Warehouse Selection Filters'),
    hr(),
    #cities
    selectizeInput(inputId = 'City', label = 'Jurisdictions - Select up to 8',
      choices = c('', sort(cities2$Zone_Label), 'unincorporated'), 
      options = list(maxItems = 8)),
    #circle selection
    sliderInput('radius', 'Circle (radius in km)', min = 1, max = 10, value = 5, step =1),
    actionButton(inputId = 'Reset', label = 'Reset circle'),
    hr(),
      div(
      h3('Advanced User Input Options'),
      checkboxInput(inputId = 'Z', label = NULL, value = FALSE)
      ),
      conditionalPanel("input.Z == true",
        numericInput(inputId = 'FAR', label = 'Floor area ratio - (0.05 to 1)', value = 0.55,
          min = 0.05, max = 1.0, step = 0.05, width = '200px'),
        numericInput(inputId = 'TruckPerTSF', label = 'Truck Trips per 1,000 sq.ft. (0.1 to 1.5)', value = 0.67,
          min = 0.1, max = 1.5, step = 0.01, width = '200px'),
        numericInput(inputId = 'avgVMT', label = 'Truck trip length (miles - 5 to 60)', value = 38,
          min = 5, max = 60, step = 1, width = '200px'),
        numericInput(inputId = 'DPMperMile', label = 'Diesel PM (lbs/mile)', value = 0.0000364,
          min = 0.0000100, max = 0.0001, step = 0.000001, width = '200px'),
        numericInput(inputId = 'NOxperMile', label = 'NOx (lbs/mile)', value = 0.00410,
          min = 0.0004, max = 0.04, step = 0.0001, width = '200px'),
        numericInput(inputId = 'CO2perMile', label = 'CO2 (lbs/mile)', value = 2.44,
          min = 1, max = 5, step = 0.01, width = '200px'),
        numericInput(inputId = 'Jobs', label = 'Jobs per acre', value = 8,
          min = 4, max = 20, step = 1, width = '200px')
        ),
        hr(),
        h3('Version and Deployment info'),
        hr(),
        paste(version, deploy_date)
        ),
        column(8, align = 'center', 
          shinycssloaders::withSpinner(dataTableOutput('Summary')),
          shinycssloaders::withSpinner(leafletOutput("map", height = 600)),
          shinycssloaders::withSpinner(dataTableOutput('warehouseDF'))
        )
    )
  ),
  tabPanel('Readme',
    div(style = 'width: 90%; margin: auto;',
    fluidRow(includeMarkdown("README.md")),
    )
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

palWH <- colorFactor(palette = c('red', 'black'), 
                           domain = centralWH_final$category
                         )

output$map <- renderLeaflet({
  leaflet() |> 
    addProviderTiles(provider = providers$CartoDB.Positron, group = 'Basemap') |> 
    addProviderTiles(provider = providers$Esri.WorldImagery, group = 'Imagery') |> 
    addLayersControl(baseGroups = c('BaseMap', 'Imagery'), 
      overlayGroups = c('Industrial Parcels', 'Cities - in progress'),
      options = layersControlOptions(collapsed = FALSE)) |> 
    addPolygons(data = cities3,
      label = ~Zone_Label,
      weight = 1, 
      fillOpacity = 0.4,
      color = 'lightblue',
      group = 'Cities - in progress')  |> 
    addLegend(data = cities3,
      labels = 'Cities - in progress',
      colors = 'lightblue',
      group = 'Cities - in progress') |> 
    addPolylines(data = cities2,
      color = 'brown',
      weight = 2) |> 
    addPolygons(data = centralWH_final,
      label = ~paste(APN, class),
      weight = 1, 
      fillOpacity = 0.5,
      color= ~palWH(category),
      group = 'Industrial Parcels') |> 
    addLegend(data = centralWH_final,
      title = 'Industrial Parcels',
      pal = palWH,
      values = ~category,
      group = 'Industrial Parcels') 
})

WH_table <- reactive({
  df <- centralWH_final |> 
    st_set_geometry(value = NULL) |> 
    mutate(footprint = round(footprint, -2)) |> 
    rename(Category = category, 
                       'Assessor parcel number' = APN, 
                       'Building classification' = class) |> 
    arrange(desc(footprint))
  return(df)
})  
    
output$warehouseDF <- DT::renderDataTable(
    WH_table(),
   # caption  = 'Warehouse list by parcel number, zoning, and category',
    rownames = FALSE, 
    options = list(
      dom = 'Btp',
      pageLength = 10)
    #  buttons = list( 
    #    list(extend = 'csv', filename = paste('Warehouse_List', sep='-')),
    #    list(extend = 'excel', filename =  paste("Warehouse_List", sep = "-")))),
    #extensions = c('Buttons'),
   # filter = list(position = 'top', clear = FALSE)
  )  

##Code to select nearby warehouse polygons
nearby_warehouses <- reactive({
  req(circle())
  nearby <- st_join(circle(), filteredParcels(), join = st_contains)  |>
    st_set_geometry(value = NULL)  |>
    as.data.frame() |> 
    rename(parcel.number = APN)  |> 
    mutate(Sq.ft. = round((footprint*input$FAR), -3), 
           acreage = round(footprint/43560, 0))  |> 
    dplyr::select(category, parcel.number, class, acreage, Sq.ft.)  |> 
    arrange(desc(Sq.ft.))
  
  return(nearby)
})

#Select warehouses within city
filteredParcels <- reactive({
#  if(input$Juris_District == 'Jurisdiction') {
    if(is.null(input$City)) {
      cityParcels <- centralWH_final
   }
    else {
      cityParcels <- centralWH_final |>  
      filter(jurisdiction %in% input$City) 
  }
  #}
  #else {
  #  if(is.null(input$District)) {
  #    cityParcels <- centralWH2
   # }
  #  else {
   #   cityParcels <- centralWH2 |>
  #      ##FIXME - note that this is theoretically districts, not cities 
  #      st_filter(selected_cities_all())
  #  }
 # }
  
  return(cityParcels)
})

##Calculate circle around a selected point
circle <- reactive({
  req(circle_click$mapClick)
  
  distance <- input$radius*1000
  lat1 <- round(circle_click$mapClick$lat, 8)
  lng1 <- round(circle_click$mapClick$lng, 8)
  clickedCoords <- data.frame(lng1, lat1)
  
  dat_point <- st_as_sf(clickedCoords, coords = c('lng1', 'lat1'), crs = 4326)  |> 
    st_transform(3857)
  circle_sf <- st_buffer(dat_point, distance)  |> 
    st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84")
  return(circle_sf)
})

##Select between data without selection radius or with
parcelDF_circle <- reactive({
  if (is.null(circle_click$mapClick)) {
    warehouse2 <- filteredParcels()  |> 
      as.data.frame()  |> 
      rename(parcel.number = APN)  |> 
      mutate(Sq.ft. = round((footprint*input$FAR), -3), 
             acreage = round(footprint/43560, 0))  |> 
      dplyr::select(category, parcel.number, class, acreage, Sq.ft.) |> 
      arrange(desc(Sq.ft.)) 
  }
  else {
    warehouse2 <- nearby_warehouses()  |> 
      as.data.frame()
  }
  return(warehouse2)
})


SumStats <- reactive({
  req(parcelDF_circle())
  
  parcelDF_circle()  |> 
    group_by(category)  |>
    summarize(Warehouses = n(), Acreage = round(sum(acreage), 0), 
      Total.Bldg.Sq.ft = round(sum(acreage*input$FAR*43560), -5), .groups = 'drop')  |>  
    mutate(Truck.Trips = round(input$TruckPerTSF*0.001*Total.Bldg.Sq.ft ,-3))  |> 
    mutate('Daily Diesel PM (pounds)' = round(input$avgVMT*Truck.Trips*input$DPMperMile,1),
           'Daily NOx (pounds)' = round(input$avgVMT*Truck.Trips*input$NOxperMile, 0),
           'Daily CO2 (metric tons)' = round(input$avgVMT*Truck.Trips*input$CO2perMile*0.000453592, 1),
           'Jobs' = round(input$Jobs*Acreage))  |> 
    rename('Warehouse floor space (Sq.Ft.)' = Total.Bldg.Sq.ft,  'Daily Truck trips' = Truck.Trips,
           Category = category, 'Warehouse count' = Warehouses)
})

##Display summary table
output$Summary <- renderDataTable(
  SumStats()  |> 
    datatable(
      caption  = 'This interactive map shows the logistics industry footprint in San Joaquin and Stanislaus Counties.  Zoom in and/or click an area to see specific community impacts. Summary statistics are estimates based on best available information. Please see Readme tab for more information on methods and data sources.',
      rownames = FALSE, 
      options = list(dom = 'Bt',
                     buttons = list( 
                       list(extend = 'csv', filename = paste('SummaryStats', sep='-')),
                       list(extend = 'excel', filename =  paste("SummaryStats", sep = "-")))),
      extensions = c('Buttons')
    )  |>
    formatRound(columns = c(2:5, 7, 9),
                interval = 3, mark = ',',
                digits = 0)
) 

## reactive circle
circle_click <- reactiveValues(mapClick = NULL)

## pass in map click
observeEvent(input$map_click,
             {circle_click$mapClick <- input$map_click})
##Unclick circle
observeEvent(input$Reset, 
             {circle_click$mapClick <- NULL}
)

#Circle select
observe({
  req(circle_click$mapClick)
  
  leafletProxy("map", data = circle()) %>%
    #  clearShapes(group = 'Circle')  |>
    clearGroup(group = 'Circle') %>%
    addPolygons(color = 'grey50',
                group = 'Circle')
})
#Remove circle
observeEvent(input$Reset,   
             {leafletProxy("map", data = circle()) %>%
                 clearGroup(group = 'Circle')
             }
)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
