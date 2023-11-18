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

deploy_date <- 'November 17, 2023'
version <- 'Warehouse CENTRAL v1.01, last updated'

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel(
    titlePanel(
      fluidRow(column(1),
               column(3,
                      div(style = 'height:60px; font-size: 30px;',
                          'Warehouse CENTRAL')),
               column(3, shiny::img(height = 60, src = 'Logo_Redford.jpg')),
              # column(2, shiny::img(height = 38, src = 'Logo.png')),
               column(3, shiny::img(height = 60, src = 'RNOW.png'))
      )
    ),
  ),

  tabsetPanel(
    tabPanel('Dashboard',
             # Display slider bar selections, checkbox, and summary text
             fluidRow(
               column(width = 10, align = 'center', leafletOutput("map", height = 700))
             ),
             fluidRow(
               column(width = 4, align = 'left',
                      hr(),
                      h3('Version and Deployment info'),
                      hr(),
                      paste(version, deploy_date))
             )),
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
                           domain = centralWH$category
                         )

  
  output$map <- renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      addProviderTiles(provider = providers$CartoDB.Positron,
                       group = 'Basemap') |> 
      addProviderTiles(provider = providers$Esri.WorldImagery,
                       group = 'Imagery') |> 
      addLayersControl(baseGroups = c('BaseMap', 'Imagery'),
                       overlayGroups = c('Industrial Parcels', 'Cities - in progress'),
                       options = layersControlOptions(collapsed = FALSE)) |> 
      addPolygons(data = centralWH,
                  label = ~paste(APN, class),
                  weight = 1, 
                  fillOpacity = 0.7,
                  color= ~palWH(category),
                  group = 'Industrial Parcels') |> 
      addLegend(data = centralWH,
                title = 'Industrial Parcels',
                pal = palWH,
                values = ~category,
                group = 'Industrial Parcels') |> 
      addPolygons(data = cities2,
                  label = ~Zone_Label,
                  weight = 1, 
                  fillOpacity = 0.4,
                  color = 'lightblue',
                  group = 'Cities - in progress')  |> 
      addLegend(data = cities2,
                labels = 'Cities - in progress',
                colors = 'lightblue',
                group = 'Cities - in progress') #|> 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
