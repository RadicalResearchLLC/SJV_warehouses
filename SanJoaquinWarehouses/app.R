#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(htmltools)
library(leaflet)
library(sf)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel(
    fluidRow(column(2, shiny::img(height = 80, src = 'RNOW.jpg')),
             column(4,
                    div(style = 'height:40px; font-size: 30px;',
                        'San Joaquin County Warehouses - Prototype')),
             column(2, shiny::img(height = 80, src = 'RNOW.jpg')))
  ),

  tabsetPanel(
    tabPanel('Dashboard',
             # Display slider bar selections, checkbox, and summary text
             fluidRow(
               column(width = 10, align = 'center', leafletOutput("map", height = 700))
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

palJuris2 <- colorFactor(palette = 'Paired', 
                           domain = whZ$ZNDESCRIPT
                         )
palTracy <- colorFactor(palette = 'Set2',
                        domain = tracy_parcels$ZONING_COD)
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addProviderTiles(provider = providers$CartoDB.Positron,
                       group = 'Basemap') %>% 
      addProviderTiles(provider = providers$Esri.WorldImagery,
                       group = 'Imagery') %>% 
      addLayersControl(baseGroups = c('BaseMap', 'Imagery'),
                       overlayGroups = c('SJV County', 'Tracy'),
                       options = layersControlOptions(collapsed = FALSE)) %>% 
      addPolygons(data = whZ,
                  label = ~htmlEscape(paste(APN, ZNDESCRIPT)),
                  weight = 1, 
                  fillOpacity = 0.4,
                  color= ~palJuris2(ZNDESCRIPT),
                  group = 'SJV County') %>% 
      addLegend(data = whZ,
                title = 'SJV County Zoning',
                pal = palJuris2,
                values = ~ZNDESCRIPT,
                group = 'SJV County') %>% 
      addPolygons(data = tracy_parcels,
                  label = ~htmlEscape(paste(APN, ZONING_COD)),
                  weight = 1, 
                  fillOpacity = 0.4,
                  color = ~palTracy(ZONING_COD),
                  group = 'Tracy')  %>% 
      addLegend(data = tracy_parcels,
                title = 'Tracy Zoning',
                pal = palTracy,
                values = ~ZONING_COD,
                group = 'Tracy') #%>% 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
