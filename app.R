
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)

hoods <- read_sf("~/Documents/CityData/Burgh/Neighborhoods/Neighborhoods_.shp") %>%
    select(objectid) %>% st_transform(4326)

# Define UI for application that draws a histogram
ui <- fillPage(

    leafletOutput("mymap",height = "100%"),
    p()

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mymap <- renderLeaflet({
        leaflet(hoods) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addPolygons(highlightOptions = highlightOptions(color = "white",
                                                            weight = 2,
                                                             bringToFront = TRUE))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
