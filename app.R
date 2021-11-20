
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)


hood_pop <- read_csv("~/Documents/CityData/Burgh/hood_population.csv")

hoods <- read_sf("~/Documents/CityData/Burgh/Neighborhoods/Neighborhoods_.shp") %>%
    select(objectid,hood) %>%
    st_transform(4326) %>%
    left_join(hood_pop) %>%
    ## prob taken from https://bikeleague.org/sites/default/files/LAB_Where_We_Ride_2016.pdf
    mutate(cycles = rbinom(n = n(),size = pop, prob = 0.26))




ui <- navbarPage(
    title = "PGH MultiModal Propensity Map",
    id = "nav",
    tabPanel(
        "Map",
        div(
            class = "outer",
            tags$head(includeCSS("styles.css")),
            leafletOutput("map",width = "100%",height = "100%"),
            absolutePanel(
                id="controls",
                class = "panel panel-default",
                fixed = TRUE,
                right = 20,
                width = 220,
                height = "auto",
                style = "opacity: 0.9; z-index: 1; positoin: absolute",
                tags$div(title = "Show/Hide Panel",
                         a(
                             id = "toggle_panel",
                             style = "font-size: 80%",
                             span(class = "glyphicon glyphicon-circle-arrow-up",
                                  "Hide")
                         )),
                         )
            )
    ),
    tabPanel("Region Stats"),
    tabPanel("National Data"),
    tabPanel("Manual"),
    tabPanel("About")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ## TODO: Add popup for cycle statistic
    output$map <- renderLeaflet(
        isolate(
            leaflet(hoods) %>%
                addProviderTiles(providers$Stamen.TonerLite,
                                 options = providerTileOptions(noWrap = TRUE)) %>%
                addPolygons(color = "#444444",
                            weight = 1,
                            smoothFactor = 0.5,
                            opacity = 1.0,
                            fillOpacity = 0.5,
                            options = pathOptions(clickable = T),
                            labelOptions = labelOptions(direction = 'auto'),
                            highlightOptions = highlightOptions(color = "white",
                                                                weight = 2,
                                                                 bringToFront = TRUE),
                            fillColor = ~colorQuantile("YlOrRd",cycles)(cycles)
                            )
        )
    )
}

# Run the application
shinyApp(ui = ui, server = server)
