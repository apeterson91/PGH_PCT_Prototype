
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
    mutate(cycles = rbeta(n = n(), shape1 =  26 , shape2 = 1000 ))

labels <-  levels(cut(hoods$cycles,
               breaks = quantile(hoods$cycles,probs = c(0,0.25,0.5,0.75,1)),
               include.lowest = TRUE))

purposes <- c("Commuting" = "commute",
              "Grocery Run" = "groceries",
              "Social" = "social")

geographies <- c(
    "Neighborhoods" = "neighborhoods",
    "Streets" ="streets"
    )

showHoodPopup <- function(id,lat,lng){
    cycle <- hoods %>% filter(hood == {{id}}) %>% pull(cycles)
    cycle <- round(100*cycle,2)
    cycle_str <-str_c("% Cycling: ",cycle)
    content <- as.character(tagList(
        tags$h4(id),
        tags$strong(cycle_str)
    ))
    leafletProxy("map") %>% addPopups(lat = lat, lng = lng,
                                      content,layerId = id)
}
zcolourscale <-  "RdYlBu"

get_colour_palette <- function(colourscale, bins = 10){
  # Manually modify to be 'standard 10 plus one extra' for 11 levels
  if (colourscale == "RdYlBu" && bins == 11) {
    local_palette <- RColorBrewer::brewer.pal(n = 10, name = colourscale)
    extra_colour <- "#2d004b"
    local_palette <- append(local_palette, extra_colour)
  } else {
    local_palette <- RColorBrewer::brewer.pal(n = bins, name = colourscale)
  }
  # Replace #e0f3f8 with #c6dbef for colourbrewer "RdYlBu"
  if (colourscale == "RdYlBu") {
    local_palette <- gsub(pattern = "#E0F3F8", replacement = "#C6DBEF", x = local_palette)
  }
  local_palette
}

ui <- navbarPage(
    title = "PGH MultiModal Propensity Map",
    id = "nav",
    tabPanel(
        "Map",
        useShinyjs(),
        div(
            class = "outer",
            tags$head(includeCSS("styles.css")),
            leafletOutput("map",width = "100%",height = "100%"),
            absolutePanel(
                id = "controls",
                class = "panel panel-default",
                fixed = TRUE,
                top = 60, left = "auto",
                right = 20, bottom = "auto",
                width = 330, height = "auto",
                #style = "opacity: 0.9 z-index: 1; position: absolute",
                tags$div(title = "Show/Hide Panel",
                         a(
                             id = "toggle_panel",
                             style = "font-size: 80%",
                             span(class = "glyphicon glyphicon-circle-arrow-up",
                                  "Hide")
                         )),
                tags$div(title = "Trip purpose",
                         selectInput("purpose", "Trip purpose:", purposes,
                                     selectize = FALSE)),
                tags$div(title = "Geography",
                         selectInput("geography","Geography:", geographies,
                                     selectize =  FALSE))
            ),
            )
    ),
    tabPanel("Region Stats"),
    tabPanel("National Data"),
    tabPanel("About",includeMarkdown("Design.Rmd"))
)

server <- function(input, output) {

    input_purpose <- reactive({
        if(is.null(input$purpose)) {
            "commute"
        } else {
            input$purpose
        }
    })
    ## TODO: change popup on click to popup on hover and only contain hood name
    ## TODO: Add zoom-to-neighborhood on click
    ## TODO: add in street level geometry
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
                            layerId = ~hood,
                            options = pathOptions(clickable = T),
                            labelOptions = labelOptions(direction = 'auto'),
                            highlightOptions = highlightOptions(color = "white",
                                                                weight = 2,
                                                                 bringToFront = TRUE),
                            fillColor = ~colorQuantile("RdYlBu",cycles)(cycles)
                            )
        )
    )

    observe({
        input_purpose()

        switch(input_purpose(),
               "commute" = { legend_title <- "% cycling to work"},
               "school" = { legend_title <-  "% cycling to school"},
               "alltrips" = {legend_title <- "% trips cycled"}
               )
        leafletProxy("map") %>%
            addLegend("topleft",
                      colors = get_colour_palette(zcolourscale,4),
                      labels = labels,
                      layerId = "zone_leg",
                      title = legend_title,
                      opacity = 0.5)
    })

    observe({
        leafletProxy("map") %>% clearPopups()
        event <- input$map_shape_click
        if(is.null(event))
            return()
        isolate({
            showHoodPopup(event$id,event$lat,event$lng)
        })
    })

}

# Run the application
shinyApp(ui = ui, server = server)
