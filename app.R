
library(shiny)
library(leaflet)
library(tidyverse)
library(shinyjs)
library(sf)


#hood_pop <- read_csv("~/Documents/CityData/Burgh/hood_population.csv")

hoods <- read_sf("~/Documents/CityData/Burgh/Neighborhoods/Neighborhoods_.shp") %>%
    select(objectid,hood) %>%
    st_transform(4326) %>%
#    left_join(hood_pop) %>%
    ## prob taken from https://bikeleague.org/sites/default/files/LAB_Where_We_Ride_2016.pdf
    mutate(cycles = rbeta(n = n(), shape1 =  26 , shape2 = 1000 ))

stdf <- read_sf("~/Documents/CityData/Burgh/alleghenycounty_streetcenterlines202107/AlleghenyCounty_StreetCenterlines202107.shp") %>%
  st_transform(st_crs(hoods)) %>%
  st_filter(hoods) %>%
  st_join(hoods %>% select(hood)) %>%
  select(OBJECTID,FULL_NAME,hood,geometry)

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
    title = "Pittsburgh Transit Propensity Tool",
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
                         a(##TODO: Fix this button
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
    tabPanel("About",includeMarkdown("About.Rmd")),
    tabPanel("Region Stats"),
    tabPanel("Trip Entry",
             useShinyjs(),
             div(
               class = "outer",
               tags$head(includeCSS("styles.css")),
               leafletOutput("tripmap",width = "100%",height = "100%"))
    )
)

server <- function(input, output) {

    input_purpose <- reactive({
        if(is.null(input$purpose)) {
            "commute"
        } else {
            input$purpose
        }
    })
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
               "groceries" = { legend_title <-  "% cycling to groceries"},
               "social" = {legend_title <- "% social trips cycled"}
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
        leafletProxy("map") %>%
          clearPopups()
        event <- input$map_shape_click
        if(is.null(event))
            return()
        isolate({
            if(!is.null(event$id)){
              leafletProxy("map") %>%
                clearGroup("streets")
            }
          hd <- event$id
           local_lines <- stdf %>% filter(hood == {{hd}})
            leafletProxy("map") %>%
            flyTo(lng = event$lng,
                  lat = event$lat,
                  zoom = 15) %>%
            addPolylines(data = local_lines,
                         group = "streets",
                         color='red')
            showHoodPopup(event$id,event$lat,event$lng)
        })
    })

    ## TODO(petersonadam): https://shiny.rstudio.com/articles/overview.html
    ## for persistent data storage -- see about getting db storgae from GCP?
    output$tripmap <- renderLeaflet(
      isolate(
        leaflet(stdf) %>%
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)) %>%
          addPolylines(color = "#444444",
                      weight = 1,
                      smoothFactor = 0.5,
                      opacity = 0.80,
                      layerId = ~OBJECTID,
                      options = pathOptions(clickable = T),
                      labelOptions = labelOptions(direction = 'auto'),
                      highlightOptions = highlightOptions(color = "white",
                                                          weight = 2,
                                                          bringToFront = TRUE)
          )
      )
    )

}

# Run the application
shinyApp(ui = ui, server = server)
