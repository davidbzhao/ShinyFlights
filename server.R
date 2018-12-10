library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)
library(shinyBS)

clean_local <- read.csv("clean_local.csv", stringsAsFactors = F)

# Define palette
airlines <- levels(factor(clean_local$OP_UNIQUE_CARRIER[clean_local$isMax]))
airline.names <- c("American", "Alaska", "JetBlue", "Independence", "Delta", "Frontier", "SkyWest", "United", "Southwest", "Mesa")
airline.colors <- c(
  "#e6194b",  # red
  "#f58231",  # orange
  "#ffe119",  # yellow
  "#bfef45",  # lime
  "#3cb44b",  # green
  "#42d4f4",  # cyan
  "#4363d8",  # blue
  "#911eb4",  # purple
  "#f032e6",  # magenta
  "#9A6324"  # brown
)
palette <- colorFactor(airline.colors, levels=airline.names)
names(airline.colors) <- airline.names


# ======== Generate Shiny visualization


function(input, output, session) {
  clean_local_this_year <- eventReactive(input$year, {
    clean_local %>% 
      filter(YEAR == input$year) %>%
      filter(isMax) %>%
      ungroup()
  }, ignoreNULL = FALSE)
  createLabel <- function(description, itin_yield_avg) {
    paste(description," ($", sprintf("%.2f", itin_yield_avg), "/mi)", sep="")
  }
  output$mymap <- renderLeaflet({
    leaflet(clean_local) %>%
      addProviderTiles(providers$Wikimedia,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude)) %>%
      addLegend("bottomright", pal = palette, values = airline.names)
  })
  observe({
    leafletProxy("mymap", data = clean_local_this_year()) %>%
      clearMarkers() %>%
      addCircleMarkers(layerId=~ORIGIN,
                       lng=~Longitude,
                       lat=~Latitude,
                       radius=~size,
                       fillColor=~palette(name),
                       color="black",
                       label=~createLabel(Description, ITIN_YIELD_AVG),
                       labelOptions=labelOptions(opacity=0.9),
                       fillOpacity=0.5,
                       opacity=~ifelse(OP_ORIG_CARRIER == OP_UNIQUE_CARRIER, 0, 1),
                       weight=2)
  })
  
  observeEvent(input$mymap_marker_click, {
    p <- input$mymap_marker_click
    print(airline.colors)
    output$airport_plot <- renderPlot({
      ggplot(clean_local %>%
               filter(YEAR == input$year) %>%
               filter(ORIGIN == p$id) %>%
               mutate(perc = count/sum(count), 0), 
             aes(reorder(name, -perc), perc)) + 
        geom_bar(aes(fill=name), stat="identity") +
        geom_label(aes(label=paste("$", sprintf("%.2f", ITIN_YIELD_AVG), sep=""))) +
        theme_bw() +
        labs(x="Airline Code", y="Outbound Flight Percentage") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(name="name", values=airline.colors)
    })
    # print(p$id)
    showModal(
      modalDialog(
        title = paste(input$year, " Airline Dominance in ", p$id),
        plotOutput("airport_plot"),
        easyClose=T,
        size="l"
      )
    )
  })
}