library(shiny)
library(sparklyr)
library(tidyverse)
library(leaflet)
library(htmltools)
library(shinyBS)
library(data.table)

# ======== Data input
spark <- spark_connect(master="local")
all <- spark_read_csv(spark, name="flight", path="all.csv", header=T)
coords <- read_csv("usairports.csv")
hubs <- c("PHX","LAX","SAN","SFO","DEN","MIA","FLL","MCO","TPA","ATL","MDW","ORD","IND","CVG","SDF","BWI","BOS","DTW","MSP","MCI","STL","LAW","EWR","TTN","JFK","LGA","CLT","GSO","CLE","LUK","DAY","PDX","PHL","PIT","CAE","MEM","BNA","DFW","DAL","IAH","SLC","IAD","DCA","SEA")

setwd("D:\\Downloads\\faredata")
fares <- fread('fares.csv')
airport_codes <- read.csv('AIRPORT_code.csv', header=T, stringsAsFactors = T)
filtered <- fares[DOLLAR_CRED == 1 & ONLINE == 1 & ROUNDTRIP == 1]

fares_clean <- filtered %>%
  group_by(YEAR, ORIGIN_AIRPORT_ID, REPORTING_CARRIER) %>%
  summarize(ITIN_YIELD_AVG = mean(ITIN_YIELD), ITIN_YIELD_COUNT = n()) %>%
  ungroup() %>%
  mutate(ORIGIN_AIRPORT_ID = as.factor(ORIGIN_AIRPORT_ID)) %>%
  inner_join(airport_codes, by=c("ORIGIN_AIRPORT_ID" = "Code.1")) %>%
  select(YEAR, Code, REPORTING_CARRIER, ITIN_YIELD_AVG, ITIN_YIELD_COUNT, Description) %>%
  mutate(Code = as.character(Code))

# ======== Filter and transform
coords <- coords %>%
  filter(locationID %in% hubs)

clean <- all %>%
  filter(ORIGIN %in% hubs) %>%
  group_by(YEAR, ORIGIN, OP_UNIQUE_CARRIER, ORIGIN_CITY_NAME) %>%
  summarize(count = n()) # %>%

clean_local <- clean %>%
  ungroup() %>%
  collect() %>%
  mutate(OP_ORIG_CARRIER = OP_UNIQUE_CARRIER) %>%
  mutate(OP_UNIQUE_CARRIER = recode(OP_UNIQUE_CARRIER,"CO"="AA","EV"="OO","FL"="WN","HP"="AA","MQ"="AA","US"="AA","NW"="DL","XE"="OO","TZ"="WN","OH"="AA","9E"="DL","VX"="AS")) %>%
  group_by(YEAR, ORIGIN, OP_UNIQUE_CARRIER, ORIGIN_CITY_NAME) %>%
  summarize(count=sum(count), OP_ORIG_CARRIER=max(OP_ORIG_CARRIER)) %>%
  ungroup() %>%
  group_by(YEAR, ORIGIN) %>%
  top_n(6, count) %>%
  mutate(isMax = count==max(count)) %>%
  arrange(YEAR, ORIGIN, desc(count)) %>%
  left_join(coords, by=c("ORIGIN"="locationID"), copy=T)

# Fix Longitude
clean_local <- clean_local %>%
  mutate(Longitude = -1 * abs(Longitude))

# Add fares
clean_local <- clean_local %>%
  inner_join(fares_clean, by=c("ORIGIN" = "Code",
                               "YEAR" = "YEAR",
                               "OP_UNIQUE_CARRIER" = "REPORTING_CARRIER"))


# Normalize Count
max_size <- 25
min_size <- 5
clean_local$size <- min_size + (max_size - min_size) * (clean_local$count - min(clean_local$count[clean_local$isMax])) / (max(clean_local$count[clean_local$isMax]) - min(clean_local$count[clean_local$isMax]))

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
clean_local <- clean_local %>%
  left_join(data.frame(OP_UNIQUE_CARRIER=airlines, name=airline.names))
palette <- colorFactor(airline.colors, levels=airline.names)
names(airline.colors) <- airline.names


# ======== Generate Shiny visualization

ui <- fluidPage(
  titlePanel(
    h2("Domestic Flight Dominance at Hubs (2003-2017)",
       style="text-align:center")
  ),
  div(
    leafletOutput("mymap"),
    style="padding:4rem"
  ),
  div(
      id="sliderContainer",
      sliderInput(inputId="year",
                  label="Choose a year",
                  value=2017,
                  min=2003,
                  max=2017,
                  sep="",
                  width="100%",
                  animate=T)
  ),
  p("Black border = the dominating airline of that year eventually merged into the specified airline"),
  p("Size = number of outgoing flights from airport for given carrier")
)

server <- function(input, output, session) {
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

shinyApp(ui, server)
#save.image(file="Final.RData")
#write_delim(clean_local, "clean_local.csv", delim=",")
