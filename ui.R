library(shiny)
library(leaflet)

fluidPage(
  leafletOutput("mymap"),
  p(),
  sliderInput(inputId="year",
              label="Choose a year",
              value=2017,
              min=2003,
              max=2017,
              sep="")
)