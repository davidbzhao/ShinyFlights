library(shiny)
library(leaflet)
library(htmltools)
library(shinyBS)

fluidPage(
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
