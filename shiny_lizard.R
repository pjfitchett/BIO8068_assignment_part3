# Shiny reptiles app

library(shiny)
library(leaflet)
library(leafem)
library(mapview)
options("rgdal_show_exportToProj4_warnings"="none")
library(sf)
library(raster)
library(ggplot2)
library(dplyr)

# Import all relevant data ----

# Add reptile images
adder_image <- base64enc::dataURI(file="www/adder_image.jpeg", mime="image/jpeg")
common_image <- base64enc::dataURI(file="www/common_lizard_image.jpeg", mime="image/jpeg")
grass_image <- base64enc::dataURI(file="www/grass_snake_image.jpeg", mime="image/jpeg")
slow_image <- base64enc::dataURI(file="www/slow_worm_image.jpeg", mime="image/jpeg")

# Define UI ----

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textOutput("panel")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Overview" 
                 ),
        tabPanel("Adder",
                 img(src=adder_image, height="50%", width="50%", align="right")
                 ),
        tabPanel("Common Lizard",
                 img(src=common_image, height="50%", width="50%", align="right")
                 ),
        tabPanel("Grass Snake",
                 img(src=grass_image, height="50%", width="50%", align="right")
                 ),
        tabPanel("Slow Worm",
                 img(src=slow_image, height="50%", width="50%", align="right")
                 )
      )
    )
  )
)

# Define server logic ----

server <- function(input, output, session) {
  output$panel <- renderText({
    paste("Current panel: ", input$tabset)
  })
}

# Run the app ----

shinyApp(ui = ui, server = server)
