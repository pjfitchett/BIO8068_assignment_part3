# Shiny reptiles app

# Packages and data import

library(shiny)
library(leaflet)
library(leafem)
library(mapview)
options("rgdal_show_exportToProj4_warnings"="none")
library(sf)
library(raster)
library(ggplot2)
library(dplyr)

# Add reptile images ----
adder_image <- base64enc::dataURI(file="www/adder_image.jpeg", mime="image/jpeg")
common_image <- base64enc::dataURI(file="www/common_lizard_image.jpeg", mime="image/jpeg")
grass_image <- base64enc::dataURI(file="www/grass_snake_image.jpeg", mime="image/jpeg")
slow_image <- base64enc::dataURI(file="www/slow_worm_image.jpeg", mime="image/jpeg")

# Add map features ----
elevation <- raster("www/spatial/elevation.tif")
ll_crs <- CRS("+init=epsg:4326")
elevation_ll <- projectRaster(elevation, crs = ll_crs)
elevation_500 <- aggregate(elevation, fact=10)
elevation_500_ll <- projectRaster(elevation_500, crs = ll_crs)

lakes <- st_read("www/spatial/cumbria_lakes.shp")
lakes <- lakes %>% 
  st_set_crs(27700) %>% 
  st_transform(27700)
lakes_ll <- st_transform(lakes, 4326)

rivers <- st_read("www/spatial/cumbria_rivers.shp")
rivers <- rivers %>% 
  st_set_crs(27700) %>% 
  st_transform(27700)
rivers_ll <- st_transform(rivers, 4326) 

roads <- st_read("www/spatial/cumbria_roads.shp")
roads <- roads %>% 
  st_set_crs(27700) %>% 
  st_transform(27700)
roads_ll <- st_transform(roads, 4326) 

settlements <- st_read("www/spatial/cumbria_settlements.shp")
settlements <- settlements %>% 
  st_set_crs(27700) %>% 
  st_transform(27700)
settlements_ll <- st_transform(settlements, 4326)



# Define UI ----

ui <- fluidPage(
  
  titlePanel(
    h1("Reptiles in Cumbria")),
  
  sidebarLayout(
    sidebarPanel(
      p("The maps on this app show Cumbria and have a few different layers",
        "to help you explore the area.  You can choose to include different ",
        "features including elevation, roads, freshwater such as",
        "rivers and lakes, or urban areas.")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Overview",
                 leafletOutput(outputId = "cumbria_map"),
                 h2("Cumbria"),
                 p("Cumbria is located in the North West of England and is",
                   "home to the Lake District National Park.  This picturesque",
                   "area is a popular holiday destination for over", strong("15 million"),
                   "visitors every year.  As well as being popular with holiday",
                   "makers, Cumbria is home to over", strong("12,000"),
                   "different species of plants and animals.  The list of",
                   "species that reside within the area includes a number", 
                   "of reptiles."),
                 h2("Reptiles"),
                 p("Although generally associated with hotter climates, there are",
                 "six native species of reptiles in the UK.  They can be found in",
                 "many different locations around the country.  Reptiles can be",
                 "difficult to spot since they are very good at staying hidden.  ",
                 "Four of the six species have been sighted in Cumbria and reported",
                 "to the National Biodiveristy Network (NBN), which is a citizen",
                 "science database for people to share sightings of different species."),
                 p("You can explore the four species of reptiles found in Cumbria",
                   "in more detail using the tabs at the top of the page.  ")
                 ),
        tabPanel("Adder",
                 h2("Adder"),
                 p("The adder can be seen in cumbria"),
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
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
) # fluidPage

# Define server logic ----

server <- function(input, output, session) {
  # Code for tabs
  output$panel <- renderText({
    paste("Current panel: ", input$tabset)
  })
  
  # Output for the cumbria map
  output$cumbria_map <- renderLeaflet({
    leaflet() %>% 
      addTiles(group = "OSM (default)") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
      # stroke = TRUE, weight = 1 gives an outline and fills in shapes with a lighter colour
      addFeatures(lakes_ll, group = "Lakes",
                  stroke = TRUE, weight = 1) %>% 
      addFeatures(rivers_ll, group = "Rivers",
                  stroke = TRUE, weight = 1) %>%
      addFeatures(roads_ll, group = "Roads",
                  stroke = TRUE, weight = 1, color = "red") %>%
      addFeatures(settlements_ll, group = "Urban areas",
                  stroke = TRUE, weight = 1, color = "black") %>%
      addRasterImage(elevation_500_ll, col = terrain.colors(25), 
                     opacity = 0.6, group = "Elevation") %>%
      # Hide groups so they don't automatically show
      hideGroup(c("Lakes", "Rivers", "Roads", "Urban areas", "Elevation")) %>%
      # Allow the user to choose which layers they want to see
      addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"), 
        overlayGroups = c("Lakes", "Rivers", "Roads", "Urban areas", "Elevation"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

# Run the app ----

shinyApp(ui = ui, server = server)

