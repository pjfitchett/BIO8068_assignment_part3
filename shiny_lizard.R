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

# Add reptile data ----

# Adder 
adder <- read.csv("www/adder_cumbria/adder_cumbria.csv")
adder <- adder[adder$identificationVerificationStatus.processed == "Accepted",]
# Records per year for UK
records_per_yr_adder <- adder %>%
  group_by(year.processed) %>%
  summarise(count_per_year = n())
# Create a variable containing year to use as a popup on a map
adder_year <- paste("Year: ", adder$year.processed)

# Common lizard 
common <- read.csv("www/common_lizard_cumbria/common_lizard_cumbria.csv")
common <- common[common$identificationVerificationStatus.processed == "Accepted",]
# Records per year for UK
records_per_yr_common <- common %>%
  group_by(year.processed) %>%
  summarise(count_per_year = n())
# Create a variable containing year to use as a popup on a map
common_year <- paste("Year: ", common$year.processed)

# Grass snake 
grass <- read.csv("www/grass_snake_cumbria/grass_snake_cumbria.csv")
grass <- grass[grass$identificationVerificationStatus.processed == "Accepted",]
# Records per year for UK
records_per_yr_grass <- grass %>%
  group_by(year.processed) %>%
  summarise(count_per_year = n())
# Create a variable containing year to use as a popup on a map
grass_year <- paste("Year: ", grass$year.processed)

# Slow worm 
slow <- read.csv("www/slow_worm_cumbria/slow_worm_cumbria.csv")
slow <- slow[slow$identificationVerificationStatus.processed == "Accepted",]
# Records per year for UK
records_per_yr_slow <- slow %>%
  group_by(year.processed) %>%
  summarise(count_per_year = n())
# Create a variable containing year to use as a popup on a map
slow_year <- paste("Year: ", slow$year.processed)

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

# Interactive map - used a lot 
interactive <- leaflet() %>% 
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


# Define UI ----

ui <- fluidPage(
  
  titlePanel(
    h1("Reptiles in Cumbria")),
  
  sidebarLayout(
    sidebarPanel(
      p("The maps on this app show Cumbria and have a few different layers",
        "to help you explore the area.  You can choose to include different ",
        "features including roads, rivers, lakes, or urban areas.  Elevation can",
        "also be displayed, areas of high elevation are shown in red, and low",
        "elevation is shown in green.")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Overview",
                 leafletOutput(outputId = "cumbria_map"),
                 h2("Cumbria"),
                 # Info on Cumbria
                 p("Cumbria is located in the North West of England and is",
                   "home to the Lake District National Park.  This picturesque",
                   "area is a popular holiday destination for over", strong("15 million"),
                   "visitors every year.  As well as being popular with holiday",
                   "makers, Cumbria is home to over", strong("12,000"),
                   "different species of plants and animals.  The list of",
                   "species that reside within the area includes a number", 
                   "of reptiles."),
                 h2("Reptiles"),
                 # Info on reptiles
                 p("Although generally associated with hotter climates, there are",
                 "six native species of reptiles in the UK.  These are: the adder",
                 "(" ,em("Vipera berus"), "), common lizard (", em("Zootoca vivipara"),
                 "), grass snake (", em("Natrix helvetica"), "), sand lizard (",
                 em("Lacerta agilis"), "), smooth snake (", em("Coronella austriaca"),
                 ") and slow worm (", em("Anguis fragilis"), ").  They can be found",
                 "in many different locations around the country.  Reptiles can be",
                 "difficult to see since they are very good at staying hidden.  ",
                 "The National Biodiversity Network (NBN) has records of four ",
                 "species of reptiles being spotted in Cumbria."),
                 p("You can explore the four species of reptiles found in Cumbria",
                   "in more detail using the tabs at the top of the page.")
                 ),
        
        # Adder panel
        tabPanel("Adder",
                 h2("Adder"),
                 p("The adder is a small snake that primarily resides in woodland,",
                   "heathland and moorland areas.  It lives on a diet of small",
                   "mammals, lizards and ground nesting birds.  The adder has a",
                   "distinctive zigzag pattern along its back.  It is best to be",
                   "cautious around adders since they are the UK's only venemous",
                   "snake.  A bite from one of these snakes is very rare and ",
                   "almost never fatal."),
                 fluidRow(
                   column(
                     width = 6, plotOutput(outputId = "adder_plot",  width="100%")),
                   column(
                     width = 6, img(src=adder_image, width="100%")
                   )), # fluidRow
                 p("The graph above shows how the number of reported adders in",
                   "Cumbria have changed over the years.  The locations of these",
                   "records can be seen on the map below.  If you click on one",
                   "of the markers, you will see the year this adder was recorded.",
                   "Don't forget you can also add different layers to the map to",
                   "see how close adders have been seen to different features, such",
                   "as water or different towns."),
                 leafletOutput(outputId = "adder_map"),
                 ), # tabPanel(Adder)
        
        # Common lizard panel
        tabPanel("Common Lizard",
                 h2("Common Lizard"),
                 p("The common lizard is most likely found in grassland,",
                   "heathland and moorland areas.  It is also known as the",
                   "viviparous lizard.  This lizard is one of the rare few reptiles",
                   "that incubates eggs inside its body, producing live young."),
                 fluidRow(
                   column(
                     width = 6, plotOutput(outputId = "common_plot",  width="100%")),
                   column(
                     width = 6, img(src=common_image, width="100%")
                   )), # fluidRow
                 p("The graph above shows the number of reported common lizards",
                   "over the years and shows how this has changed.  By clicking on",
                   "the markers on the map you can see when each individual was",
                   "reported.  You can try to match up the points on the map with",
                   "the peak shown on the graph."),
                 leafletOutput(outputId = "common_map"),
                 ), # tabPanel(Common lizard)

        # Grass Snake panel
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
    interactive
  }) # renderLeaflet
  
  # Output for adder_plot
  output$adder_plot <- renderPlot(
    ggplot(records_per_yr_adder, aes(x = year.processed, y=count_per_year)) +
      geom_line() +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Year") + ylab("Number of records")
  ) # renderPlot
  
  # Output for adder_map
  output$adder_map <- renderLeaflet({
    interactive %>%
      addCircleMarkers(adder$decimalLongitude.processed, adder$decimalLatitude.processed,  
                       radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red",
                       popup = adder_year) %>%
      addLegend(colors = "red", opacity=1, labels="Adder")
  }) # renderLeaflet
  
  # Output for common_plot
  output$common_plot <- renderPlot(
    ggplot(records_per_yr_common, aes(x = year.processed, y=count_per_year)) +
      geom_line() +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Year") + ylab("Number of records")
  )
    
  # Output for common_map
  output$common_map <- renderLeaflet({
    interactive %>%
      addCircleMarkers(common$decimalLongitude.processed, common$decimalLatitude.processed,  
                       radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red",
                       popup = common_year) %>%
      addLegend(colors = "red", opacity=1, labels="Common Lizard")
  }) # renderLeaflet
    
    
  
} # server

# Run the app ----

shinyApp(ui = ui, server = server)

