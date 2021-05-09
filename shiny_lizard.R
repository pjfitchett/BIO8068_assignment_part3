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

reptiles <- tibble::tribble(
  ~species, ~ id, 
  "Adder", "adder_image",
  "Common Lizard", "common_lizard_image",
  "Grass Snake", "grass_snake_image", 
  "Slow Worm", "slow_worm_image"
)


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

# Add interactive maps ----

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
      p("The maps on this website show Cumbria and have a few different layers",
        "to help you explore the area.  You can choose to include different ",
        "features including roads, rivers, lakes, or urban areas.  Elevation can",
        "also be displayed, areas of high elevation are shown in red, and low",
        "elevation is shown in green."),
      # Checkbox
      checkboxGroupInput(inputId = "my_checkgroup", 
                         h3("Select all the species you have seen in the UK"), 
                         choices = list("Adder" = 1, 
                                        "Common Lizard" = 2, 
                                        "Grass Snake" = 3,
                                        "Sand Lizard" = 4,
                                        "Smooth Snake" = 5,
                                        "Slow Worm" = 6),
                         selected = 1),
      p("The drop down menu below contains a list of reptiles found in Cumbria.",
        "You can select one of these options to display an image.  You can then",
        "use this while navigating the website to make comparisons between the different",
        "species of reptiles found in the area."),
      # Drop down menu to show a photo 
      selectInput("id", "Pick a species to display", choices = setNames(reptiles$id, reptiles$species)),
      imageOutput("photo")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Overview",
                 leafletOutput(outputId = "interactive"),
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
                 p("Also known as the viviparous lizard, the common lizard is",
                   "most likely found in grassland,heathland and moorland areas.",
                   "This lizard is one of the rare few reptiles that incubates eggs",
                   "inside its body, producing live young."),
                 fluidRow(
                   column(
                     width = 6, plotOutput(outputId = "common_plot",  width="100%")),
                   column(
                     width = 6, img(src=common_image, width="100%")
                   )), # fluidRow
                 p("The graph above shows the number of reported common lizards",
                   "over the years and shows how this has changed.  By clicking on",
                   "the markers on the map you can see when each individual was",
                   "reported.  Compare the records to different features to see",
                   "where common lizards are most likely seen in Cumbria."),
                 leafletOutput(outputId = "common_map"),
        ), # tabPanel(Common lizard)
        
        # Grass Snake panel
        tabPanel("Grass Snake",
                 h2("Grass Snake"),
                 p("The grass snake is the longest snake you will find in the UK.",
                   "It is completely harmless and can live for up to 25 years.",
                   "The best time to try spot a grass snake is during the summer",
                   "near water, where they can be seen swimming or basking in the sun.",
                   "They live on a diet of fish, small mammals, birds and amphibians."),
                 fluidRow(
                   column(
                     width = 6, plotOutput(outputId = "grass_plot",  width="100%")),
                   column(
                     width = 6, img(src=grass_image, width="100%")
                   )), # fluidRow
                 p("The graph above shows the number of grass snakes reported over",
                   "the years.  If you click on the markers on the map, you will be",
                   "able to match the records with the year reported.  However, as",
                   "you can see, a lot of the records do not have a year recorded",
                   "so we need to be careful when interpreting the graph above since",
                   "there are some very obvious data gaps.  If you add the rivers and",
                   "lakes features to the map you will be able to see whether the",
                   "majority of grass snakes are spotted near water as suggested."),
                 leafletOutput(outputId = "grass_map"),
        ), # tabPanel(Grass snake)
        
        # Slow worm panel
        tabPanel("Slow Worm",
                 h2("Slow Worm"),
                 p("Although a lot of people assume the slow worm is a small snake, it",
                   "is actually a legless lizard.  Like a lot of lizards, the slow",
                   "worm can shed its tail.  The slow worm can often be seen basking",
                   "in the sun in heathland or grassland.  This lizard also incubates",
                   "eggs inside the body, producing live young."
                 ),
                 fluidRow(
                   column(
                     width = 6, plotOutput(outputId = "slow_plot",  width="100%")),
                   column(
                     width = 6, img(src=slow_image, width="100%")
                   )), # fluidRow
                 p("The graph above shows the number of slow worms reported over",
                   "the years.  The markers on the map show the locations of reported",
                   "slow worms and the years they were observed.  If you select",
                   " the elevation layer and zoom in, you can see that the slow worm",
                   "has never been spotted in areas of high elevation."),
                 leafletOutput(outputId = "slow_map"),
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
  
  # Output for the drop down menu in sidepanel
  output$photo <- renderImage({
    list(
      src = file.path("www/", paste0(input$id, ".jpeg")),
      contentType = "image/jpeg",
      width = "75%"
    )
  }, deleteFile = FALSE)
  
  # Output for the cumbria map
  output$interactive <- renderLeaflet({
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
                       popup = adder_year) %>%
      addLegend(colors = "red", opacity=1, labels="Common Lizard")
  }) # renderLeaflet
  
  # Output for grass_plot
  output$grass_plot <- renderPlot(
    ggplot(records_per_yr_grass, aes(x = year.processed, y=count_per_year)) +
      geom_line() +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Year") + ylab("Number of records")
  )
  
  # Output for grass_map
  output$grass_map <- renderLeaflet({
    interactive %>%
      addCircleMarkers(grass$decimalLongitude.processed, grass$decimalLatitude.processed,  
                       radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red",
                       popup = adder_year) %>%
      addLegend(colors = "red", opacity=1, labels="Grass Snake")
  }) # renderLeaflet
  
  # Output for slow_plot
  output$slow_plot <- renderPlot(
    ggplot(records_per_yr_slow, aes(x = year.processed, y=count_per_year)) +
      geom_line() +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Year") + ylab("Number of records")
  )
  
  # Output for slow_map
  output$slow_map <- renderLeaflet({
    interactive %>%
      addCircleMarkers(slow$decimalLongitude.processed, slow$decimalLatitude.processed,  
                       radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red",
                       popup = adder_year) %>%
      addLegend(colors = "red", opacity=1, labels="Slow Worm")
  }) # renderLeaflet
  
} # server

# Run the app ----

shinyApp(ui = ui, server = server)

