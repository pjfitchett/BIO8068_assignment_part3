# Spatial data

library(leaflet)
library(leafem)
library(mapview)
options("rgdal_show_exportToProj4_warnings"="none")
library(sf)
library(raster)
library(ggplot2)
library(dplyr)

# Import data

# Elevation ####
elevation <- raster("www/spatial/elevation.tif")
plot(elevation)

# Change colours so green = low, brown = high
plot(elevation, col = terrain.colors(30)) # 30 categories

# Convert to latlong - code is 4326
ll_crs <- CRS("+init=epsg:4326")
elevation_ll <- projectRaster(elevation, crs = ll_crs)
mapview(elevation_ll) # interactive

# Hillshade map
hs = hillShade(slope = terrain(elevation, "slope"), 
               aspect = terrain(elevation, "aspect"))
plot(hs, col = gray(0:100 / 100), legend = FALSE)
# overlay with DEM
plot(elevation, col = terrain.colors(25), alpha = 0.5, add = TRUE)

# Creating contours from the raster DTM
elevation_contours <- rasterToContour(elevation) %>% st_as_sf()
plot(elevation, col = terrain.colors(25))
plot(elevation_contours, add = TRUE)
# Convert to latlong
elevation_contours_ll <- st_transform(elevation_contours, 4326)

# Lakes ####
lakes <- st_read("www/spatial/cumbria_lakes.shp")
print(lakes)
# Check coordinate system
st_crs(lakes)
# Reset to OS 27700
lakes <- lakes %>% 
  st_set_crs(27700) %>% 
  st_transform(27700)
# Transform to latitude longitude
lakes_ll <- st_transform(lakes, 4326) # Lat-Lon

# Rivers ####
rivers <- st_read("www/spatial/cumbria_rivers.shp")
print(rivers)
# Check coordinate system
st_crs(rivers)
# Reset to OS 27700
rivers <- rivers %>% 
  st_set_crs(27700) %>% 
  st_transform(27700)
# Transform to latitude longitude
rivers_ll <- st_transform(rivers, 4326) # Lat-Lon

# Roads ####
roads <- st_read("www/spatial/cumbria_roads.shp")
print(roads)
# Check coordinate system
st_crs(roads)
# Reset to OS 27700
roads <- roads %>% 
  st_set_crs(27700) %>% 
  st_transform(27700)
# Transform to latitude longitude
roads_ll <- st_transform(roads, 4326) # Lat-Lon

# Settlements ####
settlements <- st_read("www/spatial/cumbria_settlements.shp")
print(settlements)
# Check coordinate system
st_crs(settlements)
# Reset to OS 27700
settlements <- settlements %>% 
  st_set_crs(27700) %>% 
  st_transform(27700)
# Transform to latitude longitude
settlements_ll <- st_transform(settlements, 4326) # Lat-Lon

# Maps with multiple features ####

# Plot elevation and overlay features - eg roads, lakes, rivers
plot(elevation)
plot(lakes, add = TRUE)
plot(rivers, add = TRUE)
plot(roads, add = TRUE)
plot(settlements, add = TRUE)

# Make the map interactive with optional layers
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
  addRasterImage(elevation_ll, col = terrain.colors(25), 
                 opacity = 0.6, group = "Elevation") %>%
  # Hide groups so they don't automatically show
  hideGroup(c("Lakes", "Rivers", "Roads", "Urban areas", "Elevation")) %>%
  # Allow the user to choose which layers they want to see
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Lakes", "Rivers", "Roads", "Urban areas", "Elevation"),
    options = layersControlOptions(collapsed = FALSE)
  )





# Citizen science data from NBN ####

# Common lizard ####
common <- read.csv("www/common_lizard_cumbria/common_lizard_cumbria.csv")
common <- common[common$identificationVerificationStatus.processed == "Accepted",]
# Records per year for UK
records_per_yr_common <- common %>%
  group_by(year.processed) %>%
  summarise(count_per_year = n())
ggplot(records_per_yr_common, aes(x = year.processed, y=count_per_year)) +
  geom_line() +
  ggtitle("Common Lizard") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("Number of records")

# Create a variable containing year to use as a popup on a map
common_year <- paste("Year: ", common$year.processed)
# Create a map showing records of common lizard
common_map <- addProviderTiles(leaflet(),"Esri.WorldImagery") %>%
  addCircleMarkers(common$decimalLongitude.processed, common$decimalLatitude.processed,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red",
                   popup = common_year) %>%
  addLegend(colors = "red", opacity=1, labels="Common Lizard")
common_map


# Adder ####
adder <- read.csv("www/adder_cumbria/adder_cumbria.csv")
adder <- adder[adder$identificationVerificationStatus.processed == "Accepted",]
# Records per year for UK
records_per_yr_adder <- adder %>%
  group_by(year.processed) %>%
  summarise(count_per_year = n())
ggplot(records_per_yr_adder, aes(x = year.processed, y=count_per_year)) +
  geom_line() +
  ggtitle("Adder") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("Number of records")

# Create a variable containing year to use as a popup on a map
adder_year <- paste("Year: ", adder$year.processed)
# Create a map showing records of adder
adder_map <- addProviderTiles(leaflet(),"Esri.WorldImagery") %>%
  addCircleMarkers(adder$decimalLongitude.processed, adder$decimalLatitude.processed,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red") %>%
  addLegend(colors = "red", opacity=1, labels="Adder")
adder_map


# Slow worm ####
slow <- read.csv("www/slow_worm_cumbria/slow_worm_cumbria.csv")
slow <- slow[slow$identificationVerificationStatus.processed == "Accepted",]
# Records per year for UK
records_per_yr_slow <- slow %>%
  group_by(year.processed) %>%
  summarise(count_per_year = n())
ggplot(records_per_yr_slow, aes(x = year.processed, y=count_per_year)) +
  geom_line() +
  ggtitle("Slow Worm") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("Number of records")

# Create a variable containing year to use as a popup on a map
slow_year <- paste("Year: ", slow$year.processed)
# Create a map showing records of slow worm
slow_map <- addProviderTiles(leaflet(),"Esri.WorldImagery") %>%
  addCircleMarkers(slow$decimalLongitude.processed, slow$decimalLatitude.processed,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red",
                   popup = slow_year) %>%
  addLegend(colors = "red", opacity=1, labels="Slow Worm")
slow_map


# Grass snake ####
grass <- read.csv("www/grass_snake_cumbria/grass_snake_cumbria.csv")
grass <- grass[grass$identificationVerificationStatus.processed == "Accepted",]
# Records per year for UK
records_per_yr_grass <- grass %>%
  group_by(year.processed) %>%
  summarise(count_per_year = n())
ggplot(records_per_yr_grass, aes(x = year.processed, y=count_per_year)) +
  geom_line() +
  ggtitle("Grass Snake") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("Number of records")

# Create a variable containing year to use as a popup on a map
grass_year <- paste("Year: ", grass$year.processed)
# Create a map showing records of grass snake
grass_map <- addProviderTiles(leaflet(),"Esri.WorldImagery") %>%
  addCircleMarkers(grass$decimalLongitude.processed, grass$decimalLatitude.processed,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red") %>%
  addLegend(colors = "red", opacity=1, labels="Grass Snake") 
grass_map


# Interactive maps that also show the species distribution ####

# Common lizard
common_map <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addCircleMarkers(common$decimalLongitude.processed, common$decimalLatitude.processed,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red",
                   popup = common_year) %>%
  addLegend(colors = "red", opacity=1, labels="Common Lizard") %>%
  # stroke = TRUE, weight = 1 gives an outline and fills in shapes with a lighter colour
  addFeatures(lakes_ll, group = "Lakes",
              stroke = TRUE, weight = 1) %>% 
  addFeatures(rivers_ll, group = "Rivers",
              stroke = TRUE, weight = 1) %>%
  addFeatures(roads_ll, group = "Roads",
              stroke = TRUE, weight = 1, color = "red") %>%
  addFeatures(settlements_ll, group = "Urban areas",
              stroke = TRUE, weight = 1, color = "black") %>%
  addRasterImage(elevation_ll, col = terrain.colors(25), 
                 opacity = 0.6, group = "Elevation") %>%
  # Hide groups so they don't automatically show
  hideGroup(c("Lakes", "Rivers", "Roads", "Urban areas", "Elevation")) %>%
  # Allow the user to choose which layers they want to see
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Lakes", "Rivers", "Roads", "Urban areas", "Elevation"),
    options = layersControlOptions(collapsed = FALSE)
  )
common_map

# Adder
adder_map <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addCircleMarkers(adder$decimalLongitude.processed, adder$decimalLatitude.processed,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red",
                   popup = adder_year) %>%
  addLegend(colors = "red", opacity=1, labels="Adder") %>%
  # stroke = TRUE, weight = 1 gives an outline and fills in shapes with a lighter colour
  addFeatures(lakes_ll, group = "Lakes",
              stroke = TRUE, weight = 1) %>% 
  addFeatures(rivers_ll, group = "Rivers",
              stroke = TRUE, weight = 1) %>%
  addFeatures(roads_ll, group = "Roads",
              stroke = TRUE, weight = 1, color = "red") %>%
  addFeatures(settlements_ll, group = "Urban areas",
              stroke = TRUE, weight = 1, color = "black") %>%
  addRasterImage(elevation_ll, col = terrain.colors(25), 
                 opacity = 0.6, group = "Elevation") %>%
  # Hide groups so they don't automatically show
  hideGroup(c("Lakes", "Rivers", "Roads", "Urban areas", "Elevation")) %>%
  # Allow the user to choose which layers they want to see
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Lakes", "Rivers", "Roads", "Urban areas", "Elevation"),
    options = layersControlOptions(collapsed = FALSE)
  )
adder_map

# Slow worm
slow_map <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addCircleMarkers(slow$decimalLongitude.processed, slow$decimalLatitude.processed,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red",
                   popup = slow_year) %>%
  addLegend(colors = "red", opacity=1, labels="Slow Worm") %>%
  # stroke = TRUE, weight = 1 gives an outline and fills in shapes with a lighter colour
  addFeatures(lakes_ll, group = "Lakes",
              stroke = TRUE, weight = 1) %>% 
  addFeatures(rivers_ll, group = "Rivers",
              stroke = TRUE, weight = 1) %>%
  addFeatures(roads_ll, group = "Roads",
              stroke = TRUE, weight = 1, color = "red") %>%
  addFeatures(settlements_ll, group = "Urban areas",
              stroke = TRUE, weight = 1, color = "black") %>%
  addRasterImage(elevation_ll, col = terrain.colors(25), 
                 opacity = 0.6, group = "Elevation") %>%
  # Hide groups so they don't automatically show
  hideGroup(c("Lakes", "Rivers", "Roads", "Urban areas", "Elevation")) %>%
  # Allow the user to choose which layers they want to see
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Lakes", "Rivers", "Roads", "Urban areas", "Elevation"),
    options = layersControlOptions(collapsed = FALSE)
  )
slow_map

# Grass snake
grass_map <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addCircleMarkers(grass$decimalLongitude.processed, grass$decimalLatitude.processed,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red",
                   popup = grass_year) %>%
  addLegend(colors = "red", opacity=1, labels="Grass Snake") %>%
  # stroke = TRUE, weight = 1 gives an outline and fills in shapes with a lighter colour
  addFeatures(lakes_ll, group = "Lakes",
              stroke = TRUE, weight = 1) %>% 
  addFeatures(rivers_ll, group = "Rivers",
              stroke = TRUE, weight = 1) %>%
  addFeatures(roads_ll, group = "Roads",
              stroke = TRUE, weight = 1, color = "red") %>%
  addFeatures(settlements_ll, group = "Urban areas",
              stroke = TRUE, weight = 1, color = "black") %>%
  addRasterImage(elevation_ll, col = terrain.colors(25), 
                 opacity = 0.6, group = "Elevation") %>%
  # Hide groups so they don't automatically show
  hideGroup(c("Lakes", "Rivers", "Roads", "Urban areas", "Elevation")) %>%
  # Allow the user to choose which layers they want to see
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Lakes", "Rivers", "Roads", "Urban areas", "Elevation"),
    options = layersControlOptions(collapsed = FALSE)
  )
grass_map




