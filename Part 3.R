# Spatial data

library(leaflet)
library(leafem)
library(mapview)
options("rgdal_show_exportToProj4_warnings"="none")
library(sf)
library(raster)

# Import elevation and shp files ####

# Import elevation data
elevation <- raster("spatial/elevation.tif")
plot(elevation)

# Import lakes data
lakes <- st_read("spatial/cumbria_lakes.shp")
print(lakes)
# Check coordinate system
st_crs(lakes)
# Reset to OS 27700
lakes <- lakes %>% 
  st_set_crs(27700) %>% 
  st_transform(27700)
# Transform to latitude longitude
lakes_ll <- st_transform(lakes, 4326) # Lat-Lon

# Import rivers data
rivers <- st_read("spatial/cumbria_rivers.shp")
print(rivers)
# Check coordinate system
st_crs(rivers)
# Reset to OS 27700
rivers <- rivers %>% 
  st_set_crs(27700) %>% 
  st_transform(27700)
# Transform to latitude longitude
rivers_ll <- st_transform(rivers, 4326) # Lat-Lon

# Import roads data
roads <- st_read("spatial/cumbria_roads.shp")
print(roads)
# Check coordinate system
st_crs(roads)
# Reset to OS 27700
roads <- roads %>% 
  st_set_crs(27700) %>% 
  st_transform(27700)
# Transform to latitude longitude
roads_ll <- st_transform(roads, 4326) # Lat-Lon

# Import settlements data
settlements <- st_read("spatial/cumbria_settlements.shp")
print(settlements)
# Check coordinate system
st_crs(settlements)
# Reset to OS 27700
settlements <- settlements %>% 
  st_set_crs(27700) %>% 
  st_transform(27700)
# Transform to latitude longitude
settlements_ll <- st_transform(settlements, 4326) # Lat-Lon

# Working with elevation ####

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
plot(elevation)
plot(elevation_contours, add = TRUE)

# Plot elevation and overlay features - eg roads, lakes, rivers
plot(elevation)
plot(lakes, add = TRUE)
plot(rivers, add = TRUE)
plot(roads, add = TRUE)
plot(settlements, add = TRUE)

# Make the map interactive - optional layers
leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addFeatures(lakes_ll, group = "Lakes") %>% 
  addFeatures(rivers_ll, group = "Rivers") %>%
  addFeatures(roads_ll, group = "Roads") %>%
  addFeatures(settlements_ll, group = "Settlements") %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Lakes", "Rivers", "Roads", "Settlements"),
    options = layersControlOptions(collapsed = FALSE)
  )


