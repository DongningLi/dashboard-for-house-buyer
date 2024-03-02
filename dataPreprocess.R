library(sf)
preFacilities<-function(){
  # Read the shapefile data and transform it
  suburb_polygon <- sf::read_sf("data/SuburbMelb/suburb.shp")
  suburb_polygon <- st_transform(suburb_polygon, crs = 4326)
  
  # Read the facilities CSV
  facilities <- read.csv("data/facilities/facilities.csv", header = TRUE, sep = ";")
  
  # Split the "co_ordinates" column into separate latitude and longitude columns
  coordinates <- strsplit(facilities$co_ordinates, ",")
  facilities$latitude <- as.numeric(sapply(coordinates, function(coord) as.numeric(coord[1])))
  facilities$longitude <- as.numeric(sapply(coordinates, function(coord) as.numeric(coord[2])))
  
  # Delete the "co_ordinates" column
  facilities$co_ordinates <- NULL
  
  # Create an sf object with POINT geometries
  facilities_sf <- st_as_sf(facilities, coords = c("longitude", "latitude"), crs = 4326)
  
  # Perform a spatial join to find which suburb each facility is in, only joining the "Name" column
  facilities_with_suburb <- st_join(facilities_sf, suburb_polygon["Name"], join = st_intersects)
  
  # Remove rows with missing values in the "suburb" column
  facilities_with_suburb <- facilities_with_suburb[!is.na(facilities_with_suburb$suburb), ]
  
  # Rename the columns
  colnames(facilities_with_suburb) <- c("theme", "sub_theme", "name", "suburb", "geometry")
  
  # Export facilities_with_suburb to a shapefile
  st_write(facilities_with_suburb, "data/facilities/facilities_with_suburb.shp")
}

preSuburbCentral<-function(){
  # Read the suburb data
  suburb_polygon <- sf::read_sf("data/suburbMelb/suburb.shp")
  suburb_polygon <- st_transform(suburb_polygon, crs = 4326)
  
  # Create data frame with suburbs and their central coordinates
  central_coords <- data.frame(
    suburb = c("West Melbourne", "Kensington", "Flemington", "Southbank", 
               "Melbourne", "North Melbourne", "Parkville", "Carlton North", 
               "Carlton", "East Melbourne", "South Yarra", "Docklands", 
               "Port Melbourne", "South Wharf"),
    c_lat = c(-37.8078, -37.7935, -37.7870, -37.8232, -37.8136, -37.8051, 
            -37.7949, -37.7850, -37.8001, -37.8136, -37.8378, -37.8170, 
            -37.8295, -37.8250),
    c_long = c(144.9423, 144.9295, 144.9196, 144.9645, 144.9631, 144.9326, 
             144.9584, 144.9737, 144.9669, 144.9857, 144.9907, 144.9445, 
             144.9111, 144.9510)
  )
  
  # Join with suburb_polygon based on the suburb names
  suburb_polygon <- left_join(suburb_polygon, central_coords, by = c("Name" = "suburb"))
  sf::st_write(suburb_polygon, "data/suburbMelb/suburb_central.shp")
}
