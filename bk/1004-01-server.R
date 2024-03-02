server <- function(input, output, session) {
  
  # Read the shapefile data
  shape_data <- sf::read_sf("data/SuburbMelb/suburb.shp")
  shape_data <- st_transform(shape_data, crs = 4326)
  
  
  # Initialize and display a global map centered on Melbourne
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 144.9631, lat = -37.8136, zoom = 10)  # Center on Melbourne
  })
  
  # Mock data fetching functions (replace with your actual data fetching logic)
  fetchFacilitiesForSuburb <- function(suburb_name) {
    # Return some mock data
    data.frame(
      latitude = runif(10, -37.8136 - 0.05, -37.8136 + 0.05),
      longitude = runif(10, 144.9631 - 0.05, 144.9631 + 0.05)
    )
  }
  
  fetchSchoolsForSuburb <- function(suburb_name) {
    # Return some mock data
    data.frame(
      latitude = runif(10, -37.8136 - 0.05, -37.8136 + 0.05),
      longitude = runif(10, 144.9631 - 0.05, 144.9631 + 0.05)
    )
  }
  
  # Make Icon for Facilities
  facilityIcon <- makeIcon(
    iconUrl = "images/facility_icon.png",
    iconWidth = 20,
    iconHeight = 20
  )
  
  # Make Icon for Schools
  SchoolIcon <- makeIcon(
    iconUrl = "images/school_icon.png",
    iconWidth = 20,
    iconHeight = 20
  )
  
  # Observe changes in suburb input
  observeEvent(input$searchButton,{
    suburb_name <- input$suburbInput
    
    if (!is.null(suburb_name) && suburb_name != "") {
      # Filter the shapefile data based on the input suburb name
      selected_suburb <- subset(shape_data, Name == suburb_name)
      print(suburb_name)
      
      
      # Fetch data for the entered suburb
      facilities_data <- fetchFacilitiesForSuburb(suburb_name)
      schools_data <- fetchSchoolsForSuburb(suburb_name)
      
      leafletProxy("mymap") %>%
        clearShapes() %>%
        
        # Dim other regions with a gray fill color, and add popup with suburb name
        addPolygons(data = subset(shape_data, Name != suburb_name),
                    fillColor = "gray", 
                    weight = 1, color = "white", fillOpacity = 0.4, 
                    popup = ~Name) %>%  # Assuming 'NAME' is the column with suburb names
        
        # Highlight the selected suburb with no fill color and red boundary
        addPolygons(data = selected_suburb, fillColor = "transparent", 
                    weight = 4, color = "blue", fillOpacity = 1) %>%
        
        # Add facilities and school markers
        addMarkers(data = facilities_data, 
                   lat = ~latitude, lng = ~longitude, 
                   icon = facilityIcon,
                   label = "Facility", group = "Facilities") %>%
        addMarkers(data = schools_data, 
                   lat = ~latitude, lng = ~longitude, 
                   icon = SchoolIcon,
                   label = "School", group = "Schools") %>%
        addLayersControl(
          overlayGroups = c("Facilities", "Schools"),
          options = layersControlOptions(collapsed = FALSE)
        )
      }
    })
}
