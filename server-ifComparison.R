server <- function(input, output, session) {
  
  # Read the shapefile data
  shape_data <- sf::read_sf("data/SuburbMelb/suburb.shp")
  shape_data <- st_transform(shape_data, crs = 4326)
  # Read address file
  address_data <- read.csv('data/addressNcode/street-addresses.csv')
  
  #update suburb name choices
  updateSelectInput(session, "suburbName", choices = sort(unique(shape_data$Location)))
  
  observeEvent(input$suburbName,{

    suburb_name <- input$suburbName
    address_input <- input$suburbInput

    getAddList<- reactive({
      
      sub_info_add <- subset(address_data, suburb == suburb_name)
    })
    
    address_list = getAddList()
    updateSelectInput(session, "suburbInput", choices = c('',sort(unique(address_list$address))))
  })
  
  #filter data by entered text
  entered_text <- reactive({
    
    suburb_name <- input$suburbName
    address_input <- input$suburbInput
    print(address_input)
  
    #filter address data by address to get longitude, latitude and suburb name
    filterSubbyAdress<- reactive({
      subset(address_data, address == address_input)
    })
    
    sub_filtered_byAdd = filterSubbyAdress()

    #if zip code, return NULL and surburb name
    if (address_input == "") {

      list(
        "long" = NULL,
        "lat" = NULL,
        "name" = suburb_name,
        "address"=NULL)

    } else {
      
      #if address, return corresponding longitude, latitude and suburb name
      list(
        "long" = sub_filtered_byAdd$longitude,
        "lat" = sub_filtered_byAdd$latitude,
        "name" = sub_filtered_byAdd$suburb, 
        "address" = address_input)
    }
  })
  
  # Initialize and display a global map centered on Melbourne
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 144.9377, lat = -37.8136, zoom = 13)  # Center on Melbourne
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
    
    #get filtered suburb info by search button
    sub_info <- entered_text()
    
    #get longitude, latitude and suburb name
    address_lng <- sub_info$long #NULL if no address is selected
    address_lat <- sub_info$lat #NULL if no address is selected
    suburb_name <- sub_info$name
    address <- sub_info$address #NULL if no address is selected

    #TODO: REMOVE, if NOT for debug
    print(address_lng) 
    print(address_lat) 
    print(suburb_name)
    print(address)
    
    
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
    
    # Show the location of the address
    if (!is.null(address)&&!is.null(address_lat) && !is.null(address_lng)) {
      leafletProxy("mymap") %>%
        addMarkers(lng = address_lng, lat = address_lat, 
                   popup = address)  # TODO: change it to real address name
    }
    })
  
  # dynamically add suburb name to list for comparison
  comparison_list <- reactiveVal(character(0))
  
  #observe event when click add-on btn
  observeEvent(input$addListBtn,{
    
    sub_info <- entered_text()
    suburb_name <- sub_info$name
    first_sub <- comparison_list()[1]
    
    #check list length and action
    if ((length(comparison_list()) == 0) ){
      
      comparison_list(c(comparison_list(), suburb_name))
      shinyalert( title = "Successfully Added", timer = 1000)
      
    }else if ((length(comparison_list()) == 1) && (first_sub != suburb_name)){
      
      #if list length equals to 1, and name of 2nd suburb is different from 1st one
      comparison_list(c(comparison_list(), suburb_name))
      shinyalert( title = "Successfully Added", timer = 1000)
      
    }else if ((length(comparison_list()) == 1) && (first_sub == suburb_name)){
      
      # alert for duplicate comparison
      shinyalert("Duplicated Suburbs Comparison", type = "error")
    }else{
      
      #alert for more than 2 suburbs
      shinyalert("Comparisons Only Between TWO", type = "error")
    }
    
    })
  
  # render first component
  output$outputText1 <- renderUI({
    
    HTML(paste("You are searching for suburb:", comparison_list()[1]))
  })
  
  #render second component
  output$outputText2 <- renderUI({
    
    HTML(paste("You are searching for suburb:", comparison_list()[2]))
  })
}
