

server <- function(input, output, session) {
  rv <- reactiveValues(selected_suburb = NULL, church_data = NULL, schools_data = NULL, health_data = NULL, transport_data = NULL)
  
  
  # Read data
  # Read the suburb data
  suburb_polygon <- sf::read_sf("data/suburbMelb/suburb_central.shp")
  suburb_polygon <- st_transform(suburb_polygon, crs = 4326)

  # Read address file
  address_data <- read.csv('data/addressNcode/street-addresses.csv')
  # Read the facilities file
  facilities <- sf::read_sf("data/facilities/facilities_with_suburb.shp")
  
  #update suburb name choices
  updateSelectInput(session, "suburbName", choices = sort(unique(suburb_polygon$Location)))
  
  observeEvent(input$suburbName,{
    
    suburb_name <- input$suburbName
    address_input <- input$suburbInput
    
    getAddList<- reactive({
      sub_info_add <- subset(address_data, suburb == suburb_name)
    })
    
    address_list = getAddList()
    updateSelectInput(session, "suburbInput", choices = c('',sort(unique(address_list$address))))
    update_map()
  })
  
  #filter data by entered text
  entered_text <- reactive({
    
    suburb_name <- input$suburbName
    address_input <- input$suburbInput
    print(cat("address input",address_input))
    
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
  
  
  
  ## Handle click on myMap
  observe({
    click <- input$mymap_click
    if (is.null(click)) return(NULL)
    
    
    # 使用 st_point 和 st_intersects 来确定用户点击的区域
    point <- st_point(c(click$lng, click$lat))
    intersected <- st_intersects(suburb_polygon, point)
    selected_index <- which(sapply(intersected, length) > 0)
    
    
    print(selected_index)
   
    if (length(selected_index) > 0) {
      selected_suburb_name <- as.character(suburb_polygon$Name[selected_index])
      
      # 更新下拉菜单的值
      updateSelectInput(session, "suburbName", selected = selected_suburb_name)
    }
    
  })
  
  
  ## Handle click on a polygon shape
  observe({
    shape_click <- input$mymap_shape_click
    if (is.null(shape_click)) return(NULL)
    
    clicked_position <- st_point(c(shape_click$lng, shape_click$lat))
    intersected_suburbs <- st_intersects(suburb_polygon, clicked_position)
    selected_index <- which(sapply(intersected_suburbs, length) > 0)
    
    if(length(selected_index) > 0){
      selected_suburb_name <- suburb_polygon$Name[selected_index]
      print(paste("Clicked on:", selected_suburb_name))
      
      updateSelectInput(session, "suburbName", selected = selected_suburb_name)
      #update_map()
    }
  })
  
  
  
  
  ## update map
  update_map <-function(){
    
    #get filtered suburb info by search button
    sub_info <- entered_text()
    
    #get longitude, latitude and suburb name
    address_lng <- sub_info$long #NULL if no address is selected
    address_lat <- sub_info$lat #NULL if no address is selected
    suburb_name <- sub_info$name
    address <- sub_info$address #NULL if no address is selected
    
    
    #TODO: REMOVE, if NOT for debug
    print(cat("lng",address_lng)) 
    print(address_lat) 
    print(suburb_name)
    print(address)
    
    
    
    
    if (!is.null(suburb_name) && suburb_name != "") {
      # Filter the shapefile data based on the input suburb name
      selected_suburb <- subset(suburb_polygon, Name == suburb_name)
      print(suburb_name)
      
      
      # Fetch data for the entered suburb
      rv$church_data <- filter_facilities(facilities,suburb_name,"Place of Worship")
      rv$schools_data <- filter_facilities(facilities,suburb_name,"Education Centre")
      rv$health_data <- filter_facilities(facilities,suburb_name,"Health Services")
      rv$transport_data <- filter_facilities(facilities,suburb_name,"Transport")
      
      
      print("=====here get the faciliti data=====")
      print(rv$church_data)
      print(rv$schools_data)
      print(rv$health_data)
      print(rv$transport_data)
      print(suburb_polygon$c_long[suburb_polygon$Name == suburb_name])
      print(suburb_polygon$c_lat[suburb_polygon$Name == suburb_name])
      print("=====end=====")
      
      leafletProxy("mymap") %>%
        clearShapes() %>% clearMarkers() %>% 


        setView(lng = suburb_polygon$c_long[suburb_polygon$Name == suburb_name], lat = suburb_polygon$c_lat[suburb_polygon$Name == suburb_name], zoom = 14)  %>%
        
        # Dim other regions with a gray fill color, and add popup with suburb name
        addPolygons(data = subset(suburb_polygon, Name != suburb_name),
                    fillColor = "gray", 
                    weight = 1, color = "white", fillOpacity = 0.4, 
                    popup = ~Name,
                    layerId = ~Name
        ) %>% 
        
        # Highlight the selected suburb with no fill color and blue boundary
        addPolygons(data = selected_suburb, fillColor = "transparent", 
                    weight = 4, color = "blue", fillOpacity = 1) -> mymap
      
      # Conditionally add church markers if data exists
      if (nrow(rv$church_data) > 0) {
        mymap <- mymap %>%
          addMarkers(data = rv$church_data, 
                     icon = church_icon,
                     label = ~name, group = "Churchs")
      }
      # Conditionally add school markers if data exists
      if (nrow(rv$schools_data) > 0) {
        mymap <- mymap %>%
          addMarkers(data = rv$schools_data, 
                     icon = school_icon,
                     label = ~name, group = "Schools")
      }
      # Conditionally add health service markers if data exists
      if (nrow(rv$health_data) > 0) {
        mymap <- mymap %>%
          addMarkers(data = rv$health_data, 
                     icon = health_icon,
                     label = ~name, group = "Health Services")
      }
      # Conditionally add transport markers if data exists
      if (nrow(rv$transport_data) > 0) {
        mymap <- mymap %>%
          addMarkers(data = rv$transport_data, 
                     icon = transport_icon,
                     label = ~name, group = "Transport")
      }
      # Finally, add the layers control
      mymap %>%
        addLayersControl(
          overlayGroups = c("Churchs", "Schools", "Health Services", "Transport"),
          options = layersControlOptions(collapsed = FALSE)
        )
    }
    
    # Show the location of the address
    if (!is.null(address)&&!is.null(address_lat) && !is.null(address_lng)) {
      leafletProxy("mymap") %>%
        addMarkers(lng = address_lng, lat = address_lat, 
                   popup = address)  # TODO: change it to real address name
    }
    
  }
  
  
  
  # Observe changes in suburb input
  observeEvent(input$searchButton, {
    
    update_map()
    
    
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
  
  

  

  
  
  observe({
    updateTabsetPanel(session, "contentTabs", selected = input$activeTab)
  })
  
 
  
 
  

 
  
  ## RIGHT PANELS:schools info
  observe({
   
    req(input$contentTabs)
    
    if (input$contentTabs == "btnSchool"){
     
      proxy <- leafletProxy("mymap")
      
      # Hide other layer
      proxy %>% hideGroup("Churchs")
      proxy %>% hideGroup("Health Services")
      proxy %>% hideGroup("Transport")
      proxy %>% clearGroup("SelectedSchool")
      
      #Display school layer
      proxy %>% showGroup("Schools")
      
      
      if (!"sub_theme" %in% names(rv$schools_data) || nrow(rv$schools_data) == 0) {
        # Handle the case when the required column doesn't exist or the data frame is empty
        shiny::showNotification("No school data available.", type = "error")
        return()
      }
      # Initialize the sub_theme choices
      sub_theme_choices <- unique(rv$schools_data$sub_theme)
      updateSelectInput(session, "subThemeSelector", choices = sub_theme_choices)
      
      # Create a reactive expression to observe changes in subThemeSelector and return filtered data
      filtered_data <- reactive({
        selected_sub_theme <- input$subThemeSelector
        df <- as.data.frame(rv$schools_data)
        df_filtered <- df %>% filter(sub_theme == selected_sub_theme)
        return(df_filtered[["name"]])  # Return only the "name" column
      })
      
      # Render the data table based on the selected sub_theme
      output$schoolOutput <- DT::renderDataTable({
        datatable(data.frame(name = filtered_data()), selection = "single", options = list(
          searching = FALSE,   
          info = FALSE,        
          lengthChange = FALSE,
          paging = FALSE,
          columnDefs = list(list(targets = 1, title = ""))
        ))
      })
      
      observeEvent(input$schoolOutput_rows_selected, {
        #clear previous marker
        leafletProxy("mymap") %>% clearGroup("SelectedSchool")
        
        selected_row <- input$schoolOutput_rows_selected
        if (length(selected_row) > 0) {
          selected_school <- filtered_data()[selected_row]
          
          # Update Map to highlight choosen school
          school_data_selected <- rv$schools_data[rv$schools_data$name == selected_school, ]
          print(school_data_selected)
          
          iconWidth = school_icon$iconWidth
          
          if (nrow(school_data_selected) > 0) {
            
            enlarged_icon <- createEnlargedIcon(school_icon, 2)
            
            leafletProxy("mymap") %>%
              
              addMarkers(
                data = school_data_selected, 
                icon = enlarged_icon,
                label = ~name,
                group = "SelectedSchool",
                layerId = 'large'
              ) 
            
            js_code <- "
            
            setTimeout(function() {
              
              var images = document.getElementsByClassName('leaflet-marker-icon leaflet-zoom-animated leaflet-interactive');

              for (var i = 0; i < images.length; i++) {
              
                var image = images[i];
              
                if (image.height !== 20) {
                image.parentNode.removeChild(image);}
              }
              
            }, 300);"
            
            # Execute the JavaScript code using shinyjs
            runjs(js_code)
            
          }
          
          
          

        }
      })
      
      
      
      
    }
 
  })
  
  

  
  
  ## RIGHT PANELS:
  createEnlargedIcon <- function(original_icon_data, scale_factor) {
    makeIcon(
      iconUrl = original_icon_data$iconUrl,
      iconWidth = original_icon_data$iconWidth * scale_factor,
      iconHeight = original_icon_data$iconHeight * scale_factor
    )
  }
  
  
  
  ## RIGHT PANELS: Tranport info
  observe({
    
    req(input$contentTabs)
    
    if (input$contentTabs == "btnTranport") {
      
      proxy <- leafletProxy("mymap")
      
      # Hide other layer
      proxy %>% hideGroup("Churchs")
      proxy %>% hideGroup("Health Services")
      proxy %>% hideGroup("Schools")
      proxy %>% clearGroup("SelectedSchool")
      
      #Display school layer
      proxy %>% showGroup("Transport")
      
      
      output$transportOutput <- renderUI({
        p("Message form server: Details of Transport")
      })
      
    }
  })
 
  
  ## RIGHT PANELS:  crime info
  
  observe({
    
    req(input$contentTabs)
    
    if (input$contentTabs == "btnCrime"){
      
      proxy <- leafletProxy("mymap")
      
      # Hide other layer
      proxy %>% hideGroup("Churchs")
      proxy %>% hideGroup("Health Services")
      proxy %>% hideGroup("Schools")
      proxy %>% hideGroup("Transport")
      proxy %>% clearGroup("SelectedSchool")
      
      #Display school layer
      #proxy %>% showGroup("Crime")
      
      
      output$crimeOutput <- renderUI({
        p("Message form server: Details of crime")
      })
    }
  
  })
  
  ## RIGHT PANELS:  Facilities info
  observe({
    
    req(input$contentTabs)
    
    if (input$contentTabs == "btnFacilities") {
      
      proxy <- leafletProxy("mymap")
      
      # Hide other layer
      
      proxy %>% hideGroup("Health Services")
      proxy %>% hideGroup("Schools")
      proxy %>% hideGroup("Transport")
      proxy %>% clearGroup("SelectedSchool")
      
      #Display school layer
      proxy %>% showGroup("Churchs")
      
      output$facilitiesOutput <- renderUI({
        p("Message form server: Details of facilities")
        
        # suburb_name <- sub_info$name
        # print(suburb_name)
        #print(rv$church_data)
      })
      
    }
  })
  
  
}
