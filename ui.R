
# Load the global environment
source("global.R")

ui <- fluidPage(
  
  useShinyjs(),  # Include shinyjs as a dependency
  
  # Custom CSS for layout and height adjustments
  tags$style(HTML("
    body, html {
        overflow: hidden;
    }
    .row.mt-4 {
        height: calc(100vh - 60px);
    }
    .col-lg-3 {
        height: 100%;
        padding: 0 15px;
    }
    .card.h-100, .col-lg-6, .card-body {
        height: calc(100vh - 60px);
    }
    
    .fullWidthTabsContainer ul.nav.nav-tabs > li {
    width: 25%; 
    float: left;
    display: block;
    }
    
    #mymap {
        height: calc(100% - 40px) !important;
    }
    #searchDashboard {
        height: 30% !important;
    }
    #basicInfo {
        height: 70% !important;
    }
    #searchButton{
        float:right
    }
  ")),
  
  # Header
  tags$div(class="container-fluid",
           tags$div(class="row",
                    tags$div(class="col-12 bg-primary text-white text-center py-3",
                             h1("House Buyer's Dashboard")
                    )
           )),
  
  tabsetPanel(
    tabPanel(
      
      title='Details',
      
      # Main Content
      tags$div(class="row mt-4",
               
               # Left Part: Search Dashboard & Basic Information
               tags$div(class="col-lg-3",
                        
                        # Search Dashboard
                        tags$div(id="searchDashboard", class="card mb-3", 
                                 # tags$div(class="card-header bg-info text-white", "Search Dashboard"),
                                 tags$div(class="card-body", 
                                          
                                          # p("Search controls here..."),
                                          selectInput("suburbName", "Surburb* ", choices = ""),
                                          selectInput("suburbInput", "Address (optional):", choices = ""),
                                          actionButton("addListBtn", "Add Surburb to Comparison"),
                                          actionButton("searchButton", "Search"),
                                 )
                        ),
                        
                        # Basic Information
                        tags$div(id="basicInfo", class="card", 
                                 tags$div(class="card-header bg-info text-white", "Basic Information"),
                                 tags$div(class="card-body", p("Basic info about the house..."))
                        )
               ),
               
               # Middle Part: Map
               tags$div(class="col-lg-6",
                        tags$div(class="card h-100",
                                 tags$div(class="card-body",
                                          leafletOutput("mymap")
                                 )
                        )
               ),
               
               # Right Part: Additional Information
               tags$div(class="col-lg-3",
                        tags$div(class="card h-100",
                                 #tags$div(class="card-header bg-info text-white", "Additional Information"),
                                 div(class="card-body", 
                                     
                                     # Wrapper div for centering button group
                                     div(style="text-align: center;",  
                                         # Button group (as inline-block)
                                         div(class = "fullWidthTabsContainer",
                                             
                                             ## Tab panels
                                             tabsetPanel(id = "contentTabs",
                                                         
                                                         
                                                         tabPanel(title = "School", value = "btnSchool", 
                                                                  selectInput("subThemeSelector", "Select School Type:", choices = NULL),
                                                                  dataTableOutput("schoolOutput")
                                                         ),
                                                         tabPanel(title = "Transport", value = "btnTranport", 
                                                                  uiOutput("transportOutput")),
                                                         
                                                         
                                                         tabPanel(title = "Crime", value = "btnCrime", 
                                                                   uiOutput("crimeOutput")),
                                                         
                                                         
                                                         tabPanel(title = "Facilities", value = "btnFacilities",
                                                                  uiOutput("facilitiesOutput")),
                                                         
                                             )
                                         )
                                         
                                         
                                     ),
                                     

                                     
                                     
                                     
                                     #hr(), # Horizontal line for separation
                                     
                                     
                                    
                                     
                                     
                                 )
                        )
               )
      )
    ),
    tabPanel(
      title = "Key Information Comparison",
      
      tags$div(class="row mt-4",
               # Search Dashboard
               tags$div(id="comparison1", 
                        uiOutput("outputText1")
               ),
               
               
               # Right Part: Additional Information
               tags$div(id = 'comparison2',
                        uiOutput("outputText2")
               )
      )
      
    )
    
  )
  
)
