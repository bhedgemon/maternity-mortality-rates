library(shiny)      
library(dplyr)      
library(DT)        
library(leaflet) 
library(sf)        
library(htmltools)  


states <- read_sf("/home/iduarte@ad.wlu.edu/maternity-mortality-rates/us-states.geojson")
server <- function(input, output, session) {
  updateSelectInput(session,
                    "race_filter",
                    choices = c("All", unique(breast_cancer_long$race)))  
  # REACTIVE DATA - This recalculates automatically when race_filter changes
  breast_cancer_filtered_race <- reactive({
    result <- breast_cancer_long  
    
    if (input$race_filter != "All") {
      result <- result %>%
        filter(race == input$race_filter)
    }
    result
  })
  
  # REACTIVE MAP DATA - Aggregate data before joining
  map_data <- reactive({
    # First, aggregate the filtered data by state
    aggregated <- breast_cancer_filtered_race() %>%
      group_by(state) %>%
      summarise(breast_cancer = mean(breast_cancer, na.rm = TRUE), .groups = 'drop')
    
    # Then join with states
    states %>%
      left_join(aggregated, by = c("name" = "state"))
  })
  
  # RENDER THE MAP
  output$cancer_map <- renderLeaflet({
    data <- map_data()
    
    # Make sure breast_cancer_rate is numeric, not a list
    if (is.list(data$breast_cancer_rate)) {
      data$breast_cancer <- as.numeric(unlist(data$breast_cancer_rate))
    }
    
    bins <- c(0, 30, 60, 90, 120, 150, 180, Inf)
    pal <- colorBin("RdPu", domain = data$breast_cancer, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%.1f breast cancer rate",
      data$name, data$breast_cancer
    ) %>% lapply(HTML)
    
    leaflet(data) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(breast_cancer),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~breast_cancer, opacity = 0.7,
                title = "Breast Cancer Rate",
                position = "bottomright")
  })
}


## this is the ui 

navbarMenu("Cancer",
           tabPanel("Breast Cancer",
                    div(style = "background-color: #FFEEF2; min-height: 100vh; padding: 20px;",
                        titlePanel("Breast Cancer Rates by State and Race"),
                        
                        # Add intro text
                        fluidRow(
                          column(12,
                                 p("Explore breast cancer incidence rates across the United States. 
                                     Use the dropdown to filter by race/ethnicity and see how rates 
                                     vary geographically.")
                          )
                        ),
                        
                        # Sidebar with filter
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("race_filter", 
                                        "Select Race/Ethnicity:",
                                        choices = c("All"),
                                        selected = "All"),
                            
                            br(),
                            
                            h4("Map Instructions"),
                            p("• Hover over states to see exact rates"),
                            p("• The map updates when you change the race filter"),
                            p("• Darker colors indicate higher breast cancer rates")
                          ),
                          
                          mainPanel(
                            # The map
                            leafletOutput("cancer_map", height = "500px"),
                            
                            br(),
                            
                          )
                        )
                    )
           ),
           
           tabPanel("Cervical Cancer",
                    div(style = "background-color: #def7ec; min-height: 100vh; padding: 20px;",
                        titlePanel("Cervical Cancer Rates"),
                        mainPanel("statement- intro to topic of discussion"),
                        #leafletOutput()
                    )
           )
),