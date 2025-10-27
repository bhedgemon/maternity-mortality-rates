<<<<<<< HEAD
library(shiny)      
library(dplyr)      
library(DT)        
library(leaflet) 
library(sf)        
library(htmltools)  
=======
library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(htmltools)
library(sf)

server <- function(input, output, session) {














>>>>>>> f5c28d85617c81aab6c4b8db3dfd80ce8c3d27f6


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

