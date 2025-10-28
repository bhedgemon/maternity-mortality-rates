library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(htmltools)
library(sf)

  



states <- read_sf("us-states.geojson")
#breast_cancer_long <- read.csv("breast_cancer_long.csv") 

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
  
  
  #Breast Cancer Race Plot
  output$breast_race_plot <- renderPlot({
    
    
    state_data <- breast_cancer_long[breast_cancer_long$state == input$state, ]
    
    
    state_data <- state_data[!is.na(state_data$breast_cancer), ]
    
    
    rates <- state_data$breast_cancer
    races <- state_data$race
    
    races_twolines <- gsub("_", "\n", races)
    
    
    race_colors <- c(
      "White" = "#FF8BA0",
      "Black" = "#FFA8B5",
      "Hispanic" = "#FFCCCB",
      "Asian_NativeHawaiian" = "#FFDCD1",
      "AmericanIndian_AlaskaNative" = "#FF8886",
      "Overall" = "#FF8663"
    )
    
    
    bar_colors <- race_colors[races]
    
    
    barplot(rates,
            names.arg = races_twolines,
            main = input$state,
            col = bar_colors,
            ylab = "Incidence Rate per 100,000 Women",
            xlab = "Race",
            cex.names = 0.9,  # Size of bar labels
            cex.axis = 1.1,   # Size of axis numbers
            cex.main = 1.5,   # Size of title
            font.main = 2)    # Bold title (1=normal, 2=bold, 3=italic, 4=bold italic)
    
  })
  
  
}


