library(shiny)
library(leaflet)
library(dplyr)
library(maps)

# Load and prepare your data
mortality_avg <- mortality_race %>%
  group_by(state, race_group) %>%
  summarise(
    avg_mortality = mean(val, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(state_lower = tolower(state))

# Server function
function(input, output, session) {
  
  # Reactive data based on selected race
  filtered_data <- reactive({
    mortality_avg %>%
      filter(race_group == input$race_select)
  })
  
  # Create color palette
  pal <- reactive({
    colorNumeric(
      palette = "YlOrRd",
      domain = filtered_data()$avg_mortality,
      na.color = "#808080"
    )
  })
  
  # Render the map
  output$map <- renderLeaflet({
    # Get map data
    states_map <- map("state", fill = TRUE, plot = FALSE)
    
    # Get IDs for each polygon
    IDs <- sapply(strsplit(states_map$names, ":"), function(x) x[1])
    
    # Merge with mortality data
    map_data <- data.frame(
      region = IDs,
      stringsAsFactors = FALSE
    ) %>%
      left_join(filtered_data(), by = c("region" = "state_lower"))
    
    # Create color vector
    colors <- pal()(map_data$avg_mortality)
    colors[is.na(colors)] <- "#808080"
    
    # Create labels
    labels <- paste0(
      "<strong>", tools::toTitleCase(map_data$region), "</strong><br/>",
      "Mortality Rate: ", 
      ifelse(is.na(map_data$avg_mortality), 
             "No data", 
             paste0(round(map_data$avg_mortality, 1), " per 100,000"))
    )
    
    # Create the leaflet map
    leaflet(data = states_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addPolygons(
        fillColor = colors,
        fillOpacity = 0.7,
        color = "white",
        weight = 2,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal(),
        values = filtered_data()$avg_mortality,
        title = "Maternal Deaths<br>per 100,000<br>Live Births",
        opacity = 0.7
      )
  })
  
  # Update map when race selection changes
  observeEvent(input$race_select, {
    output$map <- renderLeaflet({
      # Get map data
      states_map <- map("state", fill = TRUE, plot = FALSE)
      
      # Get IDs for each polygon
      IDs <- sapply(strsplit(states_map$names, ":"), function(x) x[1])
      
      # Merge with mortality data
      map_data <- data.frame(
        region = IDs,
        stringsAsFactors = FALSE
      ) %>%
        left_join(filtered_data(), by = c("region" = "state_lower"))
      
      # Create color vector
      colors <- pal()(map_data$avg_mortality)
      colors[is.na(colors)] <- "#808080"
      
      # Create labels
      labels <- paste0(
        "<strong>", tools::toTitleCase(map_data$region), "</strong><br/>",
        "Mortality Rate: ", 
        ifelse(is.na(map_data$avg_mortality), 
               "No data", 
               paste0(round(map_data$avg_mortality, 1), " per 100,000"))
      )
      
      # Create the leaflet map
      leaflet(data = states_map) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
        addPolygons(
          fillColor = colors,
          fillOpacity = 0.7,
          color = "white",
          weight = 2,
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          ),
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal(),
          values = filtered_data()$avg_mortality,
          title = "Maternal Deaths<br>per 100,000<br>Live Births",
          opacity = 0.7
        )
    })
  })
}