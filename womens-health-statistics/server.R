library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(htmltools)

server <- function(input, output, session) {
  
  # --- Static map geometry (built once) ---
  states_map <- maps::map("state", fill = TRUE, plot = FALSE)
  IDs <- sapply(strsplit(states_map$names, ":"), `[`, 1)  # lowercase state names
  
  # --- ADD THIS: Reactive data filtered by race selection ---
  mortality_race_reactive <- reactive({
    req(input$race_select)
    mortality_race_long %>%
      filter(race == input$race_select) %>%
      transmute(
        state,
        mortality = maternal_mortality,
        region = tolower(state)
      )
  })
  
    
    # --- Initial render ---
  output$map <- renderLeaflet({
    
    # Join map IDs to the (reactive) data
    map_data <- data.frame(region = IDs, stringsAsFactors = FALSE) %>%
      left_join(mortality_race_reactive(), by = "region")
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = map_data$mortality,
      na.color = "#808080"
    )
    
    cols <- pal(map_data$mortality)
    cols[is.na(cols)] <- "#808080"
    
    lbls <- paste0(
      "<strong>", tools::toTitleCase(map_data$region), "</strong><br/>",
      "Maternal mortality: ",
      ifelse(is.na(map_data$mortality),
             "No data",
             paste0(round(map_data$mortality, 1), " per 100,000"))
    )
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addPolygons(
        data = states_map,
        fillColor = cols,
        fillOpacity = 0.7,
        color = "white",
        weight = 2,
        label = lapply(lbls, HTML),
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
        pal = pal,
        values = map_data$mortality,
        title = "Maternal Deaths<br>per 100,000<br>Live Births",
        opacity = 0.7
      )
  })
  
  # --- Update fill + legend when race selection changes (no full re-render) ---
  observeEvent(input$race_select, {
    map_data <- data.frame(region = IDs, stringsAsFactors = FALSE) %>%
      left_join(mortality_race_reactive(), by = "region")
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = map_data$mortality,
      na.color = "#808080"
    )
    
    cols <- pal(map_data$mortality)
    cols[is.na(cols)] <- "#808080"
    
    lbls <- paste0(
      "<strong>", tools::toTitleCase(map_data$region), "</strong><br/>",
      "Maternal mortality: ",
      ifelse(is.na(map_data$mortality),
             "No data",
             paste0(round(map_data$mortality, 1), " per 100,000"))
    )
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data = states_map,
        fillColor = cols,
        fillOpacity = 0.7,
        color = "white",
        weight = 2,
        label = lapply(lbls, HTML),
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
        pal = pal,
        values = map_data$mortality,
        title = "Maternal Deaths<br>per 100,000<br>Live Births",
        opacity = 0.7
      )
  })
}























