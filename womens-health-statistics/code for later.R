# Maternal Mortality Map Tab


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

#bailey code for later
server <- function(input, output, session) {
  breast_data <- read_csv("breast_cancer.csv")
  states <- read_sf("us-states.geojson")
  # Join data by state name
  merged_data <- left_join(states, breast_data, by = c("name" = "name"))
  
  output$map <- renderLeaflet({
    # Initial empty leaflet (centered)
    leaflet(merged_data) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("CartoDB.Positron")
  })
  
  # Reactive observer to update map whenever race is changed
  observe({
    race_col <- input$race_col
    bins <- c(0, 50, 100, 125, 150, 175, 200, Inf)
    pal <- colorBin("YlOrRd", domain = merged_data[[race_col]], bins = bins, na.color = "gray")
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s Rate: %g",
      merged_data$name, race_col, merged_data[[race_col]]
    ) %>% lapply(HTML)
    
    leafletProxy("map", data = merged_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(get(race_col)),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "14px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = merged_data[[race_col]],
        opacity = 0.7,
        title = paste("Breast Cancer Rate -", race_col),
        position = "bottomright"
      )
  })
}


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
    selectInput("race_col", "Select Race Category:",
                choices = c("Overall", "White", "Black", "Hispanic", 
                            "Asian_NativeHawaiian", "AmericanIndian_AlaskaNative"),
                selected = "Overall"),
    width = 3
  ),
  
  mainPanel(
    leafletOutput("map", height = "700px")
  )
)
)
),