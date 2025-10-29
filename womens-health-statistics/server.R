library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(htmltools)
library(sf)
library(ggplot2)

states <- read_sf("us-states.geojson")
breast_cancer_long <- read.csv("breast_cancer.csv") 

server <- function(input, output, session) {
  
  # Update race filter choices
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
    if (is.list(data$breast_cancer)) {
      data$breast_cancer <- as.numeric(unlist(data$breast_cancer))
    }
    
    bins <- c(0, 30, 60, 90, 120, 150, 180, Inf)
    pal <- colorBin("RdPu", domain = data$breast_cancer, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%.1f per 100,000 women",
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
                title = "Incidence Rate<br/>(per 100,000)",
                position = "bottomright")
  })
  
  # BREAST CANCER RACE PLOT - Enhanced with ggplot2
  output$breast_race_plot <- renderPlot({
    
    # Filter data for selected state
    state_data <- breast_cancer_long %>%
      filter(state == input$state, !is.na(breast_cancer))
    
    # Improve race labels for display
    state_data <- state_data %>%
      mutate(race_label = case_when(
        race == "White" ~ "White",
        race == "Black" ~ "Black",
        race == "Hispanic" ~ "Hispanic",
        race == "Asian_NativeHawaiian" ~ "Asian/Native\nHawaiian",
        race == "AmericanIndian_AlaskaNative" ~ "American Indian/\nAlaska Native",
        race == "Overall" ~ "Overall",
        TRUE ~ gsub("_", " ", race)
      ))
    
    # Define colors
    race_colors <- c(
      "White" = "#FF8BA0",
      "Black" = "#FFA8B5",
      "Hispanic" = "#FFCCCB",
      "Asian_NativeHawaiian" = "#FFDCD1",
      "AmericanIndian_AlaskaNative" = "#FF8886",
      "Overall" = "#FF8663"
    )
    
    # Create ggplot
    ggplot(state_data, aes(x = reorder(race_label, -breast_cancer), 
                           y = breast_cancer, 
                           fill = race)) +
      geom_col(width = 0.7, alpha = 0.9) +
      geom_text(aes(label = round(breast_cancer, 1)), 
                vjust = -0.5, 
                size = 4, 
                fontface = "bold",
                color = "#2C3E50") +
      scale_fill_manual(values = race_colors) +
      labs(title = paste("Breast Cancer Incidence in", input$state),
           subtitle = "Per 100,000 Women",
           x = NULL,
           y = "Incidence Rate") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 20, color = "#2C3E50", hjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "#555", hjust = 0.5, margin = margin(b = 20)),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, color = "#2C3E50"),
        axis.text.y = element_text(size = 12, color = "#2C3E50"),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 15)),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#E0E0E0", linetype = "dashed"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#FAFAFA", color = NA),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
    
  }, bg = "white")
  
}