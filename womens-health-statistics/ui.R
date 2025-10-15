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

# UI
fluidPage(
  titlePanel("Maternal Mortality by State and Race"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("race_select", 
                  "Select Race/Ethnicity:",
                  choices = unique(mortality_avg$race_group),
                  selected = "All racial and ethnic groups"),
      
      hr(),
      
      p("This map shows the average maternal mortality ratio (deaths per 100,000 live births) 
        across all years in the dataset."),
      
      p("Click on states to see specific values.")
    ),
    
    mainPanel(
      leafletOutput("map", height = "600px"),
      br(),
      textOutput("selected_state_info")
    )
  )
)