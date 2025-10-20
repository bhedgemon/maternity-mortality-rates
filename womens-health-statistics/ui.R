library(shiny)
library(leaflet)
library(dplyr)
library(maps)

navbarPage(
  title = "Women's Health Outcomes in the United States",
  
  # Home Page Tab
  tabPanel("Home",
           fluidRow(
             column(12,
                    h2("Welcome"),
                    p("This interactive dashboard explores women's health outcomes across 
                      the United States, with a focus on disparities by state and race/ethnicity."),
                    
                    h3("About This Project"),
                    p("Understanding health disparities is crucial for improving outcomes 
                      for all women. This dashboard visualizes data on maternal mortality 
                      and other health conditions to highlight where interventions are most needed."),
                    
                    h3("What You'll Find Here"),
                    tags$ul(
                      tags$li("Interactive maps showing maternal mortality by state and race"),
                      tags$li("Analysis of health disparities across different populations"),
                      tags$li("Data-driven insights to inform policy and healthcare decisions")
                    ),
                    
                    h3("How to Use This Dashboard"),
                    p("Navigate using the tabs above to explore different visualizations. 
                      Use the filters to focus on specific racial/ethnic groups or states 
                      of interest."),
                    
                    br(),
                    p(strong("Data Sources:"), "Information about your data sources here...")
             )
           )
  ),
  
  # Maternal Mortality Map Tab
  tabPanel("Maternal Mortality by State and Race",
           sidebarLayout(
             sidebarPanel(
               selectInput("race_select", 
                           "Select Race/Ethnicity:",
                           choices = unique(mortality_race_long$race),
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
  ),
  
  # About Tab
  tabPanel("About",
           fluidRow(
             column(12,
                    h2("About This Data"),
                    p("Detailed information about data sources, methodology, and limitations...")
             )
           )
  )
)