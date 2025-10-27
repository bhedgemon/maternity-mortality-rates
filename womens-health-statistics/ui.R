library(shiny)
library(leaflet)
library(dplyr)
library(maps)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)

navbarPage(
  title = "Women's Health in the United States",
  
  # Home Page Tab
  tabPanel("Home",
           fluidRow(
             column(12,
                    h2("Welcome"),
                    p("This interactive website explores women's health across 
                      the United States. We put a specific focus on disparities by state and race/ethnicity."),
                    
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
  
# sexual infections
             tabPanel("Sexual Infections",
                      titlePanel("Sexually Transmitted Infection Rates"),
                      #setBackgroundColor("#FFEEF2"),
                      mainPanel("Chlamydia, Syphilis, Gonorrhea"),
                      #leafletOutput()
             ),
 
# Maternal-Infant Health
tabPanel("Maternal-Infant Health",
         titlePanel("Maternal & Infant Mortality Rates"),
         #setBackgroundColor("#FFEEF2"),
         mainPanel("Statement TBD"),
         #leafletOutput()
),
  
# All health combined
tabPanel("Changes in Health",
         titlePanel("Women's Health Over Time"),
         #setBackgroundColor("#FFEEF2"),
         mainPanel("Statement TBD"),
         #leafletOutput()
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


