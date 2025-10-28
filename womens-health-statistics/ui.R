library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(maps)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)

# Load data
breast_cancer_long <- read.csv("breast_cancer_long.csv")

navbarPage(
  title = "Women's Health in the United States",
  theme = bs_theme(
    bg = "#FFFFFF",
    fg = "#2C3E50",
    primary = "#FF8BA0",
    base_font = font_google("Roboto")
  ),
  
  # Home Page Tab
  tabPanel("Home",
           div(style = "padding: 40px 20px;",
               fluidRow(
                 column(12,
                        h2("Welcome", style = "color: #2C3E50; font-weight: 700;"),
                        p("This interactive website explores women's health across 
                          the United States. We put a specific focus on disparities by state and race/ethnicity.",
                          style = "font-size: 16px;"),
                        
                        h3("About This Project", style = "color: #2C3E50; margin-top: 30px;"),
                        p("Understanding health disparities is crucial for improving outcomes 
                          for all women. This dashboard visualizes data on maternal mortality 
                          and other health conditions to highlight where interventions are most needed.",
                          style = "font-size: 16px;"),
                        
                        h3("What You'll Find Here", style = "color: #2C3E50; margin-top: 30px;"),
                        tags$ul(
                          style = "font-size: 16px; line-height: 1.8;",
                          tags$li("Interactive maps showing maternal mortality by state and race"),
                          tags$li("Analysis of health disparities across different populations"),
                          tags$li("Data-driven insights to inform policy and healthcare decisions")
                        ),
                        
                        h3("How to Use This Dashboard", style = "color: #2C3E50; margin-top: 30px;"),
                        p("Navigate using the tabs above to explore different visualizations. 
                          Use the filters to focus on specific racial/ethnic groups or states 
                          of interest.",
                          style = "font-size: 16px;"),
                        
                        br(),
                        p(strong("Data Sources:"), "Information about your data sources here...",
                          style = "font-size: 16px;")
                 )
               )
           )
  ),
  
  navbarMenu("Cancer",
             tabPanel("Breast Cancer",
                      div(style = "background: linear-gradient(135deg, #FFF5F7 0%, #FFE4E9 100%); 
                                   min-height: 100vh; padding: 40px 20px;",
                          
                          # Header Section
                          div(style = "text-align: center; margin-bottom: 40px; 
                                       background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                              h1("Breast Cancer Incidence Rates", 
                                 style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;"),
                              p("Understanding breast cancer disparities across the United States",
                                style = "color: #555; font-size: 18px; margin: 0;")
                          ),
                          
                          # Map Section
                          div(style = "background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                                       margin-bottom: 30px;",
                              
                              h2("Breast Cancer Rates by State and Race", 
                                 style = "color: #2C3E50; margin-bottom: 20px;"),
                              
                              fluidRow(
                                column(12,
                                       p("Explore breast cancer incidence rates across the United States. 
                                         Use the dropdown to filter by race/ethnicity and see how rates 
                                         vary geographically.",
                                         style = "color: #555; font-size: 16px; margin-bottom: 20px;")
                                )
                              ),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  style = "background: #FFF5F7; border-radius: 10px; padding: 20px;",
                                  
                                  selectInput("race_filter", 
                                              "Select Race/Ethnicity:",
                                              choices = c("All"),
                                              selected = "All"),
                                  
                                  hr(style = "border-color: #FFB6C1;"),
                                  
                                  div(style = "background: white; padding: 15px; border-radius: 8px;",
                                      h4("Map Guide", style = "color: #2C3E50; margin-top: 0;"),
                                      tags$ul(
                                        style = "color: #555; line-height: 1.8;",
                                        tags$li("Hover over states for exact rates"),
                                        tags$li("Change race filter to update map"),
                                        tags$li("Darker colors = higher rates")
                                      )
                                  )
                                ),
                                
                                mainPanel(
                                  leafletOutput("cancer_map", height = "550px")
                                )
                              )
                          ),
                          
                          # Chart Section
                          div(style = "background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                              
                              h2("Breast Cancer Incidence by Race", 
                                 style = "color: #2C3E50; margin-bottom: 20px;"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  style = "background: #FFF5F7; border-radius: 10px; padding: 20px;",
                                  
                                  selectInput("state", 
                                              "Choose a state:",
                                              choices = unique(breast_cancer_long$state),
                                              selected = "United States"),
                                  
                                  hr(style = "border-color: #FFB6C1;"),
                                  
                                  div(style = "background: white; padding: 15px; border-radius: 8px;",
                                      p("This chart shows breast cancer incidence rates by race/ethnicity 
                                        for your selected state.", 
                                        style = "color: #555; margin: 0;")
                                  )
                                ),
                                
                                mainPanel(
                                  plotOutput("breast_race_plot", height = "500px")
                                )
                              )
                          )
                      )
             ),
             
             tabPanel("Cervical Cancer",
                      div(style = "background: linear-gradient(135deg, #E8F5E9 0%, #C8E6C9 100%); 
                                   min-height: 100vh; padding: 40px 20px;",
                          
                          div(style = "text-align: center; margin-bottom: 40px; 
                                       background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                              h1("Cervical Cancer Rates", 
                                 style = "color: #2E7D32; font-weight: 700; margin-bottom: 10px;"),
                              p("Statement - intro to topic of discussion",
                                style = "color: #555; font-size: 18px; margin: 0;")
                          )
                      )
             )
  ),
  
  # Sexual infections
  tabPanel("Sexual Infections",
           div(style = "padding: 40px 20px;",
               div(style = "text-align: center; margin-bottom: 40px; 
                            background: white; padding: 30px; 
                            border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   h1("Sexually Transmitted Infection Rates", 
                      style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;"),
                   p("Chlamydia, Syphilis, Gonorrhea",
                     style = "color: #555; font-size: 18px; margin: 0;")
               )
           )
  ),
  
  # Maternal-Infant Health
  tabPanel("Maternal-Infant Health",
           div(style = "padding: 40px 20px;",
               div(style = "text-align: center; margin-bottom: 40px; 
                            background: white; padding: 30px; 
                            border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   h1("Maternal & Infant Mortality Rates", 
                      style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;"),
                   p("Statement TBD",
                     style = "color: #555; font-size: 18px; margin: 0;")
               )
           )
  ),
  
  # All health combined
  tabPanel("Changes in Health",
           div(style = "padding: 40px 20px;",
               div(style = "text-align: center; margin-bottom: 40px; 
                            background: white; padding: 30px; 
                            border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   h1("Women's Health Over Time", 
                      style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;"),
                   p("Statement TBD",
                     style = "color: #555; font-size: 18px; margin: 0;")
               )
           )
  ),
  
  # About Tab
  tabPanel("About",
           div(style = "padding: 40px 20px;",
               fluidRow(
                 column(12,
                        div(style = "background: white; padding: 30px; 
                                     border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                            h2("About This Data", style = "color: #2C3E50; font-weight: 700;"),
                            p("Detailed information about data sources, methodology, and limitations...",
                              style = "font-size: 16px;")
                        )
                 )
               )
           )
  )
)