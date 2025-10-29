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
cervical_cancer_long <- read.csv("cervical_cancer_long.csv")

navbarPage(
  title = "Women's Health in the United States",
  theme = bs_theme(
    bg = "#FFFFFF",
    fg = "#2C3E50",
    primary = "#2C3E50",
    base_font = font_google("Roboto")
  ),
  
  # Home Page Tab
  tabPanel("Home",
           div(style = "background-color: #F3E4F5; min-height: 100vh; padding: 0;",
               # Header section with title
               div(style = "background: #white; 
                            padding: 60px 20px; text-align: center; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                   h1("Women's Health Disparities Dashboard", 
                      style = "color: #2C3E50; font-family: 'Roboto', sans-serif; 
                               font-weight: 700; font-size: 48px; margin: 0;")
               ),
               
               # Main content
               div(style = "padding: 40px 20px; font-family: 'Roboto', sans-serif;",
                   fluidRow(
                     # Left column with text
                     column(6,
                            div(style = "background-color: white; padding: 30px; 
                                         border-radius: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
                                p("Women's health disparities and outcomes 
                                  have been severely overlooked throughout medical research history. Oftentimes, 
                                  this can lead to disproportionate access to healthcare and medical 
                                  interventions, especially for marginalized populations. Our interactive website 
                                  aims to highlight data from the CDC WONDER Online Database grouped 
                                  by state and race to assess the quality of health and living for women 
                                  across the United States.",
                                  style = "font-size: 18px; line-height: 1.8; color: #2C3E50;"),
                                
                                h3("About This Project", 
                                   style = "color: #2C3E50; margin-top: 30px; font-weight: 600;"),
                                p("Understanding health disparities is crucial for improving outcomes 
                                  for all women. This dashboard visualizes data on maternal mortality 
                                  and other health conditions to highlight where interventions are most needed.",
                                  style = "font-size: 16px; line-height: 1.8; color: #2C3E50;")
                            )
                     ),
                     
                     # Right column with image
                     column(6,
                            div(style = "background-color: white; padding: 20px; 
                                         border-radius: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
                                img(src = "https://p7.hiclipart.com/preview/972/512/1019/5bfcc0ef600d3.jpg",
                                    style = "width: 100%; border-radius: 10px;")
                            )
                     )
                   ),
                   
                   # Second row with image and content
                   fluidRow(style = "margin-top: 30px;",
                            column(6,
                                   div(style = "background-color: white; padding: 20px; 
                                         border-radius: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
                                       img(src = "https://www.nysut.org/-/media/images/nysut/news/2022/july/banner_220706_womenshealth_01.jpg",
                                           style = "width: 100%; border-radius: 10px;")
                                   )
                            ),
                            
                            column(6,
                                   div(style = "background-color: white; padding: 30px; 
                                         border-radius: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
                                       h3("What You'll Find Here", 
                                          style = "color: #2C3E50; font-weight: 600;"),
                                       tags$ul(
                                         style = "font-size: 16px; line-height: 1.8; color: #2C3E50;",
                                         tags$li("Interactive maps showing maternal mortality by state and race"),
                                         tags$li("Analysis of health disparities across different populations"),
                                         tags$li("Data-driven insights to inform policy and healthcare decisions")
                                       ),
                                       
                                       h3("How to Use This Dashboard", 
                                          style = "color: #2C3E50; margin-top: 30px; font-weight: 600;"),
                                       p("Navigate using the tabs above to explore different visualizations. 
                                  Use the filters to focus on specific racial/ethnic groups or states 
                                  of interest.",
                                         style = "font-size: 16px; line-height: 1.8; color: #2C3E50;")
                                   )
                            )
                   ),
                   
                   # Data Sources section
                   fluidRow(style = "margin-top: 30px;",
                            column(12,
                                   div(style = "background-color: white; padding: 30px; 
                                         border-radius: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
                                       p(strong("Data Sources:"), "All datasets were retrieved from KFF 
                                  (originally Kaiser Family Foundation), a public charity and national 
                                  nonprofit that serves to provide data and research on health policy
                                  and health disparities across the United States. KFF currently researches
                                  Women's Health policy through their Policy Analysis sector. Datasets on 
                                  women's health disparities used data from the WONDER Online Database from the 
                                  United States Department of Health and Human Services, Centers for Disease Control 
                                  and Prevention and National Cancer Institute (CDC WONDER Online Database). 
                                  All data after 1989 meets the National Center for Health Statistics data use 
                                  restrictions, where missing values are due to state incidence data not meeting 
                                  publication criteria. Suppressed values replace incidence rate, death counts, 
                                  death rates and associated confidence intervals and standard errors, as well 
                                  as corresponding population figures, when the figure represents one to nine 
                                  (1-9) persons for deaths 1999 and after (About Us, 
                                  retrieved from ", 
                                         a(href = "https://www.kff.org/about-us/", "https://www.kff.org/about-us/"), 
                                         ").",
                                         style = "font-size: 16px; line-height: 1.8; color: #2C3E50;")
                                   )
                            )
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
                          
                          # Chart Section - By Race
                          div(style = "background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                                       margin-bottom: 30px;",
                              
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
                          ),
                          
                          # Top States Ranking Section
                          div(style = "background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                              
                              h2("Top States by Breast Cancer Rates", 
                                 style = "color: #2C3E50; margin-bottom: 20px;"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  style = "background: #FFF5F7; border-radius: 10px; padding: 20px;",
                                  
                                  sliderInput("top_n", 
                                              "Number of states to show:",
                                              min = 5, 
                                              max = 20, 
                                              value = 10,
                                              step = 1),
                                  
                                  selectInput("rank_race", 
                                              "Select Race/Ethnicity:",
                                              choices = unique(breast_cancer_long$race),
                                              selected = "Overall"),
                                  
                                  hr(style = "border-color: #FFB6C1;"),
                                  
                                  div(style = "background: white; padding: 15px; border-radius: 8px;",
                                      p("This chart ranks states by breast cancer incidence rates, 
                                        helping identify areas with the highest rates for targeted interventions.", 
                                        style = "color: #555; margin: 0;")
                                  )
                                ),
                                
                                mainPanel(
                                  plotOutput("top_states_plot", height = "600px")
                                )
                              )
                          )
                      )
             ),
             
             
             tabPanel("Cervical Cancer",
                      div(style = "background: linear-gradient(135deg, #E0F7FA 0%, #B2EBF2 100%); 
                                   min-height: 100vh; padding: 40px 20px;",
                          
                          # Header Section
                          div(style = "text-align: center; margin-bottom: 40px; 
                                       background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                              h1("Cervical Cancer Incidence Rates", 
                                 style = "color: #2C3E50; font-weight: 700; margin-bottom: 10px;"),
                              p("Understanding cervical cancer disparities across the United States",
                                style = "color: #555; font-size: 18px; margin: 0;")
                          ),
                          
                          # Map Section
                          div(style = "background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                                       margin-bottom: 30px;",
                              
                              h2("Cervical Cancer Rates by State and Race", 
                                 style = "color: #2C3E50; margin-bottom: 20px;"),
                              
                              fluidRow(
                                column(12,
                                       p("Explore cervical cancer incidence rates across the United States. 
                                         Use the dropdown to filter by race/ethnicity and see how rates 
                                         vary geographically.",
                                         style = "color: #555; font-size: 16px; margin-bottom: 20px;")
                                )
                              ),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  style = "background: #E0F7FA; border-radius: 10px; padding: 20px;",
                                  
                                  selectInput("cervical_race_filter", 
                                              "Select Race/Ethnicity:",
                                              choices = c("All"),
                                              selected = "All"),
                                  
                                  hr(style = "border-color: #80DEEA;"),
                                  
                                  div(style = "background: white; padding: 15px; border-radius: 8px;",
                                      h4("Map Guide", style = "color: #00838F; margin-top: 0;"),
                                      tags$ul(
                                        style = "color: #555; line-height: 1.8;",
                                        tags$li("Hover over states for exact rates"),
                                        tags$li("Change race filter to update map"),
                                        tags$li("Darker colors = higher rates")
                                      )
                                  )
                                ),
                                
                                mainPanel(
                                  leafletOutput("cervical_map", height = "550px")
                                )
                              )
                          ),
                          
                          # Chart Section - By Race
                          div(style = "background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                                       margin-bottom: 30px;",
                              
                              h2("Cervical Cancer Incidence by Race", 
                                 style = "color: #2C3E50; margin-bottom: 20px;"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  style = "background: #E0F7FA; border-radius: 10px; padding: 20px;",
                                  
                                  selectInput("cervical_state", 
                                              "Choose a state:",
                                              choices = unique(cervical_cancer_long$state),
                                              selected = "United States"),
                                  
                                  hr(style = "border-color: #80DEEA;"),
                                  
                                  div(style = "background: white; padding: 15px; border-radius: 8px;",
                                      p("This chart shows cervical cancer incidence rates by race/ethnicity 
                                        for your selected state.", 
                                        style = "color: #555; margin: 0;")
                                  )
                                ),
                                
                                mainPanel(
                                  plotOutput("cervical_race_plot", height = "500px")
                                )
                              )
                          ),
                          
                          # Top States Ranking Section
                          div(style = "background: white; padding: 30px; 
                                       border-radius: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                              
                              h2("Top States by Cervical Cancer Rates", 
                                 style = "color: #2C3E50; margin-bottom: 20px;"),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  style = "background: #E0F7FA; border-radius: 10px; padding: 20px;",
                                  
                                  sliderInput("cervical_top_n", 
                                              "Number of states to show:",
                                              min = 5, 
                                              max = 20, 
                                              value = 10,
                                              step = 1),
                                  
                                  selectInput("cervical_rank_race", 
                                              "Select Race/Ethnicity:",
                                              choices = unique(cervical_cancer_long$race),
                                              selected = "Overall"),
                                  
                                  hr(style = "border-color: #80DEEA;"),
                                  
                                  div(style = "background: white; padding: 15px; border-radius: 8px;",
                                      p("This chart ranks states by cervical cancer incidence rates, 
                                        helping identify areas with the highest rates for targeted interventions.", 
                                        style = "color: #555; margin: 0;")
                                  )
                                ),
                                
                                mainPanel(
                                  plotOutput("cervical_top_states_plot", height = "600px")
                                )
                              )
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