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

#leafletOutput()


tabPanel("Cervical Cancer",
         titlePanel("Cervical Cancer Rates"),
         setBackgroundColor("#def7ec"),
         mainPanel("statement- intro to topic of discussion"),
         #leafletOutput()
)
),

