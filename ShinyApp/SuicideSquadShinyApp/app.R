#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load("shiny", "tmap", "ExPanDaR", "shinydashboard", "shinythemes", "tidyverse")

#Downloading the data
suicidedata_eda_formap <- read_csv("data/suicidedata_eda_formap.csv", show_col_types = FALSE)

#Downloading the relevant sf data "World"
data("World")

#Joining the two datasets together
suicidedata_eda_map <- left_join(World, 
                                 suicidedata_eda_formap %>% mutate(across(where(is.numeric), round, 2)),
                                 by = c("iso_a3" = "code")) %>%
  select(!c(2,3,4,6,7,8,9,10,11,12,13,14,15)) %>%
  mutate(area = as.numeric(str_remove(`area`, 
                                      " \\[km\\^2\\]")), 
         .after = region) %>%
  na.omit()

#========================#
###### Shiny UI ######
#========================#
ui <- navbarPage(
  title = "Suicide Squad: Interactive Data Exploration and Analysis for Worldwide suicide Data",
  fluid = TRUE,
  theme=shinytheme("flatly"),
  id = "navbarID",
  tabPanel("Introduction"),
  navbarMenu("Exploratory Data Analysis",
             tabPanel("Fixed year",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          numericInput(inputId = "analysis_year", 
                                       label = "Year of Analysis (1990 - 2019)", 
                                       value = 2015,
                                       min = 1990, max = 2019, step = 1),
                          
                          radioButtons(inputId = "suicidemetrics", 
                                       label = "Choose Metrics:", 
                                       choices = c("Suicide rate" = "SR",
                                                   "Share of deaths from suicide (%)" = "SP",
                                                   "Number of suicide" = "SN"),
                                       selected = "SR"),
                          
                          selectInput(inputId = "Gender",
                                      label = "Select Gender:",
                                      choices = c("Total" = "T",
                                                  "Male" = "M",
                                                  "Female" = "F"),
                                      selected = "T")
                        ),
                        
                        mainPanel(tmapOutput(outputId = "suicide_map")
                        )
                      )
             )
  )
)


#========================#
###### Shiny Server ######
#========================#
server <- function(input, output) {

##### Shiny Server: Plotting the map #####  
  
  age_metrics <- reactive({
    switch(input$suicidemetrics,
           "SR" = "AS",
           "SP" = "All",
           "SN" = "All")
    })
  
  metric_text <- reactive({
    switch(input$suicidemetrics,
           "SR" = "Suicide rate",
           "SP" = "Share of deaths from suicide (%)",
           "SN" = "Number of suicide")
  })
  
  gender_text <- reactive({
    switch(input$Gender,
           "T" = "Total",
           "M" = "Male",
           "F" = "Female")
  })
  
  output$suicide_map <- renderTmap({
    
    tmap_mode("view")
    
    tm_shape(suicidedata_eda_map |> 
               filter(year == input$analysis_year))+
      tm_fill(paste0(input$suicidemetrics,"_",age_metrics(),"_",input$Gender), 
              style = "jenks", 
              palette="YlOrBr", 
              id = "country",
              title = paste0(metric_text(), ", ",gender_text(),", ", input$analysis_year),
              popup.vars = c(value = paste0(input$suicidemetrics,"_",age_metrics(),"_",input$Gender))) +
      
      tm_borders(col = "grey20",
                 alpha = 0.5) 
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
