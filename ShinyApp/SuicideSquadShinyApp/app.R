#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load("shiny", "tmap", "ExPanDaR", "shinydashboard", "shinythemes", "plotly", "tidyverse")

#==============================#
###### Data Manipulation ######
#==============================#
suicidedata_eda_formap <- read_csv("data/suicidedata_eda_formap.csv", show_col_types = FALSE)

suicidedata_eda <- read_csv("data/suicidedata_eda.csv", show_col_types = FALSE)

#Downloading the relevant sf data "World"
data("World")

##### Data: Map data #####  
suicidedata_eda_map <- left_join(World, 
                                 suicidedata_eda_formap %>% mutate(across(where(is.numeric), round, 2)),
                                 by = c("iso_a3" = "code")) %>%
  select(!c(2,3,4,6,7,8,9,10,11,12,13,14,15)) %>%
  mutate(area = as.numeric(str_remove(`area`, 
                                      " \\[km\\^2\\]")), 
         .after = region) %>%
  na.omit()

##### Data: Pyramid data #####
suicidedata_pyramid <- suicidedata_eda %>%
  filter(sex_name %in% c("Male", "Female"),
         age_name %in% c("10-14", "15-24", "25-44", "45-64", "65-74", "75+")) %>%
  mutate(SN = round(ifelse(sex_name == "Male", SN*(-1), SN), 0),
         DN = round(ifelse(sex_name == "Male", DN*(-1), DN), 0),
         SP = round(ifelse(sex_name == "Male", SP*(-1), SP), 2),
         SR = round(ifelse(sex_name == "Male", SR*(-1), SR), 2),
         DR = round(ifelse(sex_name == "Male", DR*(-1), DR), 2),
         age_name = factor(age_name, levels = c("10-14", "15-24", "25-44", "45-64", "65-74", "75+"))) %>%
  rename(Age = age_name,
         Gender = sex_name)

#========================#
###### Shiny UI ######
#========================#
ui <- navbarPage(
  title = "Suicide Squad: Interactive Exploration and Analysis for Worldwide Suicide Data",
  fluid = TRUE,
  theme=shinytheme("darkly"),
  id = "navbarID",
  tabPanel("Introduction"),
  navbarMenu("Exploratory Data Analysis",
             tabPanel("Fixed year",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          sliderInput(inputId = "analysis_year", 
                                      label = "Year of Analysis (1990 - 2019)",
                                      min = 1990, max = 2019,
                                      value = 2015,
                                      sep = "",
                                      animate = animationOptions(interval = 300, loop = TRUE)),
                          
                          radioButtons(inputId = "suicidemetrics", 
                                       label = "Choose Metrics:", 
                                       choices = c("Suicide rate" = "SR",
                                                   "Share of deaths from suicide (%)" = "SP",
                                                   "Number of suicide" = "SN"),
                                       selected = "SR"),
                          
                          selectInput(inputId = "Gender",
                                      label = "Map - Select Gender:",
                                      choices = c("Both" = "T",
                                                  "Male" = "M",
                                                  "Female" = "F"),
                                      selected = "T"),
                          
                          selectInput(inputId = "mapstyle",
                                      label = "Map - Select Classification Method:",
                                      choices = c("Jenks" = "jenks",
                                                  "Fisher" = "fisher",
                                                  "Kernel Density" = "dpih",
                                                  "Headtails" = "headtails",
                                                  "Log10" = "log10_pretty"),
                                      selected = "Jenks"),
                          
                          selectizeInput(inputId = "selectedcountry",
                                         label = "Pyramid - Select Country:",
                                         choices = unique(suicidedata_eda$country),
                                         selected = "China",
                                         multiple = FALSE)
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("World Map", 
                                     fluidRow(
                                       column(12, htmlOutput("suicide_map_title")),
                                       column(12, tmapOutput("suicide_map", height = 700, width = 1100)))),
                            tabPanel("Country Pyramid", 
                                     fluidRow(
                                       column(12, htmlOutput("pyramid_title")),
                                       column(12, plotlyOutput("pyramid", height = 500, width = 750)))
                                     )
                            )
                        )
                      )
             )
  )
)


#========================#
###### Shiny Server ######
#========================#
server <- function(input, output) {

##### Shiny Server: Metrics and Title ##### 
  
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
  
  
##### Shiny Server: Plotting the map #####  
  output$suicide_map_title <- renderUI({
    h3(paste0("World-wide ", metric_text(), " in ", input$analysis_year))
  })
  
  output$suicide_map <- renderTmap({
    
    tmap_mode("view")
    
    tm_shape(suicidedata_eda_map |> 
               filter(year == input$analysis_year))+
      tm_fill(paste0(input$suicidemetrics,"_",age_metrics(),"_",input$Gender), 
              style = input$mapstyle, 
              palette="YlOrBr", 
              id = "country",
              title = paste0(metric_text(), ", ",gender_text(),", ", input$analysis_year),
              popup.vars = c(value = paste0(input$suicidemetrics,"_",age_metrics(),"_",input$Gender))) +
      
      tm_borders(col = "grey20",
                 alpha = 0.5) 
    
  })

##### Shiny Server: Plotting the pyramid ##### 
  output$pyramid_title <- renderUI({
    h3(paste0(metric_text(),' by Gender and Age Group, ',input$selectedcountry, ", ",input$analysis_year ))
  })
  
  suicidedata_pyramid_summary <- reactive({
    suicidedata_pyramid %>%
      filter(country == input$selectedcountry) %>%
      summarise(max = max(abs(!!sym(input$suicidemetrics))))
  })
  
  #computing the limits for the axis
  max_limit <- reactive({suicidedata_pyramid_summary()$max})
  interval <- reactive({suicidedata_pyramid_summary()$max/2})
  tickvals <- reactive({seq(-max_limit(), max_limit(), interval())})
  ticktext <- reactive({c(as.character(max_limit()), as.character(interval()), "0", as.character(interval()), as.character(max_limit()))})
  
  output$pyramid <- renderPlotly({
    
    ggplotly(
      ggplot(data = suicidedata_pyramid %>%
               filter(year == input$analysis_year,
                      country == input$selectedcountry),
              aes(x = Age,
                  y = !!sym(input$suicidemetrics), 
                  fill = Gender,
                  text = paste(metric_text(), ":", abs(!!sym(input$suicidemetrics))))) +
        geom_col() +
        coord_flip() +
        labs (x = metric_text()) +
        theme_bw() +
        theme(axis.ticks.y = element_blank(),
              axis.title.y = element_blank()) +
        scale_fill_manual(values = c("Male" = "lightblue", "Female" = "lightpink")),
      session="knitr", tooltip = c("x", "fill", "text")) %>%
      
      layout(xaxis = list(title = metric_text(),
                          tickmode = 'array',
                          range = list(-max_limit()*1.05, max_limit()*1.05),
                          tickvals = tickvals(),
                          ticktext = ticktext()))
  
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
