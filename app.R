# Project #1

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(shinydashboard)
library(readr)
require(scales)

# Upload Philadelphia property assessment data from Opendataphilly
# Many fields were removed to decrease data upload
# Data can be found here: https://www.phila.gov/property/data/
# There were originally 580,919 rows of data. I used a random number generator to get 2000 rows. 
# It now runs faster. 
property.load <- read_csv ("projectdata_7.csv")

pdf(NULL)
##GOOD A
header <- dashboardHeader(title = "Property Records Dashboard",
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "New Users!", 
                                                        icon = icon("users"))
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 110, color = "red",
                                                "loading")
                          ),
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Donald J. Trump",
                                         message = HTML("Help me expand Trump Organization!"),
                                         icon = icon("exclamation-circle"))
                          )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "blue"),
    # Category Select
    selectInput("categorySelect",
                "Categories:",
                choices = sort(unique(property.load$category_code_description)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Single Family", "Vacant Land", "Commercial", "Industrial")),
    # Year Select
    sliderInput("yearSelect",
                "Sale Year:",
                min = min(property.load$sale_year, na.rm = T),
                max = max(property.load$sale_year, na.rm = T),
                value = c(min(property.load$sale_year, na.rm = T), max(property.load$sale_year, na.rm = T)),
                step = 25)
  )
)

body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            infoBoxOutput("price"),
            valueBoxOutput("zipcode")
          ),
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Change in Value", plotlyOutput("plot_value")),
                   tabPanel("Properties by Ward", plotlyOutput("plot_properties")),
                   tabPanel("Purchases by Year", plotlyOutput("plot_years")))
          )
  ),
  tabItem("table",
          fluidPage(
            box(title = "Philadelphia Property Assessment Data", DT::dataTableOutput("table"), width = 12))
  )
)
)


ui <- dashboardPage(header, sidebar, body)
# Define server logic
server <- function(input, output) {
  propInput <- reactive({
    property <- property.load  %>%
      # Slider Filter
      filter(sale_year >= input$yearSelect[1] & sale_year <= input$yearSelect[2])
    # Category Filter
    if (length(input$categorySelect) > 0 ) {
      property <- subset(property, category_code_description %in% input$categorySelect)
    }
## GOOD Z
    return(property)
  })
  # Reactive melted data
  mInput <- reactive({
    property <- propInput()
    
    property_m <- property %>%
      melt(id = "category_code_description")
  })

  # A plot showing the sale price of properties
  output$plot_value <- renderPlotly({
    property <- propInput()
    ggplot(data = property, aes(x = sale_year, y = change_value, fill = category_code_description))  + 
      geom_point(stroke = 0) +
      guides(fill=FALSE) +
    scale_y_continuous(name="Property Change of Value", labels = comma, breaks=c(-10000000,-800000, -600000, -400000, -200000, 0, 200000, 400000, 600000, 800000, 1000000)) +
    scale_x_continuous(name="Sale Year", breaks=c(1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) +
      theme(legend.title = element_blank())
  })

  
 # A plot showing the height of characters
  output$plot_properties <- renderPlotly({
    property <- propInput()
    ggplot(data = property, aes(x = geographic_ward, fill = category_code_description)) +
      geom_bar(position = "stack") +
      guides(fill=FALSE) +
      scale_y_continuous( name="Count of Properties") +
      scale_x_continuous( name="Wards", breaks=c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 66))
  })
  

  # A plot showing the height of characters
  output$plot_years <- renderPlotly({
    property <- propInput()
    ggplot(data = property, aes(x = sale_year, color = category_code_description ))  + 
      geom_freqpoly() +
      guides(fill=FALSE) +
      scale_x_continuous(name = "Sale Year",breaks=c(1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) +
      scale_y_continuous(name = "Count of Property Purchases") +
      theme(legend.title = element_blank())
  })
  
  # Data table of characters
  output$table <- DT::renderDataTable({
    subset(propInput(), select = c(category_code_description, location, market_value, owner_1, parcel_number, sale_date, sale_price))
  })
  ## STOP HERE
  # Mass mean info box
  output$sale_year <- renderInfoBox({
    proper <- propInput()
    num <- round(mean(proper$category_code_description, na.rm = T), 2)
    
    infoBox("Avg Mass", value = num, subtitle = paste(nrow(proper), "characters"), icon = icon("balance-scale"), color = "purple")
  })
  # Height mean value box
  output$zipcode <- renderValueBox({
    proper <- propInput()
    num <- round(mean(proper$zipcode, na.rm = T), 2)
    
    
    valueBox(subtitle = "Avg Height", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
