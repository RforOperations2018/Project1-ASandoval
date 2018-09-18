# Project #1

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(shinydashboard)

# Upload Philadelphia property assessment data from Opendataphilly
# Many fields were removed to decrease data upload
# Data can be found here: https://www.phila.gov/property/data/
property <- read.csv ("projectdata_2.csv")

property.load <- property %>%
  mutate(location = as.character(location),
         owner_1 = as.character(owner_1),
         sale_date = as.factor(sale_date))

pdf(NULL)

header <- dashboardHeader(title = "Property Records Dashboard",
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "yass", 
                                                        icon = icon("users"))
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 110, color = "green",
                                                "MahQueen")
                          ),
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Donald J. Trump",
                                         message = HTML("Help me collude! <br> You're my only hope."),
                                         icon = icon("exclamation-circle"))
                          )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
    selectInput("categorySelect",
                "Categories:",
                choices = sort(unique(property.load$category_code_description)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Single Family", "Vacant Land", "Commercial")),
    # Birth Selection
    sliderInput("yearSelect",
                "Sale Year:",
                min = min(property.load$sale_year, na.rm = T),
                max = max(property.load$sale_year, na.rm = T),
                value = c(min(property.load$sale_year, na.rm = T), max(property.load$sale_year, na.rm = T)),
                step = 1)
  )
)

body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            infoBoxOutput("sale_price"),
            valueBoxOutput("zipcode")
          ),
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("sale_price", plotlyOutput("plot_price")),
                   tabPanel("zipcode", plotlyOutput("plot_zipcode")))
          )
  ),
  tabItem("table",
          fluidPage(
            box(title = "Selected Category Stats", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  propInput <- reactive({
    property <- property.load %>%
      # Slider Filter
      filter(sale_year >= input$yearSelect[1] & sale_year <= input$saleSelect[2])
    # Category Filter
    if (length(input$categorySelect) > 0 ) {
      property <- subset(property, category_code_description %in% input$categorySelect)
    }
    
    return(property)
  })
  # Reactive melted data
  mInput <- reactive({
    propInput() %>%
      melt(id = "category_code_description")
  })
  # A plot showing the sale price of properties
  output$plot_price <- renderPlotly({
    dat <- subset(mInput(), variable == "sale_year")
    ggplot(data = dat, aes(x = category_code_description, fill = category_code_description)) + geom_bar(stat = "identity")
  })
  # A plot showing the height of characters
  output$plot_height <- renderPlotly({
    dat <- subset(mwInput(),  variable == "height")
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  # Data table of characters
  output$table <- DT::renderDataTable({
    subset(swInput(), select = c(name, height, mass, birth_year, homeworld, species, films))
  })
  # Mass mean info box
  output$mass <- renderInfoBox({
    sw <- swInput()
    num <- round(mean(sw$mass, na.rm = T), 2)
    
    infoBox("Avg Mass", value = num, subtitle = paste(nrow(sw), "characters"), icon = icon("balance-scale"), color = "purple")
  })
  # Height mean value box
  output$height <- renderValueBox({
    sw <- swInput()
    num <- round(mean(sw$height, na.rm = T), 2)
    
    valueBox(subtitle = "Avg Height", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
