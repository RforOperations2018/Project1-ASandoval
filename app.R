# Project #1

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

# Upload Philadelphia property assessment data from Opendataphilly
# Many fields were removed to decrease data upload
# Data can be found here: https://www.phila.gov/property/data/
property <- read.csv ("projectdata_2.csv")

property.load <- property %>%
  mutate(location = as.character(location),
         owner_1 = as.character(owner_1),
         sale_date = as.factor(sale_date))

pdf(NULL)


# Define UI for application that draws a histogram
ui <- navbarPage("Property Data of Philadelphia", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Selecting type of crime
                              selectInput("categorySelect",
                                          "Type of Property:",
                                          choices = sort(unique(property.load$category_code_description)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("Single Family", "Vacant Land", "Industrial", "Commercial")),
                              
                              # Year of Incident Slider
                              sliderInput("yearSelect",
                                          "Year Property was Bought:",
                                          min = min(property.load$sale_year, na.rm = T),
                                          max = max(property.load$sale_year, na.rm = T),
                                          value = c(min(property.load$sale_year, na.rm = T), max(property.load$sale_year, na.rm = T)),
                                          step = 10),
                              # IGNORE
                              # check box Input for whether incident occured inside
                              checkboxGroupInput(inputId = "IncidentInside",
                                                 label = "Was the Incident Inside?:",
                                                 choiceNames = list("Yes", "No"),
                                                 choiceValues = list("1", "0")
                              ),
                              
                              # action button
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                            ),
                            
                            # Output plot
                            mainPanel(
                              plotlyOutput("categoryplot"), 
                              plotlyOutput("priceplot"))
                          )),
                 
                 # Data Table
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData","Download Property Assessment Data")
                          ),
                          fluidPage(DT::dataTableOutput("table"))
                 )
)

# Define server logic
server <- function(input, output, session = session) {
  # Filtered propert data
  propInput <- reactive({
    property <- property.load %>%
      # Slider Filter
      filter(sale_year >= input$yearSelect[1] & sale_year <= input$yearSelect[2])
    
    # Type of Crime Filter
    if (length(input$categorySelect) > 0 ) {
      property <- subset(property, category_code_description %in% input$categorySelect)
    } 
    # IGNORE THIS TOO
    # Location of Incident
    if (length(input$IncidentInside) > 0 ) {
      shootings <- subset(shootings, inside %in% input$IncidentInside)
    }
    
    return(property)
  })
  # Reactive melted data?
  meltInput <- reactive({
    propInput() %>%
      melt(id = "category_code_description")
  })
  
  # column plot showing types of wounds
  output$categoryplot <- renderPlotly({
    dat <- propInput()
    ggplotly(
      ggplot(data = dat, aes(x = category_code_description, fill = category_code_description )) + 
        geom_bar(position = position_dodge(width = 0.9)) +
        xlab("Types of Property") +
        theme(axis.text.x = element_text (angle = 45,
                                          hjust = 1),
              legend.title = element_blank()) +
        guides(color = FALSE)) 
    
  })
  # HOLD UP ON THIS
  # sex bar plot
  output$sexplot <- renderPlotly({
    dat <- propInput()
    ggplotly(
      ggplot(data = dat, aes(x = sex, fill = sex)) + 
        geom_bar (position = position_dodge(width = 0.9)) +
        xlab("Sex") +
        theme(legend.title = element_blank()) +
        guides(color = FALSE))
  })
  
  # Data Table
  output$table <- DT::renderDataTable({
    property <- propInput()
    subset(property, select = c(category_code_description, geographic_ward, location, market_value, owner_1, parcel_number, sale_date, sale_price))
  })
  
  # Updating the URL Bar
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("property", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(propInput(), file)
    }
  )
  
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "categorySelect", selected = c("Single Family", "Vacant Land", "Industrial", "Commercial"))
    updateSliderInput(session, "yearSelect", value = c(min(property.load$sale_year, na.rm = T), max(property.load$sale_year, na.rm = T)))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")