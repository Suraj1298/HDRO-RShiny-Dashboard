#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)

data_irl <- read.csv("hdro_indicators_irl.csv", header = TRUE, skip = 1)
data_esp <- read.csv("hdro_indicators_esp.csv", header = TRUE, skip = 1)
data_sgp <- read.csv("hdro_indicators_sgp.csv", header = TRUE, skip = 1)
data_jpn <- read.csv("hdro_indicators_jpn.csv", header = TRUE, skip = 1)
data_che <- read.csv("hdro_indicators_che.csv", header = TRUE, skip = 1)
data <- rbind(data_irl, data_esp, data_sgp, data_jpn, data_che)

data <- setNames(data, c("country_code","country_name","indicator_id",
                         "indicator_name","index_id","index_name",
                         "value","year"))
#Assign appropriate classes to the variables
data$country_code <- as.factor(data$country_code)
data$country_name <- as.factor(data$country_name)
data$indicator_id <- as.factor(data$indicator_id)
data$indicator_name <- as.factor(data$indicator_name)
data$index_id <- as.factor(data$index_id)
data$index_name <- as.factor(data$index_name)
data$value <- as.numeric(data$value)
data$year <- as.numeric(data$year)

# Define UI for application
ui <- fluidPage(
      
      #For using two tabs to display data and plots
      tabsetPanel(
        tabPanel("Data",
                 
      
          # Application title
          titlePanel("Human Development Indicators:"),
          titlePanel(textOutput("title_panel")),
          
          #Filters for data displayed
          fluidRow(
            column(4,
                   selectInput("ctry",
                               "Country:",
                               c(unique(as.character(data$country_name))), multiple = TRUE)),
            
            column(4,
                   numericInput("obs",
                                "Observations:",
                                10, min = 1, max = 1000)),
            column(4,
                   selectInput("col",
                               "Columns:",
                               colnames(data), multiple = TRUE)),
            column(4,
                   fileInput("file1", "Choose CSV File",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")))
          ),
          
          tableOutput("table"),
        ),
      
      tabPanel("Plots",
               
        #Filters for histogram plot
        sidebarLayout(
          sidebarPanel(
            selectInput("ctry2",
                        "Country:",
                        c(unique(as.character(data$country_name)))),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 15,
                        value = 10),
            
            selectInput("colour",
                        "Colour of Histogram",
                        list('colour' = c("red", "blue", "green"))
                        ),
            
            numericInput(inputId ="ylim", "Limit on y axis:", value = 20),
            
            
          ),
          
          #Display plot
          mainPanel(
            plotOutput("histPlot")
          )
        ),
        
        #Filters for scatter plot
        sidebarLayout(
          sidebarPanel(
            selectInput("ctry1",
                        "Country:",
                        c(unique(as.character(data$country_name))), multiple = TRUE),
            
            numericInput(inputId ="size", "Size:", value = 2),
            
            numericInput(inputId ="alpha", "Transparency:", value = 1.0)
          ),
          
          #Display plot
          mainPanel(
            plotOutput("scatterPlot")
          )
      )
      )
      

)

)

# Define server logic
server <- function(input, output, session) {
    
    #Looking for file upload
    observeEvent(input$file1,{
      
      req(input$file1)
      temp_df <- read.csv(input$file1$datapath,
                          header = TRUE, skip = 1)
      
      temp_df <- setNames(temp_df, c("country_code","country_name","indicator_id",
                                     "indicator_name","index_id","index_name",
                                     "value","year"))
      #Assign appropriate classes to the variables
      temp_df$country_code <- as.factor(temp_df$country_code)
      temp_df$country_name <- as.factor(temp_df$country_name)
      temp_df$indicator_id <- as.factor(temp_df$indicator_id)
      temp_df$indicator_name <- as.factor(temp_df$indicator_name)
      temp_df$index_id <- as.factor(temp_df$index_id)
      temp_df$index_name <- as.factor(temp_df$index_name)
      temp_df$value <- as.numeric(temp_df$value)
      temp_df$year <- as.numeric(temp_df$year)
      
      #Update data
      data <<- rbind(data, temp_df)
      
      #Update the inputs list
      updateSelectInput(session, 'ctry', choices = c(unique(as.character(data$country_name))))
      updateSelectInput(session, 'ctry1', choices = c(unique(as.character(data$country_name))))
      updateSelectInput(session, 'ctry2', choices = c(unique(as.character(data$country_name))))
      
    })
    
    #Filtering the data
    filter_data <- reactive({
      
      #Filter based on countries
      filtered <- data
      req(input$obs, cancelOutput = TRUE)

      if(length(input$ctry) != 0){
        #filtered <- filtered[filtered$country_name %in% input$ctry,]
        filtered <- filtered[is.element(filtered$country_name, input$ctry),]
      }
      
      #Filter by number of rows
      if(length(input$obs) != 0){
        filtered <- filtered[1:input$obs,]
      }
      
      #Selecting the columns
      if(length(input$col) != 0){
        filtered <- filtered[,input$col]
      }
      
      filtered
    })
    
    #Display the data
    output$table <- renderTable({
      filter_data()
    })
    
    #Update title according to country
    output$title_panel <- renderText({
      paste0("",input$ctry)
    })
    
    output$histPlot <- renderPlot({
      
      #Generate bins
      hrd_value <- data[data$country_name %in% input$ctry2 & data$indicator_id == 'eys_m',]
      bins <- seq(min(hrd_value$value), max(hrd_value$value), length.out = input$bins + 1)
      
      #Plot histogram
      hist(hrd_value$value, breaks = bins,
           col = input$colour, border = 'white', ylim = c(0, input$ylim),
           main = 'Histogram for expected years of schooling for male', xlab = "Value")
    })
    
    #Plot scatter plot
    output$scatterPlot <- renderPlot({
      
      if(length(input$ctry1) == 0){
        
        #Initial plot
        plot_data <- data[data$country_name == 'Ireland' & data$indicator_id == 'ineq_inc',]
        ggplot(plot_data, aes(x = year, y = value)) +
          geom_point(aes(color = as.factor(c("Ireland"))), size = input$size, alpha = input$alpha) +
          scale_x_continuous(breaks = c(min(plot_data$year):max(plot_data$year))) +
          theme_bw() +
          labs(x = 'Year', y = 'Value', title = 'Inequality in Income',
               colour = 'Country')
        
      }
      else{
      #Plots after applying inputs
      plot_data <- data[data$country_name %in% input$ctry1 & data$indicator_id == 'ineq_inc',]
      ggplot(plot_data, aes(x = year, y = value)) +
        geom_point(aes(color = country_name), size = input$size, alpha = input$alpha) +
        scale_x_continuous(breaks = c(min(plot_data$year):max(plot_data$year))) +
        theme_bw() +
        labs(x = 'Year', y = 'Value', title = 'Inequality in Income',
             colour = 'Country')
      }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
