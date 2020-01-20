#Setup
library(shiny)
library(fpp3)
library(tsibble)
library(readr)
library(tidyverse)
library(plotly)

#First tab
#Inputs:
#Select dataset
#Select a model
#Select forecasting technique (ETS or ARIMA)

#Output:
#Plot series over time
#Plot decomposition

#Second tab
#Show forecast for 2 years
#Show accuracy measures

#getwd()
setwd("/home/ilse/Dropbox/Grad School/UM/Forecast/Shiny-Forecasting/SingleApp")

#Loading data

flour <- as_tsibble(read_csv("flour_1980-2019.csv"), index=DATE)
bread <- as_tsibble(read_csv("bread_1980-2019.csv"), index=DATE)
cookies <- as_tsibble(read_csv("cookies_1980-2019.csv"), index=DATE)

#Formatting data
#Joining dataframes and renaming variables
ts <- cbind(flour, bread, cookies)

#Renaming and dropping variables
ts <- select(ts, date = DATE, flour = APU0000701111, -DATE1, bread = APU0000702111, -DATE2, cookies = APU0000702421)

#Changing date into year-month format
#ts$date <- as_date(ts$date)
ts$date <- yearmonth(ts$date)

ui <- fluidPage(
    
    # Application title
    titlePanel("Forecasting Prices for Consumer goods"),
    
    br(),
    
    h5("Author: Ilse Paniagua"),
    
    br(),
    
    
ui <- fluidPage(
    
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "data",
                         label="Choose your dataset:",
                         c("Bread" = "bread",
                           "Flour" = "flour",
                           "Cookies" = "cookies"))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput('tsPlot')
        )
    )
)

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    # Building Plotly output
    output$tsPlot <- renderPlotly({
        
        p <- ggplot(data = ts, aes(x=date)) +
            geom_line(aes_string(y=input$data)) +
            theme_light() +
            labs(title= paste0("U.S. City Average price of ", input$data, " 1980-2019 (per lb)"),
                 x="Date",
                 y="Price",
                 caption="Source: U.S. Bureau of Labor Statistics")
        
        ggplotly(p)
        
        }
  )}

# Run the application 
shinyApp(ui = ui, server = server)