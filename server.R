library(fpp3)
library(tsibble)
library(readr)
library(tidyverse)
library(plotly)
library(magrittr)
library(DT)

#Loading data

Flour <- as_tsibble(read_csv("flour_1980-2019.csv"), index=DATE)
Bread <- as_tsibble(read_csv("bread_1980-2019.csv"), index=DATE)
Cookies <- as_tsibble(read_csv("cookies_1980-2019.csv"), index=DATE)

Flour %<>% mutate(
    DATE= yearmonth(DATE),
    price = APU0000701111)

Cookies %<>% mutate(
    DATE= yearmonth(DATE),
    price = APU0000702421)

Bread %<>% mutate(
    DATE= yearmonth(DATE),
    price = APU0000702111)


server <- function(input, output) {
    
    getDataset <- reactive({
        if (input$dataset=="Cookies")
        {
            return(Cookies)
        }
        else if (input$dataset=="Flour")
        {
            return(Flour)
        }
        else
        {
            return(Bread)
        }
        
    })
    
    output$caption <- renderText({
        paste("Dataset: ", input$dataset)
    })
    
    output$dcompPlot <- renderPlot({
        ds_ts <- STL(getDataset(), price ~ season(window = Inf))
        autoplot(ds_ts)
    })
    
    output$ForecastPlot <- renderPlot({
        
        getDataset() %>%
            model(ETS= ETS(price),
                  ARIMA = ARIMA(price)) %>%
            forecast(h = paste0(input$ahead," years")) %>%
            autoplot(getDataset()) +
            labs(title = paste0(input$ahead, " year forcasts for price of ", input$dataset),
                 y="Price",
                 x="Date") +
            theme_light()
    })
    
    output$selection <- DT::renderDataTable({
        
        fit <- getDataset() %>%
            filter(DATE < yearmonth("2014 Dec")) %>%
            model(
                ETS(price),
                ARIMA(price)) %>%
            forecast(h= "5 years")
        
        selection <- fit %>% accuracy(getDataset())
        
        selection
        
    })
    
    output$residuals <- renderPlot({
        getDataset() %>%
            model(ETS(price)) %>%
            gg_tsresiduals() +
            labs(title = "Diagnostics")
        
    })
    
}