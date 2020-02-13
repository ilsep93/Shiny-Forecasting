ui <- fluidPage(
    
    # Application title
    titlePanel("Forecasting Prices 1980-2019"),
    
    br(),
    
    h5("Author: Ilse Paniagua"),
    
    br(),
    
    sidebarLayout(
        
        sidebarPanel(
            selectInput("dataset", "Dataset:",
                        list("Chocolate Chip Cookies" = "Cookies", 
                             "Bread" = "Bread",
                             "Flour" = "Flour")),
            numericInput("ahead", "Years to Forecast Ahead:", 2),
            
            submitButton("Update View")
        ),
        
        
        
        # Show the caption and forecast plots
        mainPanel(
            h3(textOutput("caption")),
            
            tabsetPanel(
                tabPanel("Forecast", plotOutput("ForecastPlot")), 
                tabPanel("Timeseries Decomposition", plotOutput("dcompPlot")),
                tabPanel("Model Selection", dataTableOutput("selection")),
                tabPanel("Residual Diagnostics", plotOutput("residuals"))
            )
        )
    ))

