library(tidyverse)

tries <- 150 #number of tries, low for graphing purposes
initial_value <- 100 #initial portfolio value


####
# User interface#
ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      h2("Setup"),
      helpText("Select a portfolio return, volatility, and time-span."),
      
      sliderInput("r", h3("Choose Return"),
                  min = 0, max = 20, value = 7),
      sliderInput("vol", h3("Choose Volatility"),
                  min = 0, max = 50, value = 5),
      sliderInput("time_periods", h3("Choose Time-Span"),
                  min = 0, max = 100, value = 15)
      
      ),
      
      mainPanel(
        h1("Value-At-Risk Calculator"),
        p("Find out what your 5% worst-case scenario is in just a few clicks!"), 
        br(),
        p("Your portfolio's summary statistics (assuming an initial investment of 100):"),
        verbatimTextOutput("summary"),
        br(),
        p("100 simulated portfolios, plotted:"),
        plotOutput("plot")
        )
  )
)

# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({ #update this value whenever the original widget changes.
    portfolio_value <- data.frame(matrix(0, ncol = tries, nrow = input$time_periods)) #
    portfolio_value[1, ] <- initial_value #sets initial value of portfolio
    
    for (i in c(1:tries)) { #full simulation, repeats portfolio simulation 500,000+ times
      monthly_returns <- rnorm(input$time_periods, input$r / 100, input$vol / 100) #generates random returns
      
      for (j in c(2:input$time_periods)) { #portfolio simulation
        portfolio_value[j, i] <- portfolio_value[j-1, i]*(1 + monthly_returns[j-1])
      }
    }
    
    portfolio_value$median <- apply(portfolio_value, 1, median) #median of every row
    portfolio_value$var <- apply(portfolio_value, 1, quantile, 0.05)#VAR 
    portfolio_value$time <- c(1:input$time_periods) #adding time index
    
    portfolio_plot <- gather(portfolio_value, run, value, -time) #changing from wide to long
    data.frame(portfolio_plot)
  })
  
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- dataInput()
    summary(dataset$value)
  })
  
  output$plot <- renderPlot({
    
    ggplot(dataInput(), aes(time, value, group = run)) + 
      geom_line(alpha = 0.4, color = "grey") +  
      geom_line(data = filter(dataInput(), run == "var"), alpha = 0.8, color = "red") + 
      guides(color = FALSE) +
      labs(x = "Time (Years)", y = "Value")
    
  })
  
  
}

shinyApp(ui, server)