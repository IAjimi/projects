### Loading Packages ####
library(shiny)
library(tidyverse)
library(lubridate)
library(quantmod)
library(DT)
library(plotly)

###Shiny ####
# User interface#
ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      h1("Return, Volatilities & Correlations Calculator"),
      p("Compute the returns, volatilities & correlations of your portfolio in just a few clicks!"), 
      p("Note: loading more than 5 stocks at once will slow down the process, due to underlying query
        restrictions."), 
      
      h3("Setup"),
      helpText("Select index funds from the list below or add  desired stocks using their stock
               ticker."),
      
      checkboxGroupInput("checkGroup", 
                         strong("Default Index Funds:"), 
                         choices = list("Dow Jones" = "^DJI", 
                                        "SP 500" = "^GSPC", 
                                        "Russell 2000" = "^RUT",
                                        "Nasdaq" = "^NDX",
                                        "Gov Bond Index (CPTNX)" = "CPTNX"),
                         selected = c("^DJI", "^GSPC","^RUT", "^NDX", "CPTNX") ),
      
      textInput("text", strong("Additional Stocks:"), 
                value = NULL) ,
      
      dateRangeInput("dates", strong("Date range:"),
                     start = "2019-01-01",
                     end   = Sys.Date()),
      
      br(),
      h3('Methodology'),
      p("The summary statistics are computed using the day-on-day log difference
                                of stock prices (i.e., daily returns). The volatility is computed by taking the
                                square root of the number of open market days x the standard deviation in daily returns.")
    ),
    
    
    mainPanel(
      
    tabsetPanel(
      
      type = "tabs",
      tabPanel("Latest Numbers", 
               h3("Latest Close:"),
               DT::dataTableOutput("latest_table")
      ),
      
      tabPanel("Charts", 
               h3('Historical Chart (Indexed)'),
               plotlyOutput("historical_chart")
      ),
      
      tabPanel("Summary Stats.", 
                 h3("Daily Returns Summary Statistics & Volatility:"),
                 DT::dataTableOutput("summary_table"),
                 br(),
                 h3("Correlations:"),
                 DT::dataTableOutput("correl")
      )
    )
  )
    
    )
  )


# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({ 
    text_input <- input$text %>% str_split(',') %>% flatten_chr() #retrieve comma-sep list input
    stocks <- c(input$checkGroup, text_input)
    date1 <- input$dates[1]
    date2 <- input$dates[2]
    
    e <- new.env() #creates new environment
    
    s <- getSymbols(stocks, src="yahoo",  from = date1, to = date2, env = e) #quantmode gets stock data from yahoo
    df <- do.call(merge, eapply(e, Cl)) #applies loop over environment
    #only keeps closing prices of the named stocks, merges df so all stocks are in one file
    
    df <- data.frame(df) #convert result of query into dataframe
    
    for (i in c(1:ncol(df))){
      assign(paste(s[i], "Return", sep = "_"), c(diff(log(df[, i]))) , envir = e ) #calc return for every stock
      assign(paste(s[i], "vol", sep = "_"),  sqrt(256)*sd(c(diff(log(df[, i])))) , envir = e ) #calc vol
    }
    
    ## Return / Vol Summary
    ### getting returns
    return_vectors <- ls(e)[str_detect(ls(e), "Return")] #subsets environment for volatilies
    assign("returns", mget(return_vectors,  envir= e), envir = globalenv()) #returns vol for everything
    
    summary_returns <- returns %>% map(summary) %>% do.call('rbind', .) #gets  summary vals for the returns
    row.names(summary_returns) <- str_replace_all(row.names(summary_returns), '(\\^)|(_Return)', '') #fixing row names
    
    ### getting volatilities
    vol_vectors <- ls(e)[str_detect(ls(e), "vol")] #subsets environment for volatilies
    assign("volatilities", mget(vol_vectors,  envir= e), envir = globalenv()) #returns vol for everything
    volatilities_col <- volatilities %>% do.call('rbind', .) 
    row.names(volatilities_col) <- str_replace_all(row.names(volatilities_col), '(\\^)|(_vol)', '') #fixing row names
    
    ### combining the two
    summary_stats <- summary_returns %>% cbind(volatilities_col) %>% as.data.frame() %>% rename(Volatility = V7)
    
    ##correlation matrix
    return_vectors <- ls(e)[str_detect(ls(e), "Return")] #subsets enviroment for return vectors
    big_corr_matrix_data <- data.frame(mget(return_vectors,  envir= e)) #greates a df made up of the return vectors
    big_corr_matrix <- cor(big_corr_matrix_data) #gets correlation of df
    big_corr_matrix <- as.data.frame(big_corr_matrix) 
    names(big_corr_matrix) <- str_replace_all(return_vectors, '(\\^)|(_Return)', '')  #adds col names
    row.names(big_corr_matrix) <- names(big_corr_matrix) #adds row names
    assign("corr_matrix", round(big_corr_matrix, 3), envir = globalenv()) #returns matrix!
    
    ##saves output as list to display in tables
    list(summary_stats, corr_matrix, df)
  })
  
  
  
  output$latest_table <- renderDataTable({
    result <- dataInput()
    DT::datatable(last(result[[3]]))
    
  })
  
  output$summary_table <- renderDataTable({
    result <- dataInput()
    DT::datatable(result[[1]] %>% apply(2, round, 3) )
    
  })
  
  output$correl <- renderDataTable({
    result <- dataInput()
    DT::datatable(result[[2]])
    
  })
  
  output$historical_chart <- renderPlotly({
    result <- dataInput()
    
    chart_df <- result[[3]] %>%
      mutate(Date = row.names(.) %>% as.Date()) %>%
      gather(stock_name, stock_price, - Date) %>% 
      mutate(stock_name = str_replace(stock_name, '.Close', '')) %>%
      split(.$stock_name) %>%
      map(mutate, stock_price_index = 100 * stock_price / first(stock_price)) %>%
      do.call('rbind', .)
    
    p <- chart_df %>% 
      mutate(Stock = paste(stock_name, "\n",
                           'Date: ', Date, '\n',
                           'Indexed Stock Value: ', round(stock_price_index, 2), 
                           sep = ''
        
      )) %>%
      ggplot(aes(Date, stock_price_index, color = stock_name, group = stock_name, label = Stock)) +
      geom_path() +
      #geom_path(alpha = 0.75) +
      #geom_smooth(method = lm, formula = y ~ splines::bs(x, 40), se = F) +
      scale_x_date(breaks = '3 month', date_labels = '%b %y') +
      labs(x = '', y = 'Indexed Stock Price', color = 'Stock',
           caption = 'Indexed to the first day of chosen period.') +
      theme_minimal() 
    
    p %>% ggplotly(tooltip = 'Stock')
    
    
  })
  
  
}

shinyApp(ui, server)
