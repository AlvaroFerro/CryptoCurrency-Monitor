library(shiny)
library(ggplot2)
library(MASS)
library(magrittr)
library(dplyr)
library(shinyWidgets)
library(shinycssloaders)
library(shinymaterial)
library(rsconnect)
library(cryptor)
library(forecast)

shinyServer(function(input, output, session) {
  
  ############## REACTIVE ###################
  values <- reactiveValues()
  
  ############## MAIN FUNCTIONS ###############
  checkURLstatus <- function(){
    if (GET(getURL())$status_code != 200)
      sendSweetAlert(session = session, 'Error', 
                     text = 'Opss, it seems that your internet connection is down or the HTTP request has failed. Meanwhile why not having a coffee?',
                     type = 'error')
  }
  
  showHighChart <- function(week, log, chart, theme, period, chartType, title = ''){
    
    checkURLstatus()
    
    df <- NULL
    if (week)
      df <- getDataFrame(timespan = period, average = T, chart = chart)
    else
      df <- getDataFrame(timespan = period, chart = chart)
    
    hc <- highchart()
    
    if (log) {
      hc <- hchart(df, chartType,  hcaes(values.x, round(log(values.y), 2))) %>%
        hc_xAxis(title = list(text = 'Date'), type = "datetime") %>%
        hc_title(text = title) %>%
        hc_yAxis(title = 'Value') %>%
        hc_add_theme(getTheme(theme))
    } 
    else{
      hc <- hchart(df, chartType,  hcaes(values.x, round(values.y, 2))) %>%
        hc_xAxis(title = list(text = 'Date'), type = "datetime") %>%
        hc_title(text = title) %>%
        hc_yAxis(title = 'Value') %>%
        hc_add_theme(getTheme(theme))
    }
    
    hc
  }
  
  showForecast <- function(crypto, currency, type, confLevel, fTime){
    price <- get_historical_price(crypto, currency)
    
    price <- price[, c('time', type)]
    priceDF <- as.data.frame(price)
    
    fore <- forecast(auto.arima(priceDF[2]), level = confLevel, h = fTime)
    
    fHC <- hchart(fore)
    
    fHC
    
  }
  
  showUserInterface <- function(period, log, week, type, theme){
    fluidRow(
      column(3,
             h3('Time period'),
             h6('Time period to show in graph'),
             selectInput(period, label = NULL, choices = c('30 days' = '30days', '60 days' = '60days', '180 days' = '180days', '1 year' = '1year', '2 years' = '2years', 'All' = 'all'), multiple = F)
      ),
      column(3,
             h3('Scale'),
             h6('Data scale to show in graph'),
             materialSwitch(inputId = log, label = 'Logaritmic scale', status = 'info', righ = T, value = F),
             h3('Week Average'),
             h6('Show data by weekly average'),
             materialSwitch(inputId = week, value = F, status = 'info', label = 'Week average', right = T)
      ),
      column(3,
             h3('Chart Type'),
             h6('Type of chart to show'),
             selectInput(type, label = NULL, choices = c('Line' = "line", 'Column' = "column", 'Bar' = "bar", 'Spline' = "spline", 'Area' = 'areaspline'))
      ),
      column(3,
             h3('Theme'),
             h6('Almost magic'),
             selectInput(inputId = theme, label = NULL, choices = hc_themes, multiple = F)
             #actionBttn('helptotalcirc', label = 'Do you need help?', style = 'stretch', color = 'primary')
      )
    )
  }
  
  ############## MAIN OUTPUTS ###############
  output$btc <- renderUI({
    #Llamamos a todo al pulsar este botón
    #react()
    
    #Todo lo que se escriba aque se va a completar en la ui en la parte de abajo de ui htmlvideo
    #Se pueden devolver todas las cosas que se quieran
    #Para devolver muchas cosas se tiene que crear una lista si o si
    list(
      #tags$iframe(width = 560, height = 315, src = paste0("https://www.youtube.com/embed/", isolate(input$idvideo))),
      #tags$iframe(src = 'https://giphy.com/embed/3orif3VHjBeYBDTGlG', width = "480", height = "362")
      #tags$head(includeScript('script.js'))
      #includeHTML('test.html')
    )
  })
  
  output$advanced <- renderUI({

      fluidRow(
        column(12,
               h3('Period'),
               br(),
               dropdownButton(
                 tags$h3("List of Input"),
                 selectInput(inputId = 'xcol', label = 'X Variable', choices = names(iris)),
                 selectInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]]),
                 sliderInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9),
                 circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                 tooltip = tooltipOptions(title = "Click to see inputs !")
               ),
               tags$script(type = widget.type, HTML(widget.mainURL %--% c(themes.chart.advanced, internalURL.advanced %--% c('BTC', 'EUR,USD'))))
               
               
          
        )
      )

    
  })
  
  output$totalcirc <- renderUI({
    
    showUserInterface(period = 'period', log = 'logSwitch', 
                      week = 'weekSwitch', type = 'charttype', theme = 'themeid')
  })
  
  output$marketPrice <- renderUI({
    
    showUserInterface(period = 'periodMK', log = 'logSwitchMK', 
                      week = 'weekSwitchMK', type = 'charttypeMK', theme = 'themeidMK')
    
  })
  
  output$marketCapital <- renderUI({

    showUserInterface(period = 'periodMKCap', log = 'logSwitchMKCap', 
                      week = 'weekSwitchMKCap', type = 'charttypeMKCap', theme = 'themeidMKCap')
    
  })
  
  output$usdVolumen <- renderUI({
    
    showUserInterface(period = 'periodUsdVol', log = 'logSwitchUsdVol', 
                      week = 'weekSwitchUsdVol', type = 'charttypeUsdVol', theme = 'themeidUsdVol')
    
    
  })
  
  output$transPerDay <- renderUI({
    
    showUserInterface(period = 'periodTrans', log = 'logSwitchTrans', 
                      week = 'weekSwitchTrans', type = 'charttypeTrans', theme = 'themeidTrans')
    
  })
  
  output$moreCurrenciesMenu <- renderUI({
    fluidRow(
      column(4,
             h3('Coin'),
             h6('Crypto coin to show price history for'),
             pickerInput(inputId = "pickerCurrencies", label = NULL, choices = sort(cryptoList), 
                         multiple = TRUE, options = list('max-options' = 5, 'max-options-text' = 'Max 5 options')),
             actionBttn('updateCurrencies', label = 'Update', style = 'stretch', color = 'primary')
      ),
      column(4,
             h3('Option'),
             h6('Choose the option to display'),
             pickerInput(inputId = 'pickerOptions', label = NULL, choices = c('Open' = 'open', 'Close' = 'close', 
                                                                              'High' = 'high', 'Low' = 'low'), multiple = FALSE)
             ),
      column(4,
             h3('Currency'),
             h6('Choose the currency to display'),
             pickerInput(inputId = 'currencyOptions', label = NULL, choices = currenciesList, multiple = FALSE)
      )
    )
  })
  
  output$forecastUI <- renderUI({
    fluidRow(
      column(2,
             h3('Coin'),
             h6('Crypto coin to show forecast for'),
             pickerInput(inputId = "pickerCurrenciesForecast", label = NULL, choices = sort(cryptoList), 
                         multiple = FALSE)
      ),
      column(3,
             h3('Option'),
             h6('Choose the option to display'),
             pickerInput(inputId = 'pickerOptionsForecast', label = NULL, choices = c('Open' = 'open', 'Close' = 'close', 
                                                                              'High' = 'high', 'Low' = 'low'), multiple = FALSE)
      ),
      column(2,
             h3('Currency'),
             h6('Choose the currency to display'),
             pickerInput(inputId = 'currencyOptionsForecast', label = NULL, choices = currenciesList, multiple = FALSE)
      ),
      column(3,
             h3('Time'),
             h6('Choose a period of time to perform forecast'),
             sliderInput(inputId = 'sliderTimeForecast', label = 'Time', min = 1, max = 35, step = 1, value = 5)
      ),
      column(2,
             h3('Confidence level'),
             h6('Choose a confidence level'),
             sliderInput(inputId = 'sliderConfForecast', label = 'Level', min = 1, max = 35, step = 1, value = 15)
      )
    )
  })
  
  observeEvent(input$updateCurrencies, {
    
    #We have to pre-set the X axis with the date
    z <- get_historical_price('XRP', 'USD', limit = 500)
    hcPlot <- highchart() %>%
      hc_chart(type = "spline") %>%
      hc_xAxis(categories = z$time, format = '{value:%Y-%m-%d}')
    
    lengthInput <- length(input$pickerCurrencies)
    options <- input$pickerOptions
    
    if (lengthInput > 0){
      for (i in 1:lengthInput) {
        #Get Values for each element on selectInput
        historical_price <- get_historical_price(input$pickerCurrencies[i], input$currencyOptions, limit = 500)
        
        #Create one var in each iteration to save DataFrame
        variable <- paste('Df_', i, sep = '')
        
        #Assign DataFrame to var to call it later
        assign(variable, historical_price)
      }
    }
    
    #Adding data to chart
    if (lengthInput == 1) {
      hcPlot <- hcPlot %>% hc_add_series(data = Df_1[[options]], name = input$pickerCurrencies[1])
    }
    else if (lengthInput == 2) {
      hcPlot <- hcPlot %>% hc_add_series(data = Df_1[[options]], name = input$pickerCurrencies[1])
      hcPlot <- hcPlot %>% hc_add_series(data = Df_2[[options]], name = input$pickerCurrencies[2])
    }
    else if (lengthInput == 3) {
      hcPlot <- hcPlot %>% hc_add_series(data = Df_1[[options]], name = input$pickerCurrencies[1])
      hcPlot <- hcPlot %>% hc_add_series(data = Df_2[[options]], name = input$pickerCurrencies[2])
      hcPlot <- hcPlot %>% hc_add_series(data = Df_3[[options]], name = input$pickerCurrencies[3])
    }
    else if (lengthInput == 4) {
      hcPlot <- hcPlot %>% hc_add_series(data = Df_1[[options]], name = input$pickerCurrencies[1])
      hcPlot <- hcPlot %>% hc_add_series(data = Df_2[[options]], name = input$pickerCurrencies[2])
      hcPlot <- hcPlot %>% hc_add_series(data = Df_3[[options]], name = input$pickerCurrencies[3])
      hcPlot <- hcPlot %>% hc_add_series(data = Df_4[[options]], name = input$pickerCurrencies[4])
    }
    else if (lengthInput == 5) {
      hcPlot <- hcPlot %>% hc_add_series(data = Df_1[[options]], name = input$pickerCurrencies[1])
      hcPlot <- hcPlot %>% hc_add_series(data = Df_2[[options]], name = input$pickerCurrencies[2])
      hcPlot <- hcPlot %>% hc_add_series(data = Df_3[[options]], name = input$pickerCurrencies[3])
      hcPlot <- hcPlot %>% hc_add_series(data = Df_4[[options]], name = input$pickerCurrencies[4])
      hcPlot <- hcPlot %>% hc_add_series(data = Df_5[[options]], name = input$pickerCurrencies[5])
    }
    
    hcPlot <- hcPlot %>% hc_add_theme(hc_theme_538())
    
    #We save the value in a reactiveValue to display
    values$chart <- hcPlot
  })
  
  observeEvent(input$buttontest,
               {
                 sendSweetAlert(session = session, title = 'Hoooola', type = 'info')
               })
  
  ############## PLOTS ###############
  output$btccirc <- renderHighchart({
    
    showHighChart(week = input$weekSwitch, 
                  log = input$logSwitch, 
                  chart = 'total-bitcoins', 
                  theme = input$themeid,
                  period = input$period,
                  chartType = input$charttype,
                  title = 'Total bitcoin circulation')
  })
  
  output$mkPriceUsd <- renderHighchart({
    
    showHighChart(week = input$weekSwitchMK, 
                  log = input$logSwitchMK, 
                  chart = 'market-price', 
                  theme = input$themeidMK,
                  period = input$periodMK,
                  chartType = input$charttypeMK,
                  title = 'Market Price (USD)')
  })
  
  output$mkCapital <- renderHighchart({
    
    showHighChart(week = input$weekSwitchMKCap, 
                  log = input$logSwitchMKCap, 
                  chart = 'market-cap', 
                  theme = input$themeidMKCap,
                  period = input$periodMKCap,
                  chartType = input$charttypeMKCap,
                  title = 'Market Capitalization')
  })
  
  output$usdVol <- renderHighchart({
    
    showHighChart(week = input$weekSwitchUsdVol, 
                  log = input$logSwitchUsdVol, 
                  chart = 'trade-volume', 
                  theme = input$themeidUsdVol,
                  period = input$periodUsdVol,
                  chartType = input$charttypeUsdVol,
                  title = 'USD Exchange Trade Volume')
  })
  
  output$transDay <- renderHighchart({
    
    showHighChart(week = input$weekSwitchTrans, 
                  log = input$logSwitchTrans, 
                  chart = 'n-transactions', 
                  theme = input$themeidTrans,
                  period = input$periodTrans,
                  chartType = input$charttypeTrans,
                  title = 'Confirmed Transactions Per Day')
  })
  
  output$moreCurrencies <- renderHighchart({
    values$chart
  })
  
  output$forecast <- renderHighchart({
    showForecast(crypto = input$pickerCurrenciesForecast,
                 currency = input$currencyOptionsForecast,
                 type = input$pickerOptionsForecast,
                 confLevel = input$sliderConfForecast,
                 fTime = input$sliderTimeForecast)
  })
  
})
