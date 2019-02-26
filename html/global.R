library(jsonlite)
library(highcharter)
library(shinyWidgets)
library(httr)

############# HTML FILES ###############
author <- 'www/html/about-author.html'
school <- 'www/html/about-school.html'
btc <- 'www/html/btc.html'

myJSON <- fromJSON('info.json')

############ GLOBAL ####################
widget <- myJSON$appinfo$widget
internalURL <- myJSON$appinfo$internalURL
themes <- myJSON$appinfo$themes
other <- myJSON$appinfo$other
bitcoin <- myJSON$appinfo$BTC

############ WIDGET ####################
widget.type <- widget$type
widget.mainURL <- widget$mainUrl              #This one uses 2 params: theme and internal URL; should be both passed

############ INTERNAL URL #############
internalURL.chart <- internalURL$chart        #This one uses 3 params: fsym vector(1, 1), tsym vector(1, 1), period vector(1, 1); should be passed
internalURL.advanced <- internalURL$advanced  #This one uses 2 params: fsym vector(1, X), tsym vector(1, 1)

############ THEMES ###################
themes.chart.white <- themes$chartWhite       #Should be used only with normal charts
themes.chart.dark <- themes$chartDark         #Should be used only with normal charts
themes.chart.advanced <- themes$advanced      #Should be used only with advanced charts

############ OTHER ####################
other.scrolling <- other$scrolling
other.scrolling.src <- other.scrolling$src
other.scrolling.width <- other.scrolling$width
other.scrolling.height <- other.scrolling$height
other.scrolling.autoscrolling <- other.scrolling$autoscrolling
other.scrolling.marginheight <- other.scrolling$marginheight
other.scrolling.frameborder <- other.scrolling$frameborder
other.scrolling.border <- other.scrolling$border
other.scrolling.style <- other.scrolling$style
other.scrolling.margin <- other.scrolling$margin
other.scrolling.padding <- other.scrolling$padding

############# BTC ########################
btc.mainbtc <- bitcoin$mainBTC
btc.mainbtcavg <- myJSON$appinfo$BTC$mainBTCAvg
btc.charts <- bitcoin$charts
btc.charts.total <- bitcoin$charts$totalbitcoin

############ OTHER CRYPTO CURRENCIES #####
cryptoList <- c('Bitcoin' = 'BTC', 'Ripple' = 'XRP', 'Ethereum' = 'ETH', 'Stellar' = 'XLM', 'Tether' = 'USDT', 'EOS' = 'EOS', 'Litecoin' = 'LTC', 
                'Bitcoin Cash' = 'BCH', 'Bitcoin SV' = 'BSV', 'TRON' = 'TRX',
                'Cardano' = 'ADA', 'Monero' = 'XMR', 'Binance Coin' = 'BNB', 'NEM' = 'XEM', 'Dash' = 'DASH', 'Ethereum Classic' = 'ETC', 
                'NEO' = 'NEO', 'Maker' = 'MKR', 'Dogecoin' = 'DOGE', 'Waves' = 'WAVES', 'Tezos' = 'XTZ', 'True USD' = 'TUSD', 
                'USD//Coin' = 'USDC', 'Bitcoin Gold' = 'BTG', 'OmiseGo' = 'OMG')

currenciesList <- c ('EUR', 'USD', 'JPY', 'GBP', 'HRK', 'CZK', 'DKK', 'HUF', 'PLN', 'RON', 'SEK', 'CHF')


###########################################################
################## HELP FUNCTIONS #########################
###########################################################

# Function to replace strings in JSON file in order to insert the user choices
"%--%" <- function(x, y) {
  
  do.call(sprintf, c(list(x), y))
  
}

# Function to convert integer timestamp dates into POSIxlt format to represent in chart correctly
convertDate <- function(x){
  date <- as.POSIXlt(x,
                     tryFormats = c("%Y-%m-%d %H:%M",
                                    "%Y/%m/%d %H:%M",
                                    "%Y-%m-%d %H:%M",
                                    "%Y/%m/%d %H:%M",
                                    "%Y-%m-%d",
                                    "%Y/%m/%d"), origin = "1970-01-01")
  return(date)
}


#Get the url just in case we want to check the status
getURL <- function(chart = 'total-bitcoins', timespan = '30days', scale = 0, average = F){
  url <- btc.mainbtcavg %--% (c(chart, timespan, scale))
  
  if (!average)
    url <- btc.mainbtc %--% (c(chart, timespan, scale))
  else
    url <- btc.mainbtcavg %--% (c(chart, timespan, scale))
  return(url)
}

#Get the data
getDataFrame <- function(chart = 'total-bitcoins', timespan = '30days', scale = 0, average = F){
  url <- NULL
  
  if (!average)
    url <- btc.mainbtc %--% (c(chart, timespan, scale))
  else
    url <- btc.mainbtcavg %--% (c(chart, timespan, scale))
  
  df <- as.data.frame(fromJSON(url))
  df[, 6] <- datetime_to_timestamp(as.Date(convertDate(df[, 6]), origin = '1970-01-01'))
  
  return(df[c(6, 7)])}


# Get the theme by the user input

############# Themes ###################
hc_themes <- c('538', 'Economist', 'Financial Times', 
               'Dotabuff', 'Flat', 'Simple', 'Elementary', 'Google' , 'Firefox' , 'Monokai' ,
               'Tufte', 'Sparkline' , 'Grid Light' , 'Sand Signika' , 'Dark Unica' ,
               'Chalk', 'Hand Drawn', 'Superhero' ,'No theme' )

getTheme <- function(input){
  theme <- switch(input,
                  '538' = hc_theme_538(),
                  'Economist' = hc_theme_economist(),
                  'Financial Times' = hc_theme_ft(),
                  'Dotabuff' = hc_theme_db(),
                  'Flat' = hc_theme_flat(),
                  'Simple' = hc_theme_smpl(),
                  'Elementary' = hc_theme_elementary(),
                  'Google' = hc_theme_google(),
                  'Firefox' = hc_theme_ffx(),
                  'Monokai' = hc_theme_monokai(),
                  'Tufte' = hc_theme_tufte(),
                  'Sparkline' = hc_theme_sparkline(),
                  'Grid Light' = hc_theme_gridlight(),
                  'Sand Signika' = hc_theme_sandsignika(),
                  'Dark Unica' = hc_theme_darkunica(),
                  'Chalk' = hc_theme_chalk(),
                  'Hand Drawn' = hc_theme_handdrawn(),
                  'Superhero' = hc_theme_superheroes(),
                  'No theme' = hc_theme_null())
  return(theme)
}

