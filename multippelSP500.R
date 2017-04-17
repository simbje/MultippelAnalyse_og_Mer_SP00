



## Prerequisites ----------------------------------------------------------------
library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(tidyverse)
library(xlsx)

# Web Scraping: Get the List of S&P500 Stocks ----------------------------------

# Web-scrape S&P500 stock list
sp_500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
  html_node("table.wikitable") %>%
  html_table() %>%
  select(`Ticker symbol`, Security, `GICS Sector`, `GICS Sub Industry`) %>%
  as_tibble()
# Format names
names(sp_500) <- sp_500 %>%
  names() %>%
  str_to_lower() %>%
  make.names()


# Creating Functions to Map ----------------------------------------------------

get_stock_prices <- function(ticker, return_format = "tibble", ...) {
  # Get stock prices
  stock_prices_xts <- getSymbols(Symbols = ticker, auto.assign = FALSE, ...)
  # Rename
  names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  # Return in xts format if tibble is not specified
  if (return_format == "tibble") {
    stock_prices <- stock_prices_xts %>%
      as_tibble() %>%
      rownames_to_column(var = "Date") %>%
      mutate(Date = ymd(Date))
  } else {
    stock_prices <- stock_prices_xts
  }
  stock_prices
}

get_log_returns <- function(x, return_format = "tibble", period = 'daily', ...) {
  # Convert tibble to xts
  if (!is.xts(x)) {
    x <- xts(x[,-1], order.by = x$Date)
  }
  # Get stock prices
  log_returns_xts <- periodReturn(x = x$Adjusted, type = 'log', period = period, ...)
  # Rename
  names(log_returns_xts) <- "Log.Returns"
  # Return in xts format if tibble is not specified
  if (return_format == "tibble") {
    log_returns <- log_returns_xts %>%
      as_tibble() %>%
      rownames_to_column(var = "Date") %>%
      mutate(Date = ymd(Date))
  } else {
    log_returns <- log_returns_xts
  }
  log_returns
}

#Get financial data (fail)
#get_fin <- function (fin, return_format = "tibble"){
#  
#  what_metrics <- yahooQF(c("Price/Sales", 
#                            "P/E Ratio",
#                            "Price/Book"))
#  getQuote(paste(ticker.symbol, sep="", collapse=";"), what=what_metrics)


# Mapping the Functions --------------------------------------------------------
{

from <- "2014-01-01"
to   <- today()
sp_500 <- sp_500 %>%
  mutate(
    stock.prices = map(ticker.symbol,
                       function(.x) get_stock_prices(.x,
                                                     return_format = "tibble",
                                                     from = from,
                                                     to   = to)
    ),
    log.returns  = map(stock.prices,
                       function(.x) get_log_returns(.x, return_format = "tibble")),
    mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
    sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
    n.trade.days = map_dbl(stock.prices, nrow)
    
    
    )
}


# 

what_metrics <- yahooQF(c("Price/Sales", 
                          "P/E Ratio",
                          "Price/Book"))


mult <- getQuote(paste(sp_500$ticker.symbol, sep =".", collapse=";"), what=what_metrics)


#skjer en feil her Gjør om tickerne til tall ikkje gjør det

mult <- mult[-c(1)]
mult <- tbl_df(mult)
mult<-mutate(mult, id = rownames(mult))
mult <- lapply(mult[c(1:3)],as.numeric)
mult <- data.frame(mult,sp_500$ticker.symbol)
names(mult)
mult <- tbl_df(mult)
mult <- rename(mult, ticker.symbol = sp_500.ticker.symbol)

sp_500<-sp_500 %>%
right_join(mult, by = "ticker.symbol")

sp_500 <- arrange(sp_500, gics.sector, gics.sub.industry)


MeanMultSector<- sp_500 %>%
  na.omit() %>%
  group_by(gics.sector) %>%
    summarise(meanPE = mean(P.E.Ratio), MeanPS = mean(Price.Sales), MeanPB = mean(Price.Book))
  
MeanMultIndustry<- sp_500 %>%
  na.omit() %>%
  group_by(gics.sub.industry) %>%
  summarise(meanPE = mean(P.E.Ratio), MeanPS = mean(Price.Sales), MeanPB = mean(Price.Book))
View(sp_500)


MeanMultSector
MeanMultIndustry

underVAL <- sp_500 %>%
  na.omit() %>%
  group_by(gics.sub.industry) %>%
  select(company,gics.sub.industry,ticker.symbol,P.E.Ratio,Price.Book)%>%
  filter(P.E.Ratio < mean(P.E.Ratio), Price.Book < mean(Price.Book))
underVAL


#excel
  
  write.xlsx(data.frame(MeanMultSector), file = "C:\\Users\\Simen\\Desktop\\M.xlsx", sheetName = "MeanIndustry")
  write.xlsx(data.frame(underVAL), file = "C:\\Users\\Simen\\Desktop\\M.xlsx", sheetName = "Undervalued", append = TRUE)
  



ggplot(MeanMultSector) + geom_bar(aes(meanPE))


??geom_bar

###plotting?








