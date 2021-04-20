library(estudy2)
library(plotly)
library(TSstudio)
library(jsonlite)
library(plyr)
library(zoo)
library(httr)
library(plotly)
library(jsonlite)
library(dplyr)
library(tidyr)
library(Quandl)
library(stringr)


ISIN <- readxl::read_xlsx("./Data/ISIN_list.xlsx") %>% select(ISIN) 

links <- c()
rawDta <- c()
for( i in ISIN){ 
  links <- paste0('https://zse.hr/json/securityHistory/', i,
                  '/2019-01-01/2021-04-13/hr?trading_model_id=ALL')
}
for (i in links){
rawDta[[i]] <- fromJSON(content(GET(i), as = "text", encoding = "UTF-8")) 
}

stockDF <- lapply(rawDta, '[[', "rows") %>% bind_rows()



TOURISMdta <- stockDF %>% select(Date = date, Close = last_price, url) %>%
  mutate(Date = gsub("[.]$","", Date)) %>%
  mutate(Ticker = str_sub(.$url,-37,-34)) %>%
  mutate(Close = as.numeric(gsub("(.*),.*", "\\1", Close))) %>%
  select(-url) %>%
  group_by(Ticker) %>%
  mutate(n = n()) %>%
  ungroup() %>% 
  filter(n > 100) %>%
  select(-n) %>%
  group_by(Ticker) %>%
  distinct(Date, .keep_all=TRUE) %>%
  ungroup() %>%
  tidyr::spread(Ticker, Close, fill=0) %>%
  mutate( Date = as.Date(Date,"%d.%m.%Y")) %>%
  arrange(desc(Date))


TOURISMdta <- zoo(TOURISMdta[,-1], order.by = TOURISMdta$Date)


source("./Secret/Passkey.R")
Crobex <- Quandl("ZAGREBSE/CROBEX", type = "zoo",collapse = "daily",start_date = "2019-01-02", end_date = Sys.Date())



rates <- get_rates_from_prices(TOURISMdta,
                               quote = "Close",
                               multi_day = TRUE,
                               compounding = "continuous")

ratesDF <- as.data.frame(rates)

ratesDF[sapply(ratesDF, is.infinite)] <- NA
ratesDF[sapply(ratesDF, is.nan)] <- NA
ratesDF[sapply(ratesDF, is.na)] <- 0
TOURISMdta <- zoo(ratesDF)



rates_indx <- get_rates_from_prices(Crobex, 
                                    quote = "Close",
                                    multi_day = TRUE,
                                    compounding = "continuous")

colnames(rates_indx) <- "Crobex"


securities_returns <- apply_market_model(
  rates = rates,
  regressor = rates_indx,
  same_regressor_for_all = TRUE,
  market_model = "sim",
  estimation_method = "ols",
  estimation_start = as.Date("2019-01-01"),
  estimation_end = as.Date("2020-01-23")
)

parametric_tests(list_of_returns = securities_returns,
                 event_start = as.Date("2020-02-24"),
                 event_end = as.Date("2020-04-14"))


nonparametric_tests(list_of_returns = securities_returns,
                    event_start = as.Date("2020-02-24"),
                    event_end = as.Date("2020-04-14"))







