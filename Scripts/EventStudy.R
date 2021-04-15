library(estudy2)
library(plotly)
library(TSstudio)
library(jsonlite)
library(plyr)
library(zoo)
library(httr)
library(plotly)
library(TSstudio)
library(jsonlite)
library(dplyr)
library(tidyr)
library(Quandl)

r <- GET("https://zse.hr/json/securityHistory/HRARNTRA0004/2019-01-01/2021-04-13/hr?trading_model_id=ALL")
response <- content(r, as = "text", encoding = "UTF-8")
ARENA <- fromJSON(response, flatten = TRUE) %>% 
  data.frame() %>%
  select(Date = rows.date, ARNT = rows.last_price ) %>%
  mutate( Date = gsub("[.]$","", Date)) %>%
  mutate( Date = as.Date(Date,"%d.%m.%Y"),
          ARNT = as.numeric(gsub("(.*),.*", "\\1", ARNT)))

 

r <- GET("https://zse.hr/json/securityHistory/HRMAISRA0007/2019-01-01/2021-04-13/hr?trading_model_id=ALL")
response <- content(r, as = "text", encoding = "UTF-8")
MAISTRA <- fromJSON(response, flatten = TRUE) %>% 
  data.frame() %>%
  select(Date = rows.date, MAIS = rows.last_price ) %>%
  mutate( Date = gsub("[.]$","", Date)) %>%
  mutate( Date = as.Date(Date,"%d.%m.%Y"),
          MAIS = as.numeric(gsub("(.*),.*", "\\1", MAIS)))


r <- GET("https://zse.hr/json/securityHistory/HRRIVPRA0000/2019-01-01/2021-04-13/hr?trading_model_id=ALL")
response <- content(r, as = "text", encoding = "UTF-8")
VALAMAR <- fromJSON(response, flatten = TRUE) %>% 
  data.frame() %>%
  select(Date = rows.date, RIVP = rows.last_price ) %>%
  mutate( Date = gsub("[.]$","", Date)) %>%
  mutate( Date = as.Date(Date,"%d.%m.%Y"),
          RIVP = as.numeric(gsub("(.*),.*", "\\1", RIVP)))



TOURISMdta <- left_join(ARENA, MAISTRA,VALAMAR, by=c("Date")) %>% 
  inner_join(.,VALAMAR, by=c("Date")) %>%
  distinct(Date, .keep_all=TRUE) 

TOURISMdta <- zoo(TOURISMdta[,-1], order.by = TOURISMdta$Date)



Quandl.api_key("jvwknzKzNdiuqGPCyXcT")
Crobex <- Quandl("ZAGREBSE/CROBEX", type = "zoo",collapse = "daily",start_date = "2019-01-02", end_date = Sys.Date())



rates <- get_rates_from_prices(TOURISMdta,
                               quote = "Close",
                               multi_day = TRUE,
                               compounding = "continuous")

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







