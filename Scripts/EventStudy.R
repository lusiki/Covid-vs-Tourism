library(estudy2)
library(plotly)
library(TSstudio)
library(jsonlite)
library(plyr)
library(zoo)


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


th <-



TOURISMdta <-  join_all(list(ARENA,MAISTRA,VALAMAR), by='Date', type='left') %>% drop_na()
TOURISMdta <- zoo(TOURISMdta[,-1], order.by = TOURISMdta$Date)





Quandl.api_key("jvwknzKzNdiuqGPCyXcT")
Crobex <- Quandl("ZAGREBSE/CROBEX", type = "zoo",collapse = "daily",start_date = "2019-01-02", end_date = Sys.Date())



rates <- get_rates_from_prices(proba,
                               quote = "Close",
                               multi_day = TRUE,
                               compounding = "continuous")

rates_indx <- get_rates_from_prices(Crobex, 
                                    quote = "Close",
                                    multi_day = TRUE,
                                    compounding = "continuous")




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




library(plotly)
library(TSstudio)
library(jsonlite)

today <- Sys.Date()
tm <- seq(0, 600, by = 10)
x <- today - tm
y <- rnorm(length(x))

ts_plot(CrobexTur)
ts_plot(Crobex)


library(httr)
r <- GET("https://zse.hr/json/securityHistory/HRARNTRA0004/2019-10-23/2021-04-13/hr?trading_model_id=ALL")
raw <- str(content(r, "parsed"))
bin <- str(content(r,"parsed"))

response <- content(r, as = "text", encoding = "UTF-8")

df <- fromJSON(response, flatten = TRUE) %>% 
  data.frame()







