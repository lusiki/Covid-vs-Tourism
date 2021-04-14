library(estudy2)
library(plotly)
library(TSstudio)
library(jsonlite)
library(plyr)


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


r <- GET("https://zse.hr/json/securityHistory/HRRIVPRA0000/2019-10-23/2021-04-13/hr?trading_model_id=ALL")
response <- content(r, as = "text", encoding = "UTF-8")
VALAMAR <- fromJSON(response, flatten = TRUE) %>% 
  data.frame() %>%
  select(Date = rows.date, RIVP = rows.last_price ) %>%
  mutate( Date = gsub("[.]$","", Date)) %>%
  mutate( Date = as.Date(Date,"%d.%m.%Y"),
          RIVP = as.numeric(gsub("(.*),.*", "\\1", RIVP)))


TOURISMdta <-  join_all(list(ARENA,MAISTRA,VALAMAR), by='Date', type='left') %>% drop_na()
  

Quandl.api_key("jvwknzKzNdiuqGPCyXcT")
Crobex <- Quandl("ZAGREBSE/CROBEX", type = "zoo",collapse = "daily",start_date = "2019-01-01", end_date = Sys.Date())

CrobexKonst <- Quandl("ZAGREBSE/CROBEXKONSTRUKT",type = "zoo",collapse = "daily",start_date = "2019-01-01", end_date = Sys.Date())
Crobex <- Quandl("ZAGREBSE/ARNTRA", type = "zoo",collapse = "daily",start_date = "2019-01-01", end_date = Sys.Date())


proba <- merge(CrobexTur, CrobexKonst, all = FALSE)

oil <- Quandl("OPEC/ORB",type = "xts",collapse = "daily",start_date = "2019-01-01", end_date = Sys.Date())
gold <- Quandl("LBMA/GOLD", type = "xts", collapse = "daily", start_date = "2019-01-01",end_date = Sys.Date())



tickers <- c("ALV.DE", "CS.PA", "G.MI", "HNR1.HA", "HSX.L", "MUV2.DE", "RSA.L",
             "TOP.CO")
prices <- get_prices_from_tickers(tickers, 
                                  start = as.Date("2000-01-01"),
                                  end = as.Date("2002-01-01"),
                                  quote = "Close",
                                  retclass = "zoo")


prices_indx <- get_prices_from_tickers("^N100",
                                       start = as.Date("2000-01-01"),
                                       end = as.Date("2002-01-01"),
                                       quote = "Close",
                                       retclass = "zoo")



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







