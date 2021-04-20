library(eventstudies)


SplitD <- data.frame(Name=c(rep("ARNT", 40), 
                            rep("MAIS", 40),
                            rep("RIVP", 40)),
                     Date = rep(seq(as.Date("2020-02-24"),
                                    by = "day",
                                    length.out = 40),3))

SplitD <- data.frame(name=colnames(rates),
                     when = rep(as.Date("2020-02-24"),12))

Returns <- na.omit(rates)

es <- eventstudy(firm.returns = Returns,
                 event.list = SplitD,
                 event.window = 40,
                 type = "None",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap")

plot(es)



rates_indx <- get_rates_from_prices(Crobex, 
                                    quote = "Close",
                                    multi_day = TRUE,
                                    compounding = "continuous")

colnames(rates_indx) <- "Crobex"

es2 <- eventstudy(firm.returns = Returns,
                 event.list = SplitD,
                 event.window = 40,
                 type = "marketModel",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap",
                 model.args = list(market.returns=rates_indx$Crobex))

plot(es2)




gold <- Quandl("LBMA/GOLD", type = "zoo", collapse = "daily", start_date = "2019-01-01",end_date = Sys.Date())
gold <- gold[,5]


add_indx <- get_rates_from_prices(gold, 
                                  quote = "Close",
                                  multi_day = TRUE,
                                  compounding = "continuous")

colnames(add_indx) <- "gold"

es3 <- eventstudy(firm.returns = Returns,
                  event.list = SplitD,
                  event.window = 40,
                  type = "marketModel",
                  to.remap = TRUE,
                  remap = "cumsum",
                  inference = TRUE,
                  inference.strategy = "bootstrap",
                  model.args = list(market.returns=rates_indx$Crobex,
                  others=add_indx$gold,
                  market.returns.purge=TRUE))

plot(es3)






#VIDI 3rd paket
#https://github.com/setzler/eventStudy
















