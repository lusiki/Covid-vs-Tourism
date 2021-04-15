library(eventstudies)


SplitD <- data.frame(Name=c(rep("ARNT", 40), 
                            rep("MAIS", 40),
                            rep("RIVP", 40)),
                     Date = rep(seq(as.Date("2020-02-24"),
                                    by = "day",
                                    length.out = 40),3))

SplitD <- data.frame(name=c("ARNT","MAIS","RIVP"),
                     when = rep(as.Date("2020-02-24"),3))

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



es1 <- eventstudy(firm.returns = Returns,
                 event.list = SplitD,
                 event.window = 40,
                 type = "marketModel",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap",
                 model.args = list(market.returns=rates_indx$Crobex))

plot(es1)








