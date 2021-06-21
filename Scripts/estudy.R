data("SplitDates", package = "eventstudies")
data("StockPriceReturns", package = "eventstudies")
data("OtherReturns", package = "eventstudies")

es <- eventstudies::eventstudy(firm.returns = StockPriceReturns,
                               event.list = SplitDates,
                               event.window = 7,
                               type = "marketModel",
                               to.remap = TRUE,
                               remap = "cumsum",
                               inference = TRUE,
                               inference.strategy = "bootstrap",
                               model.args = list(
                                 market.returns = OtherReturns[, "NiftyIndex"]
                               )
)
plot(es, col="black")
