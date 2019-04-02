#Installing quantmod
install.packages("quantmod")
library("quantmod")


#Task: get data into file
getSymbols(Symbols = "TSLA", auto.assign = TRUE)
plot(TSLA$TSLA.Close)

TSLA.pred <- HoltWinters(TSLA$TSLA.Close, beta = FALSE, gamma = FALSE)
TSLA.pred

plot(TSLA.pred)

install.packages("forecast")
library("forecast")
TSLA.pred2 <- forecast:::forecast.HoltWinters(TSLA.pred, h = 100)
TSLA.pred2
forecast:::plot.forecast(TSLA.pred2)

#Task: get data into time series

