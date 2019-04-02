#Installing quantmod
install.packages("quantmod")
library("quantmod")

#Task: get data into file
getSymbols(Symbols = "KO", auto.assign = TRUE)
plot(KO$KO.Close)

KO.pred <- HoltWinters(KO$KO.Close, beta = FALSE, gamma = FALSE)
KO.pred

plot(KO.pred)

install.packages("forecast")
library("forecast")
KO.pred2 <- forecast:::forecast.HoltWinters(KO.pred, h = 100)
KO.pred2
forecast:::plot.forecast(KO.pred2)