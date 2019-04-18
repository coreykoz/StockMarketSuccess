#Installing quantmod annd other libraries
install.packages("quantmod")
library("quantmod")
library(zoo)

#Task: get data into file
getSymbols(Symbols = "KO", auto.assign = TRUE)
plot(KO$KO.Close)

#Holtwinters
KO.pred <- HoltWinters(KO$KO.Close, beta = FALSE, gamma = FALSE)
KO.pred

plot(KO.pred)

install.packages("forecast")
library("forecast")
KO.pred2 <- forecast:::forecast.HoltWinters(KO.pred, h = 100)
KO.pred2
forecast:::plot.forecast(KO.pred2)

#NLS
#getSymbols('UNRATE', src = 'CocaCola')
#z <- as.zoo(UNRATE)

KO.df <- data.frame(Y = as.matrix(KO$KO.Close), date = time())

model <- nls(KO$KO.Close~20*log(200*time(KO$KO.Close)+20), start = time('2007-01-03'))
