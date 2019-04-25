#Installing quantmod annd other libraries
install.packages("quantmod")
library("quantmod")



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
#model <- nls(KO$KO.Close~a*log(b*time(KO$KO.Close))+c, start=list(a = 20, b = 200, c = 20))

#Filtering for all of the relevant data.
#Getting rid of all of the "noise"
KO.rel <- KO$KO.Close[1500:nrow(KO$KO.Close),]
plot(KO.rel)
