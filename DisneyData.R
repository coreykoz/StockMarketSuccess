#Installing quantmod
install.packages("quantmod")
library("quantmod")

#Task: get data into file
getSymbols(Symbols = "DIS", auto.assign = TRUE)
plot(DIS$DIS.Close)

DIS.pred <- HoltWinters(DIS$DIS.Close, beta = FALSE, gamma = FALSE)
DIS.pred

plot(DIS.pred)

install.packages("forecast")
library("forecast")
DIS.pred2 <- forecast:::forecast.HoltWinters(DIS.pred, h = 100)
DIS.pred2
forecast:::plot.forecast(DIS.pred2)

#NLS
#model <- nls(KO$KO.Close~a*log(b*time(KO$KO.Close))+c, start=list(a = 20, b = 200, c = 20))

#Filtering for all of the relevant data.
#Getting rid of all of the "noise"
DIS.rel <- DIS$DIS.Close[2000:nrow(DIS$DIS.Close),]
plot(DIS.rel)
#put in linear regression