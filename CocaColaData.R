#Installing quantmod annd other libraries
install.packages("quantmod")
install.packages("TimeWarp")
library(quantmod)
library(TimeWarp)


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

#Filtering for all of the relevant data.
#Getting rid of all of the "noise"
subset = KO[1500:nrow(KO),]

# Work with subset from now on. Chart subset (note I removed
# subset argument from call to chartSeries)
chartSeries(subset, TA = NULL, theme = "white", up.col = "green", dn.col = "red")

# Linear model on same range as your chart
indices = 1:nrow(subset)
model=lm(KO.Close~indices,data=subset)

# Draw line
abline(model$coefficients[1],model$coefficients[2])
