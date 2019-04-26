#Installing quantmod
install.packages("quantmod")
install.packages("TimeWarp")
library(quantmod)
library(TimeWarp)

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


#Filtering for all of the relevant data.
#Getting rid of all of the "noise"
subset <- DIS[2000:nrow(DIS),]

# Work with subset from now on. Chart subset (note I removed
# subset argument from call to chartSeries)
chartSeries(subset, TA = NULL, theme = "white", up.col = "green", dn.col = "red")

# Linear model on same range as your chart
indices = 1:nrow(subset)
model=lm(DIS.Close~indices,data=subset)

# Draw line
abline(model$coefficients[1],model$coefficients[2])
