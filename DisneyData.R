#Installing quantmod
install.packages("quantmod")
library("quantmod")


#Task: get data into file
getSymbols(Symbols = "DIS", auto.assign = TRUE)
plot(DIS$DIS.Close)

#Task: get data into time series