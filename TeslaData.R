#Installing quantmod
install.packages("quantmod")
library("quantmod")


#Task: get data into file
getSymbols(Symbols = "TSLA", auto.assign = TRUE)
plot(TSLA$TSLA.Close)
