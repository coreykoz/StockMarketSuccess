#Installing quantmod
install.packages("quantmod")
library("quantmod")


#Task: get data into file
getSymbols(Symbols = "DIS", auto.assign = TRUE)
head(TSLA)
