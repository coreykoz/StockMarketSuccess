#Installing quantmod
install.packages("quantmod")
library("quantmod")


#Task: get data into file
tesladata <- getSymbols(Symbols = "TSLA", auto.assign = TRUE)
