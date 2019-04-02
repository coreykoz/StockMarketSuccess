#Installing quantmod
install.packages("quantmod")
library("quantmod")


#Task: get data into file
tesladata.ts <- getSymbols(Symbols = "TSLA", auto.assign = TRUE)
