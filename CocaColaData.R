#Installing quantmod
install.packages("quantmod")
library("quantmod")

#Task: get data into file
cocacoladata <- getSymbols(Symbols = "KO", auto.assign = TRUE)
