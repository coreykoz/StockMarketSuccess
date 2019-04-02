#Installing quantmod
install.packages("quantmod")
library("quantmod")


#Task: get data into file
disneydata <- getSymbols(Symbols = "DIS", auto.assign = TRUE)

