#Installing quantmod
install.packages("quantmod")
library("quantmod")


#Task: get data into file
disneydata.ts <- getSymbols(Symbols = "DIS", auto.assign = TRUE)

