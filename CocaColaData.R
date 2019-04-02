#Installing quantmod
install.packages("quantmod")
library("quantmod")

#Task: get data into file
disneydata <- getSymbols(Symbols = "KO", auto.assign = TRUE)
