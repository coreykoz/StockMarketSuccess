#Installing quantmod
install.packages("quantmod")
library("quantmod")


#Task: get data into file
getSymbols(Symbols = "TSLA", auto.assign = TRUE)
plot(TSLA$TSLA.Close)


#holt winters forecasting
TSLA.pred <- HoltWinters(TSLA$TSLA.Close, beta = FALSE, gamma = FALSE)
TSLA.pred

plot(TSLA.pred)

install.packages("forecast")
library("forecast")
TSLA.pred2 <- forecast:::forecast.HoltWinters(TSLA.pred, h = 100)
TSLA.pred2
forecast:::plot.forecast(TSLA.pred2)

#neural networks
install.packages("neuralnet")
library("neuralnet")

TSLA.train <- TSLA[1:1656] #75% of data set
TSLA.test <- TSLA[1657:2208]

plot(TSLA$TSLA.Close)
plot(TSLA.train$TSLA.Close)
plot(TSLA.test$TSLA.Close)

TSLA.neural <- neuralnet(TSLA.Close~TSLA.Open+TSLA.Volume + TSLA.High + TSLA.Low , data = TSLA.train)
plot(TSLA.neural)

TSLA.predict <- compute(TSLA.neural, TSLA.test)
plot(TSLA.predict$net.result)
























