#Installing quantmod
install.packages("quantmod")
library("quantmod")

#Task: get data into file
getSymbols(Symbols = "KO", auto.assign = TRUE)
plot(KO$KO.Close)

#Holtwinters
KO.pred <- HoltWinters(KO$KO.Close, beta = FALSE, gamma = FALSE)
KO.pred

plot(KO.pred)

install.packages("forecast")
library("forecast")
KO.pred2 <- forecast:::forecast.HoltWinters(KO.pred, h = 100)
KO.pred2
forecast:::plot.forecast(KO.pred2)

#Math Graphing
#simulate some data
set.seed(20160227)
x<-seq(0,50,1)
y<-((runif(1,10,20)*x)/(runif(1,0,10)+x))+rnorm(51,0,1)
#for simple models nls find good starting values for the parameters even if it throw a warning
m<-nls(y~a*x/(b+x))
#get some estimation of goodness of fit
cor(y,predict(m))
#plot
plot(x,y)
lines(x,predict(m),lty=2,col="red",lwd=3)