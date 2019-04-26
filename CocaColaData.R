#Installing quantmod annd other libraries
install.packages("quantmod")
install.packages("TimeWarp")
install.packages("xts")
install.packages("forecast")
install.packages("zoo")
install.packages("lubridate")
install.packages("neuralnet")
install.packages("BBmisc")

library(quantmod)
library(TimeWarp)
library("xts")
library("forecast")
library("zoo")
library("lubridate")
library("neuralnet")

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

#Linear model

#Filtering for all of the relevant data.
#Getting rid of all of the "noise"
subset = KO[1500:nrow(KO),]

# Work with subset from now on. Chart subset (note I removed
# subset argument from call to chartSeries)
chartSeries(subset, TA = NULL, theme = "white", up.col = "green", dn.col = "red")

# Linear model on same range as your chart
indices = 1:nrow(subset)
model=lm(KO.Close~indices,data=subset)

# Draw line
abline(model$coefficients[1],model$coefficients[2])

#Neural Networks
#Convert to dataframe + Clean

KO.df <- data.frame("Open" = KO$KO.Open, "Close" = KO$KO.Close, "Year"=as.numeric(format(index(KO), "%Y")), 
                      "Month"=as.numeric(format(index(KO), "%m")), "Day"=as.numeric(format(index(KO), "%d")), 
                      "Is_Month_Start" = FALSE, "Is_Month_End" = FALSE, "Is_Year_End" = FALSE,
                      "Is_Year_Start" = FALSE, "Is_Quarter_Start" = FALSE, "Is_Quarter_End" = FALSE, "Weekday"=weekdays(index(KO)),
                      "Quarters"=quarters(index(KO)))

for (i in 1:nrow(KO.df)){
  
  date <- as.POSIXlt(paste(KO.df[i,]$Year, "-", KO.df[i,]$Month, "-", KO.df[i,]$Day, sep=""))
  
  #Determine if it is beginning/end of the month and change dataset accordingly
  if (KO.df[i,]$Day <= 7){ #7 is the first ~25% of the month
    KO.df[i,]$Is_Month_Start = TRUE
  } else if (KO.df[i,]$Day > 24){ #24 is the last ~25% of the month
    KO.df[i,]$Is_Month_End = TRUE
  }
  
  #Determine if it is the beginning/end of the year and change dataset accordingly
  if (KO.df[i,]$Quarters == 'Q1'){ #Q1 is the first ~25% of the year
    KO.df[i,]$Is_Year_Start = TRUE
  } else if (KO.df[i,]$Quarters == 'Q4'){ #Q4 is the last ~25% of the year
    KO.df[i,]$Is_Year_End = TRUE
  }
  
  #Determine if it is the beginning of the quarter and change dataset accordingly
  if (yday(date) <= 22){ #days 1-22 is the first ~25% of Q1
    KO.df[i,]$Is_Quarter_Start = TRUE
  } else if (yday(date) > 91 &  yday(date) <= 113){ #days 91-113 is the first ~25% of Q2
    KO.df[i,]$Is_Quarter_Start = TRUE
  } else if (yday(date) > 182 & yday(date) <=204){ #days 182-204 is the first ~25% of Q3
    KO.df[i,]$Is_Quarter_Start = TRUE 
  } else if (yday(date) > 273 & yday(date) <=296){ #days 273-296 is the first ~25% of Q4
    KO.df[i,]$Is_Quarter_Start = TRUE 
  }
  
  #Determine if it is the end of the quarter and change dataset accordingly
  if (yday(date) <= 91 & yday(date) >= 69){ #days 69-91 is the last ~25% of Q1
    KO.df[i,]$Is_Quarter_End = TRUE
  } else if (yday(date) <= 182 &  yday(date) >= 160){ #days 160-182 is the last ~25% of Q2
    KO.df[i,]$Is_Quarter_End = TRUE
  } else if (yday(date) <= 273 & yday(date) >= 251){ #days 251-273 is the last ~25% of Q3
    KO.df[i,]$Is_Quarter_End = TRUE 
  } else if (yday(date) <= 366 & yday(date) >= 343){ #days 343-365 is the last ~25% of Q4
    KO.df[i,]$Is_Quarter_End = TRUE 
  }
}

KO.df <- KO.df[,1:11]

Is_Month_Start <- sapply(KO.df, is.logical)
KO.df[,Is_Month_Start] <- lapply(KO.df[,Is_Month_Start], as.numeric)

KO.df <- BBmisc::normalize(KO.df)

#Create training and testing data sets
KO.train <- KO.df[1:(.75 * nrow(KO.df)),] #75% of data set
KO.test <- KO.df[(.75 * nrow(KO.df)):nrow(KO.df),]


#Predict with NeuralNet.prediction()
KO.NN <- neuralnet(KO.Close ~ KO.Open + Year + Month + Day + Is_Month_Start + Is_Month_End + Is_Year_End + 
                       Is_Year_Start + Is_Quarter_Start + Is_Quarter_End 
                     , data = KO.train, linear.output=TRUE, hidden=c(5,3,3))


plot(KO.NN)

KO.pred <- predict(KO.NN, KO.test[,1:11])
plot(KO.pred)




