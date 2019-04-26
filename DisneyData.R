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
getSymbols(Symbols = "DIS", auto.assign = TRUE)
plot(DIS$DIS.Close)

DIS.pred <- HoltWinters(DIS$DIS.Close, beta = FALSE, gamma = FALSE)
DIS.pred

plot(DIS.pred)

install.packages("forecast")
library("forecast")
DIS.pred2 <- forecast:::forecast.HoltWinters(DIS.pred, h = 100)
DIS.pred2
forecast:::plot.forecast(DIS.pred2)

#NLS


#Filtering for all of the relevant data.
#Getting rid of all of the "noise"
subset <- DIS[2000:nrow(DIS),]

# Work with subset from now on. Chart subset (note I removed
# subset argument from call to chartSeries)
chartSeries(subset, TA = NULL, theme = "white", up.col = "green", dn.col = "red")

# Linear model on same range as your chart
indices = 1:nrow(subset)
model=lm(DIS.Close~indices,data=subset)

# Draw line
abline(model$coefficients[1],model$coefficients[2])


#Neural Networks

DIS.df <- data.frame("Open" = DIS$DIS.Open, "Close" = DIS$DIS.Close, "Year"=as.numeric(format(index(DIS), "%Y")), 
                      "Month"=as.numeric(format(index(DIS), "%m")), "Day"=as.numeric(format(index(DIS), "%d")), 
                      "Is_Month_Start" = FALSE, "Is_Month_End" = FALSE, "Is_Year_End" = FALSE,
                      "Is_Year_Start" = FALSE, "Is_Quarter_Start" = FALSE, "Is_Quarter_End" = FALSE, "Weekday"=weekdays(index(DIS)),
                      "Quarters"=quarters(index(DIS)))

for (i in 1:nrow(DIS.df)){
  
  date <- as.POSIXlt(paste(DIS.df[i,]$Year, "-", DIS.df[i,]$Month, "-", DIS.df[i,]$Day, sep=""))
  
  #Determine if it is beginning/end of the month and change dataset accordingly
  if (DIS.df[i,]$Day <= 7){ #7 is the first ~25% of the month
    DIS.df[i,]$Is_Month_Start = TRUE
  } else if (DIS.df[i,]$Day > 24){ #24 is the last ~25% of the month
    DIS.df[i,]$Is_Month_End = TRUE
  }
  
  #Determine if it is the beginning/end of the year and change dataset accordingly
  if (DIS.df[i,]$Quarters == 'Q1'){ #Q1 is the first ~25% of the year
    DIS.df[i,]$Is_Year_Start = TRUE
  } else if (DIS.df[i,]$Quarters == 'Q4'){ #Q4 is the last ~25% of the year
    DIS.df[i,]$Is_Year_End = TRUE
  }
  
  #Determine if it is the beginning of the quarter and change dataset accordingly
  if (yday(date) <= 22){ #days 1-22 is the first ~25% of Q1
    DIS.df[i,]$Is_Quarter_Start = TRUE
  } else if (yday(date) > 91 &  yday(date) <= 113){ #days 91-113 is the first ~25% of Q2
    DIS.df[i,]$Is_Quarter_Start = TRUE
  } else if (yday(date) > 182 & yday(date) <=204){ #days 182-204 is the first ~25% of Q3
    DIS.df[i,]$Is_Quarter_Start = TRUE 
  } else if (yday(date) > 273 & yday(date) <=296){ #days 273-296 is the first ~25% of Q4
    DIS.df[i,]$Is_Quarter_Start = TRUE 
  }
  
  #Determine if it is the end of the quarter and change dataset accordingly
  if (yday(date) <= 91 & yday(date) >= 69){ #days 69-91 is the last ~25% of Q1
    DIS.df[i,]$Is_Quarter_End = TRUE
  } else if (yday(date) <= 182 &  yday(date) >= 160){ #days 160-182 is the last ~25% of Q2
    DIS.df[i,]$Is_Quarter_End = TRUE
  } else if (yday(date) <= 273 & yday(date) >= 251){ #days 251-273 is the last ~25% of Q3
    DIS.df[i,]$Is_Quarter_End = TRUE 
  } else if (yday(date) <= 366 & yday(date) >= 343){ #days 343-365 is the last ~25% of Q4
    DIS.df[i,]$Is_Quarter_End = TRUE 
  }
}

DIS.df <- DIS.df[,1:11]

Is_Month_Start <- sapply(DIS.df, is.logical)
DIS.df[,Is_Month_Start] <- lapply(DIS.df[,Is_Month_Start], as.numeric)

DIS.df <- BBmisc::normalize(DIS.df)

#Create training and testing data sets
DIS.train <- DIS.df[1:(.75 * nrow(DIS.df)),] #75% of data set
DIS.test <- DIS.df[(.75 * nrow(DIS.df)):nrow(DIS.df),]


#Predict with NeuralNet.prediction()
DIS.NN <- neuralnet(DIS.Close ~ DIS.Open + Year + Month + Day + Is_Month_Start + Is_Month_End + Is_Year_End + 
                       Is_Year_Start + Is_Quarter_Start + Is_Quarter_End 
                     , data = DIS.train, linear.output=TRUE, hidden=c(5,3,3))


plot(DIS.NN)

DIS.pred <- predict(DIS.NN, DIS.test[,1:11])
plot(DIS.pred)

