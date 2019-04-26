#Installing packages
install.packages("quantmod")
install.packages("xts")
install.packages("forecast")
install.packages("zoo")
install.packages("lubridate")
install.packages("neuralnet")
install.packages("BBmisc")

library("quantmod")
library("xts")
library("forecast")
library("zoo")
library("lubridate")
library("neuralnet")
  
#Task: get data into file
getSymbols(Symbols = "TSLA", auto.assign = TRUE)
plot(TSLA$TSLA.Close)


#holt Winters forecasting
TSLA.pred <- HoltWinters(TSLA$TSLA.Close, beta = FALSE, gamma = FALSE)
TSLA.pred

plot(TSLA.pred)


TSLA.pred2 <- forecast:::forecast.HoltWinters(TSLA.pred, h = 50)
TSLA.pred2
forecast:::plot.forecast(TSLA.pred2)
#End of Holt Winters




#neural networks

#Convert to dataframe + Clean

TSLA.df <- data.frame("Open" = TSLA$TSLA.Open, "Close" = TSLA$TSLA.Close, "Year"=as.numeric(format(index(TSLA), "%Y")), 
                      "Month"=as.numeric(format(index(TSLA), "%m")), "Day"=as.numeric(format(index(TSLA), "%d")), 
                      "Is_Month_Start" = FALSE, "Is_Month_End" = FALSE, "Is_Year_End" = FALSE,
                      "Is_Year_Start" = FALSE, "Is_Quarter_Start" = FALSE, "Is_Quarter_End" = FALSE,
                      "Quarters"=quarters(index(TSLA)))

for (i in 1:nrow(TSLA.df)){
  
  date <- as.POSIXlt(paste(TSLA.df[i,]$Year, "-", TSLA.df[i,]$Month, "-", TSLA.df[i,]$Day, sep=""))
  
  #Determine if it is beginning/end of the month and change dataset accordingly
  if (TSLA.df[i,]$Day <= 7){ #7 is the first ~25% of the month
    TSLA.df[i,]$Is_Month_Start = TRUE
  } else if (TSLA.df[i,]$Day > 24){ #24 is the last ~25% of the month
    TSLA.df[i,]$Is_Month_End = TRUE
  }
  
  #Determine if it is the beginning/end of the year and change dataset accordingly
  if (TSLA.df[i,]$Quarters == 'Q1'){ #Q1 is the first ~25% of the year
    TSLA.df[i,]$Is_Year_Start = TRUE
  } else if (TSLA.df[i,]$Quarters == 'Q4'){ #Q4 is the last ~25% of the year
    TSLA.df[i,]$Is_Year_End = TRUE
  }
  
  #Determine if it is the beginning of the quarter and change dataset accordingly
  if (yday(date) <= 22){ #days 1-22 is the first ~25% of Q1
    TSLA.df[i,]$Is_Quarter_Start = TRUE
  } else if (yday(date) > 91 &  yday(date) <= 113){ #days 91-113 is the first ~25% of Q2
    TSLA.df[i,]$Is_Quarter_Start = TRUE
  } else if (yday(date) > 182 & yday(date) <=204){ #days 182-204 is the first ~25% of Q3
    TSLA.df[i,]$Is_Quarter_Start = TRUE 
  } else if (yday(date) > 273 & yday(date) <=296){ #days 273-296 is the first ~25% of Q4
    TSLA.df[i,]$Is_Quarter_Start = TRUE 
  }
  
  #Determine if it is the end of the quarter and change dataset accordingly
  if (yday(date) <= 91 & yday(date) >= 69){ #days 69-91 is the last ~25% of Q1
    TSLA.df[i,]$Is_Quarter_End = TRUE
  } else if (yday(date) <= 182 &  yday(date) >= 160){ #days 160-182 is the last ~25% of Q2
    TSLA.df[i,]$Is_Quarter_End = TRUE
  } else if (yday(date) <= 273 & yday(date) >= 251){ #days 251-273 is the last ~25% of Q3
    TSLA.df[i,]$Is_Quarter_End = TRUE 
  } else if (yday(date) <= 366 & yday(date) >= 343){ #days 343-365 is the last ~25% of Q4
    TSLA.df[i,]$Is_Quarter_End = TRUE 
  }
}

TSLA.df <- TSLA.df[,1:11]#Remove the last column, it was only used to clean data

#change true/false values into 1/0s
cols <- sapply(TSLA.df, is.logical)
TSLA.df[,cols] <- lapply(TSLA.df[,cols], as.numeric)

#normalize dataset
TSLA.df <- BBmisc::normalize(TSLA.df)

#Create training and testing data sets
TSLA.train <- TSLA.df[1:(.75 * nrow(TSLA.df)),] #75% of data set
TSLA.test <- TSLA.df[(.75 * nrow(TSLA.df)):nrow(TSLA.df),]

#Predict with NeuralNet.predict()
TSLA.NN <- neuralnet(TSLA.Close ~ TSLA.Open + Year + Month + Day + Is_Month_Start + Is_Month_End + Is_Year_End + 
                       Is_Year_Start + Is_Quarter_Start + Is_Quarter_End 
                     , data = TSLA.train, linear.output=TRUE, hidden=c(5,3,3))
plot(TSLA.NN)

TSLA.pred <- predict(TSLA.NN, TSLA.test)
TSLA.pred.total <- c(TSLA.train$TSLA.Close, TSLA.pred)
plot(TSLA.pred.total, xlab="Number of Days recorded with Quantmod", ylab="Predicted Stock Value Normalized", main="Normalized Stock Predictions of TSLA")
abline(v = (.75 * nrow(TSLA.df)), col='red')

plot(TSLA.df$TSLA.Close, xlab="Number of Days recorded with Quantmod", ylab="Actual Stock Value Normalized", main="Actual Stock Values of TSLA")
abline(v = (.75 * nrow(TSLA.df)), col='red')


#Linear Regression Analysis

#Filtering for all of the relevant data.
#Getting rid of all of the "noise"
subset <- TSLA[1700:nrow(TSLA),]
# Work with subset from now on. Chart subset (note I removed
# subset argument from call to chartSeries)
chartSeries(subset, TA = NULL, theme = "white", up.col = "green", dn.col = "red")

# Linear model on same range as your chart
indices = 1:nrow(subset)
model=lm(TSLA.Close~indices,data=subset)

# Draw line
abline(model$coefficients[1],model$coefficients[2])













