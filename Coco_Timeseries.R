#1) Data acquisition
#read the file manually(use)
Coco<-read.csv(file.choose())

# Check the structure of the dataset
str(Coco)

#2)Data Preparation
# Check for missing values in the entire dataset
missing_values <- sum(is.na(Coco))
missing_values

# Check for missing values in each column
missing_values_per_column <- colSums(is.na(Coco))

#check for any duplicate values
duplicates_Coco <- Coco[duplicated(Coco), ]
duplicates_Coco

# Print the results
cat("Total missing values in the dataset:", missing_values, "\n")
print("Missing values per column:")
print(missing_values_per_column)

#3)Descriptive analysis
#In order to work with a time series in R, you have to place it into a time-series object — an R
#structure that contains the observations, the starting and ending time of the series, and
#information about its periodicity (for example, monthly, quarterly, or annual data).
# Creating a time series object in R and converting to time series
#A vector of numbers can be saved as a time series object using the ts() function.The format is
#y <- ts(data, start=, end=, frequency=)
Coco
tCoco<- ts(Coco$Price, start=c(1994, 10), frequency=12) 
tCoco

plot(tCoco)
start(tCoco) #[1] 1994    10
end(tCoco)  #[1] 2024  3
class(tCoco) #ts
typeof(tCoco) #double

#3.1) Time plot of the time series data
plot(tCoco, main = "Coco prices during October 1994 to March 2024",
     xlab = "Year", ylab = "Price", col = "blue", lwd = 2)

#3.2)Rolling Statistics: Rolling Mean and Rolling Standard Deviation
#Instead of considering the entire dataset at once, rolling statistics provide insights into 
#how the data changes over time by examining subsets of consecutive observations.

# Install and load the zoo package
install.packages("zoo")
library(zoo)

# Calculate rolling mean with window size 12
rolling_mean_tCoco <- rollmean(tCoco, k = 12, align = "center", fill = NA)

# Calculate rolling standard deviation with window size 12
rolling_sd_tCoco <- rollapply(tCoco, width = 12, FUN = sd, align = "right", fill = NA)

# Plot original time series along with Rolling Mean and Standard Deviation
plot(tCoco, main = "Original Time Series with Rolling Mean and Standard Deviation",
     xlab = "Year", ylab = "Price", col = "blue", lwd = 2)
lines(rolling_mean_tCoco, col = "red", lwd = 2)
lines(rolling_sd_tCoco, col = "green", lwd = 2)
legend("topleft", legend = c("Original", "Rolling Mean", "Rolling Standard Deviation"),
       col = c("blue", "red", "green"), lty = 1, lwd = 2)


#3.3)Perform Seasonal-Trend Decomposition using LOESS (STL)
#time series decomposition method used to separate a time series into three components: 
#seasonal, trend, and remainder (residual)

library(forecast)

# Calculate STL decomposition
stl_tCoco <- stl(tCoco, s.window = "periodic")
stl_tCoco

# Plot the decomposition
plot(stl_tCoco)

#4)Modeling along with performance
# 4.1)Splitting the data into train and test sets as per the question

start(tCoco)
end(tCoco)

# Extracting the train and test sets
train_tCoco <- window (tCoco, start = c(1994,10), end = c(2023,09))
test_tCoco <- window (tCoco, start = c(2023,10), end = c(2024,3))

# Check the length of the training and test sets
length(train_tCoco )
length(test_tCoco )

# Check if train_set and test_set contain valid data
print(head(train_tCoco)) #1447.95 1437.05 1399.43 1468.86 1510.55 1484.91
print(head(test_tCoco)) #3495.03 3799.15 3897.04 4087.54 5226.12 6510.16

# Plot Train and Test data
plot(tCoco, main ="Train and Test Data" ,
     xlab = "Year", ylab = "Price", col = "blue", lwd = 2)
lines(train_tCoco, col = "blue", lwd = 2)
lines(test_tCoco, col = "red", lwd = 2)
legend("topleft", legend = c("Train", "Test"),
       col = c("blue", "red"), lty = 1, lwd = 2)

#4.2)Simple time series models
#4.2.1) Mean method
# Calculate the mean forecast
library(forecast)
mean_forecast <- rep(mean(train_tCoco), length(test_tCoco))
mean_forecast

#performance_Mean Forecast
round(accuracy(mean_forecast, test_tCoco),2)
#              ME    RMSE     MAE   MPE  MAPE ACF1 Theil's U
#Test set 2695.19 2892.14 2695.19 57.96 57.96 0.38      4.08

# Plot the original time series data along with Mean Forecast
plot(tCoco, type = "l", col = "black", main = "MeanForecast", xlab = "Time", ylab = "Price")
lines(test_tCoco, col = "red")  
lines(train_tCoco, col = "blue")
lines(mean_forecast, col = "green")  
legend("topleft", legend = c("Train", "Test","Mean"), col = c("blue", "red","green"), lty = 1, inset = 0.01)

#4.2.2)# Naive forecast (last observation)
# Calculate the naive forecast (last observation)
naive_forecast <- rep(tail(train_tCoco, 1), length(test_tCoco))
naive_forecast

#performance_Naive Forecast
round(accuracy(naive_forecast, test_tCoco),2)
#             ME    RMSE     MAE   MPE  MAPE ACF1 Theil's U
#Test set 1106.93 1525.04 1106.93 21.02 21.02 0.38      2.04

# Plot the original time series data along with naive Forecast
plot(tCoco, type = "l", col = "black", main = "NaiveForecast", xlab = "Time", ylab = "Price")
lines(test_tCoco, col = "red")  
lines(train_tCoco, col = "blue")
lines(naive_forecast, col = "green")  
legend("topleft", legend = c("Train", "Test","Naive"), col = c("blue", "red","green"), lty = 1, inset = 0.01)



#4.2.3) Seasonal Naive forcast
# Perform seasonal naive forecast with the same forecast horizon as the length of the test set
snaive_forecast <- snaive(train_tCoco, h = length(test_tCoco))
snaive_forecast 

#performance_Snaive Forecast
round(accuracy(snaive_forecast, test_tCoco),2)

#                   ME    RMSE     MAE   MPE  MAPE MASE ACF1 Theil's U
#Training set   43.56  388.25  296.82 -0.22 16.86 1.00 0.92        NA
#Test set     2135.60 2350.09 2135.60 45.26 45.26 7.19 0.38      3.29


# Plot the original time series data along with snaive Forecast
plot(tCoco, type = "l", col = "black", main = "SeasonalNaiveForecast", xlab = "Time", ylab = "Price")
lines(test_tCoco, col = "red")  
lines(train_tCoco, col = "blue")
lines(naive_forecast, col = "green")  
legend("topleft", legend = c("Train", "Test","sNaive"), col = c("blue", "red","green"), lty = 1, inset = 0.01)

#4.3)Exponential Smoothing method
#Exponential models are among the most popular approaches to forecasting the
#future values of a time series

#4.3.1)simple exponential smoothing/single exponential model
# fits a time series that has a constant level and an irregular 
#component at time i but has neither a trend nor a seasonal component. 
#Its only smoothing parameter is level.

# Perform Simple Exponential Smoothing
ses_forecast <- ses(train_tCoco)
ses_forecast

#performance_SES Forecast
round(accuracy(ses_forecast, test_tCoco),2)
#                  ME    RMSE     MAE   MPE  MAPE MASE ACF1 Theil's U
#Training set    5.60   98.74   73.68  0.10  4.11 0.25 0.17        NA
#Test set     1106.95 1525.06 1106.95 21.02 21.02 3.73 0.38      2.04

# Plot the original time series data along with Exponential smoothing
plot(tCoco, type = "l", col = "black", main = "Single exponential model", xlab = "Time", ylab = "Price")
lines(test_tCoco, col = "red")  
lines(train_tCoco, col = "blue")

# Extract the forecasted values from ses_forecast
forecast_values <- forecast::forecast(ses_forecast)$mean
# Plot the forecasted values
lines(forecast_values, col = "green", pch = 19) 
legend("topleft", legend = c("Train", "Test","Single exponential model"), col = c("blue", "red","green"), lty = 1, inset = 0.01)


#4.3.2)double exponential model/Holt exponential smoothing
#fits a time series with both a level and a trend.
# Perform Holt's exponential smoothing (double exponential smoothing)
holt_forecast <- HoltWinters(train_tCoco, beta = FALSE, gamma = FALSE)
holt_forecast

# Predict future values using Holt's exponential smoothing
holt_predictions <- forecast(holt_forecast, h = length(test_tCoco))

#performance_Holt 
round(accuracy(holt_predictions, test_tCoco),2)
#                  ME    RMSE     MAE   MPE  MAPE MASE ACF1 Theil's U
#Training set    5.61   98.88   73.90  0.10  4.12 0.25 0.17        NA
#Test set     1106.94 1525.05 1106.94 21.02 21.02 3.73 0.38      2.04

# Plot the original time series data along with Holt's exponential smoothing
plot(tCoco, type = "l", col = "black", main = "Holt's exponential smoothing", xlab = "Time", ylab = "Price")
lines(test_tCoco, col = "red")  
lines(train_tCoco, col = "blue")
lines(holt_predictions$mean, col = "green")  
legend("topleft", legend = c("Train", "Test","Holt"), col = c("blue", "red","green"), lty = 1, inset = 0.01)

#4.3.3)triple exponential model/Holt-Winters exponential smoothing
#its a time series with level, trend, and seasonal components.
# Adjust alpha, beta, and gamma values
alpha <- 0.7  # Smoothing parameter for level
beta <- 0.1   # Smoothing parameter for trend
gamma <- 0.1  # Smoothing parameter for seasonal component

# Perform Holt-Winters exponential smoothing with adjusted parameters
hw_forecast <- HoltWinters(train_tCoco, alpha = alpha, beta = beta, gamma = gamma)

# Predict future values using Holt-Winters exponential smoothing
holtWinter_predictions <- forecast(hw_forecast, h = length(test_tCoco))

#performance_Holt Winter
round(accuracy(holtWinter_predictions, test_tCoco),2)
#                 ME    RMSE    MAE   MPE  MAPE MASE ACF1 Theil's U
#Training set   3.54  116.26  89.80  0.17  5.12 0.30 0.33        NA
#Test set     895.56 1295.24 895.56 16.66 16.66 3.02 0.36      1.71

# Plot the original time series data along with Holt Winter exponential smoothing
plot(tCoco, type = "l", col = "black", main = "Holt's Winter exponential smoothing", xlab = "Time", ylab = "Price")
lines(test_tCoco, col = "red")  
lines(train_tCoco, col = "blue")
lines(holtWinter_predictions$mean, col = "green")  
legend("topleft", legend = c("Train", "Test","HoltWinter"), col = c("blue", "red","green"), lty = 1, inset = 0.01)

#4.4)ARIMA/SARIMA
#4.4.1)Fit ARIMA model
autoplot(train_tCoco)

library(tseries)
adf.test(train_tCoco)
#Augmented Dickey-Fuller Test
#data:  train_tCoco
#Dickey-Fuller = -3.0694, Lag order = 7, p-value = 0.1256
#alternative hypothesis: stationary

ndiffs(train_tCoco)
#1 indicating that only first difference is needed

FirstDifference <- train_tCoco %>% diff()
FirstDifference
ggtsdisplay(FirstDifference, main = "First Difference")


#fitting the ARIMA model ARIMA(3,1,0)
fit310 <- Arima(train_tCoco , order=c(3,1,0))
summary(fit310)

#Series: train_tCoco 
#ARIMA(3,1,0) 
#sigma^2 = 9507:  log likelihood = -2080.11
#AIC=4168.23   AICc=4168.35   BIC=4183.63
#Training set error measures:
#  ME    RMSE      MAE        MPE     MAPE      MASE         ACF1
#Training set 5.078602 96.9434 71.93771 0.09569707 4.018656 0.2423606 -0.001640878

#now check with auto(conclusion)
auto.arima(train_tCoco) #ARIMA(0,1,1)  best model


#Diagnostics
fit011 <- Arima(train_tCoco , order=c(0,1,1))
checkresiduals(fit011$residuals) #residaul plot is shown
#	Ljung-Box test
#data:  Residuals
#Q* = 25.319, df = 24, p-value = 0.3886
#Model df: 0.   Total lags used: 24

predictions011 <- forecast::forecast(fit011,h=length(test_tCoco))

autoplot(predictions011) #forecasts from ARIMA(0,1,1) is plotted

#4.4.4)Fit SARIMA model
SARIMA <- auto.arima(train_tCoco, seasonal = TRUE)
SARIMA 

#Series: train_tCoco 
#ARIMA(0,1,1) 
#Coefficients:
#  ma1
#0.1934
#s.e.  0.0540
#sigma^2 = 9478:  log likelihood = -2080.58
#AIC=4165.16   AICc=4165.2   BIC=4172.86

# Diagnostics
fit_sarima <- Arima(train_tCoco, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1)))  # SARIMA(0,1,1)(0,1,1) model
checkresiduals(fit_sarima$residuals)  # Check residuals

# Forecast
predictions_sarima <- forecast::forecast(fit_sarima, h = length(test_tCoco))

# Plot forecasts
autoplot(predictions_sarima)


