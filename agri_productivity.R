setwd("F:/M.Sc. Semester III/Time Series/R")

## load the required libraries
library(xlsx)
library(forecast)
library(tseries)

## load the data from a CSV or Excel file. This example is done with an Excel sheet.
prod_df <- read.xlsx(file = 'agricultural_productivity.xls', sheetIndex = 'Sheet1', rowIndex = 8:65, colIndex = 2, header = FALSE)
colnames(prod_df) <- c('Rice')
View(prod_df)
## write this dataframe to a CSV file
write.csv(prod_df, file="rice.csv")

## store rice data as time series objects
rice <- ts(prod_df$Rice, start=c(1951, 1), end=c(2008, 1), frequency=1) 

## Ljung-Box Test tests the null that The data are independently distributed
Box.test(rice, type="Ljung-Box")
Box.test(rice, type="Box-Pierce")

## Physically check for transformations for which the data 'looks' stationary
plot(rice)
abline(reg=lm(rice~time(rice)))

plot(diff(rice))
abline(reg=lm(diff(rice)~time(diff(rice))))

plot(log(rice))
abline(reg=lm(log(rice)~time(log(rice))))

plot(diff(log(rice)))
abline(reg = lm(diff(log(rice))~time(diff(log(rice)))))

## or alternatively, use command ndiffs() to calculate number of differences
## to achieve stationarity
ndiffs(rice) # prints out 1 in this case

## Augmented Dickey-Fuller Test
## tests the null hypothesis of the presence of a unit root
adf.test(rice) # indicates presence of a unit root
adf.test(diff(rice)) # indicates stationarity upon first differencing
adf.test(diff(log(rice))) # indicates stationarity

## Plot the ACF and PACF to determine 'q' and 'p' respectively
## d = 1 since we take first differences of the series
acf(diff(log(rice)), lag.max = 50)
pacf(diff(log(rice)))

acf(diff(rice))
pacf(diff(rice))

## or let R suggest the appropriate ARIMA model
auto.arima(log(rice))

## Estimate the ARIMA Model and make predictions for the next 20 years based on it
fit <- Arima(log(rice),order=c(0,1,1),seasonal = list(order = c(0,1,1), period = 1))
pred <- predict(fit, n.ahead = 20)
ts.plot(rice,2.718^pred$pred, log = "y", lty = c(1:3))
AIC(fit)

# riceDiffforecasts <- HoltWinters(log(rice), beta = TRUE, gamma = TRUE)
# forecasts <- forecast.HoltWinters(riceDiffforecasts, h = 10)
# plot.forecast(forecasts)
# acf(forecasts$residuals)
# Box.test(forecasts$residuals, lag=20, type="Ljung-Box")
# plot.ts(forecasts$residuals)
# 
# 
# plotForecastErrors <- function(forecasterrors)
# {
#   # make a histogram of the forecast errors:
#   mybinsize <- IQR(forecasterrors)/4
#   mysd   <- sd(forecasterrors)
#   mymin  <- min(forecasterrors) - mysd*5
#   mymax  <- max(forecasterrors) + mysd*3
#   # generate normally distributed data with mean 0 and standard deviation mysd
#   mynorm <- rnorm(10000, mean=0, sd=mysd)
#   mymin2 <- min(mynorm)
#   mymax2 <- max(mynorm)
#   if (mymin2 < mymin) { mymin <- mymin2 }
#   if (mymax2 > mymax) { mymax <- mymax2 }
#   # make a red histogram of the forecast errors, with the normally distributed data overlaid:
#   mybins <- seq(mymin, mymax, mybinsize)
#   hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
#   # freq=FALSE ensures the area under the histogram = 1
#   # generate normally distributed data with mean 0 and standard deviation mysd
#   myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
#   # plot the normal curve as a blue line on top of the histogram of forecast errors:
#   points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
# }
# 
# plotForecastErrors(forecasts$residuals)
