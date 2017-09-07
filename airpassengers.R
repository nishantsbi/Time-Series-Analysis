library(tseries)
library(zoo)
library(forecast)

data("AirPassengers")
dat <- AirPassengers

# Is there seasonality in the data? Provide supportive analysis. --------------
boxplot(dat ~ cycle(dat)) # indicates presence of seasonality
plot(decompose(dat))

# remove unequal variance
ldat <- log(dat)
plot(ldat)

# De-trend the data
ldat_trend <- ma(ldat, order = 12, centre = T)
lines(ldat_trend)

# plot de-trended series; seasonal component still needs removing
de_trended_ldat <- ldat - ldat_trend
plot(de_trended_ldat)

# Calculate seasonal component
de_trended_ldat_matrix <- matrix(data = de_trended_ldat, nrow = 12, byrow = T)
seasonal_component <- colMeans(de_trended_ldat_matrix, na.rm = T)
seasonal_ts <- ts(rep(seasonal_component, 12), start = c(1949,1),
                  end = c(1960,12), frequency = 12)

# De-seasonalize the data
ts_final <- de_trended_ldat - seasonal_ts
plot(ts_final)
plot(ts_final)

ts_final_testing <- window(ts_final, start = c(1949,7), end = c(1960,6))
plot(ts_final_testing)
adf.test(ts_final_testing)
auto.arima(ts_final_testing)

# Order of integration required to make this series stationary? ---------------
nsdiffs(dat) # number of seasonal differences required to make the sereis stationary
ndiffs(dat) # number of first differences to make the series stationary

# ...also:

adf.test(dat) # rejects the NULL : Alternative hypothesis is of Stationarity
pp.test(dat) # rejects the NULL: Alternative hypothesis is of Stationarity
kpss.test(dat) # rejects the NULL : Alternative hypothesis is of Unit Root

# hence series is non-stationary

diff_dat <- diff(dat) # take the differenced series

adf.test(diff_dat) # rejects the NULL : Alternative hypothesis is of Stationarity
pp.test(diff_dat) # rejects the NULL : Alternative hypothesis is of Stationarity
kpss.test(diff_dat) # fails to reject the NULL hypothesis of Stationarity

# hence the differenced series is stationary. Order of Integration is 1.


# What is the value of p and q in the final model? ---------------------------
tsdisplay(decompose(dat)$random)
arima_model <- auto.arima(dat)
summary(arima_model) # p = 2, q = 1


# Value of the variable for the next 12 months? ----------------------

preds <- predict(arima_model, n.ahead = 12)

# First Plot
plot(dat, col = 'green', main = 'Actual vs ARIMA Model',
     ylab = 'Air Passengers', lwd = 3)
lines(arima_model$fitted, col = 'black', lwd = 3, lty = 2)
legend('topleft',legend = c('Actual','ARIMA Fit'), 
       col = c('green','black'), lwd = c(3,3), lty = c(1,2))


# Second Plot
ts.plot(dat,preds$pred, main = 'Actual vs ARIMA Predictions', col = c('green','black'), lty = c(1,2), lwd = c(3,3))
legend('topleft',legend = c('Actual Data','ARIMA Predictions'), 
       col = c('green','black'), lwd = c(3,3), lty = c(1,2))
