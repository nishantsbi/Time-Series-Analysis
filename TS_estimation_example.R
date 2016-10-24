# load the AirPassengers dataset
data(AirPassengers)

# the data is of class ts
class(AirPassengers)

# this is the start of the time series:
start(AirPassengers)

# this is the end of the time series:
end(AirPassengers)

# plot the frequency of the time series
frequency(AirPassengers)

# plot the time series
plot(AirPassengers)

# fit the trend line
abline(reg=lm(AirPassengers~time(AirPassengers)))

# take a lag = 1 difference
plot(diff(AirPassengers))

# fit the trend line for this one-lag difference
abline(reg=lm(diff(AirPassengers)~time(diff(AirPassengers))))


# the variance seems to be increasing as time increases, so a
# log transform would make sense
# take a lag 1 difference with a log transform, and then fit trend
plot(diff(log(AirPassengers)))
abline(reg = lm(diff(log(AirPassengers))~time(diff(log(AirPassengers)))))

# plot ACF and PACF
acf(AirPassengers)
acf(log(AirPassengers))
acf(diff(AirPassengers))

# this works best
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))

# fit an ARIMA(0,1,1) on the log(AirPassengers)
fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))

# make predictions for next 10 years (12x10 observations)
pred <- predict(fit, n.ahead = 120)

# Plot the predictions using exponential smoothing for this model
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))
