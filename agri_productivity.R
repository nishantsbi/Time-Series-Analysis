setwd("F:/M.Sc. Semester III/Time Series/R")
# download the data file from
# https://github.com/anirudhjayaraman/Time-Series-Analysis/blob/master/productivity.xls

## load the required libraries
library(xlsx)
library(forecast)
library(tseries)

## load the data from a CSV or Excel file. This example is done with an Excel sheet.
prod_df <- read.xlsx(file = 'productivity.xls',sheetIndex = 'Sheet1',rowIndex = 2:67, colIndex = 2:3, header = FALSE)
colnames(prod_df) <- c('Rice','Growth')
# View(prod_df)
## write this dataframe to a CSV file
write.csv(prod_df, file="rice.csv")

## store rice data as time series objects
rice <- ts(prod_df$Rice, start=c(1950, 1), end=c(2015, 1), frequency=1) 
growth <- ts(prod_df$Growth[2:66], start = c(1951,1), end = c(2015,1), frequency = 1)
plot(rice, ylab = 'rice productivity (Kg/hectare)', xlim = c(1950,2015), ylim = c(0,2500), col = 'blue')
abline(reg=lm(rice~time(rice)), col = 'violet')
plot(growth)
abline(reg = lm(growth ~ time(growth)))

## Arriving at a Stationary Series

## KPSS test checks the null hypothesis of trend stationarity
kpss.test(rice)
# KPSS Level = 3.2953, Truncation lag parameter = 1, p-value < 0.01
# the rice series is therefore trend stationary
## De-trending
trend <- lm(rice ~ time(rice))
# trend$fitted.values
# trend$residuals
residuals <- rice - trend$fitted.values
plot(residuals, col = 'blue', ylab = 'de-trended rice productivity')
# Check for trend component in the 'de-trended' series
kpss.test(residuals)
# KPSS Level = 0.4726, Truncation lag parameter = 1, p-value = 0.04784
## Stationarity of residuals is checked by applying the ADF Test
# ADF test on the original series
adf.test(residuals) # Dickey-Fuller = -1.5469, Lag order = 4, p-value = 0.7589
ndiffs(residuals) # the number of differences to make the series stationary = 1
diff_res <- diff(residuals)
plot(diff_res, col = 'blue', ylab = 'first-differenced de-trended residuals')
adf.test(diff_res) # Dickey-Fuller = -6.4471, Lag order = 3, p-value < 0.01

 
## Identifying the appropriate ARIMA Model
acf(diff_res, lag.max = 70) # p = 2
pacf(diff_res, lag.max = 70) # q = 1
tsdisplay(diff_res, lag.max = 30)

## testing the significance of the coefficients
Box.test(diff_res, type = 'Ljung-Box') 
# X-squared = 19.3729, df = 1, p-value = 1.075e-05
Box.test(diff_res, type = 'Box-Pierce')
# X-squared = 18.5055, df = 1, p-value = 1.694e-05

## Estimating Various Models

# ARIMA(2,1,1)
fit11 <- arima(residuals, order = c(2,1,1), method = "CSS-ML")
fit12 <- arima(residuals, order = c(2,1,1), method = "CSS")
fit13 <- arima(residuals, order = c(2,1,1), method = "ML")

# check significance of the coefficients of the models
print(fit11)
print(fit12)
print(fit13)

AIC(fit11) # 782.9799
AIC(fit12)
AIC(fit13) # 782.9799

# ARIMA(1,1,1)
fit21 <- arima(residuals, order = c(1,1,1), method = "CSS-ML")
fit22 <- arima(residuals, order = c(1,1,1), method = "CSS")
fit23 <- arima(residuals, order = c(1,1,1), method = "ML")

# check significance of the coefficients of the models
print(fit21)
print(fit22)
print(fit23)

AIC(fit21) # 781.0581
AIC(fit22)
AIC(fit23) # 781.0581

# ARIMA(0,1,1)
fit31 <- arima(residuals, order = c(0,1,1), method = "CSS-ML")
fit32 <- arima(residuals, order = c(0,1,1), method = "CSS")
fit33 <- arima(residuals, order = c(0,1,1), method = "ML")

# check significance of the coefficients of the models
print(fit31)
print(fit32)
print(fit33)

AIC(fit31) # 780.1837
AIC(fit32)
AIC(fit33) # 780.1837

# ARIMA(2,1,0)
fit41 <- arima(residuals, order = c(2,1,0), method = "CSS-ML")
fit42 <- arima(residuals, order = c(2,1,0), method = "CSS")
fit43 <- arima(residuals, order = c(2,1,0), method = "ML")

# check significance of the coefficients of the models
print(fit41)
print(fit42)
print(fit43)

AIC(fit41) # 783.7276
AIC(fit42)
AIC(fit43) # 783.7276

# ARIMA(1,1,0)
fit51 <- arima(residuals, order = c(1,1,0), method = "CSS-ML")
fit52 <- arima(residuals, order = c(1,1,0), method = "CSS")
fit53 <- arima(residuals, order = c(1,1,0), method = "ML")

# check significance of the coefficients of the models
print(fit51)
print(fit52)
print(fit53)

AIC(fit51) # 788.1823
AIC(fit52)
AIC(fit53) # 788.1823

# From the above it's clear that ARIMA(0,1,1) is the best fit for this model
fit <- auto.arima(residuals, allowdrift = FALSE)
print(fit)
AIC(fit) # 780.1837

# Plot original series vs fitted series
plot(residuals, main = 'Residuals (Actual) vs Fitted Residuals for ARIMA(0,1,1)',col = 'purple', ylab = 'Actual vs Fitted (de-trended series)')
lines(fitted(fit), col = 'red')

## Plot original series vs fitted series for our various models:

# ARIMA(0,1,1)
plot(trend$fitted.values+residuals, main = 'ARIMA(0,1,1): AIC = 780.1837', col = 'purple', ylab = 'rice productivity (Kg/hectare)', ylim = c(0,3000), yat = seq(500,3000,500))
lines(trend$fitted.values+fitted(fit), col = 'red')
legend('topleft', inset = 0.05, c('Original Series','ARIMA(0,1,1) Series'),fill = c('purple','red'), horiz = FALSE)

# ARIMA(2,1,0)
plot(trend$fitted.values+residuals, main = 'ARIMA(2,1,0): AIC = 783.7276', col = 'purple', ylab = 'rice productivity (Kg/hectare)', ylim = c(0,3000), yat = seq(500,3000,500))
lines(trend$fitted.values + fit41$residuals, col = 'red')
legend('topleft', inset = 0.05, c('Original Series','ARIMA(2,1,0) Series'),fill = c('purple','red'), horiz = FALSE)

# ARIMA(2,1,1)
plot(trend$fitted.values+residuals, main = 'ARIMA(2,1,1): AIC = 782.9799', col = 'purple', ylab = 'rice productivity (Kg/hectare)', ylim = c(0,3000), yat = seq(500,3000,500))
lines(trend$fitted.values + fit11$residuals, col = 'red')
legend('topleft', inset = 0.05, c('Original Series','ARIMA(2,1,1) Series'),fill = c('purple','red'), horiz = FALSE)

# ARIMA(1,1,1)
plot(trend$fitted.values+residuals, main = 'ARIMA(1,1,1): AIC = 781.0581', col = 'purple', ylab = 'rice productivity (Kg/hectare)', ylim = c(0,3000), yat = seq(500,3000,500))
lines(trend$fitted.values + fit21$residuals, col = 'red')
legend('topleft', inset = 0.05, c('Original Series','ARIMA(1,1,1) Series'),fill = c('purple','red'), horiz = FALSE)

# ARIMA(1,1,0)
plot(trend$fitted.values+residuals, main = 'ARIMA(1,1,0): AIC = 788.1823', col = 'purple', ylab = 'rice productivity (Kg/hectare)', ylim = c(0,3000), yat = seq(500,3000,500))
lines(trend$fitted.values + fit51$residuals, col = 'red')
legend('topleft', inset = 0.05, c('Original Series','ARIMA(1,1,0) Series'),fill = c('purple','red'), horiz = FALSE)
