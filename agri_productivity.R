setwd("F:/M.Sc. Semester III/Time Series/R")

## load the required libraries
library(xlsx)
library(forecast)
library(tseries)

## load the data from a CSV or Excel file. This example is done with an Excel sheet.
prod_df <- read.xlsx(file = 'productivity.xls',sheetIndex = 'Sheet1',rowIndex = 2:67, colIndex = 2:3, header = FALSE)
colnames(prod_df) <- c('Rice','Growth')
View(prod_df)
## write this dataframe to a CSV file
write.csv(prod_df, file="rice.csv")

## store rice data as time series objects
rice <- ts(prod_df$Rice, start=c(1950, 1), end=c(2015, 1), frequency=1) 
growth <- ts(prod_df$Growth[2:66], start = c(1951,1), end = c(2015,1), frequency = 1)
plot(rice)
abline(reg=lm(rice~time(rice)))
plot(growth)
abline(reg = lm(growth ~ time(growth)))

## Arriving at a Stationary Series

## KPSS test checks the null hypothesis of trend stationarity
kpss.test(rice)
## De-trending
trend <- lm(rice ~ time(rice))
# trend$fitted.values
# trend$residuals
residuals <- rice - trend$fitted.values
plot(residuals)
## Stationarity of residuals is checked by applying the ADF Test
# ADF test on the original series
adf.test(residuals) # Dickey-Fuller = -1.5469, Lag order = 4, p-value = 0.7589
ndiffs(residuals)
diff_res <- diff(residuals)
plot(diff(residuals))
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

print(fit41)
print(fit42)
print(fit43)

AIC(fit41) # 783.7276
AIC(fit42)
AIC(fit43) # 783.7276

fit <- auto.arima(residuals, allowdrift = FALSE)
print(fit)
AIC(fit) # 780.1837
