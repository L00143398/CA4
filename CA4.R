# Simple example of a time series object

install.packages("dplyr")
install.packages("readxl")
install.packages("forecast")
library("readxl")
library(dplyr)
library("lattice")
library(forecast)
library(tseries)

# The first step is to read the monthly Unemployment  Police Recorded crime statistics into the 
# Crime_Monthly dataframe for Northern Ireland from 1998 to 2017


Crime_Monthly<-read_xls("NI_Crime_rates_Monthly.xls")


# I then converted the key metric that I am going to measure, the Offence Count into
# a montly time series (frequency of 12) which begins on April, 1998
# I then ran some basic commands against the crime data and visually plotted it to 
# show whether the data is stationary or not.
# Based on the visual evidence of the plotted data the data does not appear to be 
# stationary due to the fluctions where the crime rate rises in the early 2000's 
# while then dropping down to a relatively stationary level from 2008 onwards

t_crime <- ts(Crime_Monthly$Offence_Count, start = c(1998,4), frequency = 12)

opar <- par(no.readonly=TRUE)
par(mfrow=c(1,1))
plot(t_crime)

opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))

# The next step is to check for Stationarity - and to do this I applied the KPSS test
# As you can see from the below output from the kpss.test command the p-value 
# is smaller than 0.01 so we can deduce that the t_crime series is not stationary 

kpss.test(t_crime)
ndiffs(t_crime)


# As the t_crime time series is not stationary the next step I need to apply is to  
# apply a diff for a lag of 1 which I do below.
# I then plot this new value, diff_crime and apply the KPSS.test test for Stationairty
# As you see from both the plot and the p-value being greater than 0.1 the time series is now Stationary

# Since there is a trend, the series is differenced 
# once (lag=1 is the default) and saved as 
# Note we had to difference the data by 1

diff_crime <- diff(t_crime, lag = 1)
ndiffs(diff_crime)

kpss.test(t_crime)
kpss.test(diff_crime)

# To confirm that the data is stationary we check with the augmented Dickey-Fuller test 
# I use the adf.test to verify that again the diff_crime time series is stationary.
# The null hypothesis of the Augmented Dickey-Fuller Test is that your autoregressive model 
# has a root unit or in simple terms is no stationary - so in order to confirm that the diff_crime
# time series is stationary we wish to reject the null hypotheis. 
# In the below code I run the adf.test against both the original t_crime data and the diff_crime ts.
# As can be shon for the t_crime data the p-value is greater than 0.05 so we are unable to reject the 
# null hypotheis for this time series.   However the p-value for the diff_crime is less than 0.01 
# so it is approprite to reject the null hypothesis or in simpe terms confirm that the diff_crime
# time series is stationary.
# Please note there is a Lag Order of 6 which I may experiment with in the Arima model later.

adf.test(t_crime, alternative = "stationary")
adf.test(diff_crime, alternative = "stationary")


# From the output of the t_crime time series we deduct that it is a non stationary series 
# as it drops gradually.

# In this next step I check to see if there is any seasonality in the data - which we do not want.
# I apply the stl command against the diff_crime data 


opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,1))

crime_decomposition <- stl(t_crime, s.window="period")
plot(crime_decomposition)

crime_decomposition$time.series

seasonal_adjusted__diff_crime <- seasadj(crime_decomposition)
plot(seasonal_adjusted__diff_crime)


# In this next step I look to calculate the AR (or P) value and the MA (or Q) value
# by running the Acf and Pacf commands against the data. 
# In the below example I ran the same command against both the original crime data, or t_crime
# and the lagged by 1 data in diff_crime.
# As you can see from the output when the data is not stationary the results are not usable.


diff_crime <- diff(t_crime, lag = 1)

acf_diff_crime <- Acf(diff_crime, main="ACF against the stationary crime data")
acf_diff_crime

pacf_diff_crime <- Pacf(diff_crime, main='Pacf against the stationary lag 1 crime data')
pacf_diff_crime


#crime_auto_arima_model <- auto.arima(t_crime, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)

#crime_auto_arima_model <- auto.arima(t_crime,  stepwise = FALSE, approximation = FALSE)

crime_auto_arima_model <- auto.arima(t_crime)

accuracy(crime_auto_arima_model)
crime_auto_arima_model
tsdisplay(residuals(crime_auto_arima_model), lag.max = 40, main='(2,0,4) with zero mean model')
qqnorm(crime_auto_arima_model$residuals)
qqline(crime_auto_arima_model$residuals)


plot(forecast(crime_auto_arima_model, 12), xlab = "Year", ylab = "Yearly Crime Levels")





# Fitting an ARIMA model -------------------------------------
# Note we use the original dataset for the ARIMA model
# and modify the d value to suit our earlier findings
# and d = 1
# We apply the model to the original time series
# and not 


crime_arima_model <- Arima(seasonal_adjusted__diff_crime, order=c(4, 1, 8))
accuracy(crime_arima_model)
crime_arima_model

# Forecast the Yearly crime rates for the next 6 months

plot(forecast(crime_arima_model, 12), xlab = "Year", ylab = "Yearly Crime Levels")


Box.test(crime_arima_model$residuals, type = "Ljung-Box")

