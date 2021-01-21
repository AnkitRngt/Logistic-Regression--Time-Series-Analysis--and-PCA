install.packages("readxl")
install.packages('fpp2')
install.packages('xts')
install.packages('adf')
library(adf)
library(readxl)
library(fpp2)
library(xts)
library(forecast)
library(tseries)

exchange_rate <- read_excel("C:/Users/Ankit/Desktop/Data Analytics/Statistics/TABA/Ankit_Time_series.xlsx")
str(exchange_rate)
exchange_rate <- ts(exchange_rate, start=c(1971,1), frequency=12) 
head(exchange_rate)
start(exchange_rate) 
end(exchange_rate)
frequency(exchange_rate)
plot.ts(exchange_rate,main="Monthly Exchange Rate of Euro to Pound from 1970 to 2020 ")


#Subsetting the series with the window function
exchange_rate.subset <- window(exchange_rate, start=c(2003, 5), end=c(2004, 6))
exchange_rate.subset
autoplot(exchange_rate.subset)
monthplot(exchange_rate,main="Month Plot")
ggseasonplot(exchange_rate,year.labels = TRUE,year.labels.left = TRUE)+ ylab("Exchange Rate of Euros in £ ") + ggtitle("Seasonal Plot: Exchange Rate of Euro to Pounds sterling from 1971 to 2020")
ggsubseriesplot(exchange_rate)+ ylab("Exchange Rate of Euros in £ ") + ggtitle("Seasonal Subseries Plot: Exchange Rate of Euro to Pounds sterling from 1971 to 2020")

smoothened_exchange_rate<-ma(exchange_rate,15)
plot(ma(smoothened_exchange_rate,20))
autoplot(exchange_rate)+autolayer(ma(exchange_rate,3),series="ma 1",PI=FALSE)+autolayer(ma(exchange_rate,7),series="ma 2",PI=FALSE)+guides(color=guide_legend(title="Forecast"))


#Seasonal decomposition using decompose() - additive 
fit.decadd<-decompose(exchange_rate, type = "additive")
plot(fit.decadd)

#Seasonal decomposition using decompose() - multiplicative
fit.decmult<-decompose(exchange_rate,type = "multiplicative")
plot(fit.decmult)
forecast(fit.decmult$trend)
plot(fit.decmult$trend)
#Seasonal decomposition using stl()
lexchangerate <- log(exchange_rate)
plot(lexchangerate, ylab="log(exchange_rate)")
fit.stl <- stl(exchange_rate, s.window="period")           
plot(fit.stl)
fit.stl$time.series                                 
exp(fit.stl$time.series)

monthplot(exchange_rate)

#Holt
holt_exchange_Rate<-holt(exchange_rate)
summary(holt_Rate)
plot(holt_Rate)
accuracy(holt_Rate)

#Exponential Smoothening
exchangefit<-ses(exchange_rate,h=4)
summary(exchangefit)
autoplot(exchangefit)
autoplot(exchangefit)+autolayer(fitted(exchangefit),series="Fitted")
round(accuracy(exchangefit),2)

exchangefit2<- ets(exchange_rate,model="MNN")
forecast(exchangefit2,2)

round(accuracy(exchangefit2),2)

exchangefit3<-holt(exchange_rate,h=5)

exchangefit_ets<- ets(exchange_rate,model="ZZZ")  #Automatic Fit
accuracy(exchangefit_ets,h=4)

#HoltWinters Model Additive
exchangefit_additive<-hw(exchange_rate,seasonal = "additive")
autoplot(exchange_rate)+autolayer(exchangefit_additive,series="HW additive forecasts",PI=FALSE)+autolayer(exchangefit_multiplicative,series="HW multiplicative forecasts",PI=FALSE)+guides(color=guide_legend(title="Forecast"))
round(accuracy(exchangefit_additive),2)
summary(exchangefit_additive)
#HoltWinters Model Multiplicative
exchangefit_multiplicative<-hw(exchange_rate,seasonal = "multiplicative")
autoplot(exchange_rate)+autolayer(exchangefit_multiplicative,series="HW multiplicative forecasts",PI=FALSE)+guides(color=guide_legend(title="Forecast"))
summary(exchangefit_multiplicative)
round(accuracy(exchangefit_multiplicative),2)
#Check the order of differencing required
ndiffs(exchange_rate)

#Naive
naive_exchange<-naive(exchange_rate,h=4)
summary(naive_exchange)
plot(naive_exchange)
round(accuracy(naive_exchange),2)
#Apply differencing and Plot the differenced  Time Series
dRate <- diff(exchange_rate)
plot(dRate)
#Assess stationarity of the differenced Time-Series 
adf.test(dRate)

#ACF/PACF plots. Choosing p and q
Acf(dRate)
Pacf(dRate)

#Fitting an ARIMA model
exchange_arima <- arima(exchange_rate, order=c(1,1,0))
summary(exchange_arima)

#Evaluating Model Fit
qqnorm(exchange_arima$residuals)
qqline(exchange_arima$residuals)
Box.test(exchange_arima$residuals, type="Ljung-Box")
checkresiduals(exchange_arima)
accuracy(exchange_arima)

#Forecasting with the fitted model
forecast(exchange_arima, 4)
plot(forecast(exchange_arima, 4), xlab="Year", ylab="Exchange Rate")

