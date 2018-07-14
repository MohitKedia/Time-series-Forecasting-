getwd()
setwd('C:/Users/MOHIT/Documents')
require(XLConnect)
require(xlsx)
require(rJava)
time <- read.xlsx(file = "UK Outward Passengers Movement.xls", sheetIndex = 1)
View(time)
# Indexing Data
time_ireland <- ts(time$Ireland, start=c(1996,1), end=c(2005,4), frequency=4)
time_remaining.eu <- ts(time$Other.EU.not.Ireland., start=c(1996,1), end=c(2005,4), frequency=4)
time_rest.eu.med <- ts(time$Rest.of.Europe.and.Med, start=c(1996,1), end=c(2005,4), frequency=4)
time_rest.world <- ts(time$Rest.of.World, start=c(1996,1), end=c(2005,4), frequency=4)
time_total <- ts(time$Total, start=c(1996,1), end=c(2005,4), frequency=4)

# Printing the indexed data
print(time_ireland)
print(time_remaining.eu)
print(time_rest.eu.med)
print(time_rest.world)
print(time_total)

# Graph of each variable
plot(time_ireland)
plot(time_remaining.eu)
plot(time_rest.eu.med)
plot(time_rest.world)
plot(time_total)

# Using stl function (Decomposition method) to forecast
stl_ireland <-  stl(time_ireland,s.window="periodic")
stl_remaining.eu <-  stl(time_remaining.eu,s.window="periodic")
stl_rest.eu.med <-  stl(time_rest.eu.med,s.window="periodic")
stl_rest.world <-  stl(time_rest.world,s.window="periodic")
stl_total <-  stl(time_total,s.window="periodic")

# Graph of decomposed variables
plot(stl_ireland)
plot(stl_remaining.eu)
plot(stl_rest.eu.med)
plot(stl_rest.world)
plot(stl_total)

# Checking accuracy of the model (STL function-decomposition)
accuracy(forecast(stl_ireland,h=2))
accuracy(forecast(stl_remaining.eu,h=2))
accuracy(forecast(stl_rest.eu.med,h=2))
accuracy(forecast(stl_rest.world,h=2))
accuracy(forecast(stl_total,h=2))

# Forecasting variables for next 2 years
require(forecast)
forecast(stl_ireland, h=8)
forecast(stl_remaining.eu,h=8)
forecast(stl_rest.eu.med,h=8)
forecast(stl_rest.world,h=8)
forecast(stl_total,h=8)

# Graph of forecasted variables by stl(decomposition)
plot(forecast(stl_ireland))
plot(forecast(stl_remaining.eu))
plot(forecast(stl_rest.eu.med))
plot(forecast(stl_rest.world))
plot(forecast(stl_total))


# Using ets function (smoothening method) to forecast
ets_ireland <- ets(time_ireland)
ets_remaining.eu <- ets(time_remaining.eu)
ets_rest.eu.med <- ets(time_rest.eu.med)
ets_rest.world <- ets(time_rest.world)
ets_total <- ets(time_total)

# Checking accuracy of the model (ETS-smoothening)
accuracy(ets_ireland$fitted,time_ireland)
accuracy(ets_remaining.eu$fitted,time_remaining.eu)
accuracy(ets_rest.eu.med$fitted,time_rest.eu.med)
accuracy(ets_rest.world$fitted,time_rest.world)
accuracy(ets_total$fitted,time_total)

# Forecasting variables for next 2 years
forecast(ets_ireland,h=8)
forecast(ets_remaining.eu,h=8)
forecast(ets_rest.eu.med,h=8)
forecast(ets_rest.world,h=8)
forecast(ets_total,h=8)

# Graph of forecasted values by ets function (Smoothening Technique)
plot(forecast(ets_ireland))
plot(forecast(ets_remaining.eu))
plot(forecast(ets_rest.eu.med))
plot(forecast(ets_rest.world))
plot(forecast(ets_total))

# Using auto.arima function to forecst
arima_ireland <- auto.arima(time_ireland)
arima_remaining.eu <- auto.arima(time_remaining.eu)
arima_rest.eu.med <- auto.arima(time_rest.eu.med)
arima_rest.world <- auto.arima(time_rest.world)
arima_total <- auto.arima(time_total)

# Checking accuracy of the model (ARIMA)
accuracy(arima_ireland)
accuracy(arima_remaining.eu)
accuracy(arima_rest.eu.med)
accuracy(arima_rest.world)
accuracy(arima_total)

# Forecasting variables for next 2 years
forecast(arima_ireland,h=8)
forecast(arima_remaining.eu,h=8)
forecast(arima_rest.eu.med,h=8)
forecast(arima_rest.world,h=8)
forecast(arima_total,h=8)

# Graph of forecasted values
plot(forecast(arima_ireland))
plot(forecast(arima_remaining.eu))
plot(forecast(arima_rest.eu.med))
plot(forecast(arima_rest.world))
plot(forecast(arima_total))
