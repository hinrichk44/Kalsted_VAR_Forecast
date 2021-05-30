#Forecasting a VAR Model
#First we will import the MTS library
library(MTS)

#We import the vars library
library(vars)

#We will use the Euro Stock Markets dataset
#Contains the daily closing prices of major European stock indices: 
#Germany DAX (Ibis), Switzerland SMI, France CAC, and UK FTSE.
eurostocks = diffM(EuStockMarkets)


#Here we will build a VAR model using vars
eurovar <- vars::VAR(eurostocks,
                     lag.max = 10, 
                     ic = 'AIC',
                     type = 'none')

#Forecasting the VAR model
fcast = predict(eurovar, n.ahead = 25)
#Here we are plotting the forecast
plot(fcast)

#There was an error stating "figure margins too large"
#Had to change margins using par() to plot the forecast
par("mar")
par(mar=c(1,1,1,1))

#You cannot see any details in the forecast. Things are too scrunched
#Because of this, we will take snippets of data
#We'll focus on DAX
DAX = fcast$fcst[1]; DAX
#Extracting the forecast column
x = DAX$DAX[,1]; x
#We need to invert the differncing of the time series
#Let's check the original dataset before it was differenced
tail(EuStockMarkets)
#The last value for DAX is 5473.72, we can use that value
x = cumsum(x) + 5473.72
plot(x)

#Adding the data and forecast to one time series
DAXinv <- ts(c(EuStockMarkets[,1], x),
             start = c(1991,130),
             frequency = 260)
plot(DAXinv)
plot.ts(DAXinv[1786:1885])

#We will create an advanced plot to show the forecasted values
library(lattice)
library(grid)
library(zoo)

#Converting X into object zoo
x = zoo(DAXinv[1786:1885])


# Advanced xyplot from lattice

xyplot(x, grid=TRUE, panel = function(x, y, ...){
  panel.xyplot(x, y, col="red", ...)
  grid.clip(x = unit(76, "native"), just=c("right"))
  panel.xyplot(x, y, col="green", ...) })
