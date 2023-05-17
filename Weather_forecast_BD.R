################Covid-19 forecasting in Bangladesh and Weather Impact###############
#                            Mohammad Nayeem Hasan                                 #
####################################################################################
library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)

####time series


# Model building
setwd("E:\\update - Mat")
world <- read.csv("owid-covid-data.csv")
world_bd <- subset(world, world$location == "Bangladesh")

world_bd <- subset(world_bd, world_bd$date >= "2020-03-08") #1/1/2020 2020-08-08
world_bd <- subset(world_bd, world_bd$date <= "2023-01-31") #5/29/2021

wdata <- read.csv("weather_data.csv")
world_bd$new_vaccinations

world_bd$CFR <- (world_bd$total_deaths/world_bd$total_cases)*100

bdwdata <- merge(world_bd, wdata, by=c("date"))



# Libraries
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

# Value used to transform the data


# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

a <- ggplot(bdwdata, aes(x=as.Date(date))) +
  
  geom_line( aes(y=bdwdata$total_tests), size=2, color=temperatureColor) + 
  geom_line( aes(y=bdwdata$total_cases * 10), size=2, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total Tests (n)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*10, name="Total Cases Count (n)",labels = function(x) format(x, scientific = TRUE)),
    labels = function(x) format(x, scientific = TRUE)
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=18),
    axis.title.y.right = element_text(color = priceColor, size=18),
  ) + xlab("")

a




# Libraries
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

# Value used to transform the data


# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

b <- ggplot(bdwdata, aes(x=as.Date(date))) +
  
  geom_line( aes(y=bdwdata$total_vaccinations), size=2, color=temperatureColor) + 
  geom_line( aes(y=bdwdata$total_cases * 200), size=2, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total Vaccination (n)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*200, name="Total Cases Count (n)",labels = function(x) format(x, scientific = TRUE)),
    labels = function(x) format(x, scientific = TRUE)
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=18),
    axis.title.y.right = element_text(color = priceColor, size=18)
  ) + xlab("")

b




# Libraries
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

# Value used to transform the data


# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

c <- ggplot(bdwdata, aes(x=as.Date(date))) +
  
  geom_line( aes(y=bdwdata$total_tests), size=2, color=temperatureColor) + 
  geom_line( aes(y=bdwdata$total_deaths * 500), size=2, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total Tests (n)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*500, name="Total Deaths Count (n)",labels = function(x) format(x, scientific = TRUE)),
    labels = function(x) format(x, scientific = TRUE)
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=18),
    axis.title.y.right = element_text(color = priceColor, size=18),
  ) + xlab("")

c




# Libraries
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

# Value used to transform the data


# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

d <- ggplot(bdwdata, aes(x=as.Date(date))) +
  
  geom_line( aes(y=bdwdata$total_vaccinations), size=2, color=temperatureColor) + 
  geom_line( aes(y=bdwdata$total_deaths * 10000), size=2, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total Vaccination (n)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*10000, name="Total deaths Count (n)",labels = function(x) format(x, scientific = TRUE)),
    labels = function(x) format(x, scientific = TRUE)
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=18),
    axis.title.y.right = element_text(color = priceColor, size=18)
  ) + xlab("")

d

tiff("vs.tiff", units="in", width=15, height=15, res=300)
gridExtra::grid.arrange(a,b,c,d)
dev.off()



history <- data.frame(ds = seq(as.Date('2020-03-08'), as.Date('2023-01-31'), by = 'd'),
                      y = bdwdata$new_cases)
m3 <- prophet(history)
future <- make_future_dataframe(m3, periods = 30)
fcst3 <- predict(m3, future)
x <-plot(m3, fcst3, xlab="Date", ylab="Daily Cases Count (n)") + ggtitle("Prophet Model") + theme(
  plot.title = element_text(size=12))
plot(x)


nrow(history)
SSE <- sum((history$y[1:1060] - fcst3$yhat[c(1:1060)])^2)
SST <- sum((history$y[1:1060] - mean(history$y[1:1060]))^2)
R_square <- 1 - SSE / SST
R_square

last_fcst3 <- fcst3[1060,]
rmse <- sqrt(mean((history$y - fcst3$yhat[c(1:1060)])^2))
mae <- mean(abs((history$y - fcst3$yhat[c(1:1060)])))
final <- cbind(last_fcst3, rmse, mae)
final

#ARIMA
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)

library(zoo)


myts <- ts(bdwdata$new_cases,start=c(2020.18), frequency = 365.25)

autoplot(myts)
auto.arima(myts)
Fit<-Arima(myts,order=c(5,1,5))
fcast <- forecast(Fit, h=30)
fcast$x
summary(Fit)

y <- autoplot(fcast, size = 1.5,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("Date")+ ylab("Daily Cases Count (n)") + ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="right") +
  theme( legend.title = element_text(color = "Black", size = 12),
         legend.text = element_text(color = "Black", size = 12))+ xlim(2020.18, 2023.2)+ ylim(-5000, 18000)

plot(y)
#R2
SSE <- sum((resid(Fit[1:1060]))^2)
SST <- sum((world_bd$new_cases[1:1060] - mean(world_bd$new_cases[1:1060]))^2)
R_square <- 1 - SSE / SST
R_square


####SES########

library(tidyverse) 
library(fpp2) 



ses.goog <- ses(myts,
                h = 30) 
summary(ses.goog)
accuracy(ses.goog)
fcast <- forecast(ses.goog, h=30)
z <- autoplot(ses.goog, main=NULL)+
  autolayer(fcast$mean, series="Forecast") +
  autolayer(fitted(ses.goog), series='Fitted') + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("Date") + ylab("Daily Cases Count (n)") + ggtitle("SES Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="right") +
  theme( legend.title = element_text(color = "Black", size = 12),
         legend.text = element_text(color = "Black", size = 12))+ xlim(2020.18, 2023.2)+ ylim(-5000, 18000)



z

SSE <- sum((resid(ses.goog[1:1060]))^2)
SST <- sum((world_bd$new_cases[1:1060] - mean(world_bd$new_cases[1:1060]))^2)
R_square <- 1 - SSE / SST
R_square



#ARIMAX
library(forecast)
library(lubridate)
library(tseries)
library(fpp)
library(TSA)

options(scipen = 999)
xreg <- cbind(scale(bdwdata$ws), scale(bdwdata$temp), scale(bdwdata$dew), 
              scale(bdwdata$prec), scale(bdwdata$rh), scale(bdwdata$sp))

library(pastecs)
stat.desc(bdwdata$ws)
stat.desc(bdwdata$temp)
stat.desc(bdwdata$dew)
stat.desc(bdwdata$prec)
stat.desc(bdwdata$rh)
stat.desc(bdwdata$sp)


modArima <- Arima(myts, xreg=xreg, order=c(5,1,5))
summary(modArima)
Forecasted_values<-forecast(modArima,xreg=xreg)
Forecasted_values

plot(myts)
lines(fitted(modArima),col=1)

library(ggplot2)
fc <- forecast(modArima,xreg=xreg)
yy <- autoplot(fc,h=2,size = 2,geom = "point")  +
  autolayer(fc$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(fc), series='Fitted', lwd = 0.6) + 
  autolayer(fc$lower, series='lower') +
  autolayer(fc$upper, series='upper') +
  xlab("Date") + ylab("Daily Cases Count (n)") +ggtitle("ARIMAX Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="right") +
  theme( legend.title = element_text(color = "Black", size = 12),
         legend.text = element_text(color = "Black", size = 12)) + xlim(2020.18, 2023.2)+ ylim(-5000, 18000)


plot(yy)

SSE <- sum((resid(modArima[1:1060]))^2)
SST <- sum((world_bd$new_cases[1:1060] - mean(world_bd$new_cases[1:1060]))^2)
R_square <- 1 - SSE / SST
R_square

library(lmtest)
coeftest(modArima)

coefci(modArima)



#ARIMAX + vaccination
library(forecast)
library(lubridate)
library(tseries)
library(fpp)
library(TSA)



options(scipen = 999)
xreg <- cbind(scale(bdwdata$ws), scale(bdwdata$temp), scale(bdwdata$dew), scale(bdwdata$prec), scale(bdwdata$rh), scale(bdwdata$sp), scale(bdwdata$new_vaccinations))
modArima <- Arima(myts, xreg=xreg, order=c(5,1,5))
summary(modArima)

plot(myts)
lines(fitted(modArima),col=1)

library(ggplot2)
fc <- forecast(modArima,xreg=xreg)
yyy <- autoplot(fc,h=2,size = 2,geom = "point")  +
  autolayer(fc$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(fc), series='Fitted', lwd = 0.6) + 
  autolayer(fc$lower, series='lower') +
  autolayer(fc$upper, series='upper') +
  xlab("Date") + ylab("Daily Cases Count (n)") +ggtitle("ARIMAX Model + Vaccination")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="right") +
  theme( legend.title = element_text(color = "Black", size = 12),
         legend.text = element_text(color = "Black", size = 12)) + xlim(2020.18, 2023.5)+ ylim(-5000, 18000)


yyy <- yy
plot(yy)

SSE <- sum((resid(modArima[1:1060]))^2)
SST <- sum((world_bd$new_cases[1:1060] - mean(world_bd$new_cases[1:1060]))^2)
R_square <- 1 - SSE / SST
R_square

library(lmtest)
coeftest(modArima)

coefci(modArima)

tiff("CCase.tiff", units="in", width=15, height=15, res=300)
gridExtra::grid.arrange(x,y,z,yy,yyy)
dev.off()


library(lmtest)
coeftest(modArima)



# Model building
setwd("E:\\update - Mat")
world <- read.csv("owid-covid-data.csv")
world_bd <- subset(world, world$location == "Bangladesh")

world_bd <- subset(world_bd, world_bd$date >= "2020-03-18") #1/1/2020 2020-08-08
world_bd <- subset(world_bd, world_bd$date <= "2023-01-31") #5/29/2021

wdata <- read.csv("weather_data.csv")
wdata <- wdata[c(1:1050),]
nrow(world_bd)
nrow(wdata)
world_bd$CFR <- (world_bd$total_deaths/world_bd$total_cases)*100

bdwdata <- merge(world_bd, wdata, by=c("date"))


history <- data.frame(ds = seq(as.Date('2020-03-18'), as.Date('2023-01-31'), by = 'd'),
                      y = world_bd$new_deaths)
m3 <- prophet(history)
future <- make_future_dataframe(m3, periods = 30)
fcst3 <- predict(m3, future)
x <-plot(m3, fcst3, xlab="Date", ylab="Daily Deaths Count (n)") + ggtitle("Prophet Model") + theme(
  plot.title = element_text(size=10))
plot(x)

SSE <- sum((history$y - fcst3$yhat)^2)
SST <- sum((history$y - mean(history$y))^2)
R_square <- 1 - SSE / SST
R_square

last_fcst3 <- fcst3
rmse <- sqrt(mean((history$y - fcst3$yhat)^2))
mae <- mean(abs((history$y - fcst3$yhat)))
final <- cbind(last_fcst3, rmse, mae)
final

#ARIMA
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)

library(zoo)


myts <- ts(world_bd$new_deaths,start=c(2020.2), frequency = 365.25)

autoplot(myts)
auto.arima(myts)
Fit<-Arima(myts,order=c(1,1,2))


fcast <- forecast(Fit, h=30)
resid(Fit)
summary(Fit)

y <- autoplot(fcast, size = 1.5,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("Date") + ylab("Daily Deaths Count (n)") + ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="right") +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8),
         plot.title = element_text(size=9))

plot(y)
#R2
SSE <- sum((resid(Fit))^2)
SST <- sum((world_bd$new_deaths - mean(world_bd$new_deaths))^2)
R_square <- 1 - SSE / SST
R_square


####SES########

library(tidyverse) 
library(fpp2) 



ses.goog <- ses(myts,
                h = 30) 
summary(ses.goog)
accuracy(ses.goog)
fcast <- forecast(ses.goog, h=30)
z <- autoplot(ses.goog, main=NULL)+
  autolayer(fcast$mean, series="Forecast") +
  autolayer(fitted(ses.goog), series='Fitted') + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("Date") + ylab("Daily Deaths Count (n)") + ggtitle("SES Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="right") +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8),
         plot.title = element_text(size=10))



z

SSE <- sum((resid(fcast))^2)
SST <- sum((world_bd$new_deaths - mean(world_bd$new_deaths))^2)
R_square <- 1 - SSE / SST
R_square

#ARIMAX
library(forecast)
library(lubridate)
library(tseries)
library(fpp)
library(TSA)

options(scipen = 999)
NROW(wdata$ws)
xreg <- cbind(scale(wdata$ws), scale(wdata$temp), scale(wdata$dew), 
              scale(wdata$prec), scale(wdata$rh), scale(wdata$sp))


stat.desc(wdata$ws)
stat.desc(wdata$temp)
stat.desc(wdata$dew)
stat.desc(wdata$prec)
stat.desc(wdata$rh)
stat.desc(wdata$sp)


modArima <- Arima(myts, xreg=xreg, order=c(1,1,2))
modArima
summary(modArima)
Forecasted_values<-forecast(modArima,xreg=xreg)
Forecasted_values

plot(myts)
lines(fitted(modArima),col=1)

library(ggplot2)
fc <- forecast(modArima,xreg=xreg)
yy <- autoplot(fc,h=2,size = 2,geom = "point")  +
  autolayer(fc$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(fc), series='Fitted', lwd = 0.6) + 
  autolayer(fc$lower, series='lower') +
  autolayer(fc$upper, series='upper') +
  xlab("Date") + ylab("Daily Deaths Count (n)") +ggtitle("ARIMAX Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="right") +
  theme( legend.title = element_text(color = "Black", size = 12),
         legend.text = element_text(color = "Black", size = 12)) + xlim(2020.2, 2023.2) + ylim(-100, 300)


plot(yy)

library(lmtest)
library(vcov)
lmtest::coeftest(modArima)
coefci(modArima)

SSE <- sum(resid(modArima)^2)
SST <- sum((world_bd$new_deaths - mean(world_bd$new_deaths))^2)
R_square <- 1 - SSE / SST
R_square

#ARIMAX + vaccination
library(forecast)
library(lubridate)
library(tseries)
library(fpp)
library(TSA)

NROW(myts)

options(scipen = 999)
xreg <- cbind(scale(bdwdata$ws), scale(bdwdata$temp), scale(bdwdata$dew), 
              scale(bdwdata$prec), scale(bdwdata$rh), scale(bdwdata$sp) ,  
              scale(bdwdata$new_vaccinations))

modArima <- Arima(myts, xreg=xreg, order=c(1,1,2))
modArima
summary(modArima)
Forecasted_values<-forecast(modArima,xreg=xreg)
Forecasted_values

plot(myts)
lines(fitted(modArima),col=1)

library(ggplot2)
fc <- forecast(modArima,xreg=xreg)
yyy <- autoplot(fc,h=2,size = 2,geom = "point")  +
  autolayer(fc$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(fc), series='Fitted', lwd = 0.6) + 
  autolayer(fc$lower, series='lower') +
  autolayer(fc$upper, series='upper') +
  xlab("Date") + ylab("Daily Deaths Count (n)") +ggtitle("ARIMAX Model + vaccination")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="right") +
  theme( legend.title = element_text(color = "Black", size = 12),
         legend.text = element_text(color = "Black", size = 12)) + xlim(2020.3, 2023.3) + ylim(-100, 275)


plot(yyy)

SSE <- sum((resid(modArima))^2)
SST <- sum((world_bd$new_deaths[1:1050] - mean(world_bd$new_deaths[1:1050]))^2)
R_square <- 1 - SSE / SST
R_square


tiff("CDeath.tiff", units="in", width=15, height=15, res=300)
gridExtra::grid.arrange(x,y,z,yy,yyy)
dev.off()



library(lmtest)
library(vcov)
lmtest::coeftest(modArima)

coefci(modArima)





