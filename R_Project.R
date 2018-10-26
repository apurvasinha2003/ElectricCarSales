library(Hmisc)
library(ggplot2)
library(forecast)
install.packages("robustHD")
library(robustHD)
library(dplyr)
library(expsmooth)
library(fpp)
library(tidyr)
library(tseries)

#Set Working directory
setwd("C:/Users/Apurva Sinha/Desktop/Project_R/")
#Read data & Pre-processing ---------
norway_cars <- read.csv('norway_new_car_sales_by_month_v1.2.csv')

#Examine Electric cars sales ------------------------------------------------------
Qty_ts <- ts(data=norway_cars$Quantity_Electric, start=2007, freq=12)
plot(Qty_ts)
Qty_ts<-log(Qty_ts)
Quantity_Cars_ts<- window(x = Qty_ts, start = 2011)
plot.ts(plot.type = 'single', Quantity_Cars_ts)
#plot of current scenario
plot(Quantity_Cars_ts)
#partition for training and test set
# qty_train<-window(Quantity_Cars_ts, start = 2011, c(2015,12))
# qty_train
# qty_test<- window(Quantity_Cars_ts, start = 2016, end = 2018)
# qty_test

# dataset
#ARIMA model 
Quantity_Cars_ts %>% diff(lag=1) %>% autoplot()
Quantity_Cars_ts<- diff(x = Quantity_Cars_ts,lag = 1)
autoArima_train <- auto.arima(Quantity_Cars_ts)
#prediction of future 11 months based on training model
electric_car_predict <- forecast(autoArima_train, h=11)
plot(electric_car_predict)
#check for accuracy
summary(electric_car_predict)
accuracy(electric_car_predict)
acf(ts(autoArima_train$residuals),main='ACF Residual')
pacf(ts(autoArima_train$residuals),main='PACF Residual')
#residual plot
qqnorm(autoArima_train$residuals)
qqline(autoArima_train$residuals)

#arima model
Arima_train <- arima(Quantity_Cars_ts,order = c(2,0,1))
#prediction of future 11 months based on training model
electric_car_predict_arima <- forecast(Arima_train, h=11)
plot(electric_car_predict_arima)
#check for accuracy
summary(electric_car_predict_arima)
accuracy(electric_car_predict_arima)
acf(ts(Arima_train$residuals),main='ACF Residual')
pacf(ts(Arima_train$residuals),main='PACF Residual')
#residual plot
qqnorm(Arima_train$residuals)
qqline(Arima_train$residuals)


#forecast model using HoltWinters method for training data set
model <- hw(Quantity_Cars_ts, initial='optimal', h=11)
plot(model)
accuracy(model)
sum(round(model$mean))  #Total sales in predicted 12 months
summary(model)
#acf and pacf plot for model residuals
acf(ts(model$residuals),main='ACF Residual')
pacf(ts(model$residuals),main='PACF Residual')
#residual plot
qqnorm(model$residuals)
qqline(model$residuals)

# #Exponential Smoothening training
# ets_train <- ets(Quantity_Cars_ts)
# ets_train
# 
# #Forecast for ets component training
# fcast_ets_train <- forecast(ets_train, h = 11)
# plot(fcast_ets_train)
# summary(fcast_ets_train)
# #Accuracy of model
# accuracy(fcast_ets_train) 
# mean(fcast_ets_train$residuals)
# #acf and pacf plot for model residuals
# acf(ts(fcast_ets_train$residuals),main='ACF Residual')
# pacf(ts(fcast_ets_train$residuals),main='PACF Residual')
# #residual plot
# qqnorm(fcast_ets_train$residuals)
# qqline(fcast_ets_train$residuals)
# 
#Examine Total CO2 ------------------------------------------------------
CO2_ts <- ts(data=norway_cars$Avg_CO2, start=2007, freq=12)
plot(CO2_ts)
CO2_ts<-log(CO2_ts)
plot.ts(plot.type = 'single', CO2_ts)
# 
# #partition of data into training and test data
# CO2_train<-window(CO2_ts, start = 2007, c(2015,12))
# CO2_train
# CO2_test<- window(CO2_ts, start = 2016, end = 2018)
# CO2_test

#dataset
#ARIMA model 
co2_ts <- diff(x = CO2_ts,lag = 12)
autoArima_train <- auto.arima(CO2_ts)
#prediction of future 11 months based on training model
ArimaModel_train <- forecast(autoArima_train, h=11)
plot(ArimaModel_train)
#check for accuracy
summary(ArimaModel_train)
mean(ArimaModel_train$residuals)
#acf and pacf plot for model residuals
acf(ts(ArimaModel_train$residuals),main='ACF Residual')
pacf(ts(ArimaModel_train$residuals),main='PACF Residual')
#residual plot
qqnorm(ArimaModel_train$residuals)
qqline(ArimaModel_train$residuals)

#forecast model using HoltWinters method for training data set
model <- hw(CO2_ts, initial='optimal', h=11)
plot(model)
accuracy(model)
sum(round(model$mean))  #Total CO2 in predicted 12 months
summary(model)
mean(model$residuals)
#acf and pacf plot for model residuals
acf(ts(model$residuals),main='ACF Residual')
pacf(ts(model$residuals),main='PACF Residual')
#residual plot
qqnorm(model$residuals)
qqline(model$residuals)

# #Exponential Smoothening training
# ets_co2_train <- ets(CO2_ts)
# ets_co2_train
# 
# #Forecast for ets component for 11 months based on training model
# fcast_ets_co2_train <- forecast(ets_co2_train, h = 11)
# plot(fcast_ets_co2_train)
# summary(fcast_ets_co2_train)
# #Accuracy
# accuracy(fcast_ets_co2_train) 
# mean(fcast_ets_co2_train$residuals)
# #acf and pacf plot for model residuals
# acf(ts(fcast_ets_co2_train$residuals),main='ACF Residual')
# pacf(ts(fcast_ets_co2_train$residuals),main='PACF Residual')
# #residual plot
# qqnorm(fcast_ets_co2_train$residuals)
# qqline(fcast_ets_co2_train$residuals)
# 
#Examine Total cars sales for 2018------------------------------------------------------
Qty_car_ts <- ts(data=norway_cars$Quantity, start=2007, freq=12)
plot(Qty_car_ts)
Qty_car_ts<- diff(x= Qty_car_ts, lag = 2)
plot.ts(plot.type = 'single', Qty_car_ts)
# 
# #partition of data into training and test model
# qty_car_train<-window(Qty_car_ts, start = 2011, c(2015,12))
# qty_car_train
# qty_car_test<- window(Qty_car_ts, start = 2016, end = 2018)
# qty_car_test

#dataset
#ARIMA model 
autoArima_train_car <- auto.arima(Qty_car_ts)
#prediction of future 11 months data using training model
ArimaModel_train_car <- forecast(autoArima_train_car, h=11)
plot(ArimaModel_train_car)
#check for accuracy
summary(ArimaModel_train_car)
mean(ArimaModel_train_car$residuals)
#acf and pacf plot for model residuals
acf(ts(ArimaModel_train_car$residuals),main='ACF Residual')
pacf(ts(ArimaModel_train_car$residuals),main='PACF Residual')
#residual plot
qqnorm(ArimaModel_train_car$residuals)
qqline(ArimaModel_train_car$residuals)

#forecast model using HoltWinters method for training data set
model <- hw(Qty_car_ts, initial='optimal', h=11)
plot(model)
accuracy(model)
sum(round(model$mean))  #Total sales in predicted 12 months
summary(model)
mean(model$residuals)
#acf and pacf plot for model residuals
acf(ts(model$residuals),main='ACF Residual')
pacf(ts(model$residuals),main='PACF Residual')
#residual plot
qqnorm(model$residuals)
qqline(model$residuals)

# #Exponential Smoothening training
# ets_train <- ets(qty_car_test)
# ets_train
# 
# #Forecast for ets component training
# fcast_ets_train <- forecast(qty_car_test, h = 11)
# plot(fcast_ets_train)
# summary(fcast_ets_train)
# #Accuracy
# accuracy(fcast_ets_train) 
# mean(fcast_ets_train$residuals)
# #acf and pacf plot for model residuals
# acf(ts(fcast_ets_train$residuals),main='ACF Residual')
# pacf(ts(fcast_ets_train$residuals),main='PACF Residual')
# #residual plot
# qqnorm(fcast_ets_train$residuals)
# qqline(fcast_ets_train$residuals)
# 
# 
###Plots
#By Month total sales plot ---------

by_month <- read.csv('norway_new_car_sales_by_month_v1.2.csv')

by_month %>% 
  filter(Year<2018) %>%
  mutate(Date=as.Date(paste(Year, Month, "1", sep="-"))) %>% 
  select(Date, Quantity, Quantity_Diesel,Quantity_Electric,Quantity_Hybrid) %>% 
  gather(key=type, value=value, -Date) %>% 
  ggplot()+
  #scale_x_continuous(breaks=2007:2018)+
  geom_line(mapping = aes(x=Date, y=value, color=type), size=1.1)+
  theme_minimal()+
  labs(y="Sales, in  units",
       x="Year",
       color=NULL,
       title="Car sales in Norway",
       subtitle="Sales of all vehicles") 

#BY MAKE PART 2 ------------
by_make <- read.csv("norway_new_car_sales_by_make_v1.2.csv")
str(by_make)

#top makers by year
group_by_year <- by_make %>%
  group_by(Year,Month,Make) %>%
  filter(Year<2018) %>%
  summarise(Qty=sum(Quantity)) %>%
  #top_n(20,Qty) %>%
  arrange(Year,Month,desc(Qty))

#top makers overall
makers <- by_make %>%
  group_by(Make) %>%
  filter(Year<2018) %>%
  summarise(Qty=sum(Quantity))%>%
  arrange(desc(Qty)) %>%
  top_n(5,Qty) %>%
  select(Make)

makers <- (makers$Make)
#str(makers)

#selecting distinct
top_maker <- NULL
top_maker_ts <- NULL

for (i in 1:5){
  top_maker[[i]] <- group_by_year %>%
    filter(Make==makers[i])
  top_maker_ts[[i]] <- ts(top_maker[[i]][[4]], start=2007, freq=12)   #convert to time series
  Model <- forecast(auto.arima(top_maker_ts[[i]]),h=12)     #fit a model and forecast
  top_maker[[i]] <- c(top_maker[[i]][[4]],round(Model$mean))
}

#group into year wise sales
#top_maker_volks <- ts(top_maker[[1]], start=2007, freq=12) 
top_makers_2 <- ts(top_maker, start=2007, frequency = 12)
str(top_makers_2)
names(top_makers_2) <- makers
names(top_maker) <- makers
top_makers_2$date <- seq(as.Date("2007/01/01"),as.Date("2018/12/01"),'month')


yearwise <- function(x){
  i<-1
  maker <- NULL
  while(i<144){
    maker <- c(maker,sum(x[i:(i+11)]))
    i <- i+12
  }
  return(maker)
}

makers
top_maker
Volkswagen <- yearwise(top_maker[[1]])
Toyota <- yearwise(top_maker[[2]])   
Volvo <- yearwise(top_maker[[3]])
Ford <- yearwise(top_maker[[4]])
BMW <- yearwise(top_maker[[5]])

#plot of top makers with prediction:
ggplot()+
  geom_line(aes(y=Volkswagen, x=c(2007:2018), colour="Volkswagen"),size=1.2)+
  geom_line(aes(y=Toyota , x=c(2007:2018), color="Toyota"),size=1.2)+
  geom_line(aes(y=Volvo , x=c(2007:2018), color="Volvo"),size=1.2)+
  geom_line(aes(y=Ford , x=c(2007:2018), color='Ford'),size=1.2)+
  geom_line(aes(y=BMW , x=c(2007:2018), color='BMW'),size=1.2)+
  scale_x_continuous(breaks=2007:2018)+
  xlab('YEAR')+
  ylab('Quantity')+
  labs(title="Top 5 Manufacturers overall Sales")+
  theme_minimal()


#By Car Model analysis ----
by_model <- read.csv("norway_new_car_sales_by_model_v1.1.csv")
str(by_model)

#top models by year
models_group_by_year <- by_model %>%
  group_by(Year,Model) %>%
  filter(Year<2018) %>%
  summarise(Qty=sum(Quantity)) %>%
  top_n(25,Qty) %>%
  arrange(Year,desc(Qty))

#top Models overall
models <- by_model %>%
  group_by(Model) %>%
  filter(Year<2018) %>%
  summarise(Qty=sum(Quantity))%>%
  arrange(desc(Qty)) %>%
  top_n(5,Qty) %>%
  select(Model)

models <- (models$Model)
#str(makers)

#selecting distinct
top_model <- NULL
top_model_ts <- NULL

for (i in 1:5){
  top_model[[i]] <- models_group_by_year %>%
    filter(Model==models[i])
  top_model_ts[[i]] <- ts(top_model[[i]][[3]], start=2007, freq=1)   #convert to time series
  Model <- forecast(auto.arima(top_model_ts[[i]]),h=1)     #fit a model and forecast
  top_model[[i]] <- c(top_model[[i]][[3]],round(Model$mean))
}

top_cars <- ts(top_model, start=2007)
str(top_cars)
names(top_cars) <- models


#plot of top models with prediction:
ggplot()+
  geom_line(aes(y=top_cars$`Volkswagen Golf` , x=c(2007:2018), colour="Volkswagen Golf"),size=1.1)+
  geom_line(aes(y=top_cars$`Volkswagen Passat` , x=c(2007:2018), color="Volkswagen Passat"),size=1.1)+
  geom_line(aes(y=top_cars$`Toyota Auris` , x=c(2007:2018), color="Toyota Auris"),size=1.1)+
  geom_line(aes(y=top_cars$`Skoda Octavia` , x=c(2007:2018), color='Skoda Octavia'),size=1.1)+
  geom_line(aes(y=top_cars$`Toyota Yaris` , x=c(2007:2018), color='Toyota Yaris'),size=1.1)+
  scale_x_continuous(breaks=2007:2018)+
  xlab('YEAR')+
  ylab('Quantity')+
  labs(title='Most Popular Cars of Norway')+
  theme_minimal()

