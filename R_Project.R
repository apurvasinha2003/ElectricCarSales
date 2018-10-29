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
#decompose data into trend and seasonality
plot(decompose(Qty_ts,type = c("additive")))

library(urca)
summary(ur.kpss(Qty_ts))#donot reject null hypothesis
### Number of differences needed to make the data stationary
ndiffs(Qty_ts)
Qty_ts %>% diff() %>% ur.kpss() %>% summary()#after diff, trend is stationary

#Qty_ts<-log(Qty_ts)
Quantity_Cars_ts<- window(x = Qty_ts, start = 2011)
plot.ts(plot.type = 'single', Quantity_Cars_ts)

#partition
qty_train<-window(Quantity_Cars_ts, start = 2011, c(2016,12))
qty_train
qty_test<- window(Quantity_Cars_ts, start = 2017, end = 2018)
qty_test
# dataset
qty_train %>% diff(lag=1)%>% diff(lag=12) %>% autoplot()
qty_train %>% diff(lag=1)%>% diff(lag=12) %>% ur.kpss() %>% summary()#after diff, trend is stationary
ndiffs(qty_train)
qty_train %>% diff() %>% ggtsdisplay(main="")

#plot of current scenario
plot(qty_train)
ggAcf(qty_train,main='ACF')
ggPacf(qty_train,main='PACF')
#ARIMA model 

autoArima_train <- auto.arima(qty_train)
summary(autoArima_train)

plot(autoArima_train)
#check for accuracy

accuracy(autoArima_train)

#check residual
checkresiduals(autoArima_train)

#arima model
qty_train<- diff(x = qty_train,lag = 12)
qty_test<- diff(x = qty_test,lag = 12)

Arima_train <- arima(qty_train,order = c(2,1,2))

plot(Arima_train)
#check for accuracy
summary(Arima_train)
accuracy(Arima_train)

#check residual
checkresiduals(Arima_train)

#forecast model using HoltWinters method for training data set
model <- hw(qty_train, initial='optimal', h=11)
plot(model)
accuracy(model)
sum(round(model$mean))  #Total sales in predicted 12 months
summary(model)
#acf and pacf plot for model residuals

checkresiduals(model)

#final prediction using auto.arima and arima
#prediction of future 11 months based on training model auto arima
electric_car_predict_auto <- forecast(autoArima_train, h=11)
checkresiduals(electric_car_predict_auto)

#prediction of future 11 months based on training model arima
electric_car_predict_arima <- forecast(Arima_train, h=11)
checkresiduals(electric_car_predict_arima)

#Examine Total CO2 ------------------------------------------------------
CO2_ts <- ts(data=norway_cars$Avg_CO2, start=2007, freq=12)
plot(CO2_ts)
#CO2_ts<-log(CO2_ts)
plot.ts(plot.type = 'single', CO2_ts)

CO2_ts %>% diff(lag=1)%>% diff(lag=12) %>% autoplot()
CO2_ts %>% diff() %>% ggtsdisplay(main="")


#partition
qty_train_co2<-window(CO2_ts, start = 2007, c(2016,12))
qty_train_co2
qty_test_co2<- window(CO2_ts, start = 2017, end = 2018)
qty_test_co2

#dataset
#ARIMA model 
qty_train_co2 <- diff(x = qty_train_co2,lag = 12)
qty_test_co2<- diff(x = qty_test_co2,lag = 12)

#plot of current scenario
plot(qty_train_co2)

autoArima_train <- auto.arima(qty_train_co2)

plot(autoArima_train)
#check for accuracy
summary(autoArima_train)
mean(autoArima_train$residuals)
#acf and pacf plot for model residuals
checkresiduals(autoArima_train)

#arima model
Arima_co2 <- arima(qty_train_co2,order = c(1,1,2))

plot(Arima_co2)
#check for accuracy
summary(Arima_co2)
accuracy(Arima_co2)
#residual plot

checkresiduals(Arima_co2)

#forecast model using HoltWinters method for training data set
model <- hw(qty_train_co2, initial='optimal', h=11, alpha =0.4)
plot(model)
accuracy(model)
sum(round(model$mean))  #Total CO2 in predicted 12 months
summary(model)

mean(model$residuals)
#acf and pacf plot for model residuals

checkresiduals(model)

#prediction of future 11 months based on training model
ArimaModel_train <- forecast(autoArima_train, h=11)
checkresiduals(ArimaModel_train)
#prediction of future 11 months based on training model
co2_predict_arima <- forecast(Arima_co2, h=11)
checkresiduals(co2_predict_arima)

#Examine Total cars sales for 2018------------------------------------------------------
Qty_car_ts <- ts(data=norway_cars$Quantity, start=2007, freq=12)
plot(Qty_car_ts)

Qty_car_ts %>% diff(lag=1)%>% autoplot()
Qty_car_ts %>% diff() %>% ggtsdisplay(main="")


#Qty_car_ts<- diff(x= Qty_car_ts, lag = 1)
plot.ts(plot.type = 'single', Qty_car_ts)

#partition
qty_train_car<-window(Qty_car_ts, start = 2007, c(2016,12))
qty_train_car
qty_test_car<- window(Qty_car_ts, start = 2017, end = 2018)
qty_test_car

#dataset
#ARIMA model 
autoArima_train_car <- auto.arima(qty_train_car)
#prediction of future 11 months data using training model
#ArimaModel_train_car <- forecast(autoArima_train_car, h=11)
plot(autoArima_train_car)
#check for accuracy
summary(autoArima_train_car)
mean(autoArima_train_car$residuals)
#acf and pacf plot for model residuals
#residual plot

checkresiduals(autoArima_train_car)

#arima model
Arima_car <- arima(qty_train_car,order = c(6,1,0))
#prediction of future 11 months based on training model
#car_predict_arima <- forecast(Arima_car, h=11)
plot(Arima_car)
#check for accuracy
summary(Arima_car)
accuracy(Arima_car)
#residual plot
checkresiduals(Arima_car)

#forecast model using HoltWinters method for training data set
model <- hw(qty_train_car, initial='optimal', h=11, alpha = 0.4)
plot(model)
accuracy(model)
sum(round(model$mean))  #Total sales in predicted 12 months
summary(model)
mean(model$residuals)
#acf and pacf plot for model residuals
#residual plot

checkresiduals(model)

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

