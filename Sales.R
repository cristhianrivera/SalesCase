packs <- c('tidyverse'
           ,'reshape2'
           ,'ggplot2'
           ,'plotly'
           ,'rpivotTable'
           ,'keras'
           ,'lubridate'
           ,'dplyr'
           ,'forecast'
           ,'xts')

library(tidyverse)
library(reshape2)
library(ggplot2)
library(plotly)
library(rpivotTable)
library(keras)
library(lubridate)
library(dplyr)
library(forecast)
library(xts)

setwd("/Users/Cristhian/Documents/Sales/")

data <- read.csv("Sales.csv", header = TRUE, sep = ",")

dictDates <- as.data.frame(ymd("2016-12-01") + days(0:395))
colnames(dictDates)<-c('date')

data$date <- as.Date(data$date)
data <- left_join(dictDates,data)

data$sales<- ifelse(is.na(data$sales),0,data$sales)

initialSummary <- data %>%
  group_by(country) %>%
  summarise(count = n_distinct(customer),
            sumSales = sum(sales)) %>%
  mutate(proportionSales = 100*sumSales/sum(sumSales),
         proportionCountries = 100*count/sum(count))

View(initialSummary)

first(as.Date(data$date))
last(as.Date(data$date))

data <- data %>%
  filter(date<=as.Date('2017-12-01'))

initialVisualization <- data %>%
  group_by(date) %>%
  summarise(sumSales = sum(sales))

ggplot(initialVisualization)+
  geom_line(aes(x=date,y=sumSales, group=1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Visualization1 <- data %>%
  group_by(date = cut(as.Date(date) ,'week'))%>%
  summarise(sumSales = sum(sales))

ggplot(Visualization1)+
  geom_line(aes(x=date,y=sumSales, group=1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

acf(Visualization1$sumSales)
pacf(Visualization1$sumSales)



acf(diff(Visualization1$sumSales))
pacf(diff(Visualization1$sumSales))

Visualization2 <- data %>%
  group_by(country)%>%
  summarise(customer = n_distinct(customer))
table(Visualization2)

Visualization3 <- data %>%
  group_by(customer)%>%
  summarise(sales = sum(sales),
            n_sales = n()) %>%
  mutate(avgSale = sales/n_sales )

Visualization4 <- data %>%
  group_by(date = cut(as.Date(date) ,'week'))%>%
  summarise(sales = sum(sales),
            n_sales = n()) %>%
  mutate(avgSale = sales/n_sales )

ggplot(Visualization4)+
  geom_line(aes(x=date,y=avgSale, group=1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

GroupSales <- data %>%
  group_by(customer)%>%
  summarise(sales = sum(sales), count = n())%>%
  mutate(limitSales = ifelse((sales-mean(sales))/sd(sales)<=2.5, 1, 0)) 

hist(GroupSales$sales,breaks = 100)

km <- kmeans(c(
  (GroupSales$sales - mean(GroupSales$sales))/sd(GroupSales$sales), 
  (GroupSales$count - mean(GroupSales$count))/sd(GroupSales$count) ),5)


GroupSales1 <- GroupSales %>%
  filter(limitSales==1)  

GroupSales0 <- GroupSales %>%
  filter(limitSales==0)  

ggplot(GroupSales1)+
  geom_point(aes( x=(sales-mean(sales))/sd(sales), y=(count-mean(count))/sd(count)))+
  theme_minimal()

ggplot(GroupSales0)+
  geom_point(aes( x=(sales-mean(sales))/sd(sales), y=(count-mean(count))/sd(count)))+
  theme_minimal()

gs1<- select(GroupSales1,sales,count)
gs1$sales<-(gs1$sales-mean(gs1$sales))/ sd(gs1$sales)
gs1$count<-(gs1$count-mean(gs1$count))/ sd(gs1$count)
km1 <- kmeans(gs1,5)
ggplot(GroupSales1, aes( x=sales, y=count, color=km1$cluster))+
  geom_point()+
  theme_minimal()


GroupSales1<- GroupSales1 %>% add_column(cluster = km1$cluster)
gs1Dict <- GroupSales1 %>% select(customer,cluster)

custDict <- data %>% 
  group_by(customer) %>%
  summarise(count = n())%>%
  select(customer)

custDict <- left_join( custDict , gs1Dict)
custDict$cluster <- ifelse(is.na(custDict$cluster),0,custDict$cluster)

ModelsByClst <- function(custDict, data){
  clModels <- list()
  for(i in 0:5){
    toTs <- custDict %>%
      filter(cluster == i) %>%
      left_join(data) %>%
      group_by(date = cut(as.Date(date) ,'week')) %>%
      summarise(sales = sum(sales), count_clients = n_distinct(customer) )%>%
      mutate(avgSales = sales/count_clients )%>%
      filter(as.Date(date) < as.Date('2017-12-01'))
    
    clTs <- xts(toTs$avgSales,as.Date(toTs$date))  
    fit <- auto.arima(clTs)
    print(i)
    print(fit)
    clModels[[i+1]] <- fit
  }
  return(clModels)
}


mods <- ModelsByClst(custDict,data)


dictDates <- as.data.frame(ymd("2016-12-01") + weeks(0:57))
colnames(dictDates)<-c("date")


ForDecSls <- function(custDict){
  CustList <- list()
  SlsList <- list()
  compData <- data.frame(customer = integer(), sales= integer())
  for (i in 1:length(custDict$customer)){
    nCustomer <- custDict$customer[i]
    nCluster <- custDict$cluster[i] +1
    
    toTs <- data %>%
      filter(customer == nCustomer) %>%
      group_by(date) %>%
      summarise(sales=sum(sales))
    
    toTs$date<-as.Date(as.character(toTs$date))
    
    toTs <- left_join(dictDates,toTs)
    
    toTs$sales <- ifelse(is.na(toTs$sales),0,toTs$sales)
    
    toTs <- toTs %>%
      group_by(date = cut(as.Date(date) ,'week')) %>%
      summarise(sales = sum(sales))%>%  
      filter(as.Date(date) < as.Date('2017-12-01'))
    
    clTs <- xts(toTs$sales,as.Date(toTs$date))  
    fit<-Arima(clTs,model = mods[[nCluster]])
    forSales<-forecast(fit, h=4)
    slsDec <- sum(forSales[4]$mean[1:4])
    
    compData <- rbind(compData,c(nCustomer, slsDec))
    
  }
  
  colnames(compData) <- c('Customer','SlsDecember')
  return(compData)
  
}

DecemberSls <- ForDecSls(custDict)
View(DecemberSls)


#------------------------------------------------
#------------------------------------------------











data <- read.csv("Sales.csv", header = TRUE, sep = ",")
dictDates <- as.data.frame(ymd("2016-12-01") + days(0:395))
colnames(dictDates)<-c('date')
# 
# data$date <- as.Date(data$date)
# data <- left_join(dictDates,data)
# data$sales<- ifelse(is.na(data$sales), 0, data$sales)


unUK <- read.csv("unemploymentUK.csv", header = FALSE, sep = "")

colnames(unUK)<-c('date','rate')
unUK$date<- as.Date( unUK$date , '%m/%d/%Y')

rpivotTable(data)

first(data$date)
last(data$date)

initialVisualization <- data %>%
  group_by(date) %>%
  summarise(sumSales = sum(sales))

ggplot(initialVisualization)+
  geom_line(aes(x=date,y=sumSales, group=1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Let's understand the data

first(as.Date(data$date))
last(as.Date(data$date))


initialSummary <- data %>%
  group_by(country) %>%
  summarise(count = n_distinct(customer),
            sumSales = sum(sales)) %>%
  mutate(proportionSales = 100*sumSales/sum(sumSales),
         proportionCountries = 100*count/sum(count))


Visualization2 <- data %>%
  group_by(country)%>%
  summarise(customer = n_distinct(customer))
table(Visualization2)

Visualization3 <- data %>%
  group_by(customer)%>%
  summarise(sales = sum(sales),
            n_sales = n()) %>%
  mutate(avgSale = sales/n_sales )

Visualization4 <- data %>%
  group_by(date = cut(as.Date(date) ,'week'))%>%
  summarise(sales = sum(sales),
            n_sales = n()) %>%
  mutate(avgSale = sales/n_sales )


mean(Visualization3$avgSale)

ggplot(Visualization4)+
  geom_line(aes(x=date,y=avgSale, group=1))


GroupSales <- data %>%
  group_by(customer)%>%
  summarise(sales = sum(sales), count = n())%>%
  mutate(limitSales = ifelse((sales-mean(sales))/sd(sales)<=2.5, 1, 0)) 
sum(GroupSales$sales)

GroupSales1 <- GroupSales %>%
  filter(limitSales==1)  
length(GroupSales1$customer)
summarise(GroupSales1, n_distinct(customer)) 
sum(GroupSales1$sales)/sum(GroupSales$sales)

GroupSales0 <- GroupSales %>%
  filter(limitSales==0)  
length(GroupSales0$customer)
summarise(GroupSales0, n_distinct(customer)) 
sum(GroupSales0$sales)/sum(GroupSales$sales)

ggplot(GroupSales1)+
  geom_point(aes( x=(sales-mean(sales))/sd(sales), y=(count-mean(count))/sd(count)))+
  theme_minimal()

ggplot(GroupSales0)+
  geom_point(aes( x=(sales-mean(sales))/sd(sales), y=(count-mean(count))/sd(count)))+
  theme_minimal()

gs1<- select(GroupSales1,sales,count)
gs1$sales<-(gs1$sales-mean(gs1$sales))/ sd(gs1$sales)
gs1$count<-(gs1$count-mean(gs1$count))/ sd(gs1$count)
km1 <- kmeans(gs1,5)
ggplot(GroupSales1, aes( x=sales, y=count, color=km1$cluster))+
  geom_point()+
  theme_minimal()


GroupSales1<- GroupSales1 %>% add_column(cluster = km1$cluster)
gs1Dict <- GroupSales1 %>% select(customer,cluster)

custDict <- data %>% 
  group_by(customer) %>%
  summarise(count = n())%>%
  select(customer)

custDict <- left_join( custDict , gs1Dict)
custDict$cluster <- ifelse(is.na(custDict$cluster),0,custDict$cluster)

toTs <- custDict %>%
  filter(cluster== 2) %>%
  left_join(data) %>%
  group_by(date = cut(as.Date(date) ,'week')) %>%
  summarise(sales = sum(sales), count_clients = n_distinct(customer) )%>%
  mutate(avgSales = sales/count_clients )%>%
  filter(as.Date(date) < as.Date('2017-12-01'))
clTs <- xts(toTs$avgSales,as.Date(toTs$date))  
plot(clTs)
acf(clTs)
pacf(clTs)
fit<-auto.arima(clTs)

plot(forecast(fit,h=3))
res1 <- residuals(fit)
tsdisplay(res1)
mean(res1)
accuracy(forecast(fit,h=3))


lcl <- 0:5

ModelsByClst <- function(custDict, data){
  clModels <- list()
  for(i in 0:5){
    toTs <- custDict %>%
      filter(cluster == i) %>%
      left_join(data) %>%
      group_by(date = cut(as.Date(date) ,'week')) %>%
      summarise(sales = sum(sales), count_clients = n_distinct(customer) )%>%
      mutate(avgSales = sales/count_clients )%>%
      filter(as.Date(date) < as.Date('2017-12-01'))
    
    clTs <- xts(toTs$avgSales,as.Date(toTs$date))  
    fit <- auto.arima(clTs)
    print(i)
    print(fit)
    clModels[[i+1]] <- fit
    }
  return(clModels)
}

mods <- ModelsByClst(custDict,data)
#--------
dictDates <- as.data.frame(ymd("2016-12-01") + weeks(0:57))
colnames(dictDates)<-c("date")

ForDecSls <- function(custDict){
  CustList <- list()
  SlsList <- list()
  compData <- data.frame(customer = integer(), sales= integer())
  for (i in 1:length(custDict$customer)){
    nCustomer <- custDict$customer[i]
    nCluster <- custDict$cluster[i] +1
    
    toTs <- data %>%
      filter(customer == nCustomer) %>%
      group_by(date) %>%
      summarise(sales=sum(sales))
    
    toTs$date<-as.Date(as.character(toTs$date))
    
    toTs <- left_join(dictDates,toTs)
    
    toTs$sales <- ifelse(is.na(toTs$sales),0,toTs$sales)
    
    toTs <- toTs %>%
      group_by(date = cut(as.Date(date) ,'week')) %>%
      summarise(sales = sum(sales))%>%  
      filter(as.Date(date) < as.Date('2017-12-01'))
    
    clTs <- xts(toTs$sales,as.Date(toTs$date))  
    fit<-Arima(clTs,model = mods[[nCluster]])
    forSales<-forecast(fit, h=4)
    slsDec <- sum(forSales[4]$mean[1:4])
    
    compData <- rbind(compData,c(nCustomer, slsDec))
    
  }
  
  colnames(compData) <- c('Customer','SlsDecember')
  return(compData)
  
}

DecemberSls <- ForDecSls(custDict)
View(DecemberSls)


compData <- data.frame(customer = numeric, sales= number)
compData[,1]- c(5, 443)
compData <- rbind(compData,c(5, 443))


nCustomer <- custDict$customer[10]
nCluster <- custDict$cluster[10] +1


toTs <- data %>%
  filter(customer == nCustomer) %>%
  group_by(date) %>%
  summarise(sales=sum(sales))

toTs$date<-as.Date(as.character(toTs$date))

toTs <- left_join(dictDates,toTs)

toTs$sales <- ifelse(is.na(toTs$sales),0,toTs$sales)

toTs <- toTs %>%
  group_by(date = cut(as.Date(date) ,'week')) %>%
  summarise(sales = sum(sales))%>%  
  filter(as.Date(date) < as.Date('2017-12-01'))

clTs <- xts(toTs$sales,as.Date(toTs$date))  
plot(clTs)
fit<-Arima(clTs,model = mods[[nCluster]])
forSales<-forecast(fit, h=4)
plot(forSales)
(slsDec <- sum(forSales[4]$mean[1:4]))

slsDec <- c(slsDec,slsDec)
nCustomer <- c(nCustomer,nCustomer)

slsDec<-as.data.frame(slsDec)
nCustomer<-as.data.frame(nCustomer)

bind_cols(slsDec,nCustomer)

hist(ss$cluster)






km1 <- kmeans(c(
  (GroupSales1$sales - mean(GroupSales1$sales))/sd(GroupSales1$sales), 
  (GroupSales1$count - mean(GroupSales1$count))/sd(GroupSales1$count) ),5)

length(GroupSales1$count)
length(km1$cluster)
km1$cluster <- as.factor(km1$cluster)
ggplot(gs1, aes( x=sales, y=count, color=km1$cluster))+
  geom_point()+
  theme_minimal()

ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$cluster)) + geom_point()

plot((GroupSales$sales - mean(GroupSales$sales))/sd(GroupSales$sales), 
  (GroupSales$count - mean(GroupSales$count))/sd(GroupSales$count) )


gs <- sample_n(GroupSales,size=1000)
plot((gs$sales - mean(gs$sales)) / sd(gs$sales),
     (gs$count - mean(gs$count))/sd(gs$count)
     )




hist(km$cluster)
hist(GroupSales$count)
hist(GroupSales$sales)
rpivotTable(Visualization3)

sample_n(Visualization3,size = 2) 
#Group by country, to see if the data can be handled with ARIMA models


#Group by product

byProduct <- data %>%
  group_by(date, country, product) %>%
  summarise(count = n(), sales = sum(sales))
  

#the aim is to predict sales of a month, so let's see how aggregate can the data be
byMonth <- data %>%
  group_by(country,date = as.Date(paste(year(data$date),month(data$date),"01", sep="/"),"%Y/%m/%d")) %>%
  summarise(count = n_distinct(customer),
            sumSales = sum(sales))

byWeek <- data %>%
  group_by(country, date = cut(as.Date(date) ,'week'))%>%
  summarise(count = n_distinct(customer),
            sumSales = sum(sales))


#by week sounds reasonable, options:
#  1- group by country and compute ARIMAS for countries
#  2- group by amount of sales and ARIMAS for different group of sales
#  3- LSTM for every customer


byWeekCountry <- data %>%
  group_by(customer, country, date = cut(as.Date(date) ,'week'))%>%
  summarise(sumSales = sum(sales))


#to test if country based groups makes sense
uk_comp<-data %>% 
  filter(country == 'United Kingdom') %>% 
  group_by(date = as.Date(cut(as.Date(date) ,'month'))) %>% 
  summarise(sumSales = sum(sales))

unUK<-unUK %>% 
  filter(date >= '2016-12-01') %>% 
  group_by(date = cut(as.Date(date) ,'month')) %>% 
  summarise(sumSales = sum(sales))

UKCOMP<-left_join(uk_comp,unUK)

q <-ggplot(data=UKCOMP)+
  geom_line(aes(x=date,y=sumSales, group = 1))+
  geom_line(aes(x=date,y=rate,group = 1),color='light blue')+
  theme_minimal()

ggplotly(q)

#for correlation purposes
UKCOMP$sumSales <- (UKCOMP$sumSales - mean(UKCOMP$sumSales))/sd(UKCOMP$sumSales)
UKCOMP$rate <- (UKCOMP$rate - mean(UKCOMP$rate))/sd(UKCOMP$rate)

cor(UKCOMP$sumSales, UKCOMP$rate) 

#by now, creating groups by country makes sense.
#next step, directly into some predictions.

byCountry <- data %>%
  group_by(country, customer, date = cut(as.Date(date) ,'month'))%>%
  summarise(sumSales = sum(sales), 
            customers = n_distinct(customer))%>%
  mutate(avgSales = sumSales/customers)
rpivotTable(byCountry)
listCustomers <- data %>%
  group_by(customer) %>%
  summarise(n_times = n())

#------------------above here everything needs to be executed
toTs<-byCountry %>%
  filter(country =="Spain", as.Date(date) < as.Date('2017-12-01'))

toTs1<-byCountry %>%
  filter(customer == "12417", as.Date(date) < as.Date('2017-12-01'))
rpivotTable(filter(data, customer== "12417"))

s1 <- ts(toTs$avgSales, frequency = 365.25/7, start = decimal_date(ymd("2016-11-28")) )

s1 <- xts(toTs$avgSales,as.Date(toTs$date))

s1 <- ts(toTs$avgSales)
s2 <- ts(toTs1$avgSales)
length(s1)
x <- 2017.923
format(date_decimal(2016.90710382514), "%Y-%m-%d")

plot(s1)
fit1<-auto.arima((s1))
fit2<-Arima(s2,model = fit1)



plot(forecast(fit1,h=4))
plot(forecast(fit2,h=4))
forSales <- forecast(fit1,h=4)
forSales[4]$mean[3]

ts(c(s1,forSales[4]))



as.xts(forSales[4]$mean)
comb <- ts.union(s1, forSales[4]$mean)

xts(rbind(s1, forSales[4]$mean))

NewS1 <- pmin(comb[,1], comb[,2], na.rm = TRUE)


ts(bind(s1,forSales[4]))
s1ToTs <- rbind(s1,forSales[4])
s1ToTs[2]
ts

res1 <- residuals(fit1)
tsdisplay(res1)
mean(res1)
accuracy(forecast(fit1,h=3))



refit <- Arima(y, model=mod1)
accuracy(fit)


#function to build different models for all the countries
CountryList <- c("Australia", "Austria", "Bahrain", "Belgium", "Brazil", "Canada", 
"Channel Islands", "Cyprus", "Czech Republic", "Denmark", "EIRE", 
"European Community", "Finland", "France", "Germany", "Greece", 
"Iceland", "Israel", "Italy", "Japan", "Lebanon", "Lithuania", 
"Malta", "Netherlands", "Norway", "Poland", "Portugal", "RSA", 
"Saudi Arabia", "Singapore", "Spain", "Sweden", "Switzerland", 
"USA", "United Arab Emirates", "United Kingdom", "Unspecified")

ModelsByCountry <- function(countryL,countryData){
  countryModels <- list()
  for(i in 1:length(countryL)){
    toTs<-countryData %>%
      filter(country == countryL[i], as.Date(date) < as.Date('2017-12-01'))
      countryTs <- xts(toTs$avgSales,as.Date(toTs$date))
    if(length(countryTs)>1){
      fit<-auto.arima(countryTs)
      countryModels[[i]] <- fit 
    }
  }
  return(countryModels)
}

res <- ModelsByCountry(CountryList,byCountry)

res[[2]]


# make groups of countries with less than 35 data points