## Load the required libraries

library(dplyr)
library(stringr)
library(graphics)
library(forecast)
library(tseries)
library(ggplot2)

## Load the data 
global_mart <- read.csv("Global Superstore.csv", stringsAsFactors = F)
head(global_mart)
str(global_mart)


## Data Preparation
# Checking for NA's
is.na(global_mart) %>% sum()
# 41296

# Checking NA's for postal code
is.na(global_mart$Postal.Code) %>% sum()
# 41296

# Removing Postal Code
global_mart$Postal.Code <- NULL

# Cheking for duplicate row
nrow(unique(global_mart))
# 51290 
# No duplicate rows

# Checking 21 unique buckets
buckets <- global_mart[,c("Market", "Segment")] %>% unique()

# Splitting for month and year for the data from 2011 to 2014
global_mart$Order.Month <- as.numeric(str_split(global_mart$Order.Date, "-", simplify = T)[,2])
global_mart$Order.Year <- as.numeric(str_split(global_mart$Order.Date, "-", simplify = T)[,3])

# Converting Order.Date to date Type
global_mart$Order.Date <- as.Date(global_mart$Order.Date,"%d-%m-%Y")


##  EDA
# Creating Sub global_mart containing monthly profit
subset_global_mart <- global_mart %>% 
  select(Market, Segment, Order.Month, Profit) %>% 
  group_by(Market, Segment, Order.Month) %>%
  mutate(Monthly.Profit = sum(Profit)) 

subset_global_mart <- subset_global_mart %>%
  select(Market, Segment, Order.Month, Monthly.Profit) %>%
  unique()

subset_global_mart$Order.Month <- as.factor(subset_global_mart$Order.Month)
subset_global_mart$Order.Month <- month.name[subset_global_mart$Order.Month]


# Plot For Monthly Profits, Market and Segment Wise
ggplot(subset_global_mart,aes(x=Order.Month,y=Monthly.Profit, group=1, color=Market)) +
  geom_point(alpha = 0.6) +
  geom_line() +
  theme_minimal() + theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(x="Order Month", y="Monthly Profits") +
  facet_grid(Market~Segment)


# Finding Coefficient of Variation of monthly sales
subset_global_mart <- subset_global_mart %>% 
  group_by(Market, Segment) %>% 
  summarise(Monthly_Profit.Mean = mean(Monthly.Profit), SD.Profit = sd(Monthly.Profit)) %>%
  mutate(CV.Profit = SD.Profit/Monthly_Profit.Mean) %>%
  arrange(CV.Profit)

# The two most profitable buckets
final_bucket <- subset_global_mart[1:2, 1:2]
final_bucket
#   Market  Segment 
# 1 EU      Consumer
# 2 APAC   Consumer


# Aggregated Sales, Profit and Quantity
aggr_sales <- global_mart %>% select(Market, Segment, Order.Date, Order.Month, Sales, Quantity, Profit) %>% 
  group_by(Market, Segment, Order.Month) %>%
  mutate(Monthly.Sales = sum(Sales), Monthly.Quantity = sum(Quantity), Monthly.Profit = sum(Profit)) 

aggr_sales$Order.Month <- month.name[aggr_sales$Order.Month]

ggplot(aggr_sales, aes(Order.Month, group=1))+
  geom_point(aes(y=Monthly.Sales, color="Monthly Sales"))+
  geom_line(aes(y=Monthly.Sales, color="Monthly Sales"))+
  geom_point(aes(y=Monthly.Quantity, color="Monthly Quantity"))+
  geom_line(aes(y=Monthly.Quantity, color="Monthly Quantity"))+
  geom_point(aes(y=Monthly.Profit, color="Monthly Profit"))+
  geom_line(aes(y=Monthly.Profit, color="Monthly Profit"))+
  facet_grid(Market~Segment)+ theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(x="Order Month", y="Count")

# Final data Preparation

## Sales Final Data
final_data_sales <- global_mart %>%
  select(Market, Segment, Order.Date, Order.Year, Order.Month, Sales) %>% 
  group_by(Market, Segment, Order.Month, Order.Year) %>%
  mutate(Monthly.Sales = sum(Sales)) %>%
  filter(Market %in% final_bucket$Market, Segment %in% final_bucket$Segment)

final_data_sales$Order.Month <- month.name[final_data_sales$Order.Month]

## Quantity Final Data
final_data_quantity <- global_mart %>%
  select(Market, Segment, Order.Date, Order.Year, Order.Month, Quantity) %>% 
  group_by(Market, Segment, Order.Month, Order.Year) %>%
  mutate(Monthly.Quantity = sum(Quantity)) %>%
  filter(Market %in% final_bucket$Market, Segment %in% final_bucket$Segment)

final_data_quantity$Order.Month <- month.name[final_data_quantity$Order.Month]


##===========================================##
#####       Smoothening Functions          #####
##===========================================##


#Smoothening the series - Moving Average Smoothing

smoothing <- function(timeser, w=3){
  smoothedseries <- stats::filter(timeser, 
                                  filter=rep(1/(2*w+1),(2*w+1)),
                                  method='convolution', sides=2)
  
  #Smoothening left end of the time series
  
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  #Smoothening right end of the time series
  
  n <- length(timeser)
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  smoothedseries
}




##############################################
### Creation of Sales Data for Time Series ###
##############################################
## EU Consumer Sales
Eu_Sales <- final_data_sales %>%
  filter(Market == "EU", Segment == "Consumer") %>%
  select(Order.Date, Monthly.Sales) %>% arrange(Order.Date)

Eu_Sales <-Eu_Sales[, c("Order.Month", "Monthly.Sales")] %>% unique()
Eu_Sales$Month <- row.names(Eu_Sales) %>% as.numeric()
Eu_Sales$Order.Month <- NULL
ploteusales <- ggplot(Eu_Sales,aes(x=Eu_Sales$Month,y=Eu_Sales$Monthly.Sales)) 
ploteusales +geom_bar(stat="identity", colour="black",fill="purple")+xlab("Month")+ylab("Monthly sales")+ggtitle("EU Consumer Sales Final data") 
 
  
## APAC Consumer Sales
Apac_Sales <- final_data_sales %>%
  filter(Market == "APAC", Segment == "Consumer") %>%
  select(Order.Date, Monthly.Sales) %>% arrange(Order.Date)

Apac_Sales <-Apac_Sales[, c("Order.Month", "Monthly.Sales")] %>% unique()
Apac_Sales$Month <- row.names(Apac_Sales) %>% as.numeric()
Apac_Sales$Order.Month <- NULL

plotapacsales <- ggplot(Apac_Sales,aes(x=Apac_Sales$Month,y=Apac_Sales$Monthly.Sales)) 
plotapacsales +geom_bar(stat="identity", colour="black",fill="purple")+xlab("Month")+ylab("Monthly sales")+ggtitle("APAC Consumer Sales Final data") 


######==============================================================================#######
## Predicting Series Using Time Series Using classical decomposition and ARIMA model

## Time Series for EU Consumer Sales

Eu_Sales_Timeser <- ts(Eu_Sales$Monthly.Sales)
plot(Eu_Sales_Timeser, col="blue")
kpss.test(Eu_Sales_Timeser)

# Creating indata and outdata with 42 train and 6 predict samples
indata_1 <- Eu_Sales[1:42, ]
outdata_1 <- data.frame(Eu_Sales[43:48, ])

# Timeseries model of indata model
timeserIn1 <- ts(indata_1$Monthly.Sales)
plot(timeserIn1, col="blue")

# Smoothing the timeseries
timeserIn2 <- smoothing(timeserIn1, 4)
lines(timeserIn2, col="orange", lwd=2)
timeserdf_1 <- as.data.frame(cbind(indata_1$Month, as.vector(timeserIn2)))
colnames(timeserdf_1) <- c('Month', 'Sales')

# Fitting Sinusoidal function
lmfit_1 <- lm(Sales~ sin(Month) + poly(Month), data = timeserdf_1)

# Prediction on indata
global_pred_1 <- predict(lmfit_1, Month=indata_1$Month)
summary(global_pred_1)
lines(indata_1$Month, global_pred_1, col='red', lwd=2)

local_pred_1 <- timeserIn2-global_pred_1
plot(local_pred_1, col='red', type = "l")
acf(local_pred_1)
acf(local_pred_1, type="partial")
armafit_1 <- auto.arima(local_pred_1)

tsdiag(armafit_1)
armafit_1

#  Check if the residual series is white noise

resi_1 <- local_pred_1-fitted(armafit_1)
plot(resi_1)
kpss.test(resi_1)

adf.test(resi_1,alternative = "stationary")
kpss.test(resi_1)

# Prediction on global data

global_pred_out_1 <- predict(lmfit_1, data.frame(Month=outdata_1$Month))
MAPE_1 <- accuracy(global_pred_out_1,outdata_1[,1])[5]
MAPE_1
# 28.30206


# Plot of prediction with original timeseries value

pred_1 <- c(ts(global_pred_1), ts(global_pred_out_1))
plot(Eu_Sales_Timeser, col="red")
lines(pred_1, col="green")


##=============================##
## Time Series for APAC Consumer Sales

Apac_Sales_Timeser <- ts(Apac_Sales$Monthly.Sales)
plot(Apac_Sales_Timeser, col="blue")
kpss.test(Apac_Sales_Timeser)

# Creating indata and outdata with 42 train and 6 predict samples
indata_2 <- Apac_Sales[1:42, ]
outdata_2 <- data.frame(Apac_Sales[43:48, ])

# Timeseries model of indata model
timeserIn3 <- ts(indata_2$Monthly.Sales)
plot(timeserIn3, col="blue")

# Smoothing the timeseries
timeserIn4 <- smoothing(timeserIn3, 4)
lines(timeserIn4, col="orange", lwd=2)
timeserdf_2 <- as.data.frame(cbind(indata_2$Month, as.vector(timeserIn4)))
colnames(timeserdf_2) <- c('Month', 'Sales')

# Fitting Sinusoidal function
lmfit_2 <- lm(Sales~ sin(Month) + poly(Month), data = timeserdf_2)

# Prediction on indata
global_pred_2 <- predict(lmfit_2, Month=indata_2$Month)
summary(global_pred_2)
lines(indata_2$Month, global_pred_2, col='red', lwd=2)

local_pred_2 <- timeserIn4-global_pred_2
plot(local_pred_2, col='red', type = "l")
acf(local_pred_2)
acf(local_pred_2, type="partial")
armafit_2 <- auto.arima(local_pred_2)

tsdiag(armafit_2)
armafit_2

#  Check if the residual series is white noise

resi_2 <- local_pred_2-fitted(armafit_2)
plot(resi_2)
kpss.test(resi_2)

adf.test(resi_2,alternative = "stationary")
kpss.test(resi_2)

# Prediction on global data
global_pred_out_2 <- predict(lmfit_2, data.frame(Month=outdata_2$Month))
MAPE_2 <- accuracy(global_pred_out_2,outdata_2[,1])[5]
MAPE_2
## 25.21844

# Plot of prediction with original timeseries value

pred_2 <- c(ts(global_pred_2), ts(global_pred_out_2))
plot(Apac_Sales_Timeser, col="red")
lines(pred_2, col="green")



#################################################
### Creation of Quantity Data for Time Series ###
#################################################

## EU Consumer Quantity

Eu_Quantity <- final_data_quantity %>%
  filter(Market == "EU", Segment == "Consumer") %>%
  select(Order.Date, Monthly.Quantity) %>% arrange(Order.Date)

Eu_Quantity <-Eu_Quantity[, c("Order.Month", "Monthly.Quantity")] %>% unique()
Eu_Quantity$Month <- row.names(Eu_Quantity) %>% as.numeric()
Eu_Quantity$Order.Month <- NULL

ploteuquantity <- ggplot(Eu_Quantity,aes(x=Eu_Quantity$Month,y=Eu_Quantity$Monthly.Quantity)) 
ploteuquantity +geom_bar(stat="identity", colour="black",fill="orange")+xlab("Month")+ylab("Monthly quantity")+ggtitle("EU Consumer Quantity Final data") 


## APAC Consumer Quantity

Apac_Quantity <- final_data_quantity %>%
  filter(Market == "APAC", Segment == "Consumer") %>%
  select(Order.Date, Monthly.Quantity) %>% arrange(Order.Date)

Apac_Quantity <-Apac_Quantity[, c("Order.Month", "Monthly.Quantity")] %>% unique()
Apac_Quantity$Month <- row.names(Apac_Quantity) %>% as.numeric()
Apac_Quantity$Order.Month <- NULL

plotapacquantity <- ggplot(Apac_Quantity,aes(x=Apac_Quantity$Month,y=Apac_Quantity$Monthly.Quantity)) 
plotapacquantity +geom_bar(stat="identity", colour="black",fill="orange")+xlab("Month")+ylab("Monthly quantity")+ggtitle("APAC Consumer Quantity Final data") 

 
##================##
## Time Series for EU Consumer Quantity

Eu_Quantity_Timeser <- ts(Eu_Quantity$Monthly.Quantity)
plot(Eu_Quantity_Timeser, col="blue")
kpss.test(Eu_Quantity_Timeser)

# Creating indata and outdata with 42 train and 6 predict samples
indata_3 <- Eu_Quantity[1:42, ]
outdata_3 <- data.frame(Eu_Quantity[43:48, ])

# Timeseries model of indata model
timeserIn5 <- ts(indata_3$Monthly.Quantity)
plot(timeserIn5, col="blue")

# Smoothening the timeseries
timeserIn6 <- smoothing(timeserIn5, 4)
lines(timeserIn6, col="orange", lwd=2)
timeserdf_3 <- as.data.frame(cbind(indata_3$Month, as.vector(timeserIn6)))
colnames(timeserdf_3) <- c('Month', 'Quantity')

# Fitting Sinusoidal function
lmfit_3 <- lm(Quantity~ sin(Month) + poly(Month), data = timeserdf_3)

# Prediction on indata
global_pred_3 <- predict(lmfit_3, Month=indata_3$Month)
summary(global_pred_3)
lines(indata_3$Month, global_pred_3, col='red', lwd=2)

local_pred_3 <- timeserIn6-global_pred_3
plot(local_pred_3, col='red', type = "l")
acf(local_pred_3)
acf(local_pred_3, type="partial")
armafit_3 <- auto.arima(local_pred_3)

tsdiag(armafit_3)
armafit_3

#Check if the residual series is white noise

resi_3 <- local_pred_3-fitted(armafit_3)
plot(resi_3)
kpss.test(resi_3)

adf.test(resi_3,alternative = "stationary")
kpss.test(resi_3)

# Prediction on global data
global_pred_out_3 <- predict(lmfit_3, data.frame(Month=outdata_3$Month))
MAPE_3 <- accuracy(global_pred_out_3,outdata_3[,1])[5]
MAPE_3
# 30.50313

# Plot of prediction with original timeseries value

pred_3 <- c(ts(global_pred_3), ts(global_pred_out_3))
plot(Eu_Quantity_Timeser, col="red")
lines(pred_3, col="green")



##=======================##
## Time Series for APAC Consumer Quantity

Apac_Quantity_Timeser <- ts(Apac_Quantity$Monthly.Quantity)
plot(Apac_Quantity_Timeser, col="blue")
kpss.test(Apac_Quantity_Timeser)

# Creating indata and outdata with 42 train and 6 predict samples
indata_4 <- Apac_Quantity[1:42, ]
outdata_4 <- data.frame(Apac_Quantity[43:48, ])

# Timeseries model of indata model
timeserIn7 <- ts(indata_4$Monthly.Quantity)
plot(timeserIn7, col="blue")

# Smoothing the timeseries
timeserIn8 <- smoothing(timeserIn7, 4)
lines(timeserIn8, col="orange", lwd=2)
timeserdf_4 <- as.data.frame(cbind(indata_4$Month, as.vector(timeserIn8)))
colnames(timeserdf_4) <- c('Month', 'Quantity')

# Fitting Sinusoidal function
lmfit_4 <- lm(Quantity~ sin(Month) + poly(Month), data = timeserdf_4)

# Prediction on indata
global_pred_4 <- predict(lmfit_4, Month=indata_4$Month)
summary(global_pred_4)
lines(indata_4$Month, global_pred_4, col='red', lwd=2)

local_pred_4 <- timeserIn8-global_pred_4
plot(local_pred_4, col='red', type = "l")
acf(local_pred_4)
acf(local_pred_4, type="partial")
armafit_4 <- auto.arima(local_pred_4)

tsdiag(armafit_4)
armafit_4

# Check if the residual series is white noise

resi_4 <- local_pred_4-fitted(armafit_4)
plot(resi_4)
kpss.test(resi_4)

adf.test(resi_4,alternative = "stationary")
kpss.test(resi_4)

# Prediction on global data
global_pred_out_4 <- predict(lmfit_4, data.frame(Month=outdata_4$Month))
MAPE_4 <- accuracy(global_pred_out_4,outdata_4[,1])[5]
MAPE_4
# 28.35134

# Plot of prediction with original timeseries value

pred_4 <- c(ts(global_pred_4), ts(global_pred_out_4))
plot(Apac_Quantity_Timeser, col="red")
lines(pred_4, col="green")

