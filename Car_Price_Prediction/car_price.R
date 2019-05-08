library(ggplot2)
library(car)
library(MASS)
library(stringr)
library(reshape2)

#########################
## Reading the dataset ##
#########################
carPrice <- read.csv("carPrice_Assignment.csv")
str(carPrice)

##############
## Cleaning ##
##############

# Checking for NA
sum(is.na(carPrice))
# 0

# Checking for repeating row
length(unique(carPrice$car_ID))
# 205

# Converting CarName to character
carPrice$CarName <- as.character(carPrice$CarName)


# Extracting Company from CarName
carPrice$company <- colsplit(carPrice$CarName, " ", names = c("company", "model"))[,1]

levels(as.factor(carPrice$company))

# Correcting company names
carPrice$company[which(carPrice$company == "Nissan")] <- "nissan"
carPrice$company[which(carPrice$company == "maxda")] <- "mazda"
carPrice$company[which(carPrice$company == "porcshce")] <- "porsche"
carPrice$company[which(carPrice$company == "toyouta")] <- "toyota"
carPrice$company[which(carPrice$company == "vokswagen")] <- "volkswagen"
carPrice$company[which(carPrice$company == "vw")] <- "volkswagen"

carPrice$company <- as.factor(carPrice$company)



# Checking for outliers

quantile(carPrice$horsepower, probs = seq(0, 1, 0.01))
###############################
## 96%     97%     98%       ##
## 182.00  184.00   206.44   ##
###############################
carPrice$horsepower[which(carPrice$horsepower > 184.0)] <- 186.0


# Removing Unecessary Columns
carPrice <- carPrice[,-1]
carPrice <- carPrice[,-2]

#####################
## Dummy Variables ##
#####################

# Fueltype
levels(carPrice$fueltype) <- c(1,0)
carPrice$fueltype
carPrice$fueltype <- as.numeric(levels(carPrice$fueltype))[carPrice$fueltype]

# Aspiration
levels(carPrice$aspiration) <- c(1,0)
carPrice$aspiration <- as.numeric(levels(carPrice$aspiration))[carPrice$aspiration]

# Doornumber
levels(carPrice$doornumber) <- c(1,0)
carPrice$doornumber <- as.numeric(levels(carPrice$doornumber))[carPrice$doornumber]

# Enginelocation
levels(carPrice$enginelocation) <- c(1,0)
carPrice$enginelocation <- as.numeric(levels(carPrice$enginelocation))[carPrice$enginelocation]

## 3 or More than 3 levels

# Carbody
dummy_1 <- data.frame(model.matrix(~carbody, data = carPrice))
dummy_1 <- dummy_1[, -1]

carPrice <- cbind(carPrice[,-5], dummy_1)

# Drivewheel
dummy_2 <- data.frame(model.matrix(~drivewheel, data = carPrice))
dummy_2 <- dummy_2[, -1]

carPrice <- cbind(carPrice[,-5], dummy_2)

# Enginetype
dummy_3 <- data.frame(model.matrix(~enginetype, data = carPrice))
dummy_3 <- dummy_3[, -1]

carPrice <- cbind(carPrice[,-11], dummy_3)

# Cylindernumber
dummy_4 <- data.frame(model.matrix(~cylindernumber, data = carPrice))
dummy_4 <- dummy_4[, -1]

carPrice <- cbind(carPrice[,-11], dummy_4)

# Fuelsystem
dummy_5 <- data.frame(model.matrix(~fuelsystem, data = carPrice))
dummy_5 <- dummy_5[,-1]

carPrice <- cbind(carPrice[, -12], dummy_5)


# Comapny
dummy_6 <- data.frame(model.matrix(~company, data = carPrice))
dummy_6 <- dummy_6[, -1]

carPrice <- cbind(carPrice[, -20], dummy_6)

##########################
## Regression modelling ##
##########################

set.seed(100)

trainindices <- sample(1:nrow(carPrice), 0.7*nrow(carPrice))

# Creating train and test datasets

train <- carPrice[trainindices, ]

test <- carPrice[-trainindices, ]


# Model 1 with all variables
model_1 <- lm(price~., data = train)
summary(model_1)

# Step AIC on model_1
stepAIC(model_1, direction = "both")

# Removing variables as given by stepAIC
model_2 <- lm(price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + boreratio + stroke + peakrpm + 
                citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + companybmw + companybuick + 
                companydodge + companyhonda + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen, 
              data = train)

summary(model_2)

sort(vif(model_2), decreasing = T)

## Since curbweight and enginesize has high vif

## Checking correlation of curbweight and enginesize

cor(carPrice$curbweight, carPrice$enginesize)
## Correlation of curbweight and enginesize is high 
## 0.8505941

## Creating two different models with each one of them and checking vif's

### Removing curbweight
model_2.1 <- lm(price ~ aspiration + enginelocation + carwidth + 
                enginesize + boreratio + stroke + peakrpm + 
                citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + companybmw + companybuick + 
                companydodge + companyhonda + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen, 
              data = train)

summary(model_2.1)

sort(vif(model_2.1), decreasing = T)

### Removing enginesize
model_2.2 <- lm(price ~ aspiration + enginelocation + carwidth + 
                  curbweight +  boreratio + stroke + peakrpm + 
                  citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
                  carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                  enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                  fuelsystem2bbl + fuelsystemmpfi + companybmw + companybuick + 
                  companydodge + companyhonda + companyjaguar + companymazda + 
                  companymercury + companymitsubishi + companynissan + companyplymouth + 
                  companyrenault + companysaab + companytoyota + companyvolkswagen, 
                data = train)

summary(model_2.2)

sort(vif(model_2.2), decreasing = T)

## Since removing curbweight decreases the vif's of other variables significantly
## Also removing curbweight changes r-squared and adjusted r-squared insignificantly
## compared to removing enginesize

# Removing curbweight from the model

#====================================#
# variable   |vif         |p-value   #
#====================================#
# curbweight | 24.235596  | 0.004060 #
#====================================#

model_3 <- lm(price ~ aspiration + enginelocation + carwidth + 
                  enginesize + boreratio + stroke + peakrpm + 
                  citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
                  carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                  enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                  fuelsystem2bbl + fuelsystemmpfi + companybmw + companybuick + 
                  companydodge + companyhonda + companyjaguar + companymazda + 
                  companymercury + companymitsubishi + companynissan + companyplymouth + 
                  companyrenault + companysaab + companytoyota + companyvolkswagen, 
                data = train)

summary(model_3)

sort(vif(model_3), decreasing = T)

#========================================#
# variable        |vif        |p-value   #
#========================================#
# fuelsystemmpfi  | 7.136996  | 0.197795 #
#----------------------------------------#
# citympg         | 6.932233  | 0.537332 #
#========================================#

## Since citympg has higher p-value and removing it causes insignificant changes
## in r-squared and adjusted r-squared


model_4 <- lm(price ~ aspiration + enginelocation + carwidth + 
                enginesize + boreratio + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + companybmw + companybuick + 
                companydodge + companyhonda + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen, 
              data = train)

summary(model_4)

sort(vif(model_4), decreasing = T)

#=========================================#
# variable         |vif        |p-value   #
#=========================================#
# fuelsystemmpfi   | 5.598425  | 0.255967 #
#=========================================#

model_5 <- lm(price ~ aspiration + enginelocation + carwidth + 
                enginesize + boreratio + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                fuelsystem2bbl + companybmw + companybuick + 
                companydodge + companyhonda + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen, 
              data = train)

summary(model_5)

sort(vif(model_5), decreasing = T)

#=========================================#
# variable         | vif       | p-value  #
#=========================================#
# boreratio        | 4.587017  | 0.082842 #
#-----------------------------------------#
# fuelsystem2bbl   | 3.306688  | 0.182791 #
#=========================================#

## Since fuelsystem2bbl has higher p-value and removing it causes insignificant changes
## in r-squared and adjusted r-squared

model_6 <- lm(price ~ aspiration + enginelocation + carwidth + 
                enginesize + boreratio + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                companybmw + companybuick + 
                companydodge + companyhonda + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen, 
              data = train)

summary(model_6)

sort(vif(model_6), decreasing = T)

#=========================================#
# variable         | vif       | p-value  #
#=========================================#
# boreratio        | 4.465689  | 0.124603 #
#=========================================#

model_7 <- lm(price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
                companybmw + companybuick + 
                companydodge + companyhonda + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen, 
              data = train)

summary(model_7)

sort(vif(model_7), decreasing = T)

#===========================================#
# variable           | vif       | p-value  #
#===========================================#
# cylindernumberfive | 2.272965  | 0.148997 #
#===========================================#

model_8 <- lm(price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberthree + 
                companybmw + companybuick + 
                companydodge + companyhonda + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen, 
              data = train)

summary(model_8)

sort(vif(model_8), decreasing = T)

#=====================================#
# variable     | vif       | p-value  #
#=====================================#
# companysaab  | 1.714136  | 0.180142 #
#=====================================#

model_9 <- lm(price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberthree + 
                companybmw + companybuick + 
                companydodge + companyhonda + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companytoyota + companyvolkswagen, 
              data = train)

summary(model_9)

sort(vif(model_9), decreasing = T)

#========================================#
# variable        | vif       | p-value  #
#========================================#
# companymercury  | 1.117978  | 0.191064 #
#========================================#

model_10 <- lm(price ~ aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberthree + 
                companybmw + companybuick + 
                companydodge + companyhonda + companyjaguar + companymazda + 
                companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companytoyota + companyvolkswagen, 
              data = train)

summary(model_10)

sort(vif(model_10), decreasing = T)

#=====================================#
# variable     | vif       | p-value  #
#=====================================#
# enginetypel  | 2.094582  | 0.060183 #
#=====================================#

model_11 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + drivewheelrwd + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + cylindernumberthree + 
                 companybmw + companybuick + 
                 companydodge + companyhonda + companyjaguar + companymazda + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companyrenault + companytoyota + companyvolkswagen, 
               data = train)

summary(model_11)

sort(vif(model_11), decreasing = T)

#=============================================#
# variable            | vif        | p-value  #
#=============================================#
# cylindernumberthree | 1.302586  | 0.177162  #
#=============================================#

model_12 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + drivewheelrwd + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companydodge + companyhonda + companyjaguar + companymazda + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companyrenault + companytoyota + companyvolkswagen, 
               data = train)

summary(model_12)

sort(vif(model_12), decreasing = T)

#=======================================#
# variable     | vif        | p-value   #
#=======================================#
# carbodysedan | 12.383240  | 0.005039  #
#=======================================#

model_13 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + 
                 carbodyhardtop + carbodyhatchback +
                 carbodywagon + drivewheelrwd + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companydodge + companyhonda + companyjaguar + companymazda + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companyrenault + companytoyota + companyvolkswagen, 
               data = train)

summary(model_13)

sort(vif(model_13), decreasing = T)

#===========================================#
# variable         | vif        | p-value   #
#===========================================#
# carbodyhatchback | 1.601205   | 0.176645  #
#-------------------------------------------#
# carbodyhardtop   | 1.449725   | 0.203677  #
#-------------------------------------------#
# carbodywagon     | 1.341492   | 0.874684  #
#===========================================#

## Since vif<2.0, considering the highest p-value

## Removing carbodywagon as it has highest p-value

model_14 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + 
                 carbodyhardtop + carbodyhatchback +
                 drivewheelrwd + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companydodge + companyhonda + companyjaguar + companymazda + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companyrenault + companytoyota + companyvolkswagen, 
               data = train)

summary(model_14)

sort(vif(model_14), decreasing = T)

#===========================================#
# variable         | vif        | p-value   #
#===========================================#
# carbodyhardtop   | 1.423856   | 0.205230  #
#-------------------------------------------#
# carbodyhatchback | 1.410124   | 0.165293  #
#===========================================#

## Removing carbodyhardtop as it has highest p-value

model_15 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + 
                 carbodyhatchback +
                 drivewheelrwd + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companydodge + companyhonda + companyjaguar + companymazda + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companyrenault + companytoyota + companyvolkswagen, 
               data = train)

summary(model_15)

sort(vif(model_15), decreasing = T)

#===========================================#
# variable         | vif        | p-value   #
#===========================================#
# carbodyhatchback | 1.383268   | 0.22152   #
#===========================================#

model_16 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + 
                 drivewheelrwd + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companydodge + companyhonda + companyjaguar + companymazda + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companyrenault + companytoyota + companyvolkswagen, 
               data = train)

summary(model_16)

sort(vif(model_16), decreasing = T)


## Since all the p-values are less than 0.05.
## Checking correlation of enginesize and carwidth

cor(carPrice$enginesize, carPrice$carwidth)
## Correlation of enginesize and carwidth is high
## 0.7354334

#Creating two different models with each one of them and checking vif's

# Removing enginesize
model_16.1 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 stroke + peakrpm + 
                 drivewheelrwd + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companydodge + companyhonda + companyjaguar + companymazda + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companyrenault + companytoyota + companyvolkswagen, 
               data = train)

summary(model_16.1)

sort(vif(model_16.1), decreasing = T)


## Removing carwidth
model_16.2 <- lm(price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 drivewheelrwd + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companydodge + companyhonda + companyjaguar + companymazda + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companyrenault + companytoyota + companyvolkswagen, 
               data = train)

summary(model_16.2)

sort(vif(model_16.2), decreasing = T)


## vif's decreases is significant for both
## rsquared and adj rsquared change is less after removing carwidth

# Removing carwidth

#====================================#
# variable  | vif        | p-value   #
#====================================#
# carwidth  | 5.052143   | 1.85e-05  #
#====================================#

model_17 <- lm(price ~ aspiration + enginelocation + 
                   enginesize + stroke + peakrpm + 
                   drivewheelrwd + enginetypedohcv + 
                   enginetypeohcf + enginetyperotor +
                   companybmw + companybuick + 
                   companydodge + companyhonda + companyjaguar + companymazda + 
                   companymitsubishi + companynissan + companyplymouth + 
                   companyrenault + companytoyota + companyvolkswagen, 
                 data = train)

summary(model_17)

sort(vif(model_17), decreasing = T)

#===========================================#
# variable          | vif        | p-value  #
#===========================================#
# companyvolkswagen | 1.635820   | 0.002133 #
#===========================================#

model_18 <- lm(price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 drivewheelrwd + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companydodge + companyhonda + companyjaguar + companymazda + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companyrenault + companytoyota, 
               data = train)

summary(model_18)

sort(vif(model_18), decreasing = T)


#===========================================#
# variable          | vif        | p-value  #
#===========================================#
# companyrenault    | 1.259745   | 0.026230 #
#===========================================#

model_19 <- lm(price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 drivewheelrwd + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companydodge + companyhonda + companyjaguar + companymazda + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companytoyota, 
               data = train)

summary(model_19)

sort(vif(model_19), decreasing = T)

#===========================================#
# variable          | vif        | p-value  #
#===========================================#
# companyhonda      | 1.823779   | 0.009500 #
#===========================================#

model_20 <- lm(price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 drivewheelrwd + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companydodge + companyjaguar + companymazda + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companytoyota, 
               data = train)

summary(model_20)

sort(vif(model_20), decreasing = T)

#===========================================#
# variable          | vif        | p-value  #
#===========================================#
# companymazda      | 1.634360   | 0.005795 #
#===========================================#

model_21 <- lm(price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 drivewheelrwd + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companydodge + companyjaguar + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companytoyota, 
               data = train)

summary(model_21)

sort(vif(model_21), decreasing = T)

#============================================#
# variable           | vif        | p-value  #
#============================================#
# drivewheelrwd      | 2.586786   | 0.011422 #
#============================================#

model_22 <- lm(price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companydodge + companyjaguar + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companytoyota, 
               data = train)

summary(model_22)

sort(vif(model_22), decreasing = T)

#============================================#
# variable           | vif        | p-value  #
#============================================#
# companynissan      | 1.127108   | 0.002417 #
#============================================#

model_23 <- lm(price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companydodge + companyjaguar + 
                 companymitsubishi + companyplymouth + 
                 companytoyota, 
               data = train)

summary(model_23)

sort(vif(model_23), decreasing = T)

#============================================#
# variable           | vif        | p-value  #
#============================================#
# companytoyota      | 1.253831   | 0.003487 #
#============================================#

model_24 <- lm(price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companydodge + companyjaguar + 
                 companymitsubishi + companyplymouth,
               data = train)

summary(model_24)

sort(vif(model_24), decreasing = T)

#============================================#
# variable           | vif        | p-value  #
#============================================#
# companydodge       | 1.061137   | 0.004723 #
#============================================#

model_25 <- lm(price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companyjaguar + 
                 companymitsubishi + companyplymouth,
               data = train)

summary(model_25)

sort(vif(model_25), decreasing = T)

#============================================#
# variable           | vif        | p-value  #
#============================================#
# companyplymouth    | 1.050727   | 0.010306 #
#============================================#

model_26 <- lm(price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companyjaguar + 
                 companymitsubishi,
               data = train)

summary(model_26)

sort(vif(model_26), decreasing = T)

#============================================#
# variable           | vif        | p-value  #
#============================================#
# companymitsubishi  | 1.053554   | 0.00263 #
#============================================#

model_27 <- lm(price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 enginetypedohcv + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companyjaguar,
               data = train)

summary(model_27)

sort(vif(model_27), decreasing = T)

#============================================#
# variable           | vif        | p-value  #
#============================================#
# enginetypedohcv    | 1.112895   | 0.001386 #
#============================================#

model_28 <- lm(price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 enginetypeohcf + enginetyperotor +
                 companybmw + companybuick + 
                 companyjaguar,
               data = train)

summary(model_28)

sort(vif(model_28), decreasing = T)

##################
##  Prediction  ##
##################

test$predicted.price <- predict(model_28, test[, -19])

r <- cor(test$price, test$predicted.price)
r
rSquared <- cor(test$price, test$predicted.price)^2
rSquared

############
##  Plot  ##
############

ggplot(model_28) + 
  geom_point(aes(x=.fitted, y=.resid), color="dodgerblue2", alpha=0.8) +
  geom_hline(yintercept=0,color="dodgerblue4", alpha=0.6) +
  theme_minimal() +
  labs(x="Fitted", y="Residuals")


ggplot(test, aes(x=price, y=predicted.price)) +
  geom_point(color="orchid3", alpha=0.8) +
  geom_abline(color="orchid4", alpha=0.6) +
  theme_minimal()+
  labs(x="Actual Price", y="Predicted Price")

