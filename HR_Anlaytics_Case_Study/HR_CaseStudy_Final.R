###################################################################
#################### HR Analytics Case Study ######################
###################################################################

##Libraries used
library(tidyr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(cowplot)
library(MASS)
library(car)
library(e1071)
library(caret)
library(caTools)
library(ROCR)

##Loading datasets
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
general_data<- read.csv("general_data.csv", stringsAsFactors = F)
in_time<- read.csv("in_time.csv", stringsAsFactors = F)
manager_survey_data<- read.csv("manager_survey_data.csv", stringsAsFactors = F)
out_time<- read.csv("out_time.csv", stringsAsFactors = F)

###############################
## Exploratory Data Analysis ##
###############################

##Structure of the datasets
str(employee_survey_data) #4410 obs. of  4 variables
str(general_data) #4410 obs. of  24 variables
str(in_time) #4410 obs. of  262 variables
str(manager_survey_data) #4410 obs. of  3 variables
str(out_time) #4410 obs. of  262 variables

## Check for duplicates
## Check for duplicates for id and member id fields
sum(duplicated(employee_survey_data$EmployeeID))#0
sum(duplicated(general_data$EmployeeID))#0
sum(duplicated(manager_survey_data$EmployeeID))#0


###Intime and Outime Datasets###
###############################

#Assigning a column name
colnames(in_time)[1] = "EmployeeID"
colnames(out_time)[1] = "EmployeeID"

#NAs columns in in_time -- 12columns
length(in_time[,colSums(is.na(in_time))==nrow(in_time)])

#NAs columns in out_time -- 12columns
length(out_time[,colSums(is.na(out_time))==nrow(out_time)])

#removing columns which has NAs in merged data frame --24columns
in_time[,colSums(is.na(in_time))==nrow(in_time)] <- NULL
out_time[,colSums(is.na(out_time))==nrow(out_time)] <- NULL

#Changing wide to long format
intime<- gather(in_time,date,in_time,X2015.01.02:X2015.12.31)
outtime<- gather(out_time,date,out_time,X2015.01.02:X2015.12.31)

#Calculating the number of leaves of each employee by calculating the no. of NA's
leaves<-summarise(group_by(intime,EmployeeID),no_of_leaves=sum(is.na(in_time)))

#Removing NA's
sum(is.na(intime))#56160
sum(is.na(outtime))#56160

intime<-na.omit(intime)
outtime<-na.omit(outtime)

#Cleaning the data and creating a final datafram "emp" with average hours worked and leaves of an employee
#merging  intime and outime datasets
time_emp <- merge(intime,outtime, by=c("EmployeeID","date"), all = F)
time_emp <- merge(time_emp,leaves, by=c("EmployeeID"), all = F)

#calculating work hours for each employee per each day
time_emp$hours<-difftime(time_emp$out_time,time_emp$in_time,units = "hours")
time_emp$date<-substring(time_emp$date,2)

#calculating montly average hours for each employee
time_emp$date<-as.Date(time_emp$date,format = "%Y.%m.%d")
time_emp$month<-month(as.POSIXlt(time_emp$date,format="%d/%m/%Y"))
emp_hours<-summarise(group_by(time_emp,EmployeeID,month),monthly_avg_hrs=round(mean(hours),2),total_hrs=round(sum(hours),2))
emp_hours_1<-aggregate(monthly_avg_hrs~EmployeeID, mean,data = emp_hours)

#final dataset containing average hours worked and leaves of an employee
emp<-merge(emp_hours_1,leaves,by="EmployeeID",all=F)
colnames(emp)[2]<-"average_emp_hrs"
emp$average_emp_hrs<-round(emp$average_emp_hrs,2)
#emp dataframe
str(emp)

###Employee and Manager Survey data###
######################################

#Employee survey dataset
#Treating NA's in employee survey dataset
sum(is.na(employee_survey_data))#83

#columns which contains NAs
colnames(employee_survey_data)[colSums(is.na(employee_survey_data)) > 0] 

#assigning average value to the NA's
employee_survey_data$WorkLifeBalance[is.na(employee_survey_data$WorkLifeBalance)]<- round(mean(employee_survey_data$WorkLifeBalance,na.rm = T))
employee_survey_data$EnvironmentSatisfaction[is.na(employee_survey_data$EnvironmentSatisfaction)]<- round(mean(employee_survey_data$EnvironmentSatisfaction,na.rm = T))
employee_survey_data$JobSatisfaction[is.na(employee_survey_data$JobSatisfaction)]<- round(mean(employee_survey_data$JobSatisfaction,na.rm = T))

#Calculating overall satisfaction of an employee
employee_survey_data$overall_satisfaction<-round((employee_survey_data$WorkLifeBalance+employee_survey_data$EnvironmentSatisfaction+employee_survey_data$JobSatisfaction)/3)

#Manager survey dataset
#Checking for NA's
sum(is.na(manager_survey_data))#0

#Calculating overall rating attribute in manager survey dataset
manager_survey_data$overall_rating<-round((manager_survey_data$JobInvolvement+manager_survey_data$PerformanceRating)/2)


###General dataset###
#####################

#checking NAs in general_data
length(which(is.na(general_data)))#28

#columns which contains NAs
colnames(general_data)[colSums(is.na(general_data)) > 0] 

#Treating NA's
#mean of NumCompaniesWorked before replacing with mean value
mean(general_data$NumCompaniesWorked, na.rm = T)

#replacing NAs value with 0s in NumCompaniesWorked
general_data$NumCompaniesWorked[is.na(general_data$NumCompaniesWorked)==T]<-'0'

#mean of NumCompaniesWorked after replacing with 0s
mean(general_data$NumCompaniesWorked)

#mean of TotalWorkingYears before replacing with mean value
mean(general_data$TotalWorkingYears, na.rm = T)

#replacing NAs value with mean value of TotalWorkingYears
general_data$TotalWorkingYears[is.na(general_data$TotalWorkingYears)==T] <- round(mean(general_data$TotalWorkingYears, na.rm = T))

#mean of TotalWorkingYears after replacing with mean value
mean(general_data$TotalWorkingYears)

#removing columns which has  only one level through out - standaradhours, over18, employeecount
general_data$StandardHours <- NULL
general_data$Over18 <- NULL
general_data$EmployeeCount <- NULL

#Treating outliers

boxplot(general_data$MonthlyIncome)
quantile(general_data$MonthlyIncome,seq(0,1,0.01))
general_data$MonthlyIncome[general_data$MonthlyIncome>=152020.0]<-152020.0

boxplot(general_data$TotalWorkingYears)
quantile(general_data$TotalWorkingYears,seq(0,1,0.01))
general_data$TotalWorkingYears[general_data$TotalWorkingYears>=29]<-26

boxplot(general_data$YearsAtCompany)
quantile(general_data$YearsAtCompany,seq(0,1,0.01))
general_data$YearsAtCompany[general_data$YearsAtCompany>=20]<-19

boxplot(general_data$YearsSinceLastPromotion)
quantile(general_data$YearsSinceLastPromotion,seq(0,1,0.01))
general_data$YearsSinceLastPromotion[general_data$YearsSinceLastPromotion>=8]<-7

boxplot(general_data$YearsWithCurrManager)
quantile(general_data$YearsWithCurrManager,seq(0,1,0.01))
general_data$YearsWithCurrManager[general_data$YearsWithCurrManager>=12]<-12

boxplot(general_data$TrainingTimesLastYear)
quantile(general_data$TrainingTimesLastYear,seq(0,1,0.01))
general_data$TrainingTimesLastYear[general_data$TrainingTimesLastYear>=4]<-4
general_data$TrainingTimesLastYear[general_data$TrainingTimesLastYear<1]<-1


###################
###Data Analysis###
###################

#on an average employees are maintaining 7 hours of work per month
ggplot(emp_hours,aes(x=as.factor(round(monthly_avg_hrs))))+geom_bar()+xlab(label = "Monthly Average Hours")

#leaves pattern
#leaves when employee has avg work time less than 6.5 and more than 8
emp_leave <- merge(emp_hours,leaves, by=c("EmployeeID"), all = F)
less_w<-subset(emp_leave,monthly_avg_hrs<=6.5)
more_w<-subset(emp_leave,monthly_avg_hrs>8)

plot_grid(ggplot(less_w,aes(x=as.factor(no_of_leaves)))+geom_bar(),
          ggplot(more_w,aes(x=as.factor(no_of_leaves)))+geom_bar(),align = "h")

#overall satisfaction is 3/4 - environment satisfaction; workbalance; job satisfaction are quite good as per employees
plot_grid(ggplot(employee_survey_data,aes(x=overall_satisfaction))+geom_bar(),
          ggplot(employee_survey_data,aes(x=JobSatisfaction))+geom_bar(),
          ggplot(employee_survey_data,aes(x=EnvironmentSatisfaction))+geom_bar(),
          ggplot(employee_survey_data,aes(x=WorkLifeBalance))+geom_bar(),align = "h")

# as per managers , job involvment and performance rating are quite decent on an average
plot_grid(ggplot(manager_survey_data,aes(x=overall_rating))+geom_bar(),
          ggplot(manager_survey_data,aes(x=JobInvolvement))+geom_bar(),
          ggplot(manager_survey_data,aes(x=as.factor(PerformanceRating)))+geom_bar()+xlab(label = "Performance Rating"),align = "h")

# 15% rate attrition
ggplot(general_data,aes(x=Attrition))+geom_bar()

# average Salary hike is 14%
boxplot(general_data$PercentSalaryHike)

#mean monthly salary of an employee is around 60000 and it ranges from 20000 to 140000
boxplot(general_data$MonthlyIncome)


###Merging datasets to create the final dataset###
##################################################

final<-merge(general_data,employee_survey_data,by=c("EmployeeID"),all=F)

final<-merge(final,manager_survey_data,by=c("EmployeeID"),all=F)

final<-merge(final,emp,by=c("EmployeeID"),all=F)

# converting categorical attributes to factors
final[,c("Attrition","BusinessTravel","Department",
         "EducationField","Gender","JobLevel","MaritalStatus","Education","JobRole"
         ,"NumCompaniesWorked","EnvironmentSatisfaction","JobSatisfaction",
         "WorkLifeBalance" ,"overall_satisfaction","JobInvolvement","PerformanceRating",
         "overall_rating")]<- data.frame(sapply(final[,c("Attrition","BusinessTravel","Department",
                                                         "EducationField","Gender","JobLevel","MaritalStatus","Education","JobRole"
                                                         ,"NumCompaniesWorked","EnvironmentSatisfaction","JobSatisfaction",
                                                         "WorkLifeBalance" ,"overall_satisfaction","JobInvolvement","PerformanceRating",
                                                         "overall_rating")], function(x) factor(x)))
#Structure of final dataset
str(final)
#removing EmployeeID
final<-final[,-1]

#scaling variables
final[,c("Age","DistanceFromHome","MonthlyIncome","PercentSalaryHike","StockOptionLevel","TotalWorkingYears","TrainingTimesLastYear",
         "YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager",
         "average_emp_hrs","no_of_leaves")]<-data.frame(sapply(final[,c("Age","DistanceFromHome","MonthlyIncome","PercentSalaryHike","StockOptionLevel","TotalWorkingYears","TrainingTimesLastYear",
                                                                        "YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager",
                                                                        "average_emp_hrs","no_of_leaves")], function(x) scale(x)))
#Levels in each categorical variable
for(i in 1:ncol(final)){
  print(paste(colnames(final[i]),"-",length(levels(final[,i]))))
}

#[1] "Attrition - 2"
#[1] "BusinessTravel - 3"
#[1] "Department - 3"
#[1] "Education - 5"
#[1] "EducationField - 6"
#[1] "Gender - 2"
#[1] "JobLevel - 5"
#[1] "JobRole - 9"
#[1] "MaritalStatus - 3"
#[1] "NumCompaniesWorked - 10"
#[1] "EnvironmentSatisfaction - 4"
#[1] "JobSatisfaction - 4"
#[1] "WorkLifeBalance - 4"
#[1] "overall_satisfaction - 4"
#[1] "JobInvolvement - 4"
#[1] "PerformanceRating - 2"
#[1] "overall_rating - 3"


#creating dummy variables and assiging it to hr_final data frame
hr_final<-data.frame(sapply(final[,c("Attrition","BusinessTravel","Department",
                                     "EducationField","Gender","JobLevel","MaritalStatus","Education","JobRole"
                                     ,"NumCompaniesWorked","EnvironmentSatisfaction","JobSatisfaction",
                                     "WorkLifeBalance" ,"overall_satisfaction","JobInvolvement","PerformanceRating",
                                     "overall_rating")], function(x) data.frame(model.matrix(~x-1,data =final))[,-1]))

hr_final<-cbind(hr_final,final[,c("Age","DistanceFromHome","MonthlyIncome","PercentSalaryHike","StockOptionLevel","TotalWorkingYears","TrainingTimesLastYear",
                                  "YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager",
                                  "average_emp_hrs","no_of_leaves")])


####################
#### Modelling #####
####################

# splitting the data between train and test
set.seed(100)

indices = sample.split(hr_final$Attrition, SplitRatio = 0.7)
train = hr_final[indices,]
test = hr_final[!(indices),]


############################
#   Logistic Regression:   #
############################

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

#checking for colinearity
vif(model_2)

#excluding MaritalStatus.xMarried - high p value
model_3<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + EducationField.xOther + JobLevel.x2 + 
               JobLevel.x5  + MaritalStatus.xSingle + 
               Education.x3 + Education.x4 + Education.x5 + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               overall_satisfaction.x4 + JobInvolvement.x3 + Age + DistanceFromHome + 
               MonthlyIncome + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
             family = "binomial", data = train)
summary(model_3)
#checking for colinearity
vif(model_2)


#excludig EducationField.xOther - hight p value
model_4<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + JobLevel.x2 +JobLevel.x5  + MaritalStatus.xSingle + 
               Education.x3 + Education.x4 + Education.x5 + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               overall_satisfaction.x4 + JobInvolvement.x3 + Age + DistanceFromHome + 
               MonthlyIncome + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
             family = "binomial", data = train)
summary(model_4)
#checking for colinearity
vif(model_4)

#excluding JobLevel.x5 - high p value
model_5<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + JobLevel.x2 + MaritalStatus.xSingle + 
               Education.x3 + Education.x4 + Education.x5 + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               overall_satisfaction.x4 + JobInvolvement.x3 + Age + DistanceFromHome + 
               MonthlyIncome + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
             family = "binomial", data = train)
summary(model_5)
#checking for colinearity
vif(model_5)

#excluding Education.x5 - high p value
model_6<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + JobLevel.x2 + MaritalStatus.xSingle + 
               Education.x3 + Education.x4 + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               overall_satisfaction.x4 + JobInvolvement.x3 + Age + DistanceFromHome + 
               MonthlyIncome + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
             family = "binomial", data = train)
summary(model_6)
#checking for colinearity
vif(model_6)


#excluding Education.x3 - high p value
model_7<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + JobLevel.x2 + MaritalStatus.xSingle + 
               Education.x4 + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               overall_satisfaction.x4 + JobInvolvement.x3 + Age + DistanceFromHome + 
               MonthlyIncome + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
             family = "binomial", data = train)
summary(model_7)
#checking for colinearity
vif(model_7)

#excluding Education.x4 - high p values
model_8<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + JobLevel.x2 + MaritalStatus.xSingle + 
               JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               overall_satisfaction.x4 + JobInvolvement.x3 + Age + DistanceFromHome + 
               MonthlyIncome + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
             family = "binomial", data = train)
summary(model_8)
#checking for colinearity
vif(model_8)

#excluding DistanceFromHome - high p value
model_9<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + JobLevel.x2 + MaritalStatus.xSingle + 
               JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
               EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
               JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
               WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
               overall_satisfaction.x4 + JobInvolvement.x3 + Age  + 
               MonthlyIncome + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
             family = "binomial", data = train)
summary(model_9)
#checking for colinearity
vif(model_9)


#excluding MonthlyIncome - high p value
model_10<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + JobLevel.x2 + MaritalStatus.xSingle + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                overall_satisfaction.x4 + JobInvolvement.x3 + Age  + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_10)
#checking for colinearity
vif(model_10)


#excluding JobInvolvement.x3 - high p value
model_11<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + JobLevel.x2 + MaritalStatus.xSingle + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                overall_satisfaction.x4 + Age  + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_11)
#checking for colinearity
vif(model_11)

#excluding overall_satisfaction.x4  - high p value
model_12<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + JobLevel.x2 + MaritalStatus.xSingle + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +Age+ 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_12)
#checking for colinearity
vif(model_12)

#excluding JobLevel.x2 - high p value
model_13<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +Age+ 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_13)
#checking for colinearity
vif(model_13)


#excluding JobRole.xLaboratory.Technician - high p value
model_14<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +Age+ 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_14)
#checking for colinearity
vif(model_14)

#excluding JobRole.xResearch.Scientist - high p value
model_15<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +Age+ 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_15)
#checking for colinearity
vif(model_15)

#excluding NumCompaniesWorked.x4 - less significant
model_16<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + NumCompaniesWorked.x1 +
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +Age+ 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_16)
#checking for colinearity
vif(model_16)


#excluding NumCompaniesWorked.x8 - less significant
model_17<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + NumCompaniesWorked.x1 +
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +Age+ 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_17)
#checking for colinearity
vif(model_17)

#excluding NumCompaniesWorked.x1 - less significant
model_18<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle + JobRole.xResearch.Director + 
                JobRole.xSales.Executive +NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +Age+ 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_18)
#checking for colinearity
vif(model_18)

#excluding JobRole.xResearch.Director  -less significant
model_19<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + MaritalStatus.xSingle +
                 JobRole.xSales.Executive +NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                 NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +Age+ 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
               family = "binomial", data = train)
summary(model_19)
#checking for colinearity
vif(model_19)


#excluding TrainingTimesLastYear - less significant
model_20<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle +
                JobRole.xSales.Executive +NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +Age+ 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_20)
#checking for colinearity
vif(model_20)


#excluding  JobRole.xSales.Executive - less significant
model_21<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle +
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +Age+ 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_21)
#checking for colinearity
vif(model_21)

#excluding NumCompaniesWorked.x6  - less significant
model_22<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle +
                NumCompaniesWorked.x5 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +Age+ 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_22)
#checking for colinearity
vif(model_22)

#excluding JobSatisfaction.x3 - less significant
model_23<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle +
                NumCompaniesWorked.x5 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 +JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +Age+ 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_23)
#checking for colinearity
vif(model_23)

#excluding JobSatisfaction.x2  - high p value
model_24<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle +
                NumCompaniesWorked.x5 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 +Age+ 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_24)
#checking for colinearity
vif(model_24)

#excluding WorkLifeBalance.x4 - less significant
model_25<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle +
                NumCompaniesWorked.x5 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 +Age+ 
                TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_25)
#checking for colinearity
vif(model_25)

#excluding WorkLifeBalance.x2 - high p value
model_26<-glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle +
                NumCompaniesWorked.x5 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +JobSatisfaction.x4 + 
                WorkLifeBalance.x3 +Age+ 
                TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_26)
#checking for colinearity
vif(model_26)

#excluding BusinessTravel.xTravel_Frequently  - highly collinear
model_27<-glm(formula = Attrition ~ BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle +
                NumCompaniesWorked.x5 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +JobSatisfaction.x4 + 
                WorkLifeBalance.x3 +Age+ 
                TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_27)
#checking for colinearity
vif(model_27)

#excluding BusinessTravel.xTravel_Rarely  - less significant
model_28<-glm(formula = Attrition ~ Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle +
                NumCompaniesWorked.x5 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +JobSatisfaction.x4 + 
                WorkLifeBalance.x3 +Age+ 
                TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_28)
#checking for colinearity
vif(model_28)

#excluding WorkLifeBalance.x3  - less significant
model_29<-glm(formula = Attrition ~ Department.xResearch...Development + 
                Department.xSales + MaritalStatus.xSingle +
                NumCompaniesWorked.x5 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +JobSatisfaction.x4 +Age+ 
                TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_29)
#checking for colinearity
vif(model_29)

#excluding Department.xResearch...Development  - highly collinear
model_30<-glm(formula = Attrition ~ Department.xSales + MaritalStatus.xSingle +
                NumCompaniesWorked.x5 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +JobSatisfaction.x4 +Age+ 
                TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_30)
#checking for colinearity
vif(model_30)


#excluding Department.xSales  - less significant
model_31<-glm(formula = Attrition ~ MaritalStatus.xSingle +
                NumCompaniesWorked.x5 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +JobSatisfaction.x4 +Age+ 
                TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_31)
#checking for colinearity
vif(model_31)

#excluding NumCompaniesWorked.x9  - less significant
model_32<-glm(formula = Attrition ~ MaritalStatus.xSingle +
                NumCompaniesWorked.x5 + NumCompaniesWorked.x7 +EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +JobSatisfaction.x4 +Age+ 
                TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + average_emp_hrs, 
              family = "binomial", data = train)
summary(model_32)
#checking for colinearity
vif(model_32)

#############################
###    Model Evaluation   ###
#############################

### Test Data ####
#predicted probabilities of Attrition 1 for test data
test_pred = predict(model_32, type = "response", newdata = test[,-1])

#Summary 
summary(test_pred)
test$prob <- test_pred
View(test)

#Probability cutoff at 50%.
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, 1, 0))
test_actual_attrition <- as.factor(test$Attrition)
table(test_actual_attrition,test_pred_attrition)

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition)

#Finding out the optimal cutoff value
perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, 1, 0))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition)
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.
# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#Optimal cutoff is the value where Accuracy,Sesitivity annd Specificity meet
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#optimal cutoff
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

#choosing 0.1616162 as the optimal cutoff
test_pred_attrition <- factor(ifelse(test_pred >= 0.1616162, 1, 0))
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition)
conf_final <- confusionMatrix(test_pred_attrition, test_actual_attrition)

#Accuracy,Sensitivity & Specificity of the model
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

### KS -statistic - Test Data ######
test_pred_attrition <- ifelse(test_pred_attrition==1,1,0)
test_actual_attrition <- ifelse(test_actual_attrition==1,1,0)

#on testing  data
pred_object_test<- prediction(test_pred_attrition, test_actual_attrition)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #41.97%


#####################
#     Accuracy      #
#     0.7105064     #
#                   #
#     Sensitivity   #
#     0.7108108     #
#                   #
#     Specificity   #
#     0.7464789     #
#                   #
#     KS-Statistic  #
#       41.97%      #
#                   #
#####################

## The correlation of Attrition with the final variables as per the model
cor(hr_final$Attrition,hr_final$BusinessTravel.xTravel_Frequently) ### 0.1151428
cor(hr_final$Attrition,hr_final$MaritalStatus.xSingle) ## 0.1754186
cor(hr_final$Attrition,hr_final$EnvironmentSatisfaction.x4) ## -0.04842598
cor(hr_final$Attrition,hr_final$JobSatisfaction.x4) ## -0.08585606
cor(hr_final$Attrition,hr_final$TotalWorkingYears) ## -0.1824202
cor(hr_final$Attrition,hr_final$YearsSinceLastPromotion) ## -0.0370935
cor(hr_final$Attrition,hr_final$YearsWithCurrManager) ## -0.1582603
cor(hr_final$Attrition,hr_final$average_emp_hrs) ## 0.2016788


########################################
###   Key Performance Indicators     ###
########################################


#NumCompaniesWorked
#EnvironmentSatisfaction
#JobSatisfaction
#TotalWorkingYears
#YearsSinceLastPromotion
#YearsWithCurrManager
#average_emp_hrs

#NumCompaniesWorked - No. of companies worked playes an important role while hiring an employee. If an employee has less experience and 
#has changed his company quite frequently then chances of changing the company after joining can be very high. Therefore this can play an 
#important factor to define attrition rate.

#EnvironmentSatisfaction - Environment satisfaction is a crucial factor to define the attrition rate of the company. If the employee is
#not satisfied with the environemt such as workplace incentives, Supervisor support, performance feedback etc is likely to leave the company.

#JobSatisfaction - An employee definitely looks for variety of factors during his work such as Purpose of work, A balanced lifestyle,
#self confidence during his work etc. These factors are related to job satisfaction which highly relate to attrition rate.

#YearsSinceLastPromotion - An employee in general seeks for an imporvement in his career and based on his performance an employee can be
#eligible for a promotion to his current role.If proper implementation and guidance is given then the employee is more likely to be satisfied
#and less likely to leave the company.

#YearsWithCurrManager - It is very much known that people leave or stay because of their bosses and not because of the company. A good manager-employee relationship 
#is crucial for the satisfaction and retention of the worker. Ensure that the managers in your organization are not driving away the technologists. 
#Provide them with the necessary training that is required to develop people management skills and good supervisory.

#TotalWorkingYears - Other than factors such as job and environment satisfaction,total number of years worked is a variable which can differ
#from different range of work experience.Employees who have work exp ranging from 0-4 tend to change the company for various reasons such as
#greater hike and role changes. Whereas employees who are more then 4 years work exp tend to settle their career in a good and friendly
#work place and will have a lower probability to change. 

#average_emp_hrs - This attribute can help us if an employee is over stressed or who works very less. If the average work hours of an employee
#exceeds than the optimal work time, it can indicate that the employee is over burdend with work and would be tending to have less job 
# satisfaction thus has an tendency to look for a change in mear future.