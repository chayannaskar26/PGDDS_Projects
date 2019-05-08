# Library
library(dplyr)
library(ggplot2)
library(reshape2)
library(Information)

# Reading dataset
dem <- read.csv("Demographic data.csv")
credit <- read.csv("Credit Bureau data.csv")

# Summary
summary(dem)
summary(credit)

str(dem)
str(credit)

# No of rows
nrow(dem)
# 71295

nrow(credit)
# 71295


#===================#
#   Color palette   #
#===================#

cp_2 <- c("#FEA47F", "#F97F51")
cp_3 <- c("#2A363B", "#E84A5F", "#FF847C")
cp_5 <- c("#2A363B", "#E84A5F", "#FF847C", "#FECEAB", "#99B898")
cp_8 <- c("#FEA47F", "#F97F51", "#B33771", "#3B3B98", "#58B19F", "#BDC581", "#2C3A47", "#82589F")

#############################
# Exploratory Data Analysis #
#############################

#=================================#
#   Checking for duplicate data   #
#=================================#

# Demographic Dataset
length(unique(dem$Application.ID))

# 71292

# Credit Dataset
length(unique(credit$Application.ID))

# 71292


dem %>%
  group_by(Application.ID) %>%
  filter(n() > 1)

credit %>%
  group_by(Application.ID) %>%
  filter(n() > 1)

# We could see that few data with same Application ID is for different persons.
# So removing all the duplicate rows

dem <- dem %>%
  group_by(Application.ID) %>%
  filter(n() == 1)

credit <- credit %>%
  group_by(Application.ID) %>%
  filter(n() == 1)

# Total 71289


# Merging the datasets
merged_data <- merge(dem, credit, by=c("Application.ID", "Performance.Tag"))


#===========================#
#   Changing Column Names   #
#===========================#
# 5 , 11:25
names(merged_data)[c(1:2, 5:6, 10:29)] <- c("Application_ID", "Performance_Tag", "Marital_Status", "No_Of_Dependents", "Type_Of_Residence", "Months_In_Current_Residence", "Months_In_Current_Company", "No_Of_90_DPD_6_months", "No_Of_60_DPD_6_months", "No_Of_30_DPD_6_months", "No_Of_90_DPD_12_months","No_Of_60_DPD_12_months","No_Of_30_DPD_12_months", "Avg_CC_Utilization_12_months", "Trades_6_months", "Trades_12_months", "PL_Trades_6_months", "PL_Trades_12_months", "Inquiries_6_months", "Inquiries_12_months", "Open_Home_Loan", "Outstanding_Balance", "Total_No_of_trades", "Open_Auto_Loan")

#=====================#
#   Performance Tag   #
#=====================#

# NA count
merged_data$Performance_Tag %>%
  is.na() %>%
  sum()

# 1425

# Plot for Performance Tag
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=as.factor(Performance_Tag), y=..count../1000, fill=as.factor(Performance_Tag))) + 
  geom_bar() +
  scale_fill_manual(values = cp_2) +
  labs(x="Performance Tag", y="Frequency in 1000s", fill="Performance Tag", title="Frequency of Performance Tag") +
  theme_minimal()

# Percentage of Default

non_default_count <- as.numeric(table(merged_data$Performance_Tag)[2])
default_count <- as.numeric(table(merged_data$Performance_Tag)[1])

default_percentage <- default_count / (default_count+non_default_count)
default_percentage*100
# 95.78 %



#=========#
#   Age   #
#=========#

# Check for Age variable rows with NA values
merged_data$Age %>% 
  is.na() %>% sum()
# 0 

# Checking for outliers
merged_data$Age %>%
  quantile(seq(0,1, 0.01))

merged_data$Age %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")

## Min age is -3
## Some ages are 0
## Capping minimum age to 18
## Since 18 is the minimum age to avail a credit card

merged_data$Age <- merged_data$Age %>%
  as.numeric()

merged_data[(which(merged_data$Age < 18)), ]$Age <- 18

## Creating age bins
merged_data$Age %>% 
  summary()

# Different Bins
# 1) 18-20
# 2) 21-25
# 3) 26-30
# 4) 31-35
# 5) 36-40 
# 6) 41-45
# 7) 46-50 
# 8) 51-55
# 9) 56-60 
# 10) 61-65

# Age Bins function
age_bin <- function(age=3){
  if(age > 17 && age < 21)
    return ("18-20")
  else if(age > 20 && age < 26)
    return ("21-25")
  else if(age > 25 && age < 31)
    return ("26-30")
  else if(age > 30 && age < 36)
    return ("31-35")
  else if(age > 35 && age < 41)
    return ("36-40")
  else if(age > 40 && age < 46)
    return ("41-45")
  else if(age > 45 && age < 51)
    return ("46-50")
  else if(age > 50 && age < 56)
    return ("51-55")
  else if(age > 55 && age < 61)
    return ("56-60")
  else if(age > 60 && age < 66)
    return ("61-65")
  
}

# Creating Age Bin field
merged_data$Age_Bin <-  merged_data$Age %>% 
  sapply(age_bin) %>% 
  as.factor()

# Plot for Frequency of Age Bins
ggplot(merged_data, aes(x=Age_Bin, y=..count../1000, fill=Age_Bin)) +
  geom_bar() +
  labs(x="Age Bin", y="Frequency in 1000s", fill="Age Bin", title="Frequency of different Age Bins") +
  theme_minimal()

# Age Bucket wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Age_Bin, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2) +
  labs(x="Age Buckets", y="Frequency in 1000s", fill="Performance Tag", title="Age Bucket wise Performance Tag Frequency")


#=============#
#   Gender    #
#=============#
# Summary for Gender
merged_data$Gender %>%
  summary()

# 2 NA's

# Converting NA for Gender variable to "M"
levels(merged_data$Gender)[1] <- "M"

# Plot for frequency of each Gender
ggplot(merged_data, aes(x=Gender, y=..count../1000, fill=Gender)) +
  geom_bar() +
  scale_fill_manual(values = cp_2)+
  labs(x="Gender", y="Frequency in 1000s", fill="Gender", title="Frequency of different Gender") +
  theme_minimal()
  
# Gender wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Gender, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2) +
  labs(x="Gender", y="Frequency in 1000s", fill="Performance Tag", title="Gender wise Performance Tag Frequency")


#=====================#
#   Marital Status   #
#=====================#

# Summary for Marital status at time of application
merged_data$Marital_Status %>% 
  summary()
# 6 NA's

# Converting NA for Marital status at time of application variable to "Married"
levels(merged_data$Marital_Status)[1] <- "Married"

# Plot for Marital status at time of application frquency
ggplot(merged_data, aes(x=Marital_Status, y=..count../1000, fill=Marital_Status)) +
  geom_bar()+
  scale_fill_manual(values = cp_8)+
  labs(x="Marital Status at time of application", y="Frequency in 1000s", fill="Marital Status", title="Frequency of different Marital Status") +
  theme_minimal()

# Marital Status wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Marital_Status, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2) +
  labs(x="Marital Status", y="Frequency in 1000s", fill="Performance Tag", title="Marital Status wise Performance Tag Frequency")



#=====================#
#   No of Dependents  #
#=====================#

# Checking for NA values
merged_data$No_Of_Dependents %>% 
  is.na() %>% 
  sum()

# 3 NA's

merged_data$No_Of_Dependents[which(is.na(merged_data$No_Of_Dependents))] <- 3

merged_data$No_Of_Dependents %>% 
  as.factor() %>% 
  summary()

# Checking for outliers
merged_data$No_Of_Dependents %>% 
  quantile(seq(0,1,0.01), na.rm = T)

merged_data$No_Of_Dependents %>% 
  boxplot(border = "#6fa058", outcol = "#ee853f")

#Converting the variable into factor type
merged_data$No_Of_Dependents <- merged_data$No_Of_Dependents %>% as.factor()

# Plot for No of Dependents Frequency
ggplot(merged_data, aes(x=as.factor(No_Of_Dependents), y=..count../1000, fill=as.factor(No_Of_Dependents))) +
  geom_bar() +
  scale_fill_manual(values=cp_5)+
  labs(x="No of Dependents", y="Frequency in 1000s", fill="No of Dependents", title="Frequency of No of Dependents") +
  theme_minimal()

# No of Dependents wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=No_Of_Dependents, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2) +
  labs(x="No of Dependents", y="Frequency in 1000s", fill="Performance Tag", title="No of Dependents wise Performance Tag Frequency")


#=============#
#   Income    #
#=============#

# checking for NA values
merged_data$Income %>%
  is.na() %>% 
  sum()
# 0

# Checking for outliers
merged_data$Income %>% 
  quantile(seq(0,1,0.01), na.rm = T)

merged_data$Income %>% 
  boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data$Income %>% 
  as.factor() %>% 
  levels()

# Converting Income less than 1 to 1.0
merged_data[(which(merged_data$Income < 1)), ] $Income <- 1.0

# Creating Income Bracket
# Income Bracket Function

income_bin <- function(income = 1){
  if(income >= 1 && income <=10)
    return ("1-10")
  else if(income >= 11 && income <=20)
    return ("11-20")
  else if(income >= 21 && income <=30)
    return ("21-30")
  else if(income >= 31 && income <=40)
    return ("31-40")
  else if(income >= 41 && income <=50)
    return ("41-50")
  else
    return ("51-60")
}


merged_data$Income_Bin <-  merged_data$Income %>% 
  sapply(income_bin) %>% 
  as.factor()

# Income Bucket wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Income_Bin, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2) +
  labs(x="Income Buckets", y="Frequency in 1000s", fill="Performance Tag", title="Income Bucket wise Performance Tag Frequency")


#===============#
#   Education   #
#===============#

# checking for NA values
merged_data$Education %>%
  is.na() %>% 
  sum()

# 0

# Checking for blank rows 
merged_data$Education %>%
  summary()

levels(merged_data$Education)[1] <- "Professional"

# Plot for Education Frequency
ggplot(merged_data, aes(x=Education, y=..count../1000, fill=Education)) +
  geom_bar() +
  scale_fill_manual(values=cp_5)+
  labs(x="Education", y="Frequency in 1000s", fill="Education", title="Frequency of Education") +
  theme_minimal()

# Education wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Education, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2) +
  labs(x="Education", y="Frequency in 1000s", fill="Performance Tag", title="Education wise Performance Tag Frequency")



#=================#
#   Profession    #
#=================#

# checking for NA values
merged_data$Profession %>%
  is.na() %>% 
  sum()

# 0

# Checking for blank rows
merged_data$Profession %>% 
  summary()

levels(merged_data$Profession)[1] <- "SAL"

# Plot for Profession Frequency
ggplot(merged_data, aes(x=Profession, y=..count../1000, fill=Profession)) +
  geom_bar() +
  scale_fill_manual(values=cp_3)+
  labs(x="Profession", y="Frequency in 1000s", fill="Profession", title="Frequency of Profession") +
  theme_minimal()

# Profession wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Profession, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2) +
  labs(x="Profession", y="Frequency in 1000s", fill="Performance Tag", title="Profession wise Performance Tag Frequency")



#=======================#
#   Type of residence   #
#=======================#
# checking for NA values
merged_data$Type_Of_Residence %>%
  is.na() %>% 
  sum()

# 0

# Checking for blank rows
merged_data$Type_Of_Residence %>% 
  summary()

levels(merged_data$Type_Of_Residence)[1] <- "Rented"

# Plot for frequency of type of residence
ggplot(merged_data, aes(x=Type_Of_Residence, y=..count../1000, fill=Type_Of_Residence)) +
  geom_bar() +
  scale_fill_manual(values=cp_5)+
  labs(x="Type of residence", y="Frequency in 1000s", fill="Type of residence", title="Frequency of Type of residence") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1)) 

# Type of Residence wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Type_Of_Residence, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = cp_2) +
  labs(x="Type of Residence", y="Frequency in 1000s", fill="Performance Tag", title="Type of Residence wise Performance Tag Frequency") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))



#===========================================#
#   Number of months in current residence   #
#===========================================#

# Checking for NA values
merged_data$Months_In_Current_Residence %>%
  is.na() %>% 
  sum()

# 0

# Checking for outliers
merged_data$Months_In_Current_Residence %>%
  quantile(seq(0,1,0.01), na.rm = T)

merged_data$Months_In_Current_Residence %>% 
  boxplot(border = "#6fa058", outcol = "#ee853f")


# Resident Years Bin Function
res_yrs_bin <- function(nom=0){
  noy = nom/12
  if(noy > 0 && noy < 1)
    return("< 1 yr")
  else if(noy >= 1 && noy < 2)
    return("1 yr")
  else if(noy >= 2 && noy < 3)
    return("2 yrs")
  else if(noy >= 3 && noy < 4)
    return("3 yrs")
  else if(noy >= 4 && noy < 5)
    return("4 yrs")
  else if(noy >= 5 && noy < 6)
    return("5 yrs")
  else if(noy >= 6 && noy < 7)
    return("6 yrs")
  else if(noy >= 7 && noy < 8)
    return("7 yrs")
  else if(noy >= 8 && noy < 9)
    return("8 yrs")
  else if(noy >= 9 && noy < 10)
    return("9 yrs")
  else
    return("> 10 yrs")
}

# Creating No of years in current residence variable
merged_data$Yrs_Curr_Res <- merged_data$Months_In_Current_Residence %>%
  sapply(res_yrs_bin) %>% 
  as.factor()

# Plot of frequency of No of years in current residence variable
ggplot(merged_data, aes(x=Yrs_Curr_Res, y=..count../1000, fill=Yrs_Curr_Res)) +
  geom_bar() +
  labs(x="No of Years in residence", y="Frequency in 1000s", fill="No of Years in residence", title="Frequency of Years in residence") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))



# Years In Current Residence wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Yrs_Curr_Res, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = cp_2) +
  labs(x="Years In Current Residence", y="Frequency in 1000s", fill="Performance Tag", title="Years In Current Residence wise Performance") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))




#=========================================#
#   Number of months in current company   #
#=========================================#

# Checking for NA values
merged_data$Months_In_Current_Company %>%
  is.na() %>% 
  sum()

# 0

# Checking for outliers
merged_data$Months_In_Current_Company %>% 
  quantile(seq(0,1,0.01), na.rm = T)


merged_data$Months_In_Current_Company %>% 
  boxplot(border = "#6fa058", outcol = "#ee853f")

# Capping No of months in current company to 74
merged_data[(which(merged_data$Months_In_Current_Company > 74)),] $Months_In_Current_Company <- 74

#   Current Company Years Bin Function
comp_yrs_bin <- function(nom=0){
  noy = nom/12
  if(noy > 0 && noy < 1)
    return("< 1 yr")
  else if(noy >= 1 && noy < 2)
    return("1 yr")
  else if(noy >= 2 && noy < 3)
    return("2 yrs")
  else if(noy >= 3 && noy < 4)
    return("3 yrs")
  else if(noy >= 4 && noy < 5)
    return("4 yrs")
  else if(noy >= 5 && noy < 6)
    return("5 yrs")
  else 
    return("> 6 yrs")
}

# Crating variable No of years in curr comp
merged_data$Yrs_Curr_Comp <- merged_data$Months_In_Current_Company %>% 
  sapply(comp_yrs_bin) %>% 
  as.factor()

# Plot for No of years in current company
ggplot(merged_data, aes(x=Yrs_Curr_Comp, y=..count../1000, fill=Yrs_Curr_Comp)) +
  geom_bar() +
  labs(x="No of Years in Current Company", y="Frequency in 1000s", fill="No of Years in Current Company", title="Frequency of Years in Current Company") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))


# Years In Current Company wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Yrs_Curr_Comp, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2) +
  labs(x="Years In Current Company", y="Frequency in 1000s", fill="Performance Tag", title="Years In Current Company wise Performance Tag Frequency")



#===================================================#
#   No of times 90 DPD or worse in last 6 months    #
#===================================================#

# Checking for NA values
merged_data$No_Of_90_DPD_6_months %>% 
  is.na() %>% 
  sum()

# 0

merged_data$No_Of_90_DPD_6_months %>% 
  as.factor() %>% 
  summary()


#===================================================#
#   No of times 60 DPD or worse in last 6 months    #
#===================================================#

# Checking for NA values
merged_data$No_Of_60_DPD_6_months %>% 
  is.na() %>% 
  sum()

# 0

merged_data$No_Of_60_DPD_6_months %>% 
  as.factor() %>% 
  summary()



#===================================================#
#   No of times 30 DPD or worse in last 6 months    #
#===================================================#

# Checking for NA values
merged_data$No_Of_30_DPD_6_months %>% 
  is.na() %>% 
  sum()

# 0

merged_data$No_Of_30_DPD_6_months %>% 
  as.factor() %>% 
  summary()



#===================================================#
#   No of times 90 DPD or worse in last 12 months   #
#===================================================#

# Checking for NA values
merged_data$No_Of_90_DPD_12_months %>% 
  is.na() %>% 
  sum()

# 0

merged_data$No_Of_90_DPD_12_months %>% 
  as.factor() %>% 
  summary()



#===================================================#
#   No of times 60 DPD or worse in last 12 months   #
#===================================================#

# Checking for NA values
merged_data$No_Of_60_DPD_12_months %>% 
  is.na() %>% 
  sum()

# 0

merged_data$No_Of_60_DPD_12_months %>% 
  as.factor() %>% 
  summary()



#===================================================#
#   No of times 30 DPD or worse in last 12 months   #
#===================================================#

# Checking for NA values
merged_data$No_Of_30_DPD_12_months %>%
  is.na() %>% 
  sum()
# 0

merged_data$No_Of_30_DPD_12_months %>% 
  as.factor() %>% 
  summary()

#===================================#
#   Correlation of DPD Variables    #
#===================================#

DPD_data_6 <- merged_data[, c(13:15)]
DPD_data_12 <- merged_data[, c(16:18)]

cor_DPD_6 <- round(cor(DPD_data_6), 2)
melted_cor_DPD_6 <- melt(cor_DPD_6)

cor_DPD_12 <- round(cor(DPD_data_12), 2)
melted_cor_DPD_12 <- melt(cor_DPD_12)

# DPD Correlation heat map for 6 months
ggplot(melted_cor_DPD_6, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  labs(x="", y="", title="DPD 6 months Heat Map", fill="Value") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))

# DPD Correlation heat map for 12 months
ggplot(melted_cor_DPD_12, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  labs(x="", y="", title="DPD 12 months Heat Map", fill="Value") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))


#=======================================================#
#   Average Credit Card utilisation in last 12 months   #
#=======================================================#

# Checking for NA values
merged_data$Avg_CC_Utilization_12_months %>%
  is.na() %>% 
  sum()
# 1058

merged_data$Avg_CC_Utilization_12_months %>%
  summary()

# Replacing the NA value with the median
merged_data$Avg_CC_Utilization_12_months[which(is.na(merged_data$Avg_CC_Utilization_12_months))] <- 15


# Checking for outliers
merged_data$Avg_CC_Utilization_12_months %>% 
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Avg_CC_Utilization_12_months %>% 
  boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$Avg_CC_Utilization_12_months > 103)),] $Avg_CC_Utilization_12_months <- 103




#==========================================#
#   No of trades opened in last 6 months   #
#==========================================#

# Checking for NA values
merged_data$Trades_6_months %>%
  is.na() %>% 
  sum()

# 1

merged_data$Trades_6_months %>%
  summary()

# Replacing the NA value with the median
merged_data$Trades_6_months[which(is.na(merged_data$Trades_6_months))] <- 2

# Checking for outliers
merged_data$Trades_6_months %>% quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Trades_6_months %>% boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$Trades_6_months > 6)),] $Trades_6_months <- 6  



#===========================================#
#   No of trades opened in last 12 months   #
#===========================================#

# Checking for NA values
merged_data$Trades_12_months %>%
  is.na() %>% 
  sum()

# 0

# Checking for outliers
merged_data$Trades_12_months %>% quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Trades_12_months %>% boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$Trades_12_months > 19)),] $Trades_12_months <- 19  

#===================================#
#   Correlation of trades opened    #
#===================================#

trades_opened <- merged_data[, c(20, 21)]

cor_trades_opened <- round(cor(trades_opened), 2)
melted_cor_trades_opened <- melt(cor_trades_opened)

# DPD Correlation heat map for 6 months
ggplot(melted_cor_trades_opened, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  labs(x="", y="", title="Trades Opened Heat Map", fill="Value") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))

#==============================================#
#   No of PL trades opened in last 6 months    #
#==============================================#

# Checking for NA values
merged_data$PL_Trades_6_months  %>%
  is.na() %>% 
  sum()

# 0

# Checking for outliers
merged_data$PL_Trades_6_months  %>% quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$PL_Trades_6_months  %>% boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$PL_Trades_6_months  > 5)),] $PL_Trades_6_months  <- 5  



#===============================================#
#   No of PL trades opened in last 12 months    #
#===============================================#

# Checking for NA values
merged_data$PL_Trades_12_months  %>%
  is.na() %>% 
  sum()

# 0

# Checking for outliers
merged_data$PL_Trades_12_months  %>% quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$PL_Trades_12_months  %>% boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$PL_Trades_12_months  > 10)),] $PL_Trades_12_months  <- 10  

#===================================#
#   Correlation of PL trades opened    #
#===================================#

pl_trades_opened <- merged_data[, c(22, 23)]

cor_pl_trades_opened <- round(cor(pl_trades_opened), 2)
melted_cor_pl_trades_opened <- melt(cor_pl_trades_opened)

# DPD Correlation heat map for 6 months
ggplot(melted_cor_pl_trades_opened, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  labs(x="", y="", title="PL Trades Opened Heat Map", fill="Value") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=40, hjust=1))


#===============================================================#
#   No if inquiries in last 6 months excluding home auto loan   #
#===============================================================#

# Checking for NA values
merged_data$Inquiries_6_months %>%
  is.na() %>% 
  sum()

# 0

# Checking for outliers
merged_data$Inquiries_6_months %>%
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Inquiries_6_months %>%
  boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$Inquiries_6_months > 7)),] $Inquiries_6_months <- 7  



#=================================================================#
#   No if inquiries in last 12 months excluding home auto loan    #
#=================================================================#

# Checking for NA values
merged_data$Inquiries_12_months %>%
  is.na() %>% 
  sum()

# 0

# Checking for outliers
merged_data$Inquiries_12_months %>% 
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Inquiries_12_months %>% 
  boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$Inquiries_12_months > 12)),] $Inquiries_12_months <- 12  




#=================================#
#   Presence of open home loan    #
#=================================#

# Checking for NA values
merged_data$Open_Home_Loan %>%
  is.na() %>% 
  sum()

# 272

merged_data$Open_Home_Loan %>% 
  as.factor() %>% 
  summary()

merged_data$Open_Home_Loan[which(is.na(merged_data$Open_Home_Loan))] <- 0

# Converting to factor type
merged_data$Open_Home_Loan <- merged_data$Open_Home_Loan %>%
  as.factor()

# Plot for  Presence of open home loan 
ggplot(merged_data, aes(x=Open_Home_Loan, y=..count../1000, fill=Open_Home_Loan)) +
  geom_bar() +
  scale_fill_manual(values = cp_2) +
  labs(x="Presence of open home loan ", y="Frequency in 1000s",fill="Presence of open home loan ", title="Frequency of Presence of open home loan ") +
  theme_minimal()

# Open Home Loan wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Open_Home_Loan, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2) +
  labs(x="Presence of Open Home Loan", y="Frequency in 1000s", fill="Performance Tag", title="Open Home Loan wise Performance Tag Frequency")




#=================================#
#   Presence of open auto loan    #
#=================================#

# Checking for NA values
merged_data$Open_Auto_Loan %>%
  is.na() %>% 
  sum()

# 0

merged_data$Open_Auto_Loan %>% 
  as.factor() %>% 
  summary()

# Converting to factor type
merged_data$Open_Auto_Loan <- merged_data$Open_Auto_Loan %>%
  as.factor()

# Plot for  Presence of open auto loan 
ggplot(merged_data, aes(x=Open_Auto_Loan, y=..count../1000, fill=Open_Auto_Loan)) +
  geom_bar() +
  scale_fill_manual(values = cp_2) +
  labs(x="Presence of open auto loan ", y="Frequency in 1000s",fill="Presence of open auto loan ", title="Frequency of Presence of open auto loan ") +
  theme_minimal()

# Open Auto Loan wise Performance Tag Frequency
merged_data %>%
  filter(!is.na(Performance_Tag)) %>%
  ggplot(aes(x=Open_Auto_Loan, y=..count../1000, fill=as.factor(Performance_Tag))) +
  geom_bar(position = "dodge") +
  theme_minimal()+
  scale_fill_manual(values = cp_2) +
  labs(x="Presence of Open Auto Loan", y="Frequency in 1000s", fill="Performance Tag", title="Open Auto Loan wise Performance Tag Frequency")




#=========================#
#   Outstanding Balance   #
#=========================#

# Checking for NA values
merged_data$Outstanding_Balance %>%
  is.na() %>% 
  sum()
# 272

merged_data$Outstanding_Balance %>% 
  summary()
# Median = 774985

merged_data$Outstanding_Balance[which(is.na(merged_data$Outstanding_Balance))] <- 774985


# Checking for outliers
merged_data$Outstanding_Balance %>% 
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Outstanding_Balance %>% 
  boxplot(border = "#6fa058", outcol = "#ee853f")


#=========================#
#   Total no of trades    #
#=========================#

# Checking for NA values
merged_data$Total_No_of_trades %>% 
  is.na() %>% 
  sum()

# Checking for outliers
merged_data$Total_No_of_trades %>% 
  quantile(seq(0, 1, 0.01), na.rm = T)

merged_data$Total_No_of_trades %>% 
  boxplot(border = "#6fa058", outcol = "#ee853f")

merged_data[(which(merged_data$Total_No_of_trades > 20)),] $Total_No_of_trades <- 20 

  
  
#=======================#
#   Information Value   #
#=======================#

merged_data_2 <- merged_data[which(!is.na(merged_data$Performance_Tag)), -c(1, 30:33)]
merged_data_2$Performance_Tag <- as.numeric(merged_data_2$Performance_Tag) 

IV <- create_infotables(data=merged_data_2, y="Performance_Tag", bins=10, parallel=TRUE)
IV_Value <- data.frame(IV$Summary)

# Printing the Information Value for each variable
print(IV$Tables$Age, row.names = F)
print(IV$Tables$Gender, row.names = F)
print(IV$Tables$Marital_Status, row.names = F)
print(IV$Tables$No_Of_Dependents, row.names = F)
print(IV$Tables$Income, row.names = F)
print(IV$Tables$Education, row.names = F)
print(IV$Tables$Profession, row.names = F)
print(IV$Tables$Type_Of_Residence, row.names = F)
print(IV$Tables$Months_In_Current_Residence, row.names = F)
print(IV$Tables$Months_In_Current_Company, row.names = F)
print(IV$Tables$No_Of_90_DPD_6_months, row.names = F)
print(IV$Tables$No_Of_60_DPD_6_months, row.names = F)
print(IV$Tables$No_Of_30_DPD_6_months, row.names = F)
print(IV$Tables$No_Of_90_DPD_12_months, row.names = F)
print(IV$Tables$No_Of_60_DPD_12_months, row.names = F)
print(IV$Tables$No_Of_30_DPD_12_months, row.names = F)
print(IV$Tables$Avg_CC_Utilization_12_months, row.names = F)
print(IV$Tables$Trades_6_months, row.names = F)
print(IV$Tables$Trades_12_months, row.names = F)
print(IV$Tables$PL_Trades_6_months , row.names = F)
print(IV$Tables$PL_Trades_12_months , row.names = F)
print(IV$Tables$Inquiries_6_months, row.names = F)
print(IV$Tables$Inquiries_12_months, row.names = F)
print(IV$Tables$Open_Home_Loan, row.names = F)
print(IV$Tables$Outstanding_Balance, row.names = F)
print(IV$Tables$Total_No_of_trades, row.names = F)
print(IV$Tables$Open_Auto_Loan, row.names = F)

# Plot of Information Table
plot_infotables(IV, "Age") + theme_minimal()
plot_infotables(IV, "Gender") + theme_minimal()
plot_infotables(IV, "Marital_Status") + theme_minimal()
plot_infotables(IV, "No_Of_Dependents") + theme_minimal()
plot_infotables(IV, "Income") + theme_minimal()
plot_infotables(IV, "Education") + theme_minimal()
plot_infotables(IV, "Profession") + theme_minimal()
plot_infotables(IV, "Type_Of_Residence" ) + theme_minimal() + theme(axis.text.x=element_text(angle=40, hjust=1))
plot_infotables(IV, "Months_In_Current_Residence") + theme_minimal()
plot_infotables(IV, "Months_In_Current_Company") + theme_minimal()
plot_infotables(IV, "No_Of_90_DPD_6_months") + theme_minimal()
plot_infotables(IV, "No_Of_60_DPD_6_months") + theme_minimal()
plot_infotables(IV, "No_Of_30_DPD_6_months") + theme_minimal()
plot_infotables(IV, "No_Of_90_DPD_12_months") + theme_minimal()
plot_infotables(IV, "No_Of_60_DPD_12_months") + theme_minimal()
plot_infotables(IV, "No_Of_30_DPD_12_months") + theme_minimal()
plot_infotables(IV, "Avg_CC_Utilization_12_months") + theme_minimal()
plot_infotables(IV, "Trades_6_months") + theme_minimal()
plot_infotables(IV, "Trades_12_months") + theme_minimal()
plot_infotables(IV, "PL_Trades_6_months") + theme_minimal()
plot_infotables(IV, "PL_Trades_12_months") + theme_minimal()
plot_infotables(IV, "Inquiries_6_months") + theme_minimal()
plot_infotables(IV, "Inquiries_12_months") + theme_minimal()
plot_infotables(IV, "Open_Home_Loan") + theme_minimal()
plot_infotables(IV, "Outstanding_Balance") + theme_minimal()+ theme(axis.text.x=element_text(angle=40, hjust=1))
plot_infotables(IV, "Total_No_of_trades") + theme_minimal()
plot_infotables(IV, "Open_Auto_Loan") + theme_minimal()

# Checking the Information Value Summary
IV_Value[order(-IV_Value$IV), ]
# Hence we can see that demographic data variables has low Information Value 
# as compared to credit data variables

# Plot
ggplot(aes(x=reorder(as.factor(Variable), IV), y=IV), data = IV_Value) +
  geom_col(fill="#23B98E") +
  theme_minimal() +
  labs(x="", y="Information Value")+
  coord_flip()

#=======================#
#    Imbalance check    #
#=======================#

# Percentage of No
tag_0 <- merged_data$Performance_Tag[which(merged_data$Performance_Tag == 0)]
length(tag_0) # 66917
round(length(tag_0)/69864*100) #96

# Percentage of Yes
tag_1 <- merged_data$Performance_Tag[which(merged_data$Performance_Tag == 1)]
length(tag_1) # 2947
round(length(tag_1)/69864*100) #4

# So we could say that the data is imbalanced




#########################
##    Model Building   ##
#########################

library(caTools)
library(MASS)
library(car)
library(caret)
library(ROSE)

# Storing final scoring data
final_score_data <- merged_data

# Removing Income Bin, Age Bin, Years in Current Residence and Company and Application ID
final_score_data <- final_score_data[,c(2:29)]

final_score_data_cat <- final_score_data[,c(3:5, 7:9, 25, 28)]
final_score_data_cat <- data.frame(sapply(final_score_data_cat, function(x) data.frame(model.matrix(~x-1, data = final_score_data_cat))[,-1]))
final_score_data <-  final_score_data[,-c(3:5, 7:9, 25, 28)]
final_score_data <- cbind(final_score_data, final_score_data_cat)

# Filtering only data without NA's as Performance Tag
final_merged_data <- merged_data %>%
  filter(complete.cases(.))
# Removing Income Bin, Age Bin, Years in Current Residence and Company and Application ID 
final_merged_data <- final_merged_data[,2:29]

# Filtering NA Performance Tag data
perf_na_data <- merged_data %>%
  filter(!complete.cases(.))

# Removing Income Bin, Age Bin, Years in Current Residence and Company and Application ID 
perf_na_data <- perf_na_data[,3:29]

#=============================#
#    Demographic Data Model   #
#=============================#

# Filtering data of Demographic from Final Merged dataset
demo_final <- final_merged_data[, 1:11]

summary(demo_final)

# Creating Dummy variables

demo_cat <- demo_final[,c(3:5, 7:9)]

dummies <- data.frame(sapply(demo_cat, function(x) data.frame(model.matrix(~x-1, data = demo_cat))[,-1]))

demo_scaled <- demo_final[,c(2, 6, 10, 11)] %>% scale()

Performance_Tag <- as.numeric(demo_final$Performance_Tag)

demo_final <-  cbind(Performance_Tag, demo_scaled, dummies) 

# Splitting the data
set.seed(100)

indices <- sample.split(demo_final$Performance_Tag, SplitRatio = 0.7)
train_demo <- demo_final[indices,]
test_demo <- demo_final[!(indices),]

# Checking for the responses
table(train_demo$Performance_Tag)

# Balancing the train data
train_demo <- ROSE(Performance_Tag ~ ., data = train_demo, seed = 1)$data

# Model 1
model_demo_1 = glm(Performance_Tag~., data = train_demo, family = "binomial")
summary(model_demo_1)

# Model 2 with Step AIC
model_demo_2 <- stepAIC(model_demo_1, direction = "both")

summary(model_demo_2)
vif(model_demo_2)

# Since the vif are less than 2.5, removing variables based on p-value
# Type_Of_Residence.xLiving.with.Parents  0.087883   0.048492   1.812 0.069937 .
model_demo_3 <- glm(Performance_Tag ~ Age + Income + Months_In_Current_Residence + 
                      Months_In_Current_Company + Marital_Status + No_Of_Dependents.x2 + 
                      Education.xBachelor + Education.xMasters + Education.xOthers + 
                      Profession.xSE + Profession.xSE_PROF + 
                      Type_Of_Residence.xOthers + Type_Of_Residence.xOwned, family = "binomial", 
                    data = train_demo)

summary(model_demo_3)

# Type_Of_Residence.xOwned     0.034588   0.019124   1.809 0.070504 .
model_demo_4 <- glm(Performance_Tag ~ Age + Income + Months_In_Current_Residence + 
                      Months_In_Current_Company + Marital_Status + No_Of_Dependents.x2 + 
                      Education.xBachelor + Education.xMasters + Education.xOthers + 
                      Profession.xSE + Profession.xSE_PROF + 
                      Type_Of_Residence.xOthers, family = "binomial", 
                    data = train_demo)

summary(model_demo_4)

# Age                          0.013970   0.007749   1.803 0.071422 .
model_demo_5 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                      Months_In_Current_Company + Marital_Status + No_Of_Dependents.x2 + 
                      Education.xBachelor + Education.xMasters + Education.xOthers + 
                      Profession.xSE + Profession.xSE_PROF + 
                      Type_Of_Residence.xOthers, family = "binomial", 
                    data = train_demo)

summary(model_demo_5)

# Marital_Status               0.039481   0.021455   1.840 0.065738 .
model_demo_6 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                      Months_In_Current_Company + No_Of_Dependents.x2 + 
                      Education.xBachelor + Education.xMasters + Education.xOthers + 
                      Profession.xSE + Profession.xSE_PROF + 
                      Type_Of_Residence.xOthers, family = "binomial", 
                    data = train_demo)

summary(model_demo_6)


# Education.xBachelor          0.035862   0.018664   1.921 0.054681 .  
model_demo_7 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                      Months_In_Current_Company + No_Of_Dependents.x2 + 
                      Education.xMasters + Education.xOthers + 
                      Profession.xSE + Profession.xSE_PROF + 
                      Type_Of_Residence.xOthers, family = "binomial", 
                    data = train_demo)

summary(model_demo_7)

# Education.xMasters           0.042557   0.016204   2.626 0.008629 ** 
model_demo_8 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                      Months_In_Current_Company + No_Of_Dependents.x2 + 
                      Education.xOthers + 
                      Profession.xSE + Profession.xSE_PROF + 
                      Type_Of_Residence.xOthers, family = "binomial", 
                    data = train_demo)

summary(model_demo_8)

# Education.xOthers            0.480750   0.164338   2.925 0.003440 ** 
model_demo_9 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                      Months_In_Current_Company + No_Of_Dependents.x2 + 
                      Profession.xSE + Profession.xSE_PROF + 
                      Type_Of_Residence.xOthers, family = "binomial", 
                    data = train_demo)

summary(model_demo_9)

# Type_Of_Residence.xOthers   -0.563454   0.173925  -3.240 0.001197 ** 
model_demo_10 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                      Months_In_Current_Company + No_Of_Dependents.x2 + 
                      Profession.xSE + Profession.xSE_PROF
                      , family = "binomial", 
                    data = train_demo)

summary(model_demo_10)

# Months_In_Current_Residence  0.025713   0.007851   3.275 0.001056 ** 
model_demo_11 <- glm(Performance_Tag ~ Income +
                       Months_In_Current_Company + No_Of_Dependents.x2 + 
                       Profession.xSE + Profession.xSE_PROF
                     , family = "binomial", 
                     data = train_demo)

summary(model_demo_11)


# Final demo model
final_demo_model <- model_demo_11

# Prediction 
test_demo_pred <- predict(final_demo_model, type = "response", 
                          newdata = test_demo[,-1]) 

summary(test_demo_pred)


test_pred <- factor(ifelse(test_demo_pred >= 0.50, "Yes", "No"))
test_actual <- factor(ifelse(test_demo$Performance_Tag==1,"Yes","No"))

table(test_actual, test_pred)


# Cutoff finding function
predict_demo_cutoff <- function(cutoff) 
{
  predicted_perform <- factor(ifelse(test_demo_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_perform, test_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.59,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = predict_demo_cutoff(s[i])
} 

cutoff_demo <- s[which(abs(OUT[,1]-OUT[,2])<0.05)]

# Finding prediction with cutoff
test_cutoff_pred <- factor(ifelse(test_demo_pred >= cutoff_demo, "Yes", "No"))

table(test_actual, test_cutoff_pred)

# Creating Confusion Matrix
test_conf_demo <- confusionMatrix(test_cutoff_pred, test_actual, positive = "Yes")
test_conf_demo

# Accuracy, Sensitivity and Specificity
lr_demo_acc <- test_conf_demo$overall[1] %>% round(2)
lr_demo_sens <- test_conf_demo$byClass[1] %>% round(2)
lr_demo_spec <- test_conf_demo$byClass[2] %>% round(2)

lr_demo_acc  # 0.56 
lr_demo_sens # 0.54
lr_demo_spec # 0.56  

# We can see the Accuracy, Sensitivity and Specificity is quite low
# Hence Demographic data alone could not be used for building the model


#========================#
#    Merged Data Model   #
#========================#

final_merged_data_cat <- final_merged_data[, c(3:5, 7:9, 25, 28)]
final_merged_data_cont <- final_merged_data[, -c(1, 3:5, 7:9, 25, 28)] %>% scale() %>% as.data.frame()
Performance_Tag <- as.numeric(final_merged_data$Performance_Tag)

dummies_final_merged_data <- data.frame(sapply(final_merged_data_cat, function(x) data.frame(model.matrix(~x-1, data = final_merged_data_cat))[,-1])) 


final_lr_data <- cbind(Performance_Tag, final_merged_data_cont, dummies_final_merged_data)

str(final_lr_data)
summary(final_lr_data)

# Splitting the data
set.seed(101)

indices <- sample.split(final_lr_data$Performance_Tag, SplitRatio = 0.7)
train_final <- final_lr_data[indices,]
test_final <- final_lr_data[!(indices),]

# Checking for the Performance Tag imbalance
table(train_final$Performance_Tag)

# Balancing the train data
train_final <- ROSE(Performance_Tag ~ ., data = train_final, seed = 1)$data

# Model 1
lr_model_1 = glm(Performance_Tag ~ ., data = train_final, family = "binomial")
summary(lr_model_1)

# Model 2 after stepAIC
lr_model_2 <- stepAIC(lr_model_1, direction = "both")
summary(lr_model_2)
lr_model_2 %>% vif() %>% sort(decreasing = T)
 

# Education.xMasters                   0.023299   0.015866   1.469 0.141967 
lr_model_3 <- glm(Performance_Tag ~ Age + Income + Months_In_Current_Residence + 
                    Months_In_Current_Company + No_Of_90_DPD_6_months + No_Of_60_DPD_6_months + 
                    No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                    No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                    Trades_12_months + PL_Trades_6_months + PL_Trades_12_months + 
                    Inquiries_12_months + Outstanding_Balance + Total_No_of_trades + 
                    Gender + Marital_Status + No_Of_Dependents.x2 +
                    Education.xOthers + Profession.xSE + Profession.xSE_PROF + 
                    Type_Of_Residence.xCompany.provided + Type_Of_Residence.xOthers + 
                    Open_Home_Loan + Open_Auto_Loan, family = "binomial", data = train_final)

summary(lr_model_3)

# Outstanding_Balance                 -0.015774   0.010133  -1.557 0.119544
lr_model_4 <- glm(Performance_Tag ~ Age + Income + Months_In_Current_Residence + 
                    Months_In_Current_Company + No_Of_90_DPD_6_months + No_Of_60_DPD_6_months + 
                    No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                    No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                    Trades_12_months + PL_Trades_6_months + PL_Trades_12_months + 
                    Inquiries_12_months + Total_No_of_trades + 
                    Gender + Marital_Status + No_Of_Dependents.x2 +
                    Education.xOthers + Profession.xSE + Profession.xSE_PROF + 
                    Type_Of_Residence.xCompany.provided + Type_Of_Residence.xOthers + 
                    Open_Home_Loan + Open_Auto_Loan, family = "binomial", data = train_final)

summary(lr_model_4)

# Age                                  0.012962   0.007644   1.696 0.089924 .  
lr_model_5 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                    Months_In_Current_Company + No_Of_90_DPD_6_months + No_Of_60_DPD_6_months + 
                    No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                    No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                    Trades_12_months + PL_Trades_6_months + PL_Trades_12_months + 
                    Inquiries_12_months + Total_No_of_trades + 
                    Gender + Marital_Status + No_Of_Dependents.x2 +
                    Education.xOthers + Profession.xSE + Profession.xSE_PROF + 
                    Type_Of_Residence.xCompany.provided + Type_Of_Residence.xOthers + 
                    Open_Home_Loan + Open_Auto_Loan, family = "binomial", data = train_final)

summary(lr_model_5)

# Profession.xSE_PROF                  0.033912   0.018182   1.865 0.062163 . 
lr_model_6 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                    Months_In_Current_Company + No_Of_90_DPD_6_months + No_Of_60_DPD_6_months + 
                    No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                    No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                    Trades_12_months + PL_Trades_6_months + PL_Trades_12_months + 
                    Inquiries_12_months + Total_No_of_trades + 
                    Gender + Marital_Status + No_Of_Dependents.x2 +
                    Education.xOthers + Profession.xSE +  
                    Type_Of_Residence.xCompany.provided + Type_Of_Residence.xOthers + 
                    Open_Home_Loan + Open_Auto_Loan, family = "binomial", data = train_final)

summary(lr_model_6)

# Type_Of_Residence.xOthers           -0.286799   0.151126  -1.898 0.057730 .
lr_model_7 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                    Months_In_Current_Company + No_Of_90_DPD_6_months + No_Of_60_DPD_6_months + 
                    No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                    No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                    Trades_12_months + PL_Trades_6_months + PL_Trades_12_months + 
                    Inquiries_12_months + Total_No_of_trades + 
                    Gender + Marital_Status + No_Of_Dependents.x2 +
                    Education.xOthers + Profession.xSE +  
                    Type_Of_Residence.xCompany.provided +  
                    Open_Home_Loan + Open_Auto_Loan, family = "binomial", data = train_final)

summary(lr_model_7)

# No_Of_60_DPD_6_months                0.018435   0.009551   1.930 0.053573 .  
lr_model_8 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                    Months_In_Current_Company + No_Of_90_DPD_6_months +  
                    No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                    No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                    Trades_12_months + PL_Trades_6_months + PL_Trades_12_months + 
                    Inquiries_12_months + Total_No_of_trades + 
                    Gender + Marital_Status + No_Of_Dependents.x2 +
                    Education.xOthers + Profession.xSE +  
                    Type_Of_Residence.xCompany.provided +  
                    Open_Home_Loan + Open_Auto_Loan, family = "binomial", data = train_final)

summary(lr_model_8)

# Total_No_of_trades                  -0.022896   0.011092  -2.064 0.039006 *
lr_model_9 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                    Months_In_Current_Company + No_Of_90_DPD_6_months +  
                    No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                    No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                    Trades_12_months + PL_Trades_6_months + PL_Trades_12_months + 
                    Inquiries_12_months + 
                    Gender + Marital_Status + No_Of_Dependents.x2 +
                    Education.xOthers + Profession.xSE +  
                    Type_Of_Residence.xCompany.provided +  
                    Open_Home_Loan + Open_Auto_Loan, family = "binomial", data = train_final)

summary(lr_model_9)

# Trades_12_months                     0.019315   0.011297   1.710 0.087311 .
lr_model_11 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                    Months_In_Current_Company + No_Of_90_DPD_6_months +  
                    No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                    No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                    PL_Trades_6_months + PL_Trades_12_months + 
                    Inquiries_12_months + 
                    Gender + Marital_Status + No_Of_Dependents.x2 +
                    Education.xOthers + Profession.xSE +  
                    Type_Of_Residence.xCompany.provided +  
                    Open_Home_Loan + Open_Auto_Loan, family = "binomial", data = train_final)

summary(lr_model_11)


# Type_Of_Residence.xCompany.provided  0.101815   0.048009   2.121 0.033945 * 
lr_model_12 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                     Months_In_Current_Company + No_Of_90_DPD_6_months +  
                     No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                     No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                     PL_Trades_6_months + PL_Trades_12_months + 
                     Inquiries_12_months + 
                     Gender + Marital_Status + No_Of_Dependents.x2 +
                     Education.xOthers + Profession.xSE +  
                     Open_Home_Loan + Open_Auto_Loan, family = "binomial", data = train_final)

summary(lr_model_12)

# Marital_Status                0.049462   0.020609   2.400 0.016396 *
lr_model_13 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                     Months_In_Current_Company + No_Of_90_DPD_6_months +  
                     No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                     No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                     PL_Trades_6_months + PL_Trades_12_months + 
                     Inquiries_12_months + 
                     Gender + No_Of_Dependents.x2 +
                     Education.xOthers + Profession.xSE +  
                     Open_Home_Loan + Open_Auto_Loan, family = "binomial", data = train_final)

summary(lr_model_13)

# Education.xOthers             0.378246   0.158858   2.381 0.017264 *
lr_model_14 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                     Months_In_Current_Company + No_Of_90_DPD_6_months +  
                     No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                     No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                     PL_Trades_6_months + PL_Trades_12_months + 
                     Inquiries_12_months + 
                     Gender + No_Of_Dependents.x2 +
                     Profession.xSE +  
                     Open_Home_Loan + Open_Auto_Loan, family = "binomial", data = train_final)

summary(lr_model_14)

# Gender                        0.042585   0.017563   2.425 0.015319 *
lr_model_15 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                     Months_In_Current_Company + No_Of_90_DPD_6_months +  
                     No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                     No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                     PL_Trades_6_months + PL_Trades_12_months + 
                     Inquiries_12_months + 
                     No_Of_Dependents.x2 +
                     Profession.xSE +  
                     Open_Home_Loan + Open_Auto_Loan, family = "binomial", data = train_final)

summary(lr_model_15)

# Open_Auto_Loan                0.068058   0.027645   2.462 0.013822 * 
lr_model_16 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                     Months_In_Current_Company + No_Of_90_DPD_6_months +  
                     No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                     No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                     PL_Trades_6_months + PL_Trades_12_months + 
                     Inquiries_12_months + 
                     No_Of_Dependents.x2 +
                     Profession.xSE +  
                     Open_Home_Loan, family = "binomial", data = train_final)

summary(lr_model_16)

# Months_In_Current_Residence  -0.021565   0.007811  -2.761 0.005768 ** 
lr_model_17 <- glm(Performance_Tag ~ Income + 
                     Months_In_Current_Company + No_Of_90_DPD_6_months +  
                     No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                     No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                     PL_Trades_6_months + PL_Trades_12_months + 
                     Inquiries_12_months + 
                     No_Of_Dependents.x2 +
                     Profession.xSE +  
                     Open_Home_Loan, family = "binomial", data = train_final)

summary(lr_model_17)

# No_Of_60_DPD_12_months        0.029015   0.009076   3.197  0.00139 **
lr_model_18 <- glm(Performance_Tag ~ Income + 
                     Months_In_Current_Company + No_Of_90_DPD_6_months +  
                     No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + 
                     No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                     PL_Trades_6_months + PL_Trades_12_months + 
                     Inquiries_12_months + 
                     No_Of_Dependents.x2 +
                     Profession.xSE +  
                     Open_Home_Loan, family = "binomial", data = train_final)

summary(lr_model_18)

# Trades_6_months               0.031982   0.009977   3.206 0.001348 ** 
lr_model_19 <- glm(Performance_Tag ~ Income + 
                     Months_In_Current_Company + No_Of_90_DPD_6_months +  
                     No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + 
                     No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months +
                     PL_Trades_6_months + PL_Trades_12_months + 
                     Inquiries_12_months + 
                     No_Of_Dependents.x2 +
                     Profession.xSE +  
                     Open_Home_Loan, family = "binomial", data = train_final)

summary(lr_model_19)



# Final Logistic Regression model
final_lr_model <- lr_model_19

# Predict
test_final_pred <- predict(final_lr_model, type = "response", 
                           newdata = test_final[,-1]) 

summary(test_final_pred)


test_final_predicted <- factor(ifelse(test_final_pred >= 0.5, "Yes", "No"))
test_final_actual <- factor(ifelse(test_final$Performance_Tag==1,"Yes","No"))

table(test_final_actual, test_final_predicted)

# Cutoff finding function
predict_final_cutoff <- function(cutoff) 
{
  predicted_perform <- factor(ifelse(test_final_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_perform, test_final_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.87,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = predict_final_cutoff(s[i])
} 

cutoff_final <- s[which(abs(OUT[,1]-OUT[,2])<0.03)]

# Prediction on cutoff
test_final_cutoff_pred <- factor(ifelse(test_final_pred >= cutoff_final, "Yes", "No"))

table(test_final_actual, test_final_cutoff_pred)


# Creating Confusion Matrix
test_conf_final <- confusionMatrix(test_final_cutoff_pred, test_final_actual, positive = "Yes")
test_conf_final

# Accuracy, Sensitivity and Specificity
lr_final_acc <- test_conf_final$overall[1] %>% round(2)
lr_final_sens <- test_conf_final$byClass[1] %>% round(2)
lr_final_spec <- test_conf_final$byClass[2] %>% round(2)

lr_final_acc  # 0.63
lr_final_sens # 0.62
lr_final_spec # 0.63  


#=====================#
# Random Forest Model #
#=====================#


library(randomForest)

set.seed(101)

# Filtering only data without NA's as Performance Tag
final_merged_data <- merged_data %>%
  filter(complete.cases(.))
# Removing Income Bin, Age Bin, Years in Current Residence and Company and Application ID 
final_merged_data <- final_merged_data[,2:29]

# Categorical Dummy Creation
final_merged_data_cat <- final_merged_data[, c(3:5, 7:9, 25, 28)]
final_merged_data <- final_merged_data[, -c(3:5, 7:9, 25, 28)]
final_merged_data_cat <- data.frame(sapply(final_merged_data_cat, function(x) data.frame(model.matrix(~x-1, data = final_merged_data_cat))[,-1])) 

final_merged_data <- final_merged_data %>% cbind(final_merged_data_cat)
  
split_indices <- sample.split(final_merged_data$Performance_Tag, SplitRatio = 0.70)

train_rf_final <- final_merged_data[split_indices, ]

test_rf_final <- final_merged_data[!split_indices, ]

nrow(train_rf_final)/nrow(final_merged_data)

nrow(test_rf_final)/nrow(final_merged_data)


# Checking for balance in train dataset
table(train_rf_final$Performance_Tag)

# Balacing the train dataset using ROSE
train_rf_final <- ROSE(Performance_Tag ~ ., data = train_rf_final, seed = 1)$data

# Converting Performance Tag to yes and no
train_rf_final$Performance_Tag <- as.factor(ifelse(train_rf_final$Performance_Tag == 1,"yes","no"))
test_rf_final$Performance_Tag <- as.factor(ifelse(test_rf_final$Performance_Tag == 1,"yes","no"))

# Building the model 

final_rf <- randomForest(Performance_Tag ~ ., data = train_rf_final, proximity = F, do.trace = T, ntree = 57, mtry = 5, nodesize = 16) # nodesize = 16, mtry = 5, ntree = 57

# Predict performance Tag for test data

rf_final_pred <- predict(final_rf, test_rf_final[, -1], type = "prob")

summary(rf_final_pred[, 2])

#==========================================================#

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_final_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf_final$Performance_Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#==========================================================#

# creating cutoff values for plotting and initialising a matrix of size 1000x4

s = seq(.01,.84,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100){
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0.25,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which.min(abs(OUT_rf[,1]-OUT_rf[,2]))]

# The plot shows that cutoff value of around 22% optimises sensitivity and accuracy

predicted_response_22 <- as.factor(ifelse(rf_final_pred[, 2] >= cutoff_rf, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response_22, test_rf_final[, 1], positive = "yes")

conf_forest


# Accuracy 
conf_forest$overall[1] %>% round(2)
# 0.64

# Sensitivity
conf_forest$byClass[1] %>% round(2)
# 0.62

# Specificity 
conf_forest$byClass[2] %>% round(2)
# 0.64


# Final RF important variables
importance <- final_rf$importance 

importance <- data.frame(importance)

View(importance)

#================================================================#
#   Predicting Performance_Tag for NA dataset using best model   #
#================================================================#

# Creating dummies
# 2 3 4 6 7 8 24 27
perf_na_dummies <- perf_na_data[,c(2:4, 6:8, 24, 27)]
perf_na_data[,c(2:4, 6:8, 24, 27)] <- NULL
perf_na_dummies <- data.frame(sapply(perf_na_dummies, function(x) data.frame(model.matrix(~x-1, data = perf_na_dummies))[,-1])) 

perf_na_data <- cbind(perf_na_data, perf_na_dummies)

# Predicting
Performance_Tag <- predict(final_rf, perf_na_data, type = "prob")
Performance_Tag <- as.factor(ifelse(Performance_Tag[, 2] >= cutoff_rf, 1, 0))
perf_na_data <- cbind(Performance_Tag, perf_na_data)

# Final Dataset using NA Performance Tag predictd data
final_data <- rbind(final_merged_data, perf_na_data)

#=============================================#
#   Logistic Regression Model for full data   #
#=============================================#

final_data_lr <- final_data

# Scaling the continious variables
final_data_lr[,2:20] <- final_data_lr[,2:20]%>% scale()

# Converting Performance Tag to factor type
final_data_lr$Performance_Tag <- final_data_lr$Performance_Tag %>% as.factor()

# Splitting data into testing and training datasets
set.seed(101)
indices <- sample.split(final_data_lr$Performance_Tag, 0.7)
train <- final_data_lr[indices,]
test <- final_data_lr[!indices,]

# Checking for imbalance in dataset
table(train$Performance_Tag)

# Balancing the dataset
train <- ROSE(Performance_Tag ~ ., data = train, seed = 1)$data

# Model 1
lr_full_model_1 <- glm(Performance_Tag ~ ., train, family = "binomial")

lr_full_model_1 %>% summary()

# Model 2 with Step AIC
lr_full_model_2 <- stepAIC(lr_full_model_1, direction = "both")

lr_full_model_2 %>% summary()

lr_full_model_2 %>% vif %>% sort(decreasing = T)

# Since all vif's are less than 2.5
# Removing variables based on p-value

# No_Of_Dependents.x5                     0.038866   0.021774   1.785  0.07427 .
lr_full_model_3 <- glm(Performance_Tag ~ Age + Income + Months_In_Current_Residence + 
                         Months_In_Current_Company + No_Of_90_DPD_6_months + No_Of_60_DPD_6_months + 
                         No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                         No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                         Trades_12_months + PL_Trades_6_months + PL_Trades_12_months + 
                         Inquiries_12_months + Outstanding_Balance + No_Of_Dependents.x2 + 
                         No_Of_Dependents.x3 + Education.xBachelor + 
                         Profession.xSE + Type_Of_Residence.xLiving.with.Parents + 
                         Type_Of_Residence.xOthers + Type_Of_Residence.xOwned + Open_Home_Loan, 
                       family = "binomial", data = train)

lr_full_model_3 %>% summary()

# Type_Of_Residence.xOwned               -0.036492   0.019958  -1.828 0.067486 . 
lr_full_model_4 <- glm(Performance_Tag ~ Age + Income + Months_In_Current_Residence + 
                         Months_In_Current_Company + No_Of_90_DPD_6_months + No_Of_60_DPD_6_months + 
                         No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                         No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                         Trades_12_months + PL_Trades_6_months + PL_Trades_12_months + 
                         Inquiries_12_months + Outstanding_Balance + No_Of_Dependents.x2 + 
                         No_Of_Dependents.x3 + Education.xBachelor + 
                         Profession.xSE + Type_Of_Residence.xLiving.with.Parents + 
                         Type_Of_Residence.xOthers + Open_Home_Loan, 
                       family = "binomial", data = train)
lr_full_model_4 %>% summary()

# Trades_12_months                        0.024132   0.012587   1.917 0.055215 . 
lr_full_model_5 <- glm(Performance_Tag ~ Age + Income + Months_In_Current_Residence + 
                         Months_In_Current_Company + No_Of_90_DPD_6_months + No_Of_60_DPD_6_months + 
                         No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                         No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                         PL_Trades_6_months + PL_Trades_12_months + 
                         Inquiries_12_months + Outstanding_Balance + No_Of_Dependents.x2 + 
                         No_Of_Dependents.x3 + Education.xBachelor + 
                         Profession.xSE + Type_Of_Residence.xLiving.with.Parents + 
                         Type_Of_Residence.xOthers + Open_Home_Loan, 
                       family = "binomial", data = train)
lr_full_model_5 %>% summary()

# Education.xBachelor                    -0.039276   0.018278  -2.149 0.031653 * 
lr_full_model_6 <- glm(Performance_Tag ~ Age + Income + Months_In_Current_Residence + 
                         Months_In_Current_Company + No_Of_90_DPD_6_months + No_Of_60_DPD_6_months + 
                         No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                         No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                         PL_Trades_6_months + PL_Trades_12_months + 
                         Inquiries_12_months + Outstanding_Balance + No_Of_Dependents.x2 + 
                         No_Of_Dependents.x3 + 
                         Profession.xSE + Type_Of_Residence.xLiving.with.Parents + 
                         Type_Of_Residence.xOthers + Open_Home_Loan, 
                       family = "binomial", data = train)
lr_full_model_6 %>% summary()


# Age                                    -0.017958   0.008164  -2.200 0.027827 *
lr_full_model_7 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                         Months_In_Current_Company + No_Of_90_DPD_6_months + No_Of_60_DPD_6_months + 
                         No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                         No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                         PL_Trades_6_months + PL_Trades_12_months + 
                         Inquiries_12_months + Outstanding_Balance + No_Of_Dependents.x2 + 
                         No_Of_Dependents.x3 + 
                         Profession.xSE + Type_Of_Residence.xLiving.with.Parents + 
                         Type_Of_Residence.xOthers + Open_Home_Loan, 
                       family = "binomial", data = train)
lr_full_model_7 %>% summary()

# Type_Of_Residence.xLiving.with.Parents -0.116994   0.050817  -2.302 0.021320 *
lr_full_model_8 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                         Months_In_Current_Company + No_Of_90_DPD_6_months + No_Of_60_DPD_6_months + 
                         No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                         No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                         PL_Trades_6_months + PL_Trades_12_months + 
                         Inquiries_12_months + Outstanding_Balance + No_Of_Dependents.x2 + 
                         No_Of_Dependents.x3 + 
                         Profession.xSE + 
                         Type_Of_Residence.xOthers + Open_Home_Loan, 
                       family = "binomial", data = train)
lr_full_model_8 %>% summary()

# Outstanding_Balance          -0.028072   0.011068  -2.536 0.011204 *
lr_full_model_9 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                         Months_In_Current_Company + No_Of_90_DPD_6_months + No_Of_60_DPD_6_months + 
                         No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                         No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                         PL_Trades_6_months + PL_Trades_12_months + 
                         Inquiries_12_months + No_Of_Dependents.x2 + 
                         No_Of_Dependents.x3 + 
                         Profession.xSE + 
                         Type_Of_Residence.xOthers + Open_Home_Loan, 
                       family = "binomial", data = train)
lr_full_model_9 %>% summary()

# No_Of_90_DPD_6_months         0.023765   0.008659   2.744 0.006062 **
lr_full_model_10 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                         Months_In_Current_Company + No_Of_60_DPD_6_months + 
                         No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                         No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + Trades_6_months + 
                         PL_Trades_6_months + PL_Trades_12_months + 
                         Inquiries_12_months + No_Of_Dependents.x2 + 
                         No_Of_Dependents.x3 + 
                         Profession.xSE + 
                         Type_Of_Residence.xOthers + Open_Home_Loan, 
                       family = "binomial", data = train)
lr_full_model_10 %>% summary()

# Trades_6_months               0.033786   0.010858   3.111 0.001862 ** 
lr_full_model_11 <- glm(Performance_Tag ~ Income + Months_In_Current_Residence + 
                          Months_In_Current_Company + No_Of_60_DPD_6_months + 
                          No_Of_30_DPD_6_months + No_Of_90_DPD_12_months + No_Of_60_DPD_12_months + 
                          No_Of_30_DPD_12_months + Avg_CC_Utilization_12_months + 
                          PL_Trades_6_months + PL_Trades_12_months + 
                          Inquiries_12_months + No_Of_Dependents.x2 + 
                          No_Of_Dependents.x3 + 
                          Profession.xSE + 
                          Type_Of_Residence.xOthers + Open_Home_Loan, 
                        family = "binomial", data = train)
lr_full_model_11 %>% summary()

final_model_lr <- lr_full_model_11

# Prediction
test_pred <- predict(final_model_lr, type = "response", 
                           newdata = test[,-1]) 

summary(test_pred)


test_predicted <- factor(ifelse(test_pred >= 0.5, "Yes", "No"))
test_actual <- factor(ifelse(test$Performance_Tag==1,"Yes","No"))

table(test_actual, test_predicted)

#==========================================================================#
# Cutoff finding function
predict_cutoff <- function(cutoff) 
{
  predicted_perform <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_perform, test_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
#==========================================================================#

s = seq(.01,.97,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = predict_cutoff(s[i])
} 

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# Prediction on cutoff
test_cutoff_pred <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))

table(test_actual, test_cutoff_pred)


# Creating Confusion Matrix
test_conf <- confusionMatrix(test_cutoff_pred, test_actual, positive = "Yes")
test_conf

# Accuracy, Sensitivity and Specificity
lr_acc <- test_conf$overall[1] %>% round(2)
lr_sens <- test_conf$byClass[1] %>% round(2)
lr_spec <- test_conf$byClass[2] %>% round(2)

lr_acc  # 0.69
lr_sens # 0.70
lr_spec # 0.69  


#=======================================#
#   Random Forest Model for full data   #
#=======================================#

# Converting Performance Tag to factor
final_data$Performance_Tag <- ifelse(final_data$Performance_Tag == 1, "yes", "no") %>% as.factor()



# Model Preparation with final dataset
set.seed(101)

split_indices <- sample.split(final_data$Performance_Tag, SplitRatio = 0.70)

train_rf <- final_data[split_indices, ]

test_rf <- final_data[!split_indices, ]

# Checking for imbalance in dataset
table(train_rf$Performance_Tag)

# Balancing the dataset
train_rf <- ROSE(Performance_Tag ~ ., data = train_rf, seed = 1)$data

# Building the random forest model
rf_model <- randomForest(Performance_Tag ~ ., data = train_rf, proximity = F, do.trace = T, ntree = 50, mtry = 4, nodesize = 16)
 
# Predcting
rf_perf_prob <- predict(rf_model, test_rf, type="prob")
summary(rf_perf_prob[,2])

# Using cutoff of 0.5
rf_perf_pred <- as.factor(ifelse(rf_perf_prob[,2] >= 0.5, "yes", "no"))

table(test_rf$Performance_Tag, rf_perf_pred)


# Finding Cutoff

#==========================================================#

# Cutoff for randomforest to assign yes or no

cutoff_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_perf_prob[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance_Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#==========================================================#

# creating cutoff values for plotting and initialising a matrix of size 1000x4
#s = seq(.01,.86,length=100)
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100){
  OUT_rf[i,] = cutoff_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0.25,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which.min(abs(OUT_rf[,1]-OUT_rf[,2]))]


predicted_response_22 <- as.factor(ifelse(rf_final_pred[, 2] >= cutoff_rf, "yes", "no"))

# Confusion matrix for the final model
conf_forest <- confusionMatrix(predicted_response_22, test_rf_final[, 1], positive = "yes")

conf_forest


# Accuracy 
conf_forest$overall[1] %>% round(2)
# 0.62

# Sensitivity
conf_forest$byClass[1] %>% round(2)
# 0.64

# Specificity 
conf_forest$byClass[2] %>% round(2)
# 0.62

#=============================================================#


#=============================================================#
# Hence we can see the best model is Logistic Regression      #
# With Accuracy of  about 69%, Sensitivity of about 70%       #
# and Specificity of about 69%                                #  
#=============================================================#

#===========#
# Scorecard #
#===========#

final_score_data_2 <- final_score_data

final_score_data_2[,c(2:20)] <- final_score_data_2[,c(2:20)] %>% scale()

final_score_data$Pred_Log <- predict(final_model_lr, type = "response", 
                    newdata = final_score_data_2[-1]) 

final_score_data$Pred_Non_Log <- 1- final_score_data$Pred_Log


# Calculating ODDs
final_score_data$Odds <- log(final_score_data$Pred_Log, final_score_data$Pred_Non_Log)


fact <- 20 / log(2)
off <- 400 - (fact *log(10))

# Caculating Score
final_score_data$Score = off + (fact * final_score_data$Odds)

summary(final_score_data$Score)
# Minimum is 333.8
# Our cutoff score is 359 which is 1st Quarter

final_score_data$Score %>% boxplot(border = "#6fa058", outcol = "#ee853f", ylab = "Score")

above_score_data <- final_score_data[which(final_score_data$Score >= 359), ]
below_score_data <- final_score_data[which(final_score_data$Score < 359), ]


nrow(below_score_data)
# 17615

#==============#
# Revenue Loss #
#==============#

nrow(below_score_data[which(below_score_data$Performance_Tag == 0), ])
# 14948
nrow(above_score_data[which(above_score_data$Performance_Tag == 1), ])
# 1686


# Suppose the Bank makes $1000 per good customer, the revenue loss will be 
# 1000 * 14948 = $14,948,000 i.e. 14.9 million
# 1000 * 1686 = $1,686,000 i.e. 1.6 million


#=======================#
# Revenue Loss Avoided  #
#=======================#

def <- nrow(above_score_data[which(above_score_data$Performance_Tag == 1), ])
# 1686

non_def <- nrow(above_score_data[which(above_score_data$Performance_Tag == 0), ])
# 51969

# Default Percentage
(def/nrow(above_score_data) * 100) %>% round(0)
# 3 %

# Non-Default Percentage
(non_def/nrow(above_score_data) * 100) %>% round(0)
# 97 %

# This model can correctly identify and acquire 97% of customers who would not default

# Suppose the Bank makes $1000 per good customer, the revenue loss avoided will be 
# 1000 * 51969 = $51,969,000 i.e. 51.9 million


