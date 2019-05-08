 # Libraries
  library(ggplot2)
  library(stringr)
  library(dplyr)

 # Loading the dataset
  uber <- read.csv("Uber Request Data.csv")

 # Summary of the dataset
  summary(uber)
  
 # Total Number of NA's
  sum(is.na(uber))
  
 # Number of NA's in Driver.id and Drop.timestamp columns
  sum(is.na(uber$Driver.id))
  sum(is.na(uber$Drop.timestamp))

 ###########################
 ## Cleaning Uber dataset ##
 ###########################

 # Converting uber driver id to factor
  uber$Driver.id <- as.factor(uber$Driver.id)

 # Clean the dates

 # Replace / with -
  uber$Request.timestamp <- gsub("/", "-", uber$Request.timestamp)
  uber$Drop.timestamp <- gsub("/", "-", uber$Drop.timestamp)

 # Conversion of timestamp
  uber$Request.timestamp <- strptime(uber$Request.timestamp,"%d-%m-%Y %H:%M")
  uber$Drop.timestamp <- strptime(uber$Drop.timestamp,"%d-%m-%Y %H:%M")


 ####################################
 ## Time Slot Function             ##
 ####################################
 ### 00 to 04 hrs = Late Night    ###
 ### 04 to 08 hrs = Early Morning ###
 ### 08 to 12 hrs = Morning       ###
 ### 12 to 16 hrs = Afternoon     ###
 ### 16 to 20 hrs = Evening       ###
 ### 20 to 00 hrs = Night         ###
 ####################################

   getTimeSlot <- function(hour){
     hour <- as.numeric(hour)
     if(hour>=00 && hour<04)
       return("Late Night")
     else if(hour>=04 && hour<08)
       return("Early Morning")
     else if(hour>=08 && hour<12)
       return("Morning")
     else if(hour>=12 && hour<16)
       return("Afternoon")
     else if(hour>=16 && hour<20)
       return("Evening")
     else
       return("Night")
   }

 # Setting Time Slots
   uber$Request.time_slot <- sapply(format(uber$Request.timestamp, "%H"), function(x) getTimeSlot(x))
   uber$Request.time_slot <- as.factor(uber$Request.time_slot)

 #################################################
 ## Percentage of Complete and Incomplete trips ##
 #################################################

 # City

   length_complete_citytotal <- length(which(uber$Status == "Trip Completed" & uber$Pickup.point == "City"))
   length_incomplete_citytotal <- length(which(uber$Status != "Trip Completed" & uber$Pickup.point == "City"))
   length_citytotal <- length(which(uber$Pickup.point == "City"))
  
   percent_complete_citytotal <- round(length_complete_citytotal / length_citytotal * 100, 2)
   ####    42.89 %   ####
   percent_incomplete_citytotal <- round(length_incomplete_citytotal / length_citytotal * 100, 2)
   ####    57.11 %   ####

 # Airport

   length_complete_airporttotal <- length(which(uber$Status == "Trip Completed" & uber$Pickup.point == "Airport"))
   length_incomplete_airporttotal <- length(which(uber$Status != "Trip Completed" & uber$Pickup.point == "Airport"))
   length_airporttotal <- length(which(uber$Pickup.point == "Airport"))
  
   percent_complete_airporttotal <- round(length_complete_airporttotal / length_airporttotal * 100, 2)
   ####    40.98 %   ####
   percent_incomplete_airporttotal <- round(length_incomplete_airporttotal / length_airporttotal * 100, 2)
   ####    59.02 %   ####



 ################
 ## Uber Plots ##
 ################

 # Frequency of total requests on time slots
  ggplot(uber, aes(x=Request.time_slot, fill=Pickup.point)) + geom_bar(position = "dodge") + labs(x="Time Slot", y="Frequency", fill="Pickup Point", title="Frequency of Total Requests at Different Time Slots") + geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(width = 1))
 
 # Breaking down with status
  ggplot(uber, aes(x=Request.time_slot, fill=Pickup.point)) + geom_bar(position = "dodge") + labs(x="Time Slot", y="Frequency", fill="Pickup Point", title="Frequency of Requests of Different Status at Various Time Slots") + facet_wrap(~Status) + geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(width = 1))

 #######################################################
 ## Sub Uber, containing only incomplete trip data    ##
 #######################################################

 # Taking subset of status "No Cars Available" and "Cancelled"
  sub_uber <- subset(uber, Status == "No Cars Available" | Status == "Cancelled")
 
 # Dropping column Drop.timestamp from sub uber (Since all values are NA)
  sub_uber <- sub_uber[ ,-which(names(sub_uber) == "Drop.timestamp")]

 ####################
 ## Sub Uber Plots ##
 ####################
 
 # Frequency of cancelled and No cars available status
   ggplot(sub_uber, aes(x=Request.time_slot, fill=Pickup.point)) + geom_bar(position = "dodge") + labs(x="Time Slot", y="Frequency", fill="Pickup Point", title="Frequency of Incomplete Requests at Different Time Slots") + geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(width = 1))
   ggplot(sub_uber, aes(x=Request.time_slot, fill=Pickup.point)) + geom_bar(position = "dodge") + labs(x="Time Slot", y="Frequency", fill="Pickup Point", title="Frequency of Incomplete Requests of Different Status at Different Time Slots") + geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(width = 1)) + facet_wrap(~Status)

 #### The highest demand not met for City pickup is Early Morning and Morning ####
 #### The highest demand not met for Airport pickup is Evening and Night ####

 ##########################################################
 ## Percentage of trip completion and incomplete in City ##
 ## Time-slots: Early Morning and Morning                ##
 ##########################################################
 
   length_city_complete <- length(which((uber$Request.time_slot == "Early Morning" | uber$Request.time_slot == "Morning") & uber$Pickup.point == "City" & uber$Status == "Trip Completed"))
   length_city_incomplete <- length(which((uber$Request.time_slot == "Early Morning" | uber$Request.time_slot == "Morning") & uber$Pickup.point == "City" & uber$Status != "Trip Completed"))
   length_city_total <- length(which((uber$Request.time_slot == "Early Morning" | uber$Request.time_slot == "Morning") & uber$Pickup.point == "City"))
   
   percent_city_complete <- round(length_city_complete / length_city_total * 100, 2)
   ####   30.87 %   ####
   percent_city_incomplete <- round(length_city_incomplete / length_city_total * 100, 2)
   ####   69.13 %   ####
   
 #############################################################
 ## Percentage of trip completion and incomplete in Airport ##
 ## Time-slots: Evening and Night                           ##
 #############################################################
 
   length_airport_complete <- length(which((uber$Request.time_slot == "Evening" | uber$Request.time_slot == "Night") & uber$Pickup.point == "Airport" & uber$Status == "Trip Completed"))
   length_airport_incomplete <- length(which((uber$Request.time_slot == "Evening" | uber$Request.time_slot == "Night") & uber$Pickup.point == "Airport" & uber$Status != "Trip Completed"))
   length_airport_total <- length(which((uber$Request.time_slot == "Evening" | uber$Request.time_slot == "Night") & uber$Pickup.point == "Airport"))
   
   percent_airport_complete <- round(length_airport_complete / length_airport_total * 100, 2)
   ####   25.82 %   ####
   percent_airport_incomplete <- round(length_airport_incomplete / length_airport_total * 100, 2)
   ####   74.18 %   ####
 
 ###########################################
 ## City during Early Morning and Morning ##
 ###########################################

  uber_city <- subset(sub_uber, (Request.time_slot == "Early Morning" | Request.time_slot == "Morning") & Pickup.point == "City")
 
 ######################################
 ## Airport during Evening and Night ##
 ######################################
 
  uber_airport <- subset(sub_uber, (Request.time_slot == "Evening" | Request.time_slot == "Night") & Pickup.point == "Airport")
 
 
 #################################
 ## Uber City and Airport Plots ##
 #################################
 
  ggplot(uber_city, aes(x=format(Request.timestamp, "%H"))) + geom_bar(fill="cornflowerblue")+ labs(x="Request Hour", y="Frequency", title="Hour wise Frequency of Incomplete Requests for City") + geom_text(stat='count', aes(label=..count..), vjust=-1)
  # 05, 06, 07, 08, 09 hrs
 
  ggplot(uber_airport, aes(x=format(Request.timestamp, "%H"))) + geom_bar(fill="aquamarine3") + labs(x="Request Hour", y="Frequency", title="Hour wise Frequency of Incomplete Requests for Airport") + geom_text(stat='count', aes(label=..count..), vjust=-1)
  # 17, 18, 19, 20, 21  hrs
 
 
   