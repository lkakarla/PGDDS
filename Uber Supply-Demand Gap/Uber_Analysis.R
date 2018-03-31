#clear Environment
rm(list = ls()) 

#load packages
library("dplyr")
library("ggplot2")
library("stringr")
library("scales")
#library("lubridate")
#library("tidyr")

#-------------------------------------------------------------------------------
# load the data and check the content and blank, NA values
#-------------------------------------------------------------------------------
#read csv
uber_df <- read.csv("Uber Request Data.csv", stringsAsFactors = F)
str(uber_df)
summary(uber_df)
head(uber_df)

#Missing values
sum(is.na(uber_df))

# get column names having NA
colnames(uber_df)[colSums(is.na(uber_df)) > 0]
sapply(uber_df, function(x) length(which(is.na(x))))

#Checking for blank "" values
sapply(uber_df, function(x) length(which(x == "")))


#Check for missing values
length(which(is.na(uber_df$Driver.id)))
length(which(is.na(uber_df$Drop.timestamp)))


#check for duplicates
duplicated(uber_df$Request.id)
sum(duplicated(uber_df$Request.id))
uber_df[which(duplicated(uber_df$Request.id) == T), ] # no duplicates found


# Look at types of Pickup point and Status - Check for Lowercase and Uppercase
summary(factor(uber_df$Pickup.point))
summary(factor(uber_df$Status))


#-------------------------------------------------------------------------------
# Data cleaning - formatting date columns
#-------------------------------------------------------------------------------

# formatting Request.timestamp column
# replace '/' with '-'
uber_df$Request.timestamp <- gsub('[/]', '-', uber_df$Request.timestamp)
# use if else condition to add seconds to timestamp, if not present
uber_df$Request.timestamp <- ifelse(str_detect(uber_df$Request.timestamp, pattern = "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"), uber_df$Request.timestamp, paste(uber_df$Request.timestamp,":00",sep=""))
# format timestamp
uber_df$Request.timestamp <- as.POSIXct(uber_df$Request.timestamp, format = "%d-%m-%Y %H:%M:%S")


# formatting Drop.timestamp column
# replace '/' with '-'
uber_df$Drop.timestamp <- gsub('[/]', '-', uber_df$Drop.timestamp)
# use if else condition to add seconds to timestamp, if not present
uber_df$Drop.timestamp <- ifelse(str_detect(uber_df$Drop.timestamp, pattern = "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"), uber_df$Drop.timestamp, paste(uber_df$Drop.timestamp,":00",sep=""))
# format timestamp
uber_df$Drop.timestamp <- as.POSIXct(uber_df$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")

#alternatively, we can use 'lubridate' package to format date columns as given below
#uber_df$Request.timestamp <- parse_date_time(x = uber_df$Request.timestamp,orders = c("dmy_HMS", "dmy_HM"))
#uber_df$Drop.timestamp <- parse_date_time(x = uber_df$Drop.timestamp,orders = c("dmy_HMS", "dmy_HM"))


#-------------------------------------------------------------------------------
#Derving new columns
#-------------------------------------------------------------------------------
# extract Request hour from Request.timestamp
uber_df$Request.hour <- as.numeric(format(uber_df$Request.timestamp, "%H"))

# extract Drop hour from Drop.timestamp
#uber_df$Drop.Hour <- as.numeric(format(uber_df$Drop.timestamp, "%H"))

# Create timeslot based on hour
uber_df <- mutate(uber_df, Request.timeslot = ifelse(Request.hour %in% 05:08,"Early Morning",
                                                     ifelse(Request.hour %in% 09:11,"Morning",
                                                            ifelse(Request.hour %in% 12:16,"Afternoon",
                                                                   ifelse(Request.hour %in% 17:21,"Evening",
                                                                          ifelse(Request.hour %in% 22:23,"Night",
                                                                                 ifelse(Request.hour %in% 00:04,"Night","Other"
                                                                                 )))))))

# derive new column - calculate time taken to travel from pickup to drop
# uber_df$Travel.time.mins <- round(difftime(uber_df$Drop.timestamp, uber_df$Request.timestamp,units="mins"))

# Derive new column 'Supply' to categorise supply 
# Supply -- Requests with 'Trip Completed' status
uber_df <- mutate(uber_df, Supply= ifelse(Status == "Trip Completed", "Supply", NA))

# Derive new column 'Demand' to categorise demand
# Demand -- All the requests with 'Trip Completed' , 'Cancelled' & 'No Cars Available' status
uber_df <- mutate(uber_df, Demand= ifelse(Status %in% c("Trip Completed","No Cars Available","Cancelled"), "Demand", NA))

# Convert 'Request.timeslot' to ordered categorical variable
uber_df$Request.timeslot <- factor(uber_df$Request.timeslot, ordered = TRUE, levels = c("Early Morning", "Morning", "Afternoon", "Evening", "Night"))

#-------------------------------------------------------------------------------
#Plots
#-------------------------------------------------------------------------------

#	Frequency of requests - 'Cancelled' or 'No Cars Available'
# ---------------------------
ggplot(uber_df, aes(x= Status, fill= Pickup.point)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), stat="count") +
  labs(title="Frequency of Requests based on Status and Pickup point") +
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequency")


# Supply Demand Gap - Timeslot
# ---------------------------
ggplot() + 
  geom_bar(data = uber_df[!is.na(uber_df$Supply), ], aes(Supply, ..count..,fill=Status), width = 0.5) +
  geom_bar(data = uber_df[!is.na(uber_df$Demand), ], aes(Demand, ..count..,fill=Status)) +
  facet_grid(~ Request.timeslot) +
  ggtitle("Supply and Demand Based on Timeslot") +
  xlab('Supply vs Demand') +
  ylab('Count') 


# Supply Demand Gap - Type of Request and  Timeslot
# ---------------------------
ggplot() + 
  geom_bar(data = uber_df[!is.na(uber_df$Supply), ], aes(Supply, ..count.., fill=Pickup.point)) +
  geom_bar(data = uber_df[!is.na(uber_df$Demand), ], aes(Demand, ..count.., fill=Pickup.point)) +
  facet_grid(~ Request.timeslot) +
  ggtitle("Supply and Demand Based on Timeslot and Pickup point") +
  xlab('Supply vs Demand') +
  ylab('Count')



#Supply Demand Gap - Issue Cause
# ---------------------------
ggplot(uber_df, aes(x=Request.timeslot, y= ..count.., group=Status,  colour=Status)) +
  facet_grid(Pickup.point ~ .) +
  geom_density(size = 1.5) +
  labs(title="Density Plot based on Pickup Requested Timeslot and Status")



# Other Plots 
#-------------------------------------
# plot shows Frequency of Requests - Timeslot wise
ggplot(uber_df, aes(x = Request.hour, fill=Request.timeslot)) +
  geom_bar(aes(y = (..count..))) +
  geom_text(stat='count',aes(label=..count..)) +
  ggtitle("Frequency of Requests - Timeslots wise") +
  scale_x_continuous(
    breaks = c(0:23)
  ) +
  ylab('Number of Requests')


# plot shows Status of Requested Pickups Per Hour on Pickup point wise
ggplot(uber_df, aes(x = Request.hour, fill = Status)) +
  facet_wrap(~ Pickup.point) +
  geom_bar(stat="count", aes(y = (..count..)), width = 0.3, position=position_dodge(), colour="black") +
  ggtitle("Status of Requested Pickups Per Hour Pickup point wise") +
  scale_x_continuous(
    breaks = c(0:23)
  ) +
  ylab("Count")


# plot shows status of pickup requests based on time slot
ggplot(uber_df, aes(x = Request.timeslot, fill = Status)) +
  geom_bar(stat="count", aes(y = (..count..)), width = 0.3, position=position_dodge(), colour="black") +
  ggtitle("Status of Pickup Requests Based on Timeslot") +
  xlab("Timeslot") +
  ylab("Count") +
  coord_flip()


# plot shows status of pickup requests based on time slot and pickup point
ggplot(uber_df, aes(x = Request.timeslot, fill = Status)) +
  facet_wrap(~ Pickup.point) +
  geom_bar(stat="count", aes(y = (..count..)), width = 0.3, position=position_dodge(), colour="black") +
  ggtitle("Status of Pickup Requests based on Timeslot and Pickup point") +
  xlab("Timeslot") +
  ylab("Count") +
  coord_flip()
