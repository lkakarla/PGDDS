#############################HR Analytics - Logistic Regression Modelling###################
#-------------------------------------------------------------------------------
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation



#######################################################################################
### Business Understanding:
#######################################################################################

# The company has maintained a database containing professional information about its past & current employees.

## AIM:

# The aim is to automate the process of predicting if an employee would leave the company or not and 
# To find the factors affecting the probability of attrition based on following data.

# 1. General data about employees
# 2. Employee and Manager Survey Data
# 3. Log in and Log out time





#######################################################################################
### Data Understanding
#######################################################################################

# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")

#install.packages("lubridate")

#clear Environment
rm(list = ls()) 

#-------------------------------------------------------------------------------
#load packages
#-------------------------------------------------------------------------------

library("MASS")  # StepAIC function
library("car")   # VIF function
library("e1071")  
library("caret") # confusionMatrix 
library("ggplot2")
library("cowplot")
library("caTools")
library("lubridate")


#-------------------------------------------------------------------------------
# Load the data and check its contents
#-------------------------------------------------------------------------------
# Run below code if PA-I_Case_Study_HR_Analytics.zip file is not extracted
# unzip(zipfile = "PA-I_Case_Study_HR_Analytics.zip", exdir = ".")

#load csv files
general_df  <-  read.csv("general_data.csv", stringsAsFactors = F) 
emp_survey_df  <-  read.csv("employee_survey_data.csv", stringsAsFactors = F) 
mgr_survey_df  <-  read.csv("manager_survey_data.csv", stringsAsFactors = F) 
in_time_df  <-  read.csv("in_time.csv", stringsAsFactors = F) 
out_time_df  <-  read.csv("out_time.csv", stringsAsFactors = F) 


# data structure 
str(general_df)   #4410 obs of 24 variables
str(emp_survey_df)  #4410 obs of 4 variables
str(mgr_survey_df)  #4410 obs of 3 variables
str(in_time_df)  #4410 obs of 262 variables
str(out_time_df)  #4410 obs of 262 variables


summary(general_df)
summary(emp_survey_df)
summary(mgr_survey_df)
summary(in_time_df)
summary(out_time_df)


head(general_df)
head(emp_survey_df)
head(mgr_survey_df)
head(in_time_df)
head(out_time_df)


#print column names
names(general_df)
names(emp_survey_df)
names(mgr_survey_df)
names(in_time_df)
names(out_time_df)

#Rename first column in in_time_df & out_time_df to "EmployeeID"
colnames(in_time_df)[1] <- "EmployeeID"
colnames(out_time_df)[1] <- "EmployeeID"


#Print columns having unique values for each row
#names(general_df[sapply(general_df, function(x) length(unique(x))==nrow(general_df))])  # EmployeeID
#names(emp_survey_df[sapply(emp_survey_df, function(x) length(unique(x))==nrow(emp_survey_df))])  # EmployeeID
#names(mgr_survey_df[sapply(mgr_survey_df, function(x) length(unique(x))==nrow(mgr_survey_df))])  # EmployeeID
#names(in_time_df[sapply(in_time_df, function(x) length(unique(x))==nrow(in_time_df))])  # EmployeeID
#names(out_time_df[sapply(out_time_df, function(x) length(unique(x))==nrow(out_time_df))])  # EmployeeID


# Unique values in the data frames
length(unique(tolower(general_df$EmployeeID)))    # 4410, confirming EmployeeID is primary key 
length(unique(tolower(emp_survey_df$EmployeeID))) # 4410, confirming EmployeeID is primary key
length(unique(tolower(mgr_survey_df$EmployeeID))) # 4410, confirming EmployeeID is primary key
length(unique(tolower(in_time_df$EmployeeID)))    # 4410, confirming EmployeeID is primary key
length(unique(tolower(out_time_df$EmployeeID)))   # 4410, confirming EmployeeID is primary key


# Check whether primary key matches across all data frames
setdiff(general_df$EmployeeID,emp_survey_df$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_df$EmployeeID,mgr_survey_df$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_df$EmployeeID,in_time_df$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_df$EmployeeID,out_time_df$EmployeeID) # Identical EmployeeID across these datasets


#######################################################################################
### Data Preparation & Exploratory Data Analysis
#######################################################################################


#-------------------------------------------------------------------------------
# Derive New Metrics -- Average Work hours and Leaves Calculation
#-------------------------------------------------------------------------------

############# Derive Average Work hours 

#Holiday - Exclude the columns where logged in data not available for any employee
in_time_df <- in_time_df[, which(sapply(in_time_df, function(x) sum(is.na(x))) != nrow(in_time_df))] #

#Convert date to ymd_hms format
in_time_df[,-1] <- sapply(in_time_df[,-1], ymd_hms)

#Holiday -Exclude the columns where logged out data not available for any employee
out_time_df <- out_time_df[, which(sapply(out_time_df, function(x) sum(is.na(x))) != nrow(in_time_df))]

#Convert date to ymd_hms format
out_time_df[,-1] <- sapply(out_time_df[,-1], ymd_hms)

# total working hours calculation
tot_work_hrs <- (out_time_df[,-1] - in_time_df[,-1])/3600 

# average working hours calculation
avg_work_hrs <- data.frame(EmployeeID = in_time_df$EmployeeID, 
                           AvgWorkHrs = round(rowMeans(tot_work_hrs, na.rm = T),2))


############# Derive Total Leaves

# Considered if employee logged in as present, otherwise as employee took leave.
leaves_df <- as.data.frame(ifelse(is.na(in_time_df[,-1]), 1, 0))

# Holiday - Exclude the columns where logged in data not available for any employee
# exclude holidays from the leave data frame
leaves_df <- leaves_df[, which(colSums(leaves_df) != nrow(in_time_df))]

# Calcualte total leaves
tot_leaves <- data.frame(EmployeeID = in_time_df$EmployeeID, TotalLeaves = (rowSums(leaves_df))) 


#-------------------------------------------------------------------------------
# Merge All Data Sets
#-------------------------------------------------------------------------------

#merge all the data frames
master_df  <- merge(general_df, emp_survey_df, by="EmployeeID", all = F)
master_df  <- merge(master_df, mgr_survey_df, by="EmployeeID", all = F)
master_df <- merge(master_df, avg_work_hrs, by = "EmployeeID", all = F)
master_df <- merge(master_df, tot_leaves, by = "EmployeeID", all = F)

#master data frame
View(master_df)  # 31 columns

# Understanding the structure of data frame
str(master_df)

# Age, DistanceFromHome,  MonthlyIncome,  NumCompaniesWorked, TrainingTimesLastYear, PercentSalaryHike , TotalWorkingYears, YearsAtCompany, 
# YearsSinceLastPromotion, YearsWithCurrManager, AvgWorkHrs,  TotalLeaves  are continuous variables

#backup master data frame
#master_bkp_df <- master_df


#-------------------------------------------------------------------------------
#Remove Unnecesaary Columns
#-------------------------------------------------------------------------------

#print no of unique values under each column
as.data.frame(rapply(master_df,function(x)length(unique(x))))


#Print column names having same value for all rows
names(master_df[sapply(master_df, function(x) length(unique(x))==1)])  # "EmployeeCount" "Over18" & "StandardHours"


#Remove all columns having same value for all rows  -- "EmployeeCount" "Over18" & "StandardHours"
master_df <- master_df[sapply(master_df, function(x) length(unique(x))>1)]  


#Print columns having unique values for each row
names(master_df[sapply(master_df, function(x) length(unique(x))==nrow(master_df))])  # "EmployeeID"


#-------------------------------------------------------------------------------
#Check for duplicates
#-------------------------------------------------------------------------------
sum(duplicated(master_df)) # no duplicate rows



#-------------------------------------------------------------------------------
#Missing values
#-------------------------------------------------------------------------------

#Check for Missing values
sum(is.na(master_df))   # 111 NA values

#Get column names having NA
colnames(master_df)[colSums(is.na(master_df)) > 0]  
na_col <- sapply(master_df, function(x) length(which(is.na(x))))
na_col   #  "NumCompaniesWorked" --19, "EnvironmentSatisfaction" -- 25, "JobSatisfaction" --20, "WorkLifeBalance"  --38, "TotalWorkingYears" -- 9 , 
table(na_col)


# Checking for blank "" values
blank_col <- sapply(master_df, function(x) length(which(x == "")))
blank_col
table(blank_col)  # 0 blanks


# missing values  -- 111/4410 = 0.02 i.e 2%

# replace missing values in below columns with the value repeated mostly in respective column
# "NumCompaniesWorked" --19, "EnvironmentSatisfaction" -- 25, "JobSatisfaction" --20, "WorkLifeBalance"  --38
master_df$NumCompaniesWorked[which(is.na(master_df$NumCompaniesWorked))] <- as.integer(names(sort(table(master_df$NumCompaniesWorked), decreasing = T)[1]))
master_df$EnvironmentSatisfaction[which(is.na(master_df$EnvironmentSatisfaction))] <- as.integer(names(sort(table(master_df$EnvironmentSatisfaction), decreasing = T)[1]))
master_df$JobSatisfaction[which(is.na(master_df$JobSatisfaction))] <- as.integer(names(sort(table(master_df$JobSatisfaction), decreasing = T)[1]))
master_df$WorkLifeBalance[which(is.na(master_df$WorkLifeBalance))] <- as.integer(names(sort(table(master_df$WorkLifeBalance), decreasing = T)[1]))

# replace missing values in "TotalWorkingYears" column with "YearsAtCompany" column values
master_df$TotalWorkingYears[which(is.na(master_df$TotalWorkingYears))] <- master_df$YearsAtCompany[which(is.na(master_df$TotalWorkingYears))]


# Check again for NA values
na_col <- sapply(master_df, function(x) length(which(is.na(x))))
na_col
table(na_col)

sum(is.na(master_df)) #  0 NA values


#-------------------------------------------------------------------------------
# Convert Ordered Categorical Variables data from Numeric to Text values
#-------------------------------------------------------------------------------

str(master_df)

#Education
#------------
factor(master_df$Education)

master_df$Education<- ifelse(master_df$Education==1,"BelowCollege",
                             ifelse(master_df$Education==2,"College",
                                    ifelse(master_df$Education==3,"Bachelor",
                                           ifelse(master_df$Education==4,"Master","Doctor"))))

#convert to factor with ordered levels
master_df$Education <- factor(master_df$Education, ordered = TRUE, levels = c('BelowCollege', 'College', 'Bachelor', 'Master', 'Doctor'))



#JobLevel
#------------
factor(master_df$JobLevel)

master_df$JobLevel<- ifelse(master_df$JobLevel==1,"L1",
                            ifelse(master_df$JobLevel==2,"L2",
                                   ifelse(master_df$JobLevel==3,"L3",
                                          ifelse(master_df$JobLevel==4,"L4","L5"))))

#convert to factor with ordered levels
master_df$JobLevel <- factor(master_df$JobLevel, ordered = TRUE, levels = c('L1', 'L2', 'L3', 'L4', 'L5'))



#StockOptionLevel
#------------
factor(master_df$StockOptionLevel)

master_df$StockOptionLevel<- ifelse(master_df$StockOptionLevel==0,"L0",
                                    ifelse(master_df$StockOptionLevel==1,"L1",
                                           ifelse(master_df$StockOptionLevel==2,"L2","L3")))

#convert to factor with ordered levels
master_df$StockOptionLevel <- factor(master_df$StockOptionLevel, ordered = TRUE, levels = c('L0', 'L1', 'L2', 'L3'))



#EnvironmentSatisfaction
#------------
factor(master_df$EnvironmentSatisfaction)

master_df$EnvironmentSatisfaction <- ifelse(master_df$EnvironmentSatisfaction==1,"Low",
                                            ifelse(master_df$EnvironmentSatisfaction==2,"Medium",
                                                   ifelse(master_df$EnvironmentSatisfaction==3,"High",
                                                          ifelse(master_df$EnvironmentSatisfaction==4,"VeryHigh",""))))
#convert to factor with ordered levels
master_df$EnvironmentSatisfaction <- factor(master_df$EnvironmentSatisfaction, ordered = TRUE, levels = c('Low', 'Medium', 'High', 'VeryHigh'))



#JobSatisfaction
#------------
factor(master_df$JobSatisfaction)

master_df$JobSatisfaction<- ifelse(master_df$JobSatisfaction==1,"Low",
                                   ifelse(master_df$JobSatisfaction==2,"Medium",
                                          ifelse(master_df$JobSatisfaction==3,"High",
                                                 ifelse(master_df$JobSatisfaction==4,"VeryHigh",""))))
#convert to factor with ordered levels
master_df$JobSatisfaction <- factor(master_df$JobSatisfaction, ordered = TRUE, levels = c('Low', 'Medium', 'High', 'VeryHigh'))


#WorkLifeBalance
#------------
factor(master_df$WorkLifeBalance)

master_df$WorkLifeBalance<- ifelse(master_df$WorkLifeBalance==1,"Bad",
                                   ifelse(master_df$WorkLifeBalance==2,"Good",
                                          ifelse(master_df$WorkLifeBalance==3,"Better",
                                                 ifelse(master_df$WorkLifeBalance==4,"Best",""))))
#convert to factor with ordered levels
master_df$WorkLifeBalance <- factor(master_df$WorkLifeBalance, ordered = TRUE, levels = c('Bad', 'Good', 'Better', 'Best'))

#JobInvolvement
#------------
factor(master_df$JobInvolvement)

master_df$JobInvolvement<- ifelse(master_df$JobInvolvement==1,"Low",
                                  ifelse(master_df$JobInvolvement==2,"Medium",
                                         ifelse(master_df$JobInvolvement==3,"High",
                                                ifelse(master_df$JobInvolvement==4,"VeryHigh",""))))

#convert to factor with ordered levels
master_df$JobInvolvement <- factor(master_df$JobInvolvement, ordered = TRUE, levels = c('Low', 'Medium', 'High', 'VeryHigh'))


#PerformanceRating
#------------
factor(master_df$PerformanceRating)

master_df$PerformanceRating<- ifelse(master_df$PerformanceRating==1,"Low",
                                     ifelse(master_df$PerformanceRating==2,"Good",
                                            ifelse(master_df$PerformanceRating==3,"Excellent",
                                                   ifelse(master_df$PerformanceRating==4,"Outstanding",""))))

#convert to factor with ordered levels
master_df$PerformanceRating <- factor(master_df$PerformanceRating, ordered = TRUE, levels = c('Low', 'Good', 'Excellent', 'Outstanding'))

str(master_df)

#-------------------------------------------------------------------------------
# Plots to Explore Data
#-------------------------------------------------------------------------------

# Barcharts for categorical features with stacked Attrition information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(master_df, aes(x=BusinessTravel,fill=Attrition))+ geom_bar() + bar_theme1,
          ggplot(master_df, aes(x=Department,fill=Attrition))+ geom_bar()+ bar_theme1,
          ggplot(master_df, aes(x=EducationField,fill=Attrition))+ geom_bar()+ bar_theme1,
          ggplot(master_df, aes(x=Gender,fill=Attrition))+ geom_bar()+ bar_theme1,
          ggplot(master_df, aes(x=JobRole,fill=Attrition))+ geom_bar()+ bar_theme1,
          ggplot(master_df, aes(x=MaritalStatus,fill=Attrition))+ geom_bar() + bar_theme1,
          align = "h") 

# Unmarried employees have significant impact on attrition rate, followed by employees who travel frequently


plot_grid(ggplot(master_df, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+ bar_theme1,
          ggplot(master_df, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+ bar_theme1,
          ggplot(master_df, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar() + bar_theme1,
          ggplot(master_df, aes(x=JobInvolvement,fill=Attrition))+ geom_bar() + bar_theme1,
          ggplot(master_df, aes(x=PerformanceRating,fill=Attrition))+ geom_bar() + bar_theme1,
          align = "h")

# Employees with Low EnvironmentSatisfaction have high attrition rate compare to other levels.
# Similarly employees with low job satisfaction also have high attrition rate



# Histogram and Boxplots for numeric variables 
box_theme <- theme(axis.line=element_blank(),axis.title=element_blank(), 
                   axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y <- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                     axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                     legend.position="none")


plot_grid(ggplot(master_df, aes(Age))+ geom_histogram(),
          ggplot(master_df, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#no outliers in Age observations

plot_grid(ggplot(master_df, aes(DistanceFromHome))+ geom_histogram(),
          ggplot(master_df, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#no outliers in DistanceFromHome observations

plot_grid(ggplot(master_df, aes(MonthlyIncome))+ geom_histogram(),
          ggplot(master_df, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#outliers noticed in MonthlyIncome observations

plot_grid(ggplot(master_df, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 1),
          ggplot(master_df, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#outliers noticed in NumCompaniesWorked observations

plot_grid(ggplot(master_df, aes(PercentSalaryHike))+ geom_histogram(binwidth = 1),
          ggplot(master_df, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#no outliers in PercentSalaryHike observations

plot_grid(ggplot(master_df, aes(TotalWorkingYears))+ geom_histogram(binwidth = 1),
          ggplot(master_df, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#outliers noticed in NumCompaniesWorked observations

plot_grid(ggplot(master_df, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 1),
          ggplot(master_df, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#outliers noticed in TrainingTimesLastYear observations

plot_grid(ggplot(master_df, aes(YearsAtCompany))+ geom_histogram(binwidth = 1),
          ggplot(master_df, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#outliers noticed in YearsAtCompany observations

plot_grid(ggplot(master_df, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 1),
          ggplot(master_df, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#outliers noticed in YearsSinceLastPromotion observations

plot_grid(ggplot(master_df, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 1),
          ggplot(master_df, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#outliers noticed in YearsWithCurrManager observations

plot_grid(ggplot(master_df, aes(AvgWorkHrs))+ geom_histogram(binwidth = 1),
          ggplot(master_df, aes(x="",y=AvgWorkHrs))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#outliers noticed in AvgWorkHrs observations

plot_grid(ggplot(master_df, aes(TotalLeaves))+ geom_histogram(binwidth = 1),
          ggplot(master_df, aes(x="",y=TotalLeaves))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#no outliers in TotalLeaves observations


#need to check outliers on numeric variables.

#MonthlyIncome
#TotalWorkingYears
#TrainingTimesLastYear
#YearsAtCompany, 
#YearsSinceLastPromotion
#YearsWithCurrManager
#AvgWorkHrs


# Boxplots of numeric variables relative to attrition status
plot_grid(ggplot(master_df, aes(x=Attrition,y = Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(master_df, aes(x=Attrition,y = DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_df, aes(x=Attrition,y = MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,          
          ggplot(master_df, aes(x=Attrition,y = NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_df, aes(x=Attrition,y = PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,          
          ggplot(master_df, aes(x=Attrition,y = TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_df, aes(x=Attrition,y = TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_df, aes(x=Attrition,y = YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_df, aes(x=Attrition,y = YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_df, aes(x=Attrition,y = YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(master_df, aes(x=Attrition,y = AvgWorkHrs, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,          
          ggplot(master_df, aes(x=Attrition,y = TotalLeaves, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,             
          align = "v",nrow = 1)


#Following numeric variables seems have more impact on attrition rate 
#DistanceFromHome
#NumCompaniesWorked
#PercentSalaryHike
#AvgWorkHrs
#TotalLeaves



# Correlation between numeric variables
library(GGally)
ggpairs(master_df[, c("Age","DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", 
                      "PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear",
                      "YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager",
                      "AvgWorkHrs","TotalLeaves")])

# TotalWorkingYears and Age are highly correlated (corr 0.68)
# YearsAtCompany and TotalWorkingYears are highly correlated (corr 0.63)
# YearsSinceLastPromotion and YearsAtCompany are highly correlated (corr 0.62)
# YearsWithCurrManager and YearsAtCompany are highly correlated (corr 0.77)


#-------------------------------------------------------------------------------
# Outlier Treatment
#-------------------------------------------------------------------------------

#MonthlyIncome
#---------------
#checking outliers for MonthlyIncome
quantile(master_df$MonthlyIncome,seq(0,1,0.01))

table(master_df$MonthlyIncome)

#There is a jump between each quantile from 0-2%. Here, we will floor all the values below 18590.0(3% value)to 18590.0
master_df$MonthlyIncome[which(master_df$MonthlyIncome < 18590.0)] <- 18590.0

#There is a jump between 95% and 100%. So, capping all values above 178560.0 to 178560.0
master_df$MonthlyIncome[which(master_df$MonthlyIncome > 178560.0)] <- 178560.0


#NumCompaniesWorked
#---------------
#checking outliers for NumCompaniesWorked
quantile(master_df$NumCompaniesWorked,seq(0,1,0.01))

table(master_df$NumCompaniesWorked)

sort(boxplot.stats(master_df$NumCompaniesWorked)$out)  # outlier values.
#boxplot(master_df$NumCompaniesWorked, main="NumCompaniesWorked")

#As per box plot, capping all values above 8 to 8
master_df$NumCompaniesWorked[which(master_df$NumCompaniesWorked > 8)] <- 8


#TotalWorkingYears
#---------------
#checking outliers for TotalWorkingYears
quantile(master_df$TotalWorkingYears,seq(0,1,0.01))

table(master_df$TotalWorkingYears)

#There is a jump between 96% and 100%. So, capping all values above 29 to 29
master_df$TotalWorkingYears[which(master_df$TotalWorkingYears > 29)] <- 29


#TrainingTimesLastYear
#---------------
#checking outliers for TrainingTimesLastYear
quantile(master_df$TrainingTimesLastYear,seq(0,1,0.01))

table(master_df$TrainingTimesLastYear)


#YearsAtCompany
#---------------
#checking outliers for YearsAtCompany
quantile(master_df$YearsAtCompany,seq(0,1,0.01))

table(master_df$YearsAtCompany)

#There is a jump between 98% and 100%. So, capping all values above 24 to 24
master_df$YearsAtCompany[which(master_df$YearsAtCompany > 24)] <- 24


#YearsSinceLastPromotion 
#---------------
#checking outliers for YearsSinceLastPromotion
quantile(master_df$YearsSinceLastPromotion,seq(0,1,0.01))

table(master_df$YearsSinceLastPromotion)

#There is a jump between 95% and 100%. So, capping all values above 9 to 9
master_df$YearsSinceLastPromotion[which(master_df$YearsSinceLastPromotion > 9)] <- 9


#YearsWithCurrManager 
#---------------
#checking outliers for YearsWithCurrManager
quantile(master_df$YearsWithCurrManager,seq(0,1,0.01))

table(master_df$YearsWithCurrManager)

#There is a jump between 99% and 100%. So, capping all values above 14 to 14
master_df$YearsWithCurrManager[which(master_df$YearsWithCurrManager > 14)] <- 14


#AvgWorkHrs 
#---------------
#checking outliers for AvgWorkHrs
quantile(master_df$AvgWorkHrs,seq(0,1,0.01))

table(master_df$AvgWorkHrs)

#There is a jump between 99% and 100%. So, capping all values above 10.9000 to 10.9000
master_df$AvgWorkHrs[which(master_df$AvgWorkHrs > 10.9000)] <- 10.9000


#-------------------------------------------------------------------------------
# Feature Standardisation
#-------------------------------------------------------------------------------

# Normalising continuous variables

#mean(master_df$TotalLeaves)
#sd(master_df$TotalLeaves)

master_df$Age <- scale(master_df$Age) # scale used: mean 36.92, sd 9.13
master_df$DistanceFromHome <- scale(master_df$DistanceFromHome) # scale used: mean 9.19, sd 8.1
master_df$MonthlyIncome <- scale(master_df$MonthlyIncome) # scale used: mean 64499.93, sd 45313.64
master_df$NumCompaniesWorked <- scale(master_df$NumCompaniesWorked) # scale used: mean 2.65, sd 2.41
master_df$PercentSalaryHike <- scale(master_df$PercentSalaryHike) # scale used: mean 15.2, sd 3.659
master_df$TotalWorkingYears <- scale(master_df$TotalWorkingYears) # scale used: mean 11.12, sd 7.35
master_df$TrainingTimesLastYear <- scale(master_df$TrainingTimesLastYear) # scale used: mean 2.79, sd 1.286
master_df$YearsAtCompany <- scale(master_df$YearsAtCompany) # scale used: mean 6.88, sd 5.66
master_df$YearsSinceLastPromotion <- scale(master_df$YearsSinceLastPromotion) # scale used: mean 2.02, sd 2.71
master_df$YearsWithCurrManager <- scale(master_df$YearsWithCurrManager) # scale used: mean 4.1, sd 3.5
master_df$AvgWorkHrs <- scale(master_df$AvgWorkHrs) # scale used: mean 7.7, sd 1.34
master_df$TotalLeaves <- scale(master_df$TotalLeaves) # scale used: mean 12.73, sd 5.5

# converting target variable Attrition from No/Yes character to factor with levels 0/1 
master_df$Attrition<- ifelse(master_df$Attrition=="Yes",1,0)

# Checking Attrition rate of Employee
Attrition <- sum(master_df$Attrition)/nrow(master_df)
Attrition # 16.12% attrition rate which is almost same as(15%) mentioned in the case study

#backup master data frame
#master_bkp2_df <- master_df

#-------------------------------------------------------------------------------
# Dummy Variables - Create dummy variables to convert the categorical variables to numerical
#-------------------------------------------------------------------------------

#variables having 2 levels

#Gender
#----------
#Check the summary of "Gender" variable
summary(factor(master_df$Gender))

# converting Gender variable from Female/Male to factor with levels 0/1 
master_df$Gender<- ifelse(master_df$Gender=="Female",1,0)

indices <- which(colnames(master_df) %in% 
                   c('BusinessTravel','Department', 'Education', 'EducationField', 'JobLevel','JobRole', 
                     'MaritalStatus', 'StockOptionLevel', 'EnvironmentSatisfaction',
                     'JobSatisfaction', 'WorkLifeBalance', 'JobInvolvement','PerformanceRating'))

master_chr <- master_df[, indices]

master_chr<- data.frame(sapply(master_chr, function(x) if (is.factor(x)) x else factor(x) ))
str(master_chr)

# creating dummy variables for factor attributes
dummies <- data.frame(sapply(master_chr, 
                            function(x) data.frame(model.matrix(~x-1,data = master_chr))[,-1]))


# Final dataset
final_df <- cbind(master_df[,-c(1, indices)],dummies) # removed "EmployeeID" variable

View(final_df) # 4410 obs. of  57 variables



#######################################################################################
### Model Buliding
#######################################################################################

#-------------------------------------------------------------------------------
# Splitting the data between train and test
#-------------------------------------------------------------------------------
set.seed(100)

indices = sample.split(final_df$Attrition, SplitRatio = 0.7)  # splitting indices

train = final_df[indices,]  # train data consisting of 70% of original obs.

test = final_df[!(indices),] # test data consisting of 30% of original obs.


#-------------------------------------------------------------------------------
# Logistic Regression
#-------------------------------------------------------------------------------

#Model 1
#------------------------------------
model_1 <- glm(Attrition ~ ., data = train, family = "binomial")

#check summary of the model
summary(model_1)  #AIC# 2140.7  Coefficients#57   Null deviance#2728  Residual deviance# 2026.7


#Model 2
#------------------------------------
# Stepwise selection - To remove unnecessary independent variables
#library("MASS")
model_2 <- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
#library(car)
sort(vif(model_2),decreasing = TRUE)

#Excluding YearsAtCompany due to high VIF and low significance

#Model 3
#------------------------------------

model_3 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.xBelowCollege + Education.xCollege + EducationField.xOther + 
                JobLevel.xL2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + StockOptionLevel.xL1 + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                WorkLifeBalance.xGood + JobInvolvement.xLow, family = "binomial", 
              data = train)

summary(model_3) 

sort(vif(model_3),decreasing = TRUE)


#Excluding EducationField.xOther due to low significance

#Model 4
#------------------------------------

model_4 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.xBelowCollege + Education.xCollege +  
                 JobLevel.xL2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.xL1 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                 JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood + JobInvolvement.xLow, family = "binomial", 
               data = train)

summary(model_4) 

sort(vif(model_4),decreasing = TRUE)



##Excluding JobInvolvement.xLow due to low significance

#Model 5
#------------------------------------

model_5 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.xBelowCollege + Education.xCollege +  
                 JobLevel.xL2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.xL1 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                 JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood, family = "binomial", 
               data = train)

summary(model_5) 

sort(vif(model_5),decreasing = TRUE)


#Excluding Education.xBelowCollege due to low significance

#Model 6
#------------------------------------

model_6 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.xCollege +  
                 JobLevel.xL2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.xL1 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                 JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood, family = "binomial", 
               data = train)

summary(model_6) 

sort(vif(model_6),decreasing = TRUE)



#Excluding DistanceFromHome due to low significance

#Model 7
#------------------------------------

model_7 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.xCollege +  
                 JobLevel.xL2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.xL1 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                 JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood, family = "binomial", 
               data = train)

summary(model_7) 

sort(vif(model_7),decreasing = TRUE)

#Excluding StockOptionLevel.xL1 due to low significance

#Model 8
#------------------------------------

model_8 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.xCollege +  
                 JobLevel.xL2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                 JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood, family = "binomial", 
               data = train)

summary(model_8) 

sort(vif(model_8),decreasing = TRUE)


#Excluding MonthlyIncome due to low significance

#Model 9
#------------------------------------

model_9 <- glm(formula = Attrition ~ Age +  
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.xCollege +  
                 JobLevel.xL2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                 JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood, family = "binomial", 
               data = train)

summary(model_9) 

sort(vif(model_9),decreasing = TRUE)



#Excluding Education.xCollege due to low significance

#Model 10
#------------------------------------

model_10 <- glm(formula = Attrition ~ Age +  
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  JobLevel.xL2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                  JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_10) 

sort(vif(model_10),decreasing = TRUE)


#Excluding JobLevel.xL2 due to low significance

#Model 11
#------------------------------------

model_11 <- glm(formula = Attrition ~ Age +  
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                  JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_11) 

sort(vif(model_11),decreasing = TRUE)


#Excluding JobRole.xLaboratory.Technician due to low significance

#Model 12
#------------------------------------

model_12 <- glm(formula = Attrition ~ Age +  
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                  JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_12) 

sort(vif(model_12),decreasing = TRUE)


#Excluding JobRole.xResearch.Scientist due to low significance

#Model 13
#------------------------------------

model_13 <- glm(formula = Attrition ~ Age +  
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVeryHigh + JobSatisfaction.xLow + 
                  JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_13) 

sort(vif(model_13),decreasing = TRUE)


#Excluding EnvironmentSatisfaction.xVeryHigh due to low significance

#Model 14
#------------------------------------

model_14 <- glm(formula = Attrition ~ Age +  
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_14) 

sort(vif(model_14),decreasing = TRUE)


#Excluding JobRole.xResearch.Director due to low significance

#Model 15
#------------------------------------

model_15 <- glm(formula = Attrition ~ Age +  
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_15) 

sort(vif(model_15),decreasing = TRUE)


#Excluding JobRole.xSales.Executive due to low significance

#Model 16
#------------------------------------

model_16 <- glm(formula = Attrition ~ Age +  
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVeryHigh + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_16) 

sort(vif(model_16),decreasing = TRUE)


#Excluding WorkLifeBalance.xBest due to low significance

#Model 17
#------------------------------------

model_17 <- glm(formula = Attrition ~ Age +  
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVeryHigh + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood, family = "binomial", 
                data = train)

summary(model_17) 

sort(vif(model_17),decreasing = TRUE)


#Excluding WorkLifeBalance.xGood due to low significance

#Model 18
#------------------------------------

model_18 <- glm(formula = Attrition ~ Age +  
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkHrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  JobSatisfaction.xLow + 
                  JobSatisfaction.xVeryHigh + WorkLifeBalance.xBetter , family = "binomial", 
                data = train)

summary(model_18) 

sort(vif(model_18),decreasing = TRUE)

# Most of the variables have low VIF
# Varibles with higher VIF are very significant and not correlated

########################################################################
# Final Model: with 16 significant variables in the model #
#######################################################################

final_model <- model_18


#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)                        -2.46118    0.34706  -7.091 1.33e-12 ***
#  Age                                -0.31969    0.07860  -4.067 4.76e-05 ***
#  NumCompaniesWorked                  0.39630    0.05771   6.867 6.58e-12 ***
#  TotalWorkingYears                  -0.53406    0.10412  -5.129 2.91e-07 ***
#  TrainingTimesLastYear              -0.20391    0.05674  -3.594 0.000326 ***
#  YearsSinceLastPromotion             0.48600    0.07543   6.443 1.17e-10 ***
#  YearsWithCurrManager               -0.44962    0.08587  -5.236 1.64e-07 ***
#  AvgWorkHrs                          0.52733    0.05340   9.875  < 2e-16 ***
#  BusinessTravel.xTravel_Frequently   1.84427    0.28120   6.559 5.43e-11 ***
#  BusinessTravel.xTravel_Rarely       1.12050    0.26538   4.222 2.42e-05 ***
#  Department.xResearch...Development -1.15252    0.22158  -5.201 1.98e-07 ***
#  Department.xSales                  -1.22189    0.23376  -5.227 1.72e-07 ***
#  MaritalStatus.xSingle               1.01602    0.11395   8.916  < 2e-16 ***
#  EnvironmentSatisfaction.xLow        1.06664    0.12869   8.288  < 2e-16 ***
#  JobSatisfaction.xLow                0.51588    0.13709   3.763 0.000168 ***
#  JobSatisfaction.xVeryHigh          -0.64108    0.13790  -4.649 3.34e-06 ***
#  WorkLifeBalance.xBetter            -0.37286    0.11268  -3.309 0.000936 ***
 


#######################################################################################
### Model Evaluation
#######################################################################################

#predicted probabilities of attrition '1' for test data
test_pred = predict(final_model, type = "response", 
                    newdata = test)


# summary of the predicted values
summary(test_pred) 

test$prob <- test_pred

#View(test)

#-------------------------------------------------------------------------------
# Probability cutoff at 50%
#-------------------------------------------------------------------------------
# Let's use the probability cutoff of 50%.
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition == 1,"Yes","No"))


table(test_actual_attrition, test_pred_attrition) # 86% accuracy , 25% sensitivity, 98% specificity

#-------------------------------------------------------------------------------
# Probability cutoff at 40%
#-------------------------------------------------------------------------------
# Let's use the probability cutoff of 40%.
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))


#library("caret")
#library("e1071")
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#Accuracy : 0.8481  
#Sensitivity : 0.32864        
#Specificity : 0.94775  

#-------------------------------------------------------------------------------
# Optimal Probability Cutoff Value
#-------------------------------------------------------------------------------

perform_fn <- function(cutoff)  {
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) #transpose the matrix
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Summary of test probability
summary(test_pred)

# Creating cutoff values
cutoff_values = seq(0.01,0.80,length=100)

#Initializing a matrix of 100 X 3.
OUT = matrix(0,100,3)

#run helper function to derive "sensitivity", "specificity", "accuracy" for 
for(i in 1:100)
{
  OUT[i,] = perform_fn(cutoff_values[i])
} 


cutoff <- cutoff_values[which(abs(OUT[,1]-OUT[,2]) < 0.016)]
cutoff


plot(cutoff_values, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoff_values,OUT[,2],col="darkgreen",lwd=2)
lines(cutoff_values,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
text(0.1, 0.3, labels=sprintf("cutoff value: %0.7f", cutoff))

# Let's choose a cutoff value of 0.1616162 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >= 0.1616162, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc  #72%
sens #71%
spec #72%

#View(test)

#-------------------------------------------------------------------------------
# Kolmogorov-Smirnov (KS) statistics - Test Data
#-------------------------------------------------------------------------------

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
# Creating performance object
pred_object_test <- prediction(test_cutoff_attrition, test_actual_attrition)

# Get data for ROC curve
performance_measures_test <- performance(pred_object_test, measure = "tpr", x.measure = "fpr")

#Area Under the Curve
auc <- performance(pred_object_test,"auc")
auc_value <- unlist(auc@y.values)
auc_value  #0.7176183

#Receiver Operating Characteristic (ROC) curve
plot(performance_measures_test, main="ROC Curve", colorize=T)

plot(performance_measures_test, 
               type= "l", 
               col = "red", 
               lwd = 2,
               main = "ROC Curve",
               ylab = "TPR", 
               xlab = "FPR")
abline(0, 1, col = "green", lty = 10, untf = T)
text(0.8, 0.2, labels=sprintf("AUC: %0.3f", auc_value))

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #0.4352366



#-------------------------------------------------------------------------------
# Lift & Gain Chart 
#-------------------------------------------------------------------------------

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels, predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
Attrition_decile

#Lift Chart
#-------------------------------
plot(Attrition_decile$bucket, Attrition_decile$Cumlift, 
               type="l", 
               xlim = c(0,10),
               ylim = c(0,4),
               main = "Lift Chart",
               ylab="Cumulative Lift", 
               xlab="Bucket(Decile)",
               lwd=2, col="red")
axis(1, 1:10)
abline(h = 1, col="green", lwd=2)
abline(h = 0:10, v = 0:10, lty = 3)


#Gain Chart
#-------------------------------
gain <- performance(pred_object_test, "tpr", "rpp")

plot(x=c(0, 1), 
     y=c(0, 1), 
     type="l", 
     col="red", 
     lwd=2,
     main = "Gain Chart",
     ylab="True Positive Rate", 
     xlab="Rate of Positive Predictions")
lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)
gain.x = unlist(slot(gain, 'x.values'))
gain.y = unlist(slot(gain, 'y.values'))
lines(x=gain.x, y=gain.y, col="orange", lwd=2)

