#############################Assignment - Risk Stratification###################
#-------------------------------------------------------------------------------
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building  -- Logistic Regression and Random Forest
#Model Evaluation -- Logistic Regression and Random Forest
#Model Comparison -- Logistic Regression and Random Forest
#Risk Stratification



#######################################################################################
### Business Understanding:
#######################################################################################

# The care management organisation has maintained a database containing  Medical history of diabetic patients.

## AIM:

# The aim is identify the diabetic patients, that are at high risk of getting re-admitted to the hospital and 
# To find the factors affecting the probability of patient getting re-admitted based on following data.

# 1. Medical history of diabetic patients



#######################################################################################
### Data Understanding
#######################################################################################

# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("ggplot2")
#install.packages("cowplot")
#install.packages("caTools")

#clear Environment
rm(list = ls()) 

#-------------------------------------------------------------------------------
#load packages
#-------------------------------------------------------------------------------

library("MASS")  # StepAIC function
library("car")   # VIF function
library("caret") # confusionMatrix 
library("ggplot2")
library("cowplot")
library("caTools")  # for sample split

#-------------------------------------------------------------------------------
# Load the data and check its contents
#-------------------------------------------------------------------------------
#load csv file
#setwd("D:/Backup/R/input")
diabetic_df <- read.csv("diabetic_data.csv", stringsAsFactors = F)

# data structure 
str(diabetic_df)

#summary
summary(diabetic_df)

#View top rows
head(diabetic_df)

#print column names
names(diabetic_df)

#Print columns having unique values for each row
names(diabetic_df[sapply(diabetic_df, function(x) length(unique(x))==nrow(diabetic_df))])  # encounter_id

# Unique values in the data frames
length(unique(tolower(diabetic_df$encounter_id)))    # 101766, confirming encounter_id is primary key 


#######################################################################################
### Data Preparation & Exploratory Data Analysis
#######################################################################################

#-------------------------------------------------------------------------------
#Remove Redundant/Unnecesaary Columns
#-------------------------------------------------------------------------------

#print no of unique values under each column
as.data.frame(rapply(diabetic_df,function(x)length(unique(x))))

#Print column names having same value for all rows
names(diabetic_df[sapply(diabetic_df, function(x) length(unique(x))==1)])  # "examide" & "citoglipton"


#Remove all columns having same value for all rows  -- "examide" & "citoglipton"
diabetic_df <- diabetic_df[sapply(diabetic_df, function(x) length(unique(x))>1)]  


#Print columns having unique values for each row
names(diabetic_df[sapply(diabetic_df, function(x) length(unique(x))==nrow(diabetic_df))])  # "encounter_id"


#Remove columns having unique value for all rows  -- "encounter_id"
diabetic_df <- diabetic_df[sapply(diabetic_df, function(x) length(unique(x))!=nrow(diabetic_df))]


#Remove unnecesaary columns that have no relationship to medical conditions, diagnoses, treatment
# patient_nbr --> Its having arbitrary numbers that have no relationship to the outcome
# admission_type_id --> Not relevant to the analysis
# max_glu_serum --> As mentioned by Rohit, this test result not important for our analysis
# medications --> Ignored all the medications except "insulin"
diabetic_df <- diabetic_df[!names(diabetic_df) %in% c("patient_nbr","admission_type_id", "max_glu_serum", "metformin", "repaglinide", 
                                                      "nateglinide", "chlorpropamide", "glimepiride", "acetohexamide","glipizide",
                                                      "glyburide","tolbutamide", "pioglitazone", "rosiglitazone","acarbose","miglitol", 
                                                      "troglitazone","tolazamide","glyburide.metformin", "glipizide.metformin",
                                                      "glimepiride.pioglitazone","metformin.rosiglitazone", "metformin.pioglitazone")]

str(diabetic_df)

#-------------------------------------------------------------------------------
#Check for duplicates
#-------------------------------------------------------------------------------
sum(duplicated(diabetic_df)) # no duplicate rows


#-------------------------------------------------------------------------------
#Missing values
#-------------------------------------------------------------------------------

# Checking for blank "" values
blank_col <- sapply(diabetic_df, function(x) length(which(x == "")))
blank_col
table(blank_col)  # 0 blanks


# Check for Invalid Values
invalid_col <- sapply(diabetic_df, function(x) sum(x == "?"))
invalid_col  

#race - 2273, weight - 98569, payer_code - 40256, medical_specialty - 49949, diag_1 - 21, diag_2- 358, diag_3 - 1423
table(invalid_col)  

#Remove columns having high percentage of missing values  -- "payer_code" & "weight" , "medical_specialty"
# payer_code  --> Its having high percentage of missing values and considered as irrelevant to the outcome
# weight      --> Its having high percentage of missing values. So ignored for further analysis
# medical_specialty --> Its having high percentage of missing values. So ignored for further analysis
diabetic_df <- diabetic_df[!names(diabetic_df) %in% c("payer_code", "weight", "medical_specialty")]


#Dropped observations having invalid values as percentage of missing values is very low compare to total no of observations
diabetic_df <- diabetic_df[diabetic_df$race != "?",] # 2273 observations dropped
diabetic_df <- diabetic_df[diabetic_df$diag_1 != "?",] # 21 observations dropped
diabetic_df <- diabetic_df[diabetic_df$diag_2 != "?",] # 358 observations dropped
diabetic_df <- diabetic_df[diabetic_df$diag_3 != "?",] # 1423 observations dropped


#Replace all invalid values with NA
#diabetic_df[diabetic_df ==  "?"] <- NA

#Check for Missing values
sum(is.na(diabetic_df))   # 0 NA values

#Get column names having NA
colnames(diabetic_df)[colSums(is.na(diabetic_df)) > 0]  
na_col <- sapply(diabetic_df, function(x) length(which(is.na(x))))
na_col  
table(na_col)

#-------------------------------------------------------------------------------
# Club values in "readmitted" column
#-------------------------------------------------------------------------------
#Change the variable 'readmitted' to binary type by clubbing the values ">30" and "<30" as "YES".
diabetic_df$readmitted[diabetic_df$readmitted == "<30" | diabetic_df$readmitted == ">30"]  <- "YES"

table(as.factor(diabetic_df$readmitted))

#-------------------------------------------------------------------------------
# Derive New Metric -- comorbidity
#-------------------------------------------------------------------------------

#function to check ICD 9 code in diag_1, diag_2 & diag_3 columns
# Condtions
# If one of the Diagnosis columns value equal to 250.xx and in between 390 to 459, then comorbidity = 3
# If one of the Diagnosis columns value not equal to 250.xx, but in between 390 to 459, then comorbidity = 2
# If one of the Diagnosis columns value equal to 250.xx, but not in between 390 to 459, then comorbidity = 1
# If one of the Diagnosis columns value not equal to 250.xx and not in between 390 to 459, then comorbidity = 0

comorbidity_func = function(row){
  result <- 4
  temp_1 <- as.numeric(substring(row[1],1,3))
  temp_2 <- as.numeric(substring(row[2],1,3))
  temp_3 <- as.numeric(substring(row[3],1,3))
  
  temp_1[is.na(temp_1)] <- 0
  temp_2[is.na(temp_2)] <- 0
  temp_3[is.na(temp_3)] <- 0
  
  if(temp_1 == 250 || temp_2 == 250 || temp_3 == 250) {
    if(temp_1 %in% 390:459 || temp_2 %in% 390:459 || temp_3 %in% 390:459) {
      result <- 3
    } else {
      result <- 1
    }
  } else if(temp_1 != 250 && temp_2 != 250 && temp_3 != 250) {
    if(temp_1 %in% 390:459 || temp_2 %in% 390:459 || temp_3 %in% 390:459) {
      result <- 2
    } else {
      result <- 0
    }
  }
  return(result)
}

diabetic_df$comorbidity <- apply(diabetic_df[, c("diag_1", "diag_2", "diag_3")], MARGIN = 1, FUN=comorbidity_func)

str(diabetic_df)

table(diabetic_df$comorbidity)

#Convert to Factor
diabetic_df <- cbind(diabetic_df[c(6:12,16)], lapply(diabetic_df[c(1:5,13:15,17:22)],factor))

#Drop large factors "diag_1, diag_2, diag_3" as metric "comorbidity" already derived out out of these columns.
diabetic_df <- diabetic_df[!names(diabetic_df) %in% c("diag_1", "diag_2", "diag_3")]

#Convert data type to Numeric 
diabetic_df$time_in_hospital <- as.numeric(diabetic_df$time_in_hospital);
diabetic_df$num_lab_procedures <- as.numeric(diabetic_df$num_lab_procedures);
diabetic_df$num_procedures <- as.numeric(diabetic_df$num_procedures);
diabetic_df$num_medications <- as.numeric(diabetic_df$num_medications);
diabetic_df$number_outpatient <- as.numeric(diabetic_df$number_outpatient);
diabetic_df$number_emergency <- as.numeric(diabetic_df$number_emergency);
diabetic_df$number_inpatient <- as.numeric(diabetic_df$number_inpatient);
diabetic_df$number_diagnoses <- as.numeric(diabetic_df$number_diagnoses);

str(diabetic_df)

#-------------------------------------------------------------------------------
# Plots to Explore Data
#-------------------------------------------------------------------------------
# Barcharts for categorical features with stacked Attrition information
bar_theme <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="right",)

plot_grid(ggplot(diabetic_df, aes(x=race,fill=readmitted))+ geom_bar(position=position_dodge()) + bar_theme,
          ggplot(diabetic_df, aes(x=gender,fill=readmitted))+ geom_bar(position=position_dodge())+ bar_theme,
          ggplot(diabetic_df, aes(x=age,fill=readmitted))+ geom_bar(position=position_dodge())+ bar_theme,
          ggplot(diabetic_df, aes(x=A1Cresult,fill=readmitted))+ geom_bar(position=position_dodge())+ bar_theme,
          ggplot(diabetic_df, aes(x=insulin,fill=readmitted))+ geom_bar(position=position_dodge()) + bar_theme,
          ggplot(diabetic_df, aes(x=change,fill=readmitted))+ geom_bar(position=position_dodge()) + bar_theme,
          ggplot(diabetic_df, aes(x=diabetesMed,fill=readmitted))+ geom_bar(position=position_dodge()) + bar_theme,
          ggplot(diabetic_df, aes(x=comorbidity,fill=readmitted))+ geom_bar(position=position_dodge()) + bar_theme,
          align = "h") 



# Histogram and Boxplots for numeric variables 
box_theme <- theme(axis.line=element_blank(),axis.title=element_blank(), 
                   axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y <- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                     axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                     legend.position="none")

#time_in_hospital
plot_grid(ggplot(diabetic_df, aes(time_in_hospital))+ geom_histogram(),
          ggplot(diabetic_df, aes(x="",y=time_in_hospital))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#num_lab_procedures
plot_grid(ggplot(diabetic_df, aes(num_lab_procedures))+ geom_histogram(),
          ggplot(diabetic_df, aes(x="",y=num_lab_procedures))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#num_procedures
plot_grid(ggplot(diabetic_df, aes(num_procedures))+ geom_histogram(),
          ggplot(diabetic_df, aes(x="",y=num_procedures))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#num_medications
plot_grid(ggplot(diabetic_df, aes(num_medications))+ geom_histogram(),
          ggplot(diabetic_df, aes(x="",y=num_medications))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#number_outpatient
plot_grid(ggplot(diabetic_df, aes(number_outpatient))+ geom_histogram(),
          ggplot(diabetic_df, aes(x="",y=number_outpatient))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#number_emergency
plot_grid(ggplot(diabetic_df, aes(number_emergency))+ geom_histogram(),
          ggplot(diabetic_df, aes(x="",y=number_emergency))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#number_inpatient
plot_grid(ggplot(diabetic_df, aes(number_inpatient))+ geom_histogram(),
          ggplot(diabetic_df, aes(x="",y=number_inpatient))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#number_diagnoses
plot_grid(ggplot(diabetic_df, aes(number_diagnoses))+ geom_histogram(),
          ggplot(diabetic_df, aes(x="",y=number_diagnoses))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#Histogram
plot_grid(ggplot(diabetic_df, aes(x=readmitted,y = time_in_hospital, fill=readmitted))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(diabetic_df, aes(x=readmitted,y = num_lab_procedures, fill=readmitted))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,          
          ggplot(diabetic_df, aes(x=readmitted,y = num_procedures, fill=readmitted))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(diabetic_df, aes(x=readmitted,y = num_medications, fill=readmitted))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(diabetic_df, aes(x=readmitted,y = number_outpatient, fill=readmitted))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(diabetic_df, aes(x=readmitted,y = number_emergency, fill=readmitted))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(diabetic_df, aes(x=readmitted,y = number_inpatient, fill=readmitted))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(diabetic_df, aes(x=readmitted,y = number_diagnoses, fill=readmitted))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,             
          align = "v",nrow = 1)



#-------------------------------------------------------------------------------
# Feature Standardisation
#-------------------------------------------------------------------------------

# Normalising continuous variables

#mean(diabetic_df$number_diagnoses)
#sd(diabetic_df$number_diagnoses)

diabetic_df$time_in_hospital <- scale(diabetic_df$time_in_hospital) # scale used: mean 4.421976, sd 2.993074
diabetic_df$num_lab_procedures <- scale(diabetic_df$num_lab_procedures) # scale used: mean 43.14807, sd 19.71203
diabetic_df$num_procedures <- scale(diabetic_df$num_procedures) # scale used: mean 1.350749, sd 1.708506
diabetic_df$num_medications <- scale(diabetic_df$num_medications) # scale used: mean 16.11965, sd 8.108476
diabetic_df$number_outpatient <- scale(diabetic_df$number_outpatient) # scale used: mean 0.3763781, sd 1.283359
diabetic_df$number_emergency <- scale(diabetic_df$number_emergency) # scale used: mean 0.2024619, sd 0.9428923
diabetic_df$number_inpatient <- scale(diabetic_df$number_inpatient) # scale used: mean 0.6468645, sd 1.27102
diabetic_df$number_diagnoses <- scale(diabetic_df$number_diagnoses) # scale used: mean 7.51206, sd 1.832497

# converting target variable "readmitted" from No/Yes character to factor with levels 0/1 
diabetic_df$readmitted <- ifelse(diabetic_df$readmitted=="YES",1,0)


# Checking Attrition rate of Employee
ReAdmitted <- sum(diabetic_df$readmitted)/nrow(diabetic_df)
ReAdmitted # 46% readmission rate

#backup master data frame
diabetic_df_bkp2 <- diabetic_df



#-------------------------------------------------------------------------------
# Dummy Variables - Create dummy variables to convert the categorical variables to numerical
#-------------------------------------------------------------------------------

#variables having 2 levels

#diabetesMed
#----------
#Check the summary of "diabetesMed" variable
summary(factor(diabetic_df$diabetesMed))

# converting "diabetesMed" variable from Yes/No to factor with levels 1/0 
diabetic_df$diabetesMed<- ifelse(diabetic_df$diabetesMed=="Yes",1,0)


#change
#----------
#Check the summary of "change" variable
summary(factor(diabetic_df$change))

# converting "change" variable from Ch/No to factor with levels 1/0 
diabetic_df$change<- ifelse(diabetic_df$change=="Ch",1,0)



#Converting other factor variable to dummy
#-------------------------------------
indices <- which(colnames(diabetic_df) %in% 
                   c('race','gender', 'age', 'discharge_disposition_id','admission_source_id', 
                     'A1Cresult', 'insulin', 'comorbidity'))

diabetic_chr <- diabetic_df[, indices]

diabetic_chr<- data.frame(sapply(diabetic_chr, function(x) if (is.factor(x)) x else factor(x) ))
str(diabetic_chr)

# creating dummy variables for factor attributes
dummies <- data.frame(sapply(diabetic_chr, 
                             function(x) data.frame(model.matrix(~x-1,data = diabetic_chr))[,-1]))


# Final dataset
final_df <- cbind(diabetic_df[,-c(indices)],dummies)

str(final_df) # 98053 obs. of  76 variables


#-------------------------------------------------------------------------------
# Splitting the data between train and test
#-------------------------------------------------------------------------------
set.seed(100)

#library("caTools")
indices = sample.split(final_df$readmitted, SplitRatio = 0.7)  # splitting indices

train = final_df[indices,]  # train data consisting of 70% of original obs.

test = final_df[!(indices),] # test data consisting of 30% of original obs.



#######################################################################################
### Model Buliding - Logistic Regression
#######################################################################################

#Model 1
#------------------------------------
model_1 <- glm(readmitted ~ ., data = train, family = "binomial")

#check summary of the model
summary(model_1)  #AIC# 87179  Coefficients#76   Null deviance#94838  Residual deviance#87027



#Model 2
#------------------------------------
# Stepwise selection - To remove unnecessary independent variables
#library("MASS")
model_2 <- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
#library(car)
sort(vif(model_2),decreasing = TRUE)

#Excluding "discharge_disposition_id.x4" due to low significance


#Model 3
#------------------------------------

model_3 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + change + diabetesMed + race.xAsian + race.xCaucasian + 
                 race.xHispanic + race.xOther + gender.xMale + age.x.40.50. + 
                 age.x.50.60. + age.x.60.70. + age.x.70.80. + age.x.80.90. + 
                 discharge_disposition_id.x11 + discharge_disposition_id.x13 + 
                 discharge_disposition_id.x14 + discharge_disposition_id.x15 + 
                 discharge_disposition_id.x18 + discharge_disposition_id.x19 + 
                 discharge_disposition_id.x22 + discharge_disposition_id.x23 + 
                 discharge_disposition_id.x25 + discharge_disposition_id.x28 + 
                 discharge_disposition_id.x3 + 
                 discharge_disposition_id.x5 + discharge_disposition_id.x6 + 
                 discharge_disposition_id.x7 + admission_source_id.x17 + admission_source_id.x2 + 
                 admission_source_id.x20 + admission_source_id.x3 + admission_source_id.x4 + 
                 admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
                 admission_source_id.x9 + A1Cresult.x.8 + A1Cresult.xNone + 
                 insulin.xNo + insulin.xSteady + insulin.xUp + comorbidity.x1 + 
                 comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)

summary(model_3) 

sort(vif(model_3),decreasing = TRUE)

#Excluding "discharge_disposition_id.x7" due to low significance

#Model 4
#------------------------------------

model_4 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + change + diabetesMed + race.xAsian + race.xCaucasian + 
                 race.xHispanic + race.xOther + gender.xMale + age.x.40.50. + 
                 age.x.50.60. + age.x.60.70. + age.x.70.80. + age.x.80.90. + 
                 discharge_disposition_id.x11 + discharge_disposition_id.x13 + 
                 discharge_disposition_id.x14 + discharge_disposition_id.x15 + 
                 discharge_disposition_id.x18 + discharge_disposition_id.x19 + 
                 discharge_disposition_id.x22 + discharge_disposition_id.x23 + 
                 discharge_disposition_id.x25 + discharge_disposition_id.x28 + 
                 discharge_disposition_id.x3 + 
                 discharge_disposition_id.x5 + discharge_disposition_id.x6 + 
                 admission_source_id.x17 + admission_source_id.x2 + 
                 admission_source_id.x20 + admission_source_id.x3 + admission_source_id.x4 + 
                 admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
                 admission_source_id.x9 + A1Cresult.x.8 + A1Cresult.xNone + 
                 insulin.xNo + insulin.xSteady + insulin.xUp + comorbidity.x1 + 
                 comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)

summary(model_4) 

sort(vif(model_4),decreasing = TRUE)


#Excluding "discharge_disposition_id.x25" due to low significance

#Model 5
#------------------------------------

model_5 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + change + diabetesMed + race.xAsian + race.xCaucasian + 
                 race.xHispanic + race.xOther + gender.xMale + age.x.40.50. + 
                 age.x.50.60. + age.x.60.70. + age.x.70.80. + age.x.80.90. + 
                 discharge_disposition_id.x11 + discharge_disposition_id.x13 + 
                 discharge_disposition_id.x14 + discharge_disposition_id.x15 + 
                 discharge_disposition_id.x18 + discharge_disposition_id.x19 + 
                 discharge_disposition_id.x22 + discharge_disposition_id.x23 + 
                 discharge_disposition_id.x28 + 
                 discharge_disposition_id.x3 + 
                 discharge_disposition_id.x5 + discharge_disposition_id.x6 + 
                 admission_source_id.x17 + admission_source_id.x2 + 
                 admission_source_id.x20 + admission_source_id.x3 + admission_source_id.x4 + 
                 admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
                 admission_source_id.x9 + A1Cresult.x.8 + A1Cresult.xNone + 
                 insulin.xNo + insulin.xSteady + insulin.xUp + comorbidity.x1 + 
                 comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)

summary(model_5) 

sort(vif(model_5),decreasing = TRUE)


#Excluding "admission_source_id.x3", "admission_source_id.x9", "discharge_disposition_id.x19", "race.xCaucasian" due to low significance

#Model 6
#------------------------------------

model_6 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + change + diabetesMed + race.xAsian + 
                 race.xHispanic + race.xOther + gender.xMale + age.x.40.50. + 
                 age.x.50.60. + age.x.60.70. + age.x.70.80. + age.x.80.90. + 
                 discharge_disposition_id.x11 + discharge_disposition_id.x13 + 
                 discharge_disposition_id.x14 + discharge_disposition_id.x15 + 
                 discharge_disposition_id.x18 + 
                 discharge_disposition_id.x22 + discharge_disposition_id.x23 + 
                 discharge_disposition_id.x28 + 
                 discharge_disposition_id.x3 + 
                 discharge_disposition_id.x5 + discharge_disposition_id.x6 + 
                 admission_source_id.x17 + admission_source_id.x2 + 
                 admission_source_id.x20 + admission_source_id.x4 + 
                 admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
                 A1Cresult.x.8 + A1Cresult.xNone + 
                 insulin.xNo + insulin.xSteady + insulin.xUp + comorbidity.x1 + 
                 comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)

summary(model_6) 

sort(vif(model_6),decreasing = TRUE)

#Excluding "insulin.xUp" due to high VIF and low significance

#Model 7
#------------------------------------

model_7 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + change + diabetesMed + race.xAsian + 
                 race.xHispanic + race.xOther + gender.xMale + age.x.40.50. + 
                 age.x.50.60. + age.x.60.70. + age.x.70.80. + age.x.80.90. + 
                 discharge_disposition_id.x11 + discharge_disposition_id.x13 + 
                 discharge_disposition_id.x14 + discharge_disposition_id.x15 + 
                 discharge_disposition_id.x18 + 
                 discharge_disposition_id.x22 + discharge_disposition_id.x23 + 
                 discharge_disposition_id.x28 + 
                 discharge_disposition_id.x3 + 
                 discharge_disposition_id.x5 + discharge_disposition_id.x6 + 
                 admission_source_id.x17 + admission_source_id.x2 + 
                 admission_source_id.x20 + admission_source_id.x4 + 
                 admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
                 A1Cresult.x.8 + A1Cresult.xNone + 
                 insulin.xNo + insulin.xSteady + comorbidity.x1 + 
                 comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)

summary(model_7) 

sort(vif(model_7),decreasing = TRUE)



#Excluding "change", " age.x.40.50.", "discharge_disposition_id.x15", "discharge_disposition_id.x18", "discharge_disposition_id.x28"
# "discharge_disposition_id.x3" , "discharge_disposition_id.x5", "admission_source_id.x17", "admission_source_id.x2"
# due to low significance

#Model 8
#------------------------------------

model_8 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + diabetesMed + race.xAsian + 
                 race.xHispanic + race.xOther + gender.xMale +
                 age.x.50.60. + age.x.60.70. + age.x.70.80. + age.x.80.90. + 
                 discharge_disposition_id.x11 + discharge_disposition_id.x13 + 
                 discharge_disposition_id.x14 + 
                 discharge_disposition_id.x22 + discharge_disposition_id.x23 + 
                 discharge_disposition_id.x6 + 
                 admission_source_id.x20 + admission_source_id.x4 + 
                 admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
                 A1Cresult.x.8 + A1Cresult.xNone + 
                 insulin.xNo + insulin.xSteady + comorbidity.x1 + 
                 comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)

summary(model_8) 

sort(vif(model_8),decreasing = TRUE)

#Excluding "age.x.50.60.", "discharge_disposition_id.x11", "discharge_disposition_id.x22", "insulin.xNo"
# due to low significance



#Model 9
#------------------------------------

model_9 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + diabetesMed + race.xAsian + 
                 race.xHispanic + race.xOther + gender.xMale +
                 age.x.60.70. + age.x.70.80. + age.x.80.90. + 
                 discharge_disposition_id.x13 + 
                 discharge_disposition_id.x14 + 
                 discharge_disposition_id.x23 + 
                 discharge_disposition_id.x6 + 
                 admission_source_id.x20 + admission_source_id.x4 + 
                 admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
                 A1Cresult.x.8 + A1Cresult.xNone + 
                 insulin.xSteady + comorbidity.x1 + 
                 comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)

summary(model_9) 

sort(vif(model_9),decreasing = TRUE)

#Excluding "discharge_disposition_id.x23", "gender.xMale"
# due to low significance

#Model 10
#------------------------------------

model_10 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                  num_procedures + number_outpatient + number_emergency + number_inpatient + 
                  number_diagnoses + diabetesMed + race.xAsian + 
                  race.xHispanic + race.xOther + 
                  age.x.60.70. + age.x.70.80. + age.x.80.90. + 
                  discharge_disposition_id.x13 + 
                  discharge_disposition_id.x14 + 
                  discharge_disposition_id.x6 + 
                  admission_source_id.x20 + admission_source_id.x4 + 
                  admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
                  A1Cresult.x.8 + A1Cresult.xNone + 
                  insulin.xSteady + comorbidity.x1 + 
                  comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)

summary(model_10) 

sort(vif(model_10),decreasing = TRUE)

sort(vif(model_9),decreasing = TRUE)

#Excluding "race.xHispanic"
# due to low significance

#Model 11
#------------------------------------

model_11 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                  num_procedures + number_outpatient + number_emergency + number_inpatient + 
                  number_diagnoses + diabetesMed + race.xAsian + 
                  race.xOther + 
                  age.x.60.70. + age.x.70.80. + age.x.80.90. + 
                  discharge_disposition_id.x13 + 
                  discharge_disposition_id.x14 + 
                  discharge_disposition_id.x6 + 
                  admission_source_id.x20 + admission_source_id.x4 + 
                  admission_source_id.x5 + admission_source_id.x6 + admission_source_id.x7 + 
                  A1Cresult.x.8 + A1Cresult.xNone + 
                  insulin.xSteady + comorbidity.x1 + 
                  comorbidity.x2 + comorbidity.x3, family = "binomial", data = train)

summary(model_11) 

sort(vif(model_11),decreasing = TRUE)

# All the of the variables have low VIF and high significance

########################################################################
# Final Model: with 27 significant variables in the model #
#######################################################################

final_model_lr <- model_11

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                  -0.753959   0.038345 -19.662  < 2e-16 ***
#  time_in_hospital              0.055934   0.008916   6.274 3.53e-10 ***
#  num_lab_procedures            0.032191   0.009092   3.541 0.000399 ***
#  num_procedures               -0.067269   0.008535  -7.881 3.24e-15 ***
#  number_outpatient             0.097843   0.009429  10.377  < 2e-16 ***
#  number_emergency              0.192745   0.013955  13.812  < 2e-16 ***
#  number_inpatient              0.468044   0.010721  43.658  < 2e-16 ***
#  number_diagnoses              0.152676   0.009162  16.665  < 2e-16 ***
#  diabetesMed                   0.299794   0.020697  14.485  < 2e-16 ***
#  race.xAsian                  -0.412810   0.102778  -4.017 5.91e-05 ***
#  race.xOther                  -0.321624   0.066434  -4.841 1.29e-06 ***
#  age.x.60.70.                  0.136844   0.022094   6.194 5.87e-10 ***
#  age.x.70.80.                  0.184786   0.021403   8.634  < 2e-16 ***
#  age.x.80.90.                  0.170181   0.024490   6.949 3.68e-12 ***
#  discharge_disposition_id.x13 -1.870561   0.178468 -10.481  < 2e-16 ***
#  discharge_disposition_id.x14 -2.827811   0.250631 -11.283  < 2e-16 ***
#  discharge_disposition_id.x6   0.192118   0.024290   7.910 2.58e-15 ***
#  admission_source_id.x20       0.868106   0.194564   4.462 8.13e-06 ***
#  admission_source_id.x4       -0.510685   0.050149 -10.183  < 2e-16 ***
#  admission_source_id.x5       -0.372524   0.088712  -4.199 2.68e-05 ***
#  admission_source_id.x6       -0.510673   0.065218  -7.830 4.87e-15 ***
#  admission_source_id.x7        0.102919   0.017984   5.723 1.05e-08 ***
#  A1Cresult.x.8                 0.149607   0.039502   3.787 0.000152 ***
#  A1Cresult.xNone               0.154784   0.029033   5.331 9.75e-08 ***
#  insulin.xSteady              -0.123644   0.018689  -6.616 3.70e-11 ***
#  comorbidity.x1                0.193093   0.026281   7.347 2.02e-13 ***
#  comorbidity.x2                0.179507   0.021160   8.483  < 2e-16 ***
#  comorbidity.x3                0.287835   0.026608  10.817  < 2e-16 ***



#######################################################################################
### Model Evaluation  - Logistic Regression
#######################################################################################

#predicted probabilities of attrition '1' for test data
test_pred = predict(final_model_lr, type = "response", 
                    newdata = test)


# summary of the predicted values
summary(test_pred) 

test$prob_lr <- test_pred

#View(test)



#-------------------------------------------------------------------------------
# Probability cutoff at 50%
#-------------------------------------------------------------------------------
# Let's use the probability cutoff of 50%.
test_pred_readmitted <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_readmitted <- factor(ifelse(test$readmitted == 1,"Yes","No"))

table(test_actual_readmitted, test_pred_readmitted)

#library("caret") # confusionMatrix 
test_conf_lr_1 <- confusionMatrix(test_pred_readmitted, test_actual_readmitted, positive = "Yes")
test_conf_lr_1

#Accuracy : 61%  
#Sensitivity : 44%        
#Specificity : 76%  


#-------------------------------------------------------------------------------
# Probability cutoff at 40%
#-------------------------------------------------------------------------------
# Let's use the probability cutoff of 40%.
test_pred_readmitted <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf_lr_2 <- confusionMatrix(test_pred_readmitted, test_actual_readmitted, positive = "Yes")
test_conf_lr_2

#Accuracy : 58%  
#Sensitivity : 77%        
#Specificity : 43%  


#-------------------------------------------------------------------------------
# Optimal Probability Cutoff Value
#-------------------------------------------------------------------------------

perform_fn <- function(cutoff)  {
  predicted_readmitted <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_readmitted, test_actual_readmitted, positive = "Yes")
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
cutoff  #0.4488889


# Let's choose a cutoff value of 0.4488889 for final model

test_cutoff_readmitted <- factor(ifelse(test_pred >= 0.4488889, "Yes", "No"))

conf_final_lr <- confusionMatrix(test_cutoff_readmitted, test_actual_readmitted, positive = "Yes")

accuracy_lr <- conf_final_lr$overall[1]

accuracy_lr  #61%


#######################################################################################
### Model Buliding - Radom Forest
#######################################################################################

library(randomForest)

set.seed(71)

#Making our target class to factor
train$readmitted <- factor(train$readmitted)
test$readmitted <- factor(test$readmitted)


#3-fold cross validation to find optimal mtry value
trControl <- trainControl(method = "cv",
                          number = 3,
                          search = "grid",
                          verboseIter = TRUE)

tuneGrid <- expand.grid(.mtry = c(1: 10))

rf_mtry <- train(readmitted ~ .,
                 data = train,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 100,
                 verbose = TRUE)

#Optimal mtry value = 7

model_rf <- randomForest(readmitted ~ ., data=train, proximity=FALSE,
                         ntree=1000, mtry=7, do.trace=TRUE, na.action=na.omit)

model_rf

#######################################################################################
### Model Evaluation  - Radom Forest
#######################################################################################

testPred <- predict(model_rf, newdata=test)

table(testPred, test$readmitted)

conf_final_rf <- confusionMatrix(testPred, test$readmitted)

accuracy_rf <- conf_final_rf$overall[1]

accuracy_rf  #63%


#######################################################################################
### Model Comparison
#######################################################################################


#Based on Accuracy levels, Random Forest Model(63%) performed better than Logistic Regression Model(61%).


#######################################################################################
### Risk Stratification
#######################################################################################

#High risk (Probability of readmission >0.7)
#Medium risk (0.3 < Probability of readmission < 0.7)
#Low risk (Probability of readmission < 0.3)

readmitted_prob <- predict(model_rf, newdata=final_df, type="prob")

final_df$readmitted_prob <- readmitted_prob[,2]

final_df$risk_bucket <- ifelse(round(final_df$readmitted_prob,2) > 0.70, "High Risk",
                               ifelse(round(final_df$readmitted_prob,2) >= 0.30 & round(final_df$readmitted_prob,2) <= 0.70, "Medium Risk",
                                   ifelse(round(final_df$readmitted_prob,2) < 0.30, "Low Risk", "")))

table(as.factor(final_df$risk_bucket))


#High risk (Probability of readmission >0.7)
HighRiskRatio <- sum(final_df$risk_bucket == "High Risk")/nrow(final_df)
HighRiskRatio # 19% population has high risk of readmission


#Medium risk (0.3 < Probability of readmission < 0.7)
MediumRiskRatio <- sum(final_df$risk_bucket == "Medium Risk")/nrow(final_df)
MediumRiskRatio # 47% population has high risk of readmission


#Low risk (Probability of readmission < 0.3)
LowRiskRatio <- sum(final_df$risk_bucket == "Low Risk")/nrow(final_df)
LowRiskRatio # 34% population has high risk of readmission


