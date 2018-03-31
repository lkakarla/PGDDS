#Linear Regression - Assignment

#load packages
library("dplyr")
library("ggplot2")
library("stringr")
library("scales")
#library("lubridate")
library("tidyr")
library("car")
library("MASS")


#Load CSV file into data frame
carprice_df <- read.csv("CarPrice_Assignment.csv")

#View(carprice_df)

str(carprice_df)

#summary of dataset
summary(carprice_df)

#check few observations
head(carprice_df)

#print column names
names(carprice_df)

#print no of unique values under each column
as.data.frame(rapply(carprice_df,function(x)length(unique(x))))

#-------------------------------------------------------------------------------
#Check for duplicates
#-------------------------------------------------------------------------------

sum(duplicated(carprice_df)) # no duplicate rows


#-------------------------------------------------------------------------------
#Missing values
#-------------------------------------------------------------------------------

sum(is.na(carprice_df)) # no NA values

#length(which(is.na(loan_df)))
#Get column names having NA
colnames(carprice_df)[colSums(is.na(carprice_df)) > 0]

# Checking for NA values
na_col <- sapply(carprice_df, function(x) length(which(is.na(x))))
na_col
table(na_col)

# Checking for blank "" values
blank_col <- sapply(carprice_df, function(x) length(which(x == "")))
blank_col
table(blank_col)

#-------------------------------------------------------------------------------
#Outliers Treatment
#-------------------------------------------------------------------------------

#wheelbase
#--------------
#checking outliers for wheelbase
quantile(carprice_df$wheelbase,seq(0,1,0.01))

table(carprice_df$wheelbase)

#boxplot.stats(carprice_df$wheelbase)$out  # outlier values.
#boxplot(carprice_df$enginesize, main="Wheel Base")

#There is a jump between 99% and 100%. So, capping all values above 115.544(99% value) to 115.544.
carprice_df$wheelbase[which(carprice_df$wheelbase > 115.544)]<-115.544


#carlength
#---------------
#checking outliers for carlength
quantile(carprice_df$carlength,seq(0,1,0.01))

table(carprice_df$carlength)

#boxplot(carprice_df$carlength, main="Car Length")
#boxplot.stats(carprice_df$carlength)$out  # outlier values.

#There is a jump between 98% and 100%. So, capping all values above 199.568(98% value) to 199.568
carprice_df$carlength[which(carprice_df$carlength > 199.568)] <- 199.568

#Also there is a jump between each quantile from 0-3%. Here, we will floor all the values below 155.900(3% value)to 155.900
carprice_df$carlength[which(carprice_df$carlength < 155.900)] <- 155.900


#carwidth
#---------------
#checking outliers for carwidth
quantile(carprice_df$carwidth,seq(0,1,0.01))  ##There is no sudden jump in the quantile values indicating there are no outliers in wheelbase.

table(carprice_df$carwidth)

#boxplot(carprice_df$carwidth, main="Car Width")
#boxplot.stats(carprice_df$carwidth)$out  # outlier values.


#carheight
#---------------
#checking outliers for carheight
quantile(carprice_df$carheight,seq(0,1,0.01)) ###There is no sudden jump in the quantile values indicating there are no outliers in wheelbase.

table(carprice_df$carheight)

#boxplot(carprice_df$carheight, main="Car Height")
#boxplot.stats(carprice_df$carheight)$out  # outlier values.

#curbweight
#---------------
#checking outliers for curbweight
quantile(carprice_df$curbweight,seq(0,1,0.01))

table(carprice_df$curbweight)

#boxplot(carprice_df$curbweight, main="Curb Weight")
#boxplot.stats(carprice_df$curbweight)$out  # outlier values.

#There is a jump between 98% and 100%. So, capping all values above 3768.40(98% value) to 3768.40
carprice_df$curbweight[which(carprice_df$curbweight > 3768.40)] <- 3768.40

#There is a jump between each quantile from 0-1%. Here, we will floor all the values below 1819.72(1% value)to 1819.72
carprice_df$curbweight[which(carprice_df$curbweight < 1819.72)] <- 1819.72


#enginesize
#---------------
#checking outliers for enginesize
quantile(carprice_df$enginesize,seq(0,1,0.01))

table(carprice_df$enginesize)

#boxplot(carprice_df$enginesize, main="Engine Size")
#boxplot.stats(carprice_df$enginesize)$out  # outlier values.

#There is a jump between 95% and 100%. So, capping all values above 201.20(95% value) to 3768.40
carprice_df$enginesize[which(carprice_df$enginesize > 201.20)] <- 201.20


#boreratio
#---------------
#checking outliers for boreratio
quantile(carprice_df$boreratio,seq(0,1,0.01))

table(carprice_df$boreratio)

#boxplot(carprice_df$boreratio, main="Bore Ratio")
#boxplot.stats(carprice_df$boreratio)$out  # outlier values.

#There is a jump between each quantile from 0-1%. Here, we will floor all the values below 2.9100(1% value)to 2.9100
carprice_df$boreratio[which(carprice_df$boreratio < 2.9100)] <- 2.9100


#stroke
#---------------
#checking outliers for stroke
quantile(carprice_df$stroke,seq(0,1,0.01))

table(carprice_df$stroke)

#boxplot(carprice_df$stroke, main="Stroke")
#boxplot.stats(carprice_df$stroke)$out  # outlier values.

#There is a jump between each quantile from 99-100%. Here, we will cap all the values above 3.9000(99% value)to 3.9000
carprice_df$stroke[which(carprice_df$stroke > 3.9000)] <- 3.9000


#There is a jump between each quantile from 0-2%. Here, we will floor all the values below 2.6400(2% value)to 2.6400
carprice_df$stroke[which(carprice_df$stroke < 2.6400)] <- 2.6400


#compressionratio
#---------------
#checking outliers for compressionratio
quantile(carprice_df$compressionratio,seq(0,1,0.01))

table(carprice_df$compressionratio)

#boxplot(carprice_df$compressionratio, main="Compression Ratio")
#boxplot.stats(carprice_df$compressionratio)$out  # outlier values.

#There is a jump between 90% and 100%. So, capping all values above 10.9400(90% value) to 10.9400.
carprice_df$compressionratio[which(carprice_df$compressionratio > 10.9400)] <- 10.9400

#There is a jump between each quantile from 0-4%. Here, we will floor all the values below 7.5000(2% value)to 7.5000
carprice_df$compressionratio[which(carprice_df$compressionratio < 7.5000)] <- 7.5000


#horsepower
#---------------
#checking outliers for horsepower
quantile(carprice_df$horsepower,seq(0,1,0.01))

table(carprice_df$horsepower)

#boxplot(carprice_df$horsepower, main="Horse Power")
#boxplot.stats(carprice_df$horsepower)$out  # outlier values.

#There is a jump between 99% and 100%. So, capping all values above 207.00 (99% value) to 207.00 
carprice_df$horsepower[which(carprice_df$horsepower > 207.00 )] <- 207.00 


#peakrpm
#---------------
#checking outliers for peakrpm
quantile(carprice_df$peakrpm,seq(0,1,0.01))

table(carprice_df$peakrpm)

#boxplot(carprice_df$peakrpm, main="Peak RPM")
#boxplot.stats(carprice_df$peakrpm)$out  # outlier values.

#There is a jump between 99% and 100%. So, capping all values above 6000 (99% value) to 6000 
carprice_df$peakrpm[which(carprice_df$peakrpm > 6000)] <- 6000


#citympg
#---------------
#checking outliers for citympg
quantile(carprice_df$citympg,seq(0,1,0.01))

table(carprice_df$citympg)

#boxplot(carprice_df$citympg, main="City MPG")
#boxplot.stats(carprice_df$citympg)$out  # outlier values.

#There is a jump between 99% and 100%. So, capping all values above 44.72 (99% value) to 44.72 
carprice_df$peakrpm[which(carprice_df$citympg > 44.72)] <- 44.72


#highwaympg
#---------------
#checking outliers for highwaympg
quantile(carprice_df$highwaympg,seq(0,1,0.01))

table(carprice_df$highwaympg)

#boxplot(carprice_df$highwaympg, main="Highway MPG")
#boxplot.stats(carprice_df$highwaympg)$out  # outlier values.

#There is a jump between 99% and 100%. So, capping all values above 49.88 (99% value) to 49.88 
carprice_df$highwaympg[which(carprice_df$highwaympg > 49.88)] <- 49.88


#---------------------------------
# Remove unnecessary columns
#----------------------------------
#Print column names having same value for all rows
names(carprice_df[sapply(carprice_df, function(x) length(unique(x))==1)])

#Print columns having unique values for each row
names(carprice_df[sapply(carprice_df, function(x) length(unique(x))==nrow(carprice_df))])

# "car_ID" column  ==>  contains unique values. since we do analysis on collection, these values are of no use
remove_col <- c("car_ID")
carprice_df <- carprice_df[ , -which(names(carprice_df) %in% remove_col)]

#-------------------------------------------------------------------------------
# Data Cleaning
#-------------------------------------------------------------------------------

str(carprice_df)

# CarName
#------------------
#Convert CarName to lower case and replace '-'
carprice_df$CarName<- tolower(carprice_df$CarName)

#Consider only Car Company name and ignore Car Model for model building
#Create new column CarCompany
carprice_df <- separate(carprice_df, CarName, into = c("CarCompany"), sep= " ", extra = "drop")

#Check for invalid values in car company names
summary(as.factor(carprice_df$CarCompany))


#Correct spelling mistakes in Car Company names
carprice_df$CarCompany <- str_replace_all(carprice_df$CarCompany, "maxda",  "mazda")
carprice_df$CarCompany <- str_replace_all(carprice_df$CarCompany, "porcshce",  "porsche")
carprice_df$CarCompany <- str_replace_all(carprice_df$CarCompany, "toyouta",  "toyota")
carprice_df$CarCompany <- str_replace_all(carprice_df$CarCompany, "vokswagen","volkswagen")
carprice_df$CarCompany <- str_replace_all(carprice_df$CarCompany, "vw","volkswagen")

summary(as.factor(carprice_df$CarCompany))
#table(carprice_df$CarCompany)

#Convert CarCompany as factor 
carprice_df$CarCompany <- as.factor(carprice_df$CarCompany)


#-------------------------------------------------------------------------------
# Plots to Explore Data
#-------------------------------------------------------------------------------

#CarCompany Vs DriveWheel
ggplot(carprice_df,aes(x=CarCompany)) + 
  geom_bar(aes(fill = factor(drivewheel))) +
  xlab("CarCompany") + 
  ylab("Frequency") +
  ggtitle("CarCompany vs DriveWheel") +
  geom_text(stat='count',aes(label=..count..),hjust=0) +
  coord_flip()

#Toyota company deals with all three drivewheel types of vehicle and Audi, Toyota & Subaru only deals with 4wd type vehicles.


#CarCompany Vs EngineType
ggplot(carprice_df,aes(x=CarCompany,fill = enginetype)) +
  geom_bar() +
  xlab("EngineType") + 
  ylab("Frequency") +
  ggtitle("CarCompany vs EngineType") +
  coord_flip()

#"ohc" EngineType heavily used by almost all Car Companies


#CarCompany vs CarBody
ggplot(carprice_df,aes(x = CarCompany,fill = carbody)) +
  geom_bar() +
  coord_flip() +
  xlab("CarCompany") + 
  ylab("CarBody") +
  ggtitle("CarCompany vs CarBody")


#EngineType Vs Fuelsystem
ggplot(carprice_df,aes(x=enginetype,fill = fuelsystem)) + 
  geom_bar() +
  xlab("EngineType") + 
  ylab("Frequency") + 
  ggtitle("EngineType vs Fuelsystem") +
  coord_flip()

#"ohc" EngineType supports almost all kinds of fuelsystem types.


#-------------------------------------------------------------------------------
# Dummy Variables - Create dummy variables to convert the categorical variables to numerical
#-------------------------------------------------------------------------------

str(carprice_df)

#variables having 2 levels

#fueltype
#---------------------
#Check the summary of "fueltype" variable
summary(factor(carprice_df$fueltype))

#convert "fueltype" variable to numeric is to replace the levels - diesel and gas with 0 and 1
levels(carprice_df$fueltype) <- c(0,1)

#Now store the numeric values in the same variable
carprice_df$fueltype<- as.numeric(levels(carprice_df$fueltype))[carprice_df$fueltype]

#Check the summary of "fueltype" variable after conversion to numeric
summary(carprice_df$fueltype)


#aspiration
#---------------------
#Check the summary of "aspiration" variable
summary(factor(carprice_df$aspiration))

#convert "aspiration" variable to numeric is to replace the levels - std and turbo with 0 and 1
levels(carprice_df$aspiration) <- c(0,1)

#Now store the numeric values in the same variable
carprice_df$aspiration<- as.numeric(levels(carprice_df$aspiration))[carprice_df$aspiration]

#Check the summary of "aspiration" variable after conversion to numeric
summary(carprice_df$aspiration)


#doornumber
#---------------------
#Check the summary of "doornumber" variable
summary(factor(carprice_df$doornumber))

#Replacing the doornumber values to number
carprice_df$doornumber <- ifelse(carprice_df$doornumber == "four",4,2)

#convert column type to numeric
carprice_df$doornumber  <- as.numeric(carprice_df$doornumber)

summary(carprice_df$doornumber)

#enginelocation
#---------------------
#Check the summary of "enginelocation" variable
summary(factor(carprice_df$enginelocation))

#convert "enginelocation" variable to numeric is to replace the levels - front and rear with 0 and 1
levels(carprice_df$enginelocation) <- c(0,1)

#Now store the numeric values in the same variable
carprice_df$enginelocation<- as.numeric(levels(carprice_df$enginelocation))[carprice_df$enginelocation]

#Check the summary of "enginelocation" variable after conversion to numeric
summary(carprice_df$enginelocation)


#variables having more than 2 levels

#cylindernumber
#----------------------
summary(factor(carprice_df$cylindernumber))

# convert from character to number
carprice_df$cylindernumber <- ifelse(carprice_df$cylindernumber == "two",2,
                                     ifelse(carprice_df$cylindernumber == "three",3,
                                            ifelse(carprice_df$cylindernumber == "four",4,
                                                   ifelse(carprice_df$cylindernumber == "five",5,
                                                          ifelse(carprice_df$cylindernumber == "six",6,
                                                                 ifelse(carprice_df$cylindernumber == "eight",8,12
                                                                 ))))))

#convert column type to numeric
carprice_df$cylindernumber  <- as.numeric(carprice_df$cylindernumber)

summary(carprice_df$cylindernumber)

#CarCompany
#----------------------
summary(factor(carprice_df$CarCompany))

#Converting "CarCompany" into dummies . 
dummy_carcompany <- data.frame(model.matrix( ~CarCompany, data = carprice_df))

#check the dummy_carcompany data frame.
#View(dummy_carcompany)

#This column should be removed from the newly created dummy_carcompany dataframe containing the dummy values for the variable "CarCompany". 
dummy_carcompany <- dummy_carcompany[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "CarCompany" column
which(colnames(carprice_df)=="CarCompany")
carprice_df <- cbind(carprice_df[,-2], dummy_carcompany)
#View(carprice_df)



#carbody
#----------------------
summary(factor(carprice_df$carbody))

#Converting "carbody" into dummies . 
dummy_carbody <- data.frame(model.matrix( ~carbody, data = carprice_df))

#check the dummy_carbody data frame.
#View(dummy_carbody)

#This column should be removed from the newly created dummy_carbody dataframe containing the dummy values for the variable "carbody". 
dummy_carbody <- dummy_carbody[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
which(colnames(carprice_df)=="carbody")
carprice_df <- cbind(carprice_df[,-5], dummy_carbody)
#View(carprice_df)



#drivewheel
#----------------------
summary(factor(carprice_df$drivewheel))

#Converting "drivewheel" into dummies . 
dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = carprice_df))

#check the dummy_drivewheel data frame.
#View(dummy_drivewheel)

#This column should be removed from the newly created dummy_drivewheel dataframe containing the dummy values for the variable "drivewheel". 
dummy_drivewheel <- dummy_drivewheel[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "drivewheel" column
which(colnames(carprice_df)=="drivewheel")
carprice_df <- cbind(carprice_df[,-5], dummy_drivewheel)
#View(carprice_df)



#enginetype
#----------------------
summary(factor(carprice_df$enginetype))

#Converting "enginetype" into dummies . 
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = carprice_df))

#check the dummy_enginetype data frame.
#View(dummy_enginetype)

#This column should be removed from the newly created dummy_enginetype dataframe containing the dummy values for the variable "enginetype". 
dummy_enginetype <- dummy_enginetype[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "enginetype" column
which(colnames(carprice_df)=="enginetype")
carprice_df <- cbind(carprice_df[,-11], dummy_enginetype)
#View(carprice_df)



#fuelsystem
#----------------------
summary(factor(carprice_df$fuelsystem))

#Converting "fuelsystem" into dummies . 
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = carprice_df))

#check the dummy_fuelsystem data frame.
#View(dummy_fuelsystem)

#This column should be removed from the newly created dummy_fuelsystem dataframe containing the dummy values for the variable "fuelsystem". 
dummy_fuelsystem <- dummy_fuelsystem[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "fuelsystem" column
which(colnames(carprice_df)=="fuelsystem")
carprice_df <- cbind(carprice_df[,-13], dummy_fuelsystem)
#View(carprice_df)


#symboling
#----------------------
carprice_df$symboling <- as.factor(carprice_df$symboling)

summary(factor(carprice_df$symboling))

# "symboling" variable is of factor type with 6 levels from -2 to 2. 
# Club the levels of a factor variable together to reduce the number of levels.
# Club the levels of "symboling" into groups of 3; i.e after the transformation, the "symboling" variable will have 3 levels.
# Create three new levels by binning the levels of "symboling" into equal bins of 3. 
# Create the first level
levels(carprice_df$symboling)[1:2] <- "LowRisk"

#Create the second level
levels(carprice_df$symboling)[2:3] <- "MediumRisk"

#create the third level
levels(carprice_df$symboling)[3:4] <- "HighRisk"

summary(factor(carprice_df$symboling))

#Converting "symboling" into dummies . 
dummy_symboling <- data.frame(model.matrix( ~symboling, data = carprice_df))

#check the dummy_symboling data frame.
#View(dummy_symboling)

#This column should be removed from the newly created dummy_symboling dataframe containing the dummy values for the variable "symboling". 
dummy_symboling <- dummy_symboling[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "symboling" column
which(colnames(carprice_df)=="symboling")
carprice_df <- cbind(carprice_df[,-1], dummy_symboling)
#View(carprice_df)

##All variables converted to numeric/int data types to start the model buliding
str(carprice_df)


#Correaltion of numerical variables

car_corr <- cor(carprice_df)


#-------------------------------------------------------------------------------
# Modelling Building and Evaluation
#-------------------------------------------------------------------------------

#Create Train and Test data sets
set.seed(100)

trainindices <- sample(1:nrow(carprice_df), 0.7*nrow(carprice_df))

train <- carprice_df[trainindices,]
test <-  carprice_df[-trainindices,]



#Model 1
#------------------------------------
model_1 <- lm(price~., data=train)

#check summary of the model
summary(model_1)


#stepAIC to remove unnecessary independent variables
step<-stepAIC(model_1, direction = "both")
step



#Model 2
#------------------------------------
#Second Model based on output from stepAIC
model_2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + carheight + curbweight + cylindernumber + enginesize + 
                boreratio + stroke + horsepower + peakrpm + CarCompanybmw + 
                CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                CarCompanymercury + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                CarCompanysaab + CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelfwd + enginetyperotor + symbolingMediumRisk + symbolingHighRisk, 
              data = train)

summary(model_2)

# Multiple R-squared:  0.9755,	Adjusted R-squared:  0.9675 

#VIF for the model
#vif(model_2)
sort(vif(model_2),decreasing = TRUE)


#stroke is insignificant and has high vif. So it can be removed from the model



#Model 3
#------------------------------------
model_3 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + carheight + curbweight + cylindernumber + enginesize + 
                boreratio + horsepower + peakrpm + CarCompanybmw + 
                CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                CarCompanymercury + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                CarCompanysaab + CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelfwd + enginetyperotor + symbolingMediumRisk + symbolingHighRisk, 
              data = train)

summary(model_3)

# Multiple R-squared:  0.9747,	Adjusted R-squared:  0.9667 

#VIF for the model
sort(vif(model_3),decreasing = TRUE)

#symbolingHighRisk is insignificant and has high vif. So it can be removed from the model



#Model 4
#------------------------------------
model_4 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + carheight + curbweight + cylindernumber + enginesize + 
                boreratio + horsepower + peakrpm + CarCompanybmw + 
                CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                CarCompanymercury + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                CarCompanysaab + CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelfwd + enginetyperotor + symbolingMediumRisk, 
              data = train)

summary(model_4)

# Multiple R-squared:  0.9741,	Adjusted R-squared:  0.9662

#VIF for the model
sort(vif(model_4),decreasing = TRUE)

#carlength is insignificant and has high vif. So it can be removed from the model



#Model 5
#------------------------------------
model_5 <- lm(formula = price ~ aspiration + enginelocation +  
                carwidth + carheight + curbweight + cylindernumber + enginesize + 
                boreratio + horsepower + peakrpm + CarCompanybmw + 
                CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                CarCompanymercury + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                CarCompanysaab + CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelfwd + enginetyperotor + symbolingMediumRisk, 
              data = train)

summary(model_5)

# Multiple R-squared:  0.9733,	Adjusted R-squared:  0.9656 

#VIF for the model
sort(vif(model_5),decreasing = TRUE)

#drivewheelfwd is insignificant and has high vif. So it can be removed from the model



#Model 6
#------------------------------------
model_6 <- lm(formula = price ~ aspiration + enginelocation +  
                carwidth + carheight + curbweight + cylindernumber + enginesize + 
                boreratio + horsepower + peakrpm + CarCompanybmw + 
                CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                CarCompanymercury + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                CarCompanysaab + CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetyperotor + symbolingMediumRisk, 
              data = train)

summary(model_6)

# Multiple R-squared:  0.9727,	Adjusted R-squared:  0.9651

#VIF for the model
sort(vif(model_6),decreasing = TRUE)

#carheight is insignificant and has high vif. So it can be removed from the model



#Model 7
#------------------------------------
model_7 <- lm(formula = price ~ aspiration + enginelocation +  
                carwidth + curbweight + cylindernumber + enginesize + 
                boreratio + horsepower + peakrpm + CarCompanybmw + 
                CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                CarCompanymercury + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                CarCompanysaab + CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetyperotor + symbolingMediumRisk, 
              data = train)

summary(model_7)

#Multiple R-squared:  0.9721,	Adjusted R-squared:  0.9646

#VIF for the model
sort(vif(model_7),decreasing = TRUE)

#Check the correlation of the variables enginesize and curbweight.
cor(train$enginesize, train$curbweight)

# The correlation is ~ 85%, indicating that the variables are highly correlated. 
# Removing "enginesize" variable as it has lower significance level out of the two



#Model 8
#------------------------------------
model_8 <- lm(formula = price ~ aspiration + enginelocation +  
                carwidth + curbweight + cylindernumber +  
                boreratio + horsepower + peakrpm + CarCompanybmw + 
                CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                CarCompanymercury + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                CarCompanysaab + CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetyperotor + symbolingMediumRisk, 
              data = train)

summary(model_8)

#Multiple R-squared:  0.9676,	Adjusted R-squared:  0.9593

#VIF for the model
sort(vif(model_8),decreasing = TRUE)

#horsepower is insignificant and has high vif. So it can be removed from the model



#Model 9
#------------------------------------
model_9 <- lm(formula = price ~ aspiration + enginelocation +  
                carwidth + curbweight + cylindernumber +  
                boreratio + peakrpm + CarCompanybmw + 
                CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                CarCompanymercury + CarCompanymitsubishi + CarCompanynissan + 
                CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                CarCompanysaab + CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetyperotor + symbolingMediumRisk, 
              data = train)

summary(model_9)

#Multiple R-squared:  0.9669,	Adjusted R-squared:  0.9588 

#VIF for the model
sort(vif(model_9),decreasing = TRUE)

#peakrpm is insignificant. So it can be removed from the model



#Model 10
#------------------------------------
model_10 <- lm(formula = price ~ aspiration + enginelocation +  
                 carwidth + curbweight + cylindernumber +  
                 boreratio + CarCompanybmw + 
                 CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymercury + CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                 CarCompanysaab + CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetyperotor + symbolingMediumRisk, 
               data = train)

summary(model_10)

#Multiple R-squared:  0.9666,	Adjusted R-squared:  0.9588

#VIF for the model
sort(vif(model_10),decreasing = TRUE)

#CarCompanymercury is insignificant. So it can be removed from the model



#Model 11
#------------------------------------
model_11 <- lm(formula = price ~ aspiration + enginelocation +  
                 carwidth + curbweight + cylindernumber +  
                 boreratio + CarCompanybmw + 
                 CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                 CarCompanysaab + CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetyperotor + symbolingMediumRisk, 
               data = train)

summary(model_11)

#Multiple R-squared:  0.9663,	Adjusted R-squared:  0.9587 

#VIF for the model
sort(vif(model_11),decreasing = TRUE)

#symbolingMediumRisk is insignificant. So it can be removed from the model



#Model 12
#------------------------------------
model_12 <- lm(formula = price ~ aspiration + enginelocation +  
                 carwidth + curbweight + cylindernumber +  
                 boreratio + CarCompanybmw + 
                 CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                 CarCompanysaab + CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetyperotor, 
               data = train)

summary(model_12)

#Multiple R-squared:  0.9659,	Adjusted R-squared:  0.9586 

#VIF for the model
sort(vif(model_12),decreasing = TRUE)

#CarCompanysaab is insignificant. So it can be removed from the model



#Model 13
#------------------------------------
model_13 <- lm(formula = price ~ aspiration + enginelocation +  
                 carwidth + curbweight + cylindernumber +  
                 boreratio + CarCompanybmw + 
                 CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                 CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetyperotor, 
               data = train)

summary(model_13)

#Multiple R-squared:  0.9648,	Adjusted R-squared:  0.9577

#VIF for the model
sort(vif(model_13),decreasing = TRUE)

#carbodyhardtop is least significant. So it can be removed from the model



#Model 14
#------------------------------------
model_14 <- lm(formula = price ~ aspiration + enginelocation +  
                 carwidth + curbweight + cylindernumber +  
                 boreratio + CarCompanybmw + 
                 CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                 CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetyperotor, 
               data = train)

summary(model_14)

#Multiple R-squared:  0.9634,	Adjusted R-squared:  0.9563 

#VIF for the model
sort(vif(model_14),decreasing = TRUE)

#carbodysedan is least significant value. So it can be removed from the model



#Model 15
#------------------------------------
model_15 <- lm(formula = price ~ aspiration + enginelocation +  
                 carwidth + curbweight + cylindernumber +  
                 boreratio + CarCompanybmw + 
                 CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                 CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                 carbodyhatchback + carbodywagon + 
                 enginetyperotor, 
               data = train)

summary(model_15)

#Multiple R-squared:  0.9624,	Adjusted R-squared:  0.9555 

#VIF for the model
sort(vif(model_15),decreasing = TRUE)

#carbodyhatchback has least significant value. So it can be removed from the model



#Model 16
#------------------------------------
model_16 <- lm(formula = price ~ aspiration + enginelocation +  
                 carwidth + curbweight + cylindernumber +  
                 boreratio + CarCompanybmw + 
                 CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                 CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                 carbodywagon + 
                 enginetyperotor, 
               data = train)

summary(model_16)

#Multiple R-squared:  0.9621,	Adjusted R-squared:  0.9555 

#VIF for the model
sort(vif(model_16),decreasing = TRUE)

#boreratio has high VIF and least significant value. So it can be removed from the model



#Model 17
#------------------------------------
model_17 <- lm(formula = price ~ aspiration + enginelocation +  
                 carwidth + curbweight + cylindernumber +  
                 CarCompanybmw + 
                 CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                 CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                 carbodywagon + 
                 enginetyperotor, 
               data = train)

summary(model_17)

#Multiple R-squared:  0.9589,	Adjusted R-squared:  0.9522 

#VIF for the model
sort(vif(model_17),decreasing = TRUE)

#aspiration has insignificant. So it can be removed from the model



#Model 18
#------------------------------------
model_18 <- lm(formula = price ~ enginelocation +  
                 carwidth + curbweight + cylindernumber +  
                 CarCompanybmw + 
                 CarCompanybuick + CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                 CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                 carbodywagon + 
                 enginetyperotor, 
               data = train)

summary(model_18)

#Multiple R-squared:  0.9577,	Adjusted R-squared:  0.9512 

#VIF for the model
sort(vif(model_18),decreasing = TRUE)

#CarCompanydodge is insignificant. So it can be removed from the model



#Model 19
#------------------------------------
model_19 <- lm(formula = price ~ enginelocation +  
                 carwidth + curbweight + cylindernumber +  
                 CarCompanybmw + 
                 CarCompanybuick + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                 CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                 carbodywagon + 
                 enginetyperotor, 
               data = train)

summary(model_19)

#Multiple R-squared:  0.9564,	Adjusted R-squared:  0.9501 

#VIF for the model
sort(vif(model_19),decreasing = TRUE)

#Check the correlation of the variables carwidth and curbweight
cor(train$carwidth, train$curbweight)

# The correlation is ~ 87%, indicating that the variables are highly correlated. 
# Removing "carwidth" variable as it has lower significance level out of the two



#Model 20
#------------------------------------
model_20 <- lm(formula = price ~ enginelocation +  
                 curbweight + cylindernumber +  
                 CarCompanybmw + 
                 CarCompanybuick + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth + CarCompanyrenault + 
                 CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                 carbodywagon + 
                 enginetyperotor, 
               data = train)

summary(model_20)

#Multiple R-squared:  0.9487,	Adjusted R-squared:  0.9417 

#VIF for the model
sort(vif(model_20),decreasing = TRUE)

#CarCompanyrenault is insignificant. So it can be removed from the model



#Model 21
#------------------------------------
model_21 <- lm(formula = price ~ enginelocation +  
                 curbweight + cylindernumber +  
                 CarCompanybmw + 
                 CarCompanybuick + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth +  
                 CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                 carbodywagon + 
                 enginetyperotor, 
               data = train)

summary(model_21)

#Multiple R-squared:  0.9476,	Adjusted R-squared:  0.941 

#VIF for the model
sort(vif(model_21),decreasing = TRUE)

#CarCompanymazda has least significant value. So it can be removed from the model



#Model 22
#------------------------------------
model_22 <- lm(formula = price ~ enginelocation +  
                 curbweight + cylindernumber +  
                 CarCompanybmw + 
                 CarCompanybuick + CarCompanyjaguar + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth +  
                 CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                 carbodywagon + 
                 enginetyperotor, 
               data = train)

summary(model_22)

#Multiple R-squared:  0.9462,	Adjusted R-squared:  0.9398 

#VIF for the model
sort(vif(model_22),decreasing = TRUE)

#CarCompanyvolkswagen has least significant value. So it can be removed from the model



#Model 23
#------------------------------------
model_23 <- lm(formula = price ~ enginelocation +  
                 curbweight + cylindernumber +  
                 CarCompanybmw + 
                 CarCompanybuick + CarCompanyjaguar + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth +  
                 CarCompanysubaru + CarCompanytoyota +  
                 carbodywagon + 
                 enginetyperotor, 
               data = train)

summary(model_23)

#Multiple R-squared:  0.9451,	Adjusted R-squared:  0.9391 

#VIF for the model
sort(vif(model_23),decreasing = TRUE)

#Check the correlation of the variables curbweight and cylindernumber
cor(train$curbweight, train$cylindernumber)

#The correlation is ~ 61%, indicating that the variables are highly correlated. 
#Removing "cylindernumber" variable as it has lower significance level out of the two



#Model 24
#------------------------------------
model_24 <- lm(formula = price ~ enginelocation +  
                 curbweight +   
                 CarCompanybmw + 
                 CarCompanybuick + CarCompanyjaguar + 
                 CarCompanymitsubishi + CarCompanynissan + 
                 CarCompanypeugeot + CarCompanyplymouth +  
                 CarCompanysubaru + CarCompanytoyota +  
                 carbodywagon + 
                 enginetyperotor, 
               data = train)

summary(model_24)

#Multiple R-squared:  0.9242,	Adjusted R-squared:  0.9165 

#VIF for the model
sort(vif(model_24),decreasing = TRUE)

#CarCompanynissan is insignificant. So it can be removed from the model



#Model 25
#------------------------------------
model_25 <- lm(formula = price ~ enginelocation +  
                 curbweight +   
                 CarCompanybmw + 
                 CarCompanybuick + CarCompanyjaguar + 
                 CarCompanymitsubishi + 
                 CarCompanypeugeot + CarCompanyplymouth +  
                 CarCompanysubaru + CarCompanytoyota +  
                 carbodywagon + 
                 enginetyperotor, 
               data = train)

summary(model_25)

#Multiple R-squared:  0.9241,	Adjusted R-squared:  0.9171 

#VIF for the model
sort(vif(model_25),decreasing = TRUE)

#enginetyperotor is insignificant. So it can be removed from the model



#Model 26
#------------------------------------
model_26 <- lm(formula = price ~ enginelocation +  
                 curbweight +   
                 CarCompanybmw + 
                 CarCompanybuick + CarCompanyjaguar + 
                 CarCompanymitsubishi + 
                 CarCompanypeugeot + CarCompanyplymouth +  
                 CarCompanysubaru + CarCompanytoyota +  
                 carbodywagon, 
               data = train)

summary(model_26)

#Multiple R-squared:  0.924,	Adjusted R-squared:  0.9176 

#VIF for the model
sort(vif(model_26),decreasing = TRUE)

#CarCompanysubaru is insignificant. So it can be removed from the model



#Model 27
#------------------------------------
model_27 <- lm(formula = price ~ enginelocation +  
                 curbweight +   
                 CarCompanybmw + 
                 CarCompanybuick + CarCompanyjaguar + 
                 CarCompanymitsubishi + 
                 CarCompanypeugeot + CarCompanyplymouth +  
                 CarCompanytoyota +  
                 carbodywagon, 
               data = train)

summary(model_27)

#Multiple R-squared:  0.9231,	Adjusted R-squared:  0.9172 

#VIF for the model
sort(vif(model_27),decreasing = TRUE)

#CarCompanyplymouth is insignificant. So it can be removed from the model



#Model 28
#------------------------------------
model_28 <- lm(formula = price ~ enginelocation +  
                 curbweight +   
                 CarCompanybmw + 
                 CarCompanybuick + CarCompanyjaguar + 
                 CarCompanymitsubishi + 
                 CarCompanypeugeot + 
                 CarCompanytoyota +  
                 carbodywagon, 
               data = train)

summary(model_28)

#Multiple R-squared:  0.9223,	Adjusted R-squared:  0.917 

#VIF for the model
sort(vif(model_28),decreasing = TRUE)

#CarCompanymitsubishi is insignificant. So it can be removed from the model



#Model 29
#------------------------------------
model_29 <- lm(formula = price ~ enginelocation +  
                 curbweight +   
                 CarCompanybmw + 
                 CarCompanybuick + CarCompanyjaguar + 
                 CarCompanypeugeot + 
                 CarCompanytoyota +  
                 carbodywagon, 
               data = train)

summary(model_29)

#Multiple R-squared:  0.919,	Adjusted R-squared:  0.9142 

#VIF for the model
sort(vif(model_29),decreasing = TRUE)

#CarCompanytoyota is insignificant. So it can be removed from the model



#Model 30
#------------------------------------
model_30 <- lm(formula = price ~ enginelocation +  
                 curbweight +   
                 CarCompanybmw + 
                 CarCompanybuick + 
                 CarCompanyjaguar + 
                 CarCompanypeugeot + 
                 carbodywagon, 
               data = train)

summary(model_30)

#Multiple R-squared:  0.9126,	Adjusted R-squared:  0.9081 

#VIF for the model
sort(vif(model_30),decreasing = TRUE)


#model_30 is the final model as all the independent variables are significant and VIF < 2



# Predict the car prices in the testing dataset
# which(colnames(test)=="price")

predict_1 <- predict(model_30, test[, -19])  

test$test_price <- predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)

# calculate R-squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# R-squared value
rsquared

#0.8667103


# Adjusted R-Squared is 90% and test R-squared is 87%. 
# There is a deviation of 3% between the Adjusted R-Squared and test R-squared . This is in the acceptable range.
# model_30 is the final model based on which car price can be Predicted.


#Following variables are significant in predicting the price of a car. Based on these variables, the price of the Car can be derived.
#---------------------------------
# enginelocation  --> front or rear
# curbweight
# CarCompany --> bmw, buick, jaguar, peugot
# carbody --> wagon

