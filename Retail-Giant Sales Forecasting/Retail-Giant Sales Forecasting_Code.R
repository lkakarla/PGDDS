################### Retail-Giant Sales Forecasting - Time Series Analysis ##################
#-------------------------------------------------------------------------------

#######################################################################################
### Business Understanding:
#######################################################################################

# The online store has maintained a database containing orders and 
# delivers across the globe and deals with all the major product 
# categories - consumer, corporate & home office.

## AIM:

# The aim is forecast the sales and the demand for the next 6 months,that would help 
# sales/operations manager to manage the revenue and inventory accordingly.

# Provided data.
# 1. Transaction level data - Each row represents a particular order made on the online store



#######################################################################################
### Data Understanding
#######################################################################################

#clear Environment and ignore warnings
rm(list = ls()) 
options(warn=-1)

#-------------------------------------------------------------------------------
#load packages
#-------------------------------------------------------------------------------

library("ggplot2")
library("dplyr")
library("tidyr")
library("lubridate")

library("forecast")
library("tseries")
require("graphics")


#-------------------------------------------------------------------------------
# Load the data and check its contents
#-------------------------------------------------------------------------------

#load csv file
gs_df <- read.csv("Global Superstore.csv", header = T, sep = ',')
#gs_df <- read.csv("Global Superstore.csv", header = T, sep = ',', stringsAsFactors = F)

#Exploring the data
summary(gs_df)

# data structure 
str(gs_df)   #51290 obs. of  24 variables

head(gs_df)

#print column names
names(gs_df)

#######################################################################################
### Data Preparation & Exploratory Data Analysis
#######################################################################################

#-------------------------------------------------------------------------------
#Check Unique Columns
#-------------------------------------------------------------------------------

#print no of unique values under each column
as.data.frame(rapply(gs_df,function(x)length(unique(x))))

#Print column names having same value for all rows
names(gs_df[sapply(gs_df, function(x) length(unique(x))==1)])  # 

#Print columns having unique values for each row
names(gs_df[sapply(gs_df, function(x) length(unique(x))==nrow(gs_df))])  # "Row.ID"

# Unique values in the data frames
length(unique(tolower(gs_df$Row.ID)))    # 51290, confirming Row.ID is primary/unique key 



#-------------------------------------------------------------------------------
#Check for duplicates
#-------------------------------------------------------------------------------
sum(duplicated(gs_df)) # no duplicate rows



#-------------------------------------------------------------------------------
#Missing values
#-------------------------------------------------------------------------------

#Check for Missing values
sum(is.na(gs_df))   # 41296 NA values

#Get attribute names having NA
colnames(gs_df)[colSums(is.na(gs_df)) > 0]
na_col <- sapply(gs_df, function(x) length(which(is.na(x))))
na_col   #  "gs_df" -- 41296 

#Postal.Code attribute having NA values. As per our analysis, Postal Code not present for other than US Market


# Checking for blank "" values
blank_col <- sapply(gs_df, function(x) length(which(x == "")))
blank_col
table(blank_col)  # 0 blanks



#-------------------------------------------------------------------------------
# Format Date Column
#-------------------------------------------------------------------------------

# formatting date columns -- Order.Date
gs_df$Order.Date <- as.Date(gs_df$Order.Date, "%d-%m-%Y")
gs_df$Ship.Date  <- as.Date(gs_df$Ship.Date, "%d-%m-%Y")

# data structure 
str(gs_df)   #51290 obs. of  24 variables



#-------------------------------------------------------------------------------
# Data Exploration using Plots
#-------------------------------------------------------------------------------

# Seasonality in sales by Quarter
plot1 <- gs_df %>% 
  mutate(Order.Quarter = quarter(Order.Date, with_year = 'true')) %>%
  group_by(Order.Quarter)%>%
  summarise(Sales=sum(Sales)) %>%
  ggplot(aes(x=Order.Quarter,y=Sales)) +
  geom_line(color = 'blue') +
  labs(title="Seasonality in Sales by Quarter",
       x="Year",
       y="Sales")  

plot1

# Seasonality in sales by Month

plot2 <- gs_df %>% 
  separate(Order.Date,c("year","month","day"),sep="-") %>%
  group_by(year,month) %>%
  summarise(sumsale=sum(Sales)) %>%
  ggplot(aes(reorder(month, month), sumsale))+
  geom_bar(stat="identity")+
  facet_grid(. ~ year)+
  labs(title="Seasonality in Sales by Month",
       x="Month",
       y="Sales")  

plot2

#There is seasonality in sales.


#-------------------------------------------------------------------------------
# Find 2 most profitable and consistently profitable segments
#-------------------------------------------------------------------------------

# Derive new column with combination of Order Year & Month from Order.Date
#gs_df$Order.Month.Year <- format(gs_df$Order.Date,"%m-%Y")
gs_df$Order.Year.Month <- format(gs_df$Order.Date,"%Y-%m")


# Check  Market and Segment Levels
levels(gs_df$Market) # 7 Markets --> "Africa" "APAC" "Canada" "EMEA" "EU" "LATAM" "US" 
levels(gs_df$Segment) # 3 Segments --> "Consumer" "Corporate" "Home Office"


# Derive new factor variable with combination of "Market" and "Segment" variables 
gs_df$MarketSegment <- as.factor(paste(gs_df$Market,gs_df$Segment))


# Calculate the coefficient of variation (CV)
calc_cv <- function(inputVar){
  gs_agg <- gs_df %>%
    subset(MarketSegment == inputVar)%>%
    aggregate(cbind(Sales,Profit,Quantity) ~ Order.Year.Month, . ,sum)
  cv <- sd(gs_agg$Profit)/mean(gs_agg$Profit)
  return (cv)
}


#Create a 21*2 dimensions empty Matrix 
cv_df <- matrix(nrow=21, ncol=2)


#Calculate CV

seg_length <- length(levels(gs_df$MarketSegment))

for(i in 1:seg_length){
  cv_df[i,1] <- levels(gs_df$MarketSegment)[i]
  cv_df[i,2] <- calc_cv(levels(gs_df$MarketSegment)[i])[]
}


#Convert to Data Frame
cv_df <- data.frame(cv_df)


#Name the columns
colnames(cv_df)<- c("MarketSegment","CV")


# Order the data frame based on CV - default sorted in increasing order.
cv_df <- cv_df[order(cv_df$CV), ]
cv_df

# Get Top 2 profitiable Market Segments with Least CV
head(cv_df, 2)

cv_df$CV <- round(as.numeric(as.character(cv_df$CV)), digits=2)
cv_df$MarketSegment <- factor(cv_df$MarketSegment, levels = cv_df$MarketSegment[order(cv_df$CV, decreasing = TRUE)])
cv_df$colour <- ifelse(cv_df$CV < 0.65, "top2","rest")

plot3 <- ggplot(cv_df, aes(x = cv_df$MarketSegment ,y = cv_df$CV, fill = colour))+
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=cv_df$CV), hjust=1, colour = "white") +
  xlab("Market Segment") +
  ylab("Coefficient of Variation") +
  ggtitle("Coefficient of Variation for Market Segment") +
  scale_fill_manual(values=c(top2="firebrick1",rest="steelblue")) +
  scale_y_continuous(
    breaks = c(0.5:7)
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(legend.position="none") +
  coord_flip()

plot3

# Based on coefficient of variation (CV), following 2 Market segments identified as most profitable.

# MarketSegment   CV
# ==============  =====
# EU Consumer     0.62
# APAC Consumer   0.63



#######################################################################################
### Sales Forecasting
#######################################################################################

detach("package:dplyr", unload=TRUE)

#-------------------------------------------------------------------------------
# EU Consumer  -- Sales Forecasting
#-------------------------------------------------------------------------------

top <- 1
eu_df <- subset(gs_df, MarketSegment == cv_df$MarketSegment[top])
eu_sales_df <- aggregate(Sales ~ Order.Year.Month, data = eu_df, FUN=sum)
Month   <- seq(1, nrow(eu_sales_df), 1)
eu_sales_df <- cbind(eu_sales_df, Month)

# Decompose timeseries to see the components - Trend, Seasonality, Error etc
# Considered frequency = 12
eu_sales_timeser_freq <- ts(eu_sales_df$Sales, frequency=12)
eu_sales_timeser_stl <- stl(eu_sales_timeser_freq, s.window="periodic")
eu_sales_timeser_deseasonal <- seasadj(eu_sales_timeser_stl)
plot(eu_sales_timeser_stl, main="EU Consumer - Sales - Time Series Components")

eu_sales_timeser_decompose <- decompose(eu_sales_timeser_freq)  # Additive Time Series
#eu_sales_timeser_decompose <- decompose(eu_sales_timeser_freq, type="mult") # Multiplicative Time Series
plot(eu_sales_timeser_decompose)

# Time series decomposition shows periodic seasonal pattern

# Let's create the model using the first 42 months 
# Then test the model on the remaining 6 months later

eu_sales_total_timeser <- ts(eu_sales_df$Sales)
eu_sales_indata <- eu_sales_df[1:42,]
eu_sales_timeser <- ts(eu_sales_indata$Sales)
plot(eu_sales_timeser,  main="EU Consumer - Sales")

#Smoothing the series - Moving Average Smoothing

w <-1
eu_sales_smoothedseries <- filter(eu_sales_timeser, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)

#Smoothing left end of the time series

eu_sales_diff <- eu_sales_smoothedseries[w+2] - eu_sales_smoothedseries[w+1]

for (i in seq(w,1,-1)) {
  eu_sales_smoothedseries[i] <- eu_sales_smoothedseries[i+1] - eu_sales_diff
}

#Smoothing right end of the time series  

n <- length(eu_sales_timeser)
eu_sales_diff <- eu_sales_smoothedseries[n-w] - eu_sales_smoothedseries[n-w-1]

for (i in seq(n-w+1, n)) {
  eu_sales_smoothedseries[i] <- eu_sales_smoothedseries[i-1] + eu_sales_diff
}

#Plot the smoothed time series

eu_sales_timevals_in <- eu_sales_indata$Month

plot(eu_sales_timeser,xlab = "Month", ylab= paste(cv_df$MarketSegment[top]), main="EU Consumer - Sales")
lines(eu_sales_smoothedseries, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

eu_sales_smootheddf <- as.data.frame(cbind(eu_sales_timevals_in, as.vector(eu_sales_smoothedseries)))
colnames(eu_sales_smootheddf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

eu_sales_lmfit <- lm(Sales ~ sin(0.6*Month) * poly(Month,3) + cos(0.6*Month) * poly(Month,3)
                     + Month, data=eu_sales_smootheddf)

eu_sales_global_pred <- predict(eu_sales_lmfit, Month=eu_sales_timevals_in)

summary(eu_sales_global_pred)

lines(eu_sales_timevals_in, eu_sales_global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

eu_sales_local_pred <- eu_sales_timeser - eu_sales_global_pred
plot(eu_sales_local_pred, col='red', type = "l")
acf(eu_sales_local_pred)
acf(eu_sales_local_pred, type="partial")
eu_sales_armafit <- auto.arima(eu_sales_local_pred)

tsdiag(eu_sales_armafit)
eu_sales_armafit  #ARIMA(0,0,2) with zero mean 

#We'll check if the residual series is white noise

eu_sales_resi <- eu_sales_local_pred - fitted(eu_sales_armafit)

adf.test(eu_sales_resi,alternative = "stationary")
kpss.test(eu_sales_resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

eu_sales_outdata <- eu_sales_df[43:48,]
eu_sales_timevals_out <- as.data.frame(eu_sales_outdata$Month)
eu_sales_global_pred_out <- predict(eu_sales_lmfit,eu_sales_timevals_out)
eu_sales_fcast <- eu_sales_global_pred_out[43:48]

#Now, let's compare our prediction with the actual values, using MAPE

eu_sales_MAPE_class_dec <- accuracy(eu_sales_fcast,eu_sales_outdata[,2])[5]
eu_sales_MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

eu_sales_class_dec_pred <- c(ts(eu_sales_global_pred),ts(eu_sales_global_pred_out[43:48]))
#eu_sales_class_dec_pred <- ts(eu_sales_global_pred_out)
plot(eu_sales_total_timeser, col = "black", main="EU Consumer - Sales Forecasting - Classical Decomposition")
lines(eu_sales_class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

eu_sales_autoarima <- auto.arima(eu_sales_timeser)
eu_sales_autoarima
tsdiag(eu_sales_autoarima)
plot(eu_sales_autoarima$x, col="black", main="EU Consumer - Sales - Auto Arima")
lines(fitted(eu_sales_autoarima), col="red")

#Again, let's check if the residual series is white noise

eu_sales_resi_auto_arima <- eu_sales_timeser - fitted(eu_sales_autoarima)

adf.test(eu_sales_resi_auto_arima,alternative = "stationary")
kpss.test(eu_sales_resi_auto_arima)

#Also, let's evaluate the model using MAPE
eu_sales_fcast_auto_arima <- predict(eu_sales_autoarima, n.ahead = 6)

eu_sales_MAPE_auto_arima <- accuracy(eu_sales_fcast_auto_arima$pred,eu_sales_outdata[,2])[5]
eu_sales_MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

eu_sales_auto_arima_pred <- c(fitted(eu_sales_autoarima),ts(eu_sales_fcast_auto_arima$pred))
plot(eu_sales_total_timeser, col = "black", main="EU Consumer - Sales Forecasting - Auto Arima")
lines(eu_sales_auto_arima_pred, col = "red")



#-------------------------------------------------------------------------------
# APAC Consumer  -- Sales Forecasting
#-------------------------------------------------------------------------------

top <- 2
apac_df <- subset(gs_df, MarketSegment == cv_df$MarketSegment[top])
apac_sales_df <- aggregate(Sales ~ Order.Year.Month, data = apac_df, FUN=sum)
Month   <- seq(1, nrow(apac_sales_df), 1)
apac_sales_df <- cbind(apac_sales_df, Month)

# Decompose timeseries to see the components - Trend, Seasonality, Error etc
# Considered frequency = 12
apac_sales_timeser_freq <- ts(apac_sales_df$Sales, frequency=12)
apac_sales_timeser_stl <- stl(apac_sales_timeser_freq, s.window="periodic")
apac_sales_timeser_deseasonal <- seasadj(apac_sales_timeser_stl)
plot(apac_sales_timeser_stl,main="APAC Consumer - Sales - Time Series Components")

apac_sales_timeser_decompose <- decompose(apac_sales_timeser_freq)  # Additive Time Series
#apac_sales_timeser_decompose <- decompose(apac_sales_timeser_freq, type="mult") # Multiplicative Time Series
plot(apac_sales_timeser_decompose)

# Time series decomposition shows periodic seasonal pattern

# Let's create the model using the first 42 months 
# Then test the model on the remaining 6 months later

apac_sales_total_timeser <- ts(apac_sales_df$Sales)
apac_sales_indata <- apac_sales_df[1:42,]
apac_sales_timeser <- ts(apac_sales_indata$Sales)
plot(apac_sales_timeser,main="APAC Consumer - Sales")

#Smoothing the series - Moving Average Smoothing

w <-1
apac_sales_smoothedseries <- filter(apac_sales_timeser, 
                                    filter=rep(1/(2*w+1),(2*w+1)), 
                                    method='convolution', sides=2)

#Smoothing left end of the time series

apac_sales_diff <- apac_sales_smoothedseries[w+2] - apac_sales_smoothedseries[w+1]

for (i in seq(w,1,-1)) {
  apac_sales_smoothedseries[i] <- apac_sales_smoothedseries[i+1] - apac_sales_diff
}

#Smoothing right end of the time series  

n <- length(apac_sales_timeser)
apac_sales_diff <- apac_sales_smoothedseries[n-w] - apac_sales_smoothedseries[n-w-1]

for (i in seq(n-w+1, n)) {
  apac_sales_smoothedseries[i] <- apac_sales_smoothedseries[i-1] + apac_sales_diff
}

#Plot the smoothed time series

apac_sales_timevals_in <- apac_sales_indata$Month

plot(apac_sales_timeser,xlab = "Month", ylab= paste(cv_df$MarketSegment[top]), main="APAC Consumer - Sales")
lines(apac_sales_smoothedseries, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

apac_sales_smootheddf <- as.data.frame(cbind(apac_sales_timevals_in, as.vector(apac_sales_smoothedseries)))
colnames(apac_sales_smootheddf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

apac_sales_lmfit <- lm(Sales ~ sin(0.6*Month) * poly(Month,3) + cos(0.6*Month) * poly(Month,3)
                       + Month, data=apac_sales_smootheddf)

apac_sales_global_pred <- predict(apac_sales_lmfit, Month=apac_sales_timevals_in)

summary(apac_sales_global_pred)

lines(apac_sales_timevals_in, apac_sales_global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

apac_sales_local_pred <- apac_sales_timeser - apac_sales_global_pred
plot(apac_sales_local_pred, col='red', type = "l")
acf(apac_sales_local_pred)
acf(apac_sales_local_pred, type="partial")
apac_sales_armafit <- auto.arima(apac_sales_local_pred)

tsdiag(apac_sales_armafit)
apac_sales_armafit  #ARIMA(0,0,2) with zero mean 

#We'll check if the residual series is white noise

apac_sales_resi <- apac_sales_local_pred - fitted(apac_sales_armafit)

adf.test(apac_sales_resi,alternative = "stationary")
kpss.test(apac_sales_resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

apac_sales_outdata <- apac_sales_df[43:48,]
apac_sales_timevals_out <- as.data.frame(apac_sales_outdata$Month)
apac_sales_global_pred_out <- predict(apac_sales_lmfit,apac_sales_timevals_out)
apac_sales_fcast <- apac_sales_global_pred_out[43:48]

#Now, let's compare our prediction with the actual values, using MAPE

apac_sales_MAPE_class_dec <- accuracy(apac_sales_fcast,apac_sales_outdata[,2])[5]
apac_sales_MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

apac_sales_class_dec_pred <- c(ts(apac_sales_global_pred),ts(apac_sales_global_pred_out[43:48]))
#apac_sales_class_dec_pred <- ts(apac_sales_global_pred_out)
plot(apac_sales_total_timeser, col = "black",main="APAC Consumer - Sales Forecasting - Classical Decomposition")
lines(apac_sales_class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

apac_sales_autoarima <- auto.arima(apac_sales_timeser)
apac_sales_autoarima
tsdiag(apac_sales_autoarima)
plot(apac_sales_autoarima$x, col="black",main="APAC Consumer - Sales - Auto Arima")
lines(fitted(apac_sales_autoarima), col="red")

#Again, let's check if the residual series is white noise

apac_sales_resi_auto_arima <- apac_sales_timeser - fitted(apac_sales_autoarima)

adf.test(apac_sales_resi_auto_arima,alternative = "stationary")
kpss.test(apac_sales_resi_auto_arima)

#Also, let's evaluate the model using MAPE
apac_sales_fcast_auto_arima <- predict(apac_sales_autoarima, n.ahead = 6)

apac_sales_MAPE_auto_arima <- accuracy(apac_sales_fcast_auto_arima$pred,apac_sales_outdata[,2])[5]
apac_sales_MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

apac_sales_auto_arima_pred <- c(fitted(apac_sales_autoarima),ts(apac_sales_fcast_auto_arima$pred))
plot(apac_sales_total_timeser, col = "black" ,main="APAC Consumer - Sales Forecasting - Auto Arima")
lines(apac_sales_auto_arima_pred, col = "red")





#######################################################################################
### Quantity Forecasting
#######################################################################################


#-------------------------------------------------------------------------------
# EU Consumer  -- Quantity Forecasting
#-------------------------------------------------------------------------------

top <- 1
eu_df <- subset(gs_df, MarketSegment == cv_df$MarketSegment[top])
eu_quantity_df <- aggregate(Quantity ~ Order.Year.Month, data = eu_df, FUN=sum)
Month   <- seq(1, nrow(eu_quantity_df), 1)
eu_quantity_df <- cbind(eu_quantity_df, Month)

# Decompose timeseries to see the components - Trend, Seasonality, Error etc
# Considered frequency = 12
eu_quantity_timeser_freq <- ts(eu_quantity_df$Quantity, frequency=12)
eu_quantity_timeser_stl <- stl(eu_quantity_timeser_freq, s.window="periodic")
eu_quantity_timeser_deseasonal <- seasadj(eu_quantity_timeser_stl)
plot(eu_quantity_timeser_stl, main="EU Consumer - Quantity - Time Series Components")

eu_quantity_timeser_decompose <- decompose(eu_quantity_timeser_freq)  # Additive Time Series
#eu_quantity_timeser_decompose <- decompose(eu_quantity_timeser_freq, type="mult") # Multiplicative Time Series
plot(eu_quantity_timeser_decompose)

# Time series decomposition shows periodic seasonal pattern

# Let's create the model using the first 42 months 
# Then test the model on the remaining 6 months later

eu_quantity_total_timeser <- ts(eu_quantity_df$Quantity)
eu_quantity_indata <- eu_quantity_df[1:42,]
eu_quantity_timeser <- ts(eu_quantity_indata$Quantity)
plot(eu_quantity_timeser, main="EU Consumer - Quantity")

#Smoothing the series - Moving Average Smoothing

w <-1
eu_quantity_smoothedseries <- filter(eu_quantity_timeser, 
                                     filter=rep(1/(2*w+1),(2*w+1)), 
                                     method='convolution', sides=2)

#Smoothing left end of the time series

eu_quantity_diff <- eu_quantity_smoothedseries[w+2] - eu_quantity_smoothedseries[w+1]

for (i in seq(w,1,-1)) {
  eu_quantity_smoothedseries[i] <- eu_quantity_smoothedseries[i+1] - eu_quantity_diff
}

#Smoothing right end of the time series  

n <- length(eu_quantity_timeser)
eu_quantity_diff <- eu_quantity_smoothedseries[n-w] - eu_quantity_smoothedseries[n-w-1]

for (i in seq(n-w+1, n)) {
  eu_quantity_smoothedseries[i] <- eu_quantity_smoothedseries[i-1] + eu_quantity_diff
}

#Plot the smoothed time series

eu_quantity_timevals_in <- eu_quantity_indata$Month

plot(eu_quantity_timeser,xlab = "Month", ylab= paste(cv_df$MarketSegment[top]), main="EU Consumer - Quantity")
lines(eu_quantity_smoothedseries, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

eu_quantity_smootheddf <- as.data.frame(cbind(eu_quantity_timevals_in, as.vector(eu_quantity_smoothedseries)))
colnames(eu_quantity_smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

eu_quantity_lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                        + Month, data=eu_quantity_smootheddf)

eu_quantity_global_pred <- predict(eu_quantity_lmfit, Month=eu_quantity_timevals_in)

summary(eu_quantity_global_pred)

lines(eu_quantity_timevals_in, eu_quantity_global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

eu_quantity_local_pred <- eu_quantity_timeser - eu_quantity_global_pred
plot(eu_quantity_local_pred, col='red', type = "l")
acf(eu_quantity_local_pred)
acf(eu_quantity_local_pred, type="partial")
eu_quantity_armafit <- auto.arima(eu_quantity_local_pred)

tsdiag(eu_quantity_armafit)
eu_quantity_armafit  #ARIMA(0,0,2) with zero mean 

#We'll check if the residual series is white noise

eu_quantity_resi <- eu_quantity_local_pred - fitted(eu_quantity_armafit)

adf.test(eu_quantity_resi,alternative = "stationary")
kpss.test(eu_quantity_resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

eu_quantity_outdata <- eu_quantity_df[43:48,]
eu_quantity_timevals_out <- as.data.frame(eu_quantity_outdata$Month)
eu_quantity_global_pred_out <- predict(eu_quantity_lmfit,eu_quantity_timevals_out)
eu_quantity_fcast <- eu_quantity_global_pred_out[43:48]

#Now, let's compare our prediction with the actual values, using MAPE

eu_quantity_MAPE_class_dec <- accuracy(eu_quantity_fcast,eu_quantity_outdata[,2])[5]
eu_quantity_MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

eu_quantity_class_dec_pred <- c(ts(eu_quantity_global_pred),ts(eu_quantity_global_pred_out[43:48]))
#eu_quantity_class_dec_pred <- ts(eu_quantity_global_pred_out)
plot(eu_quantity_total_timeser, col = "black", main="EU Consumer - Quantity Forecasting - Classical Decomposition")
lines(eu_quantity_class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

eu_quantity_autoarima <- auto.arima(eu_quantity_timeser)
eu_quantity_autoarima
tsdiag(eu_quantity_autoarima)
plot(eu_quantity_autoarima$x, col="black", main="EU Consumer - Quantity - Auto Arima")
lines(fitted(eu_quantity_autoarima), col="red")

#Again, let's check if the residual series is white noise

eu_quantity_resi_auto_arima <- eu_quantity_timeser - fitted(eu_quantity_autoarima)

adf.test(eu_quantity_resi_auto_arima,alternative = "stationary")
kpss.test(eu_quantity_resi_auto_arima)

#Also, let's evaluate the model using MAPE
eu_quantity_fcast_auto_arima <- predict(eu_quantity_autoarima, n.ahead = 6)

eu_quantity_MAPE_auto_arima <- accuracy(eu_quantity_fcast_auto_arima$pred,eu_quantity_outdata[,2])[5]
eu_quantity_MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

eu_quantity_auto_arima_pred <- c(fitted(eu_quantity_autoarima),ts(eu_quantity_fcast_auto_arima$pred))
plot(eu_quantity_total_timeser, col = "black", main="EU Consumer - Quantity Forecasting - Auto Arima")
lines(eu_quantity_auto_arima_pred, col = "red")





#-------------------------------------------------------------------------------
# APAC Consumer  -- Quantity Forecasting
#-------------------------------------------------------------------------------

top <- 2
apac_df <- subset(gs_df, MarketSegment == cv_df$MarketSegment[top])
apac_quantity_df <- aggregate(Quantity ~ Order.Year.Month, data = apac_df, FUN=sum)
Month   <- seq(1, nrow(apac_quantity_df), 1)
apac_quantity_df <- cbind(apac_quantity_df, Month)

# Decompose timeseries to see the components - Trend, Seasonality, Error etc
# Considered frequency = 12
apac_quantity_timeser_freq <- ts(apac_quantity_df$Quantity, frequency=12)
apac_quantity_timeser_stl <- stl(apac_quantity_timeser_freq, s.window="periodic")
apac_quantity_timeser_deseasonal <- seasadj(apac_quantity_timeser_stl)
plot(apac_quantity_timeser_stl, main="APAC Consumer - Quantity - Time Series Components")

apac_quantity_timeser_decompose <- decompose(apac_quantity_timeser_freq)  # Additive Time Series
#apac_quantity_timeser_decompose <- decompose(apac_quantity_timeser_freq, type="mult") # Multiplicative Time Series
plot(apac_quantity_timeser_decompose)

# Time series decomposition shows periodic seasonal pattern

# Let's create the model using the first 42 months 
# Then test the model on the remaining 6 months later

apac_quantity_total_timeser <- ts(apac_quantity_df$Quantity)
apac_quantity_indata <- apac_quantity_df[1:42,]
apac_quantity_timeser <- ts(apac_quantity_indata$Quantity)
plot(apac_quantity_timeser,main="APAC Consumer - Quantity")

#Smoothing the series - Moving Average Smoothing

w <-1
apac_quantity_smoothedseries <- filter(apac_quantity_timeser, 
                                       filter=rep(1/(2*w+1),(2*w+1)), 
                                       method='convolution', sides=2)

#Smoothing left end of the time series

apac_quantity_diff <- apac_quantity_smoothedseries[w+2] - apac_quantity_smoothedseries[w+1]

for (i in seq(w,1,-1)) {
  apac_quantity_smoothedseries[i] <- apac_quantity_smoothedseries[i+1] - apac_quantity_diff
}

#Smoothing right end of the time series  

n <- length(apac_quantity_timeser)
apac_quantity_diff <- apac_quantity_smoothedseries[n-w] - apac_quantity_smoothedseries[n-w-1]

for (i in seq(n-w+1, n)) {
  apac_quantity_smoothedseries[i] <- apac_quantity_smoothedseries[i-1] + apac_quantity_diff
}

#Plot the smoothed time series

apac_quantity_timevals_in <- apac_quantity_indata$Month

plot(apac_quantity_timeser,xlab = "Month", ylab= paste(cv_df$MarketSegment[top]), main="APAC Consumer - Quantity")
lines(apac_quantity_smoothedseries, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

apac_quantity_smootheddf <- as.data.frame(cbind(apac_quantity_timevals_in, as.vector(apac_quantity_smoothedseries)))
colnames(apac_quantity_smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

apac_quantity_lmfit <- lm(Quantity ~ sin(0.6*Month) * poly(Month,3) + cos(0.6*Month) * poly(Month,3)
                          + Month, data=apac_quantity_smootheddf)

apac_quantity_global_pred <- predict(apac_quantity_lmfit, Month=apac_quantity_timevals_in)

summary(apac_quantity_global_pred)

lines(apac_quantity_timevals_in, apac_quantity_global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

apac_quantity_local_pred <- apac_quantity_timeser - apac_quantity_global_pred
plot(apac_quantity_local_pred, col='red', type = "l")
acf(apac_quantity_local_pred)
acf(apac_quantity_local_pred, type="partial")
apac_quantity_armafit <- auto.arima(apac_quantity_local_pred)

tsdiag(apac_quantity_armafit)
apac_quantity_armafit  #ARIMA(0,0,2) with zero mean 

#We'll check if the residual series is white noise

apac_quantity_resi <- apac_quantity_local_pred - fitted(apac_quantity_armafit)

adf.test(apac_quantity_resi,alternative = "stationary")
kpss.test(apac_quantity_resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

apac_quantity_outdata <- apac_quantity_df[43:48,]
apac_quantity_timevals_out <- as.data.frame(apac_quantity_outdata$Month)
apac_quantity_global_pred_out <- predict(apac_quantity_lmfit,apac_quantity_timevals_out)
apac_quantity_fcast <- apac_quantity_global_pred_out[43:48]

#Now, let's compare our prediction with the actual values, using MAPE

apac_quantity_MAPE_class_dec <- accuracy(apac_quantity_fcast,apac_quantity_outdata[,2])[5]
apac_quantity_MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

apac_quantity_class_dec_pred <- c(ts(apac_quantity_global_pred),ts(apac_quantity_global_pred_out[43:48]))
#apac_quantity_class_dec_pred <- ts(apac_quantity_global_pred_out)
plot(apac_quantity_total_timeser, col = "black",main="APAC Consumer - Quantity Forecasting - Classical Decomposition")
lines(apac_quantity_class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

apac_quantity_autoarima <- auto.arima(apac_quantity_timeser)
apac_quantity_autoarima
tsdiag(apac_quantity_autoarima)
plot(apac_quantity_autoarima$x, col="black", main="APAC Consumer - Quantity - Auto Arima")
lines(fitted(apac_quantity_autoarima), col="red")

#Again, let's check if the residual series is white noise

apac_quantity_resi_auto_arima <- apac_quantity_timeser - fitted(apac_quantity_autoarima)

adf.test(apac_quantity_resi_auto_arima,alternative = "stationary")
kpss.test(apac_quantity_resi_auto_arima)

#Also, let's evaluate the model using MAPE
apac_quantity_fcast_auto_arima <- predict(apac_quantity_autoarima, n.ahead = 6)

apac_quantity_MAPE_auto_arima <- accuracy(apac_quantity_fcast_auto_arima$pred,apac_quantity_outdata[,2])[5]
apac_quantity_MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

apac_quantity_auto_arima_pred <- c(fitted(apac_quantity_autoarima),ts(apac_quantity_fcast_auto_arima$pred))
plot(apac_quantity_total_timeser, col = "black", main="APAC Consumer - Quantity Forecasting - Auto Arima")
lines(apac_quantity_auto_arima_pred, col = "red")


########################  END OF ANALYSIS   ###########################
