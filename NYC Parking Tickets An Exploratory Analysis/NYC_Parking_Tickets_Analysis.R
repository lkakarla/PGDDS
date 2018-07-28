########################################################
# Examine the data
########################################################

#Load SparkR library and other required libraries

library(SparkR);
library(ggplot2);
library(scales);

# Initialize SparkR Session
sparkR.session(master = "local");


#Load 2015 Year CSV data file
nyc_2015 <- SparkR::read.df("s3://lk-ds-s3/nyc/Parking_Violations_Issued_-_Fiscal_Year_2015.csv", source = "csv", inferSchema = "true", header = "true");


#Load 2016 Year CSV data file
nyc_2016 <- SparkR::read.df("s3://lk-ds-s3/nyc/Parking_Violations_Issued_-_Fiscal_Year_2016.csv", source = "csv", inferSchema = "true", header = "true");


#Load 2017 Year CSV data file
nyc_2017 <- SparkR::read.df("s3://lk-ds-s3/nyc/Parking_Violations_Issued_-_Fiscal_Year_2017.csv", source = "csv", inferSchema = "true", header = "true");


#============================
#DATA CLEANING
#============================

#Remove duplicate rows in data set for each year.
nyc_unique_2015 <- dropDuplicates(nyc_2015)
nyc_unique_2016 <- dropDuplicates(nyc_2016)
nyc_unique_2017 <- dropDuplicates(nyc_2017)



#Replace spaces in all the 3 data frames with under score
colnames(nyc_unique_2015) <- gsub("\\s+","_",gsub("^\\s+|\\s+$", "" , colnames(nyc_unique_2015)))
colnames(nyc_unique_2016) <- gsub("\\s+","_",gsub("^\\s+|\\s+$", "" , colnames(nyc_unique_2016)))
colnames(nyc_unique_2017) <- gsub("\\s+","_",gsub("^\\s+|\\s+$", "" , colnames(nyc_unique_2017)))


#Parse Date Columns

# Convert Issue_Date column to Date Type
nyc_unique_2015$Issue_Date_Parsed <- to_date(nyc_unique_2015$Issue_Date, 'MM/dd/yyyy')
nyc_unique_2016$Issue_Date_Parsed <- to_date(nyc_unique_2016$Issue_Date, 'MM/dd/yyyy')
nyc_unique_2017$Issue_Date_Parsed <- to_date(nyc_unique_2017$Issue_Date, 'MM/dd/yyyy')


#------------
# ASSUMPTION: 
# Considered only records having issue date relevant to Fiscal year(July 1 to June 30)
#------------


# Following commands will discard the records with incorrect Fiscal year.

#Filter only Fiscal Year 2015 (July 2014 to June 2015) Records
nyc_filter_2015 <- filter(nyc_unique_2015, ((year(nyc_unique_2015$Issue_Date_Parsed) == 2014  &  
                                               (month(nyc_unique_2015$Issue_Date_Parsed) >= 7 &  
                                                  month(nyc_unique_2015$Issue_Date_Parsed) <= 12)) | 
                                              (year(nyc_unique_2015$Issue_Date_Parsed) == 2015  &  
                                                 (month(nyc_unique_2015$Issue_Date_Parsed) >= 1 &
                                                    month(nyc_unique_2015$Issue_Date_Parsed) <= 6))))


#Filter only Fiscal Year 2016 (July 2015 to June 2016) Records
nyc_filter_2016 <- filter(nyc_unique_2016, ((year(nyc_unique_2016$Issue_Date_Parsed) == 2015  &  
                                               (month(nyc_unique_2016$Issue_Date_Parsed) >= 7 &  
                                                  month(nyc_unique_2016$Issue_Date_Parsed) <= 12)) | 
                                              (year(nyc_unique_2016$Issue_Date_Parsed) == 2016  &  
                                                 (month(nyc_unique_2016$Issue_Date_Parsed) >= 1 &
                                                    month(nyc_unique_2016$Issue_Date_Parsed) <= 6))))


#Filter only Fiscal Year 2017 (July 2016 to June 2017) Records
nyc_filter_2017 <- filter(nyc_unique_2017, ((year(nyc_unique_2017$Issue_Date_Parsed) == 2016  &  
                                               (month(nyc_unique_2017$Issue_Date_Parsed) >= 7 &  
                                                  month(nyc_unique_2017$Issue_Date_Parsed) <= 12)) | 
                                              (year(nyc_unique_2017$Issue_Date_Parsed) == 2017  &  
                                                 (month(nyc_unique_2017$Issue_Date_Parsed) >= 1 &
                                                    month(nyc_unique_2017$Issue_Date_Parsed) <= 6))))


#------------
# ASSUMPTION: 
# Interpreted State Code 99 as Invalid
#------------


# Following commands will discard the records with incorrect State Code.

#Discard records having State Code as 99
nyc_filter_2015 <- filter(nyc_filter_2015, nyc_filter_2015$Registration_State != "99")

nyc_filter_2016 <- filter(nyc_filter_2016, nyc_filter_2016$Registration_State != "99")

nyc_filter_2017 <- filter(nyc_filter_2017, nyc_filter_2017$Registration_State != "99")

#Plots and  Histograms



#--------------------------------------------------
##  Q1. Find total number of tickets for each year.
#--------------------------------------------------


#------------
# ASSUMPTION:
# Considered "Summons Number" as primary key.
# Counting Number of Tickets based on number of unique records for column 'Summons Number'
# Interpreted "Year" in the question as "Fiscal Year"
#------------


################   2015   ################
# Below command returns total number of tickets for year 2015
collect(select(nyc_filter_2015, countDistinct(nyc_filter_2015$Summons_Number)))

#Total number of tickets for year 2015
#10558797



################   2016   ################
# Below command returns total number of tickets for year 2016
collect(select(nyc_filter_2016, countDistinct(nyc_filter_2016$Summons_Number)))

#Total number of tickets for year 2016
#10357238



################   2017   ################
# Below command returns total number of tickets for year 2017
collect(select(nyc_filter_2017, countDistinct(nyc_filter_2017$Summons_Number)))

#Total number of tickets for year 2017
#10504843



#------------------------------------------------------------------------------------
##  Q2. Find out how many unique states the cars which got parking tickets came from.
#------------------------------------------------------------------------------------

################   2015   ################
# Below command returns total number of unique states the cars which got parking tickets came from. for year 2015
collect(select(nyc_filter_2015, countDistinct(nyc_filter_2015$Registration_State)))

#Total number of tickets for year 2015
#68

#Below command summarizes data by  Registration_State wise for year 2015.
state_summary_2015 <- collect(summarize(groupBy(nyc_filter_2015, nyc_filter_2015$Registration_State),
                                        count = n(nyc_filter_2015$Registration_State)))

#Below command plot bar chart to show Registration_State wise data summary
ggplot(state_summary_2015, aes(x = Registration_State, y = count)) + 
  geom_bar(stat = "identity") +
  xlab("Registration State") + 
  ylab("Frequency for 2015") +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) +
  geom_text(aes(label = count),  angle=90 , hjust=0.1)

  
   
################   2016   ################
# Below command returns total number of unique states the cars which got parking tickets came from. for year 2016
collect(select(nyc_filter_2016, countDistinct(nyc_filter_2016$Registration_State)))

#Total number of tickets for year 2016
#67

#Below command summarizes data by  Registration_State wise for year 2016
state_summary_2016 <- collect(summarize(groupBy(nyc_filter_2016, nyc_filter_2016$Registration_State),
                                        count = n(nyc_filter_2016$Registration_State)))

#Below command plot bar chart to show Registration_State wise data summary
ggplot(state_summary_2016, aes(x = Registration_State, y = count)) + 
  geom_bar(stat = "identity") +
  xlab("Registration State") + 
  ylab("Frequency for 2016") +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) +
  geom_text(aes(label = count),  angle=90 , hjust=0.1)

     

################   2017   ################
# Below command returns total number of unique states the cars which got parking tickets came from. for year 2017
collect(select(nyc_filter_2017, countDistinct(nyc_filter_2017$Registration_State)))

#Total number of tickets for year 2017
#66

#Below command summarizes data by  Registration_State wise for year 2017.
state_summary_2017 <- collect(summarize(groupBy(nyc_filter_2017, nyc_filter_2017$Registration_State),
                                        count = n(nyc_filter_2017$Registration_State)))

#Below command plot bar chart to show Registration_State wise data summary
ggplot(state_summary_2017, aes(x = Registration_State, y = count)) + 
  geom_bar(stat = "identity") +
  xlab("Registration State") + 
  ylab("Frequency for 2017") +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) +
  geom_text(aes(label = count),  angle=90 , hjust=0.1)


  
#-------------------------------------------------------------------------------------------------------------------------------
##  Q3. Some parking tickets don’t have addresses on them, which is cause for concern. Find out how many such tickets there are.
#-------------------------------------------------------------------------------------------------------------------------------


#------------
# ASSUMPTION:
# We've interpreted "address" in the question as "Violation Location" in the given data set.
# Considered records having "Violation Location" as NULL for analysis
#------------


################   2015   ################
# Below command returns total number of tickets having Violation Location as NULL for year 2015
nrow(filter(nyc_filter_2015, isNull(nyc_filter_2015$Violation_Location)))

#Total number of tickets having Violation Location as NULL for year 2015
#1602550



################   2016   ################
# Below command returns total number of tickets having Violation Location as NULL for year 2016
nrow(filter(nyc_filter_2016, isNull(nyc_filter_2016$Violation_Location)))

#Total number of tickets having Violation Location as NULL for year 2016
#1806516



################   2017   ################
# Below command returns total number of tickets having Violation Location as NULL for year 2017
nrow(filter(nyc_filter_2017, isNull(nyc_filter_2017$Violation_Location)))

#Total number of tickets having Violation Location as NULL for year 2017
#1949347



########################################################
# Aggregation tasks
########################################################

#------------------------------------------------------------------------------------------------
##  Q1. How often does each violation code occur? (frequency of violation codes - find the top 5)
#------------------------------------------------------------------------------------------------

################   2015   ################
# Below commands returns top 5 violation codes for year 2015
nyc_vc_2015 <- summarize(groupBy(nyc_filter_2015, nyc_filter_2015$Violation_Code), 
                              count= n(nyc_filter_2015$Violation_Code))
                        
head(arrange(nyc_vc_2015, desc(nyc_vc_2015$count)), n=5)


#Top 5 violation codes for year 2015
#------------------------
# Violation_Code   count
#------------------------
# 21               1592519
# 38               1397978
# 14               969177
# 36               824610
# 37               784747



################   2016   ################
# Below command returns total number of tickets having Violation Location as NULL for year 2016
nyc_vc_2016 <- summarize(groupBy(nyc_filter_2016, nyc_filter_2016$Violation_Code), 
                         count= n(nyc_filter_2016$Violation_Code))

head(arrange(nyc_vc_2016, desc(nyc_vc_2016$count)), n=5)

#Top 5 violation codes for year 2016
#------------------------
# Violation_Code   count
#------------------------
# 21               1490775
# 36               1232910
# 38               1125950
# 14               857521
# 37               677448



################   2017   ################
# Below command returns total number of tickets having Violation Location as NULL for year 2017
nyc_vc_2017 <- summarize(groupBy(nyc_filter_2017, nyc_filter_2017$Violation_Code), 
                         count= n(nyc_filter_2017$Violation_Code))

head(arrange(nyc_vc_2017, desc(nyc_vc_2017$count)), n=5)

#Top 5 violation codes for year 2017
#------------------------
# Violation_Code   count
#------------------------
# 21               1494775
# 36               1345192
# 38               1049457
# 14               877730
# 20               607510



#-------------------------------------------------------------------------------------------------------------------------------
##  Q2. How often does each vehicle body type get a parking ticket? How about the vehicle make? (find the top 5 for both)
#-------------------------------------------------------------------------------------------------------------------------------


################   2015   ################
# Below commands returns top 5 vehicle body types got a parking ticket for year 2015

nyc_vbt_2015 <- summarize(groupBy(nyc_filter_2015, nyc_filter_2015$Vehicle_Body_Type), 
                          count= n(nyc_filter_2015$Vehicle_Body_Type))

head(arrange(nyc_vbt_2015, desc(nyc_vbt_2015$count)), n=5)

#Top 5 vehicle body types got a parking ticket for year 2015
#----------------------------
# Vehicle_Body_Type   count
#----------------------------
# SUBN                3604356
# 4DSD                3231197
# VAN                 1668301
# DELV                869720
# SDN                 486665


# Below commands returns top 5 vehicle make got a parking ticket for year 2015

nyc_vm_2015 <- summarize(groupBy(nyc_filter_2015, nyc_filter_2015$Vehicle_Make), 
                         count= n(nyc_filter_2015$Vehicle_Make))

head(arrange(nyc_vm_2015, desc(nyc_vm_2015$count)), n=5)

#Top 5 vehicle make got a parking ticket for year 2015
#----------------------------
# Vehicle_Make   count
#----------------------------
# FORD           1471180
# TOYOT          1170476
# HONDA          1061237
# NISSA          872262
# CHEVR          869751




################   2016   ################
# Below commands returns top 5 vehicle body types got a parking ticket for year 2016s

nyc_vbt_2016 <- summarize(groupBy(nyc_filter_2016, nyc_filter_2016$Vehicle_Body_Type), 
                          count= n(nyc_filter_2016$Vehicle_Body_Type))

head(arrange(nyc_vbt_2016, desc(nyc_vbt_2016$count)), n=5)

#Top 5 vehicle body types got a parking ticket for year 2016
#----------------------------
# Vehicle_Body_Type   count
#----------------------------
# SUBN                3384675
# 4DSD                2933883
# VAN                 1485753
# DELV                734962
# SDN                 392262


# Below commands returns top 5 vehicle make got a parking ticket for year 2016

nyc_vm_2016 <- summarize(groupBy(nyc_filter_2016, nyc_filter_2016$Vehicle_Make), 
                         count= n(nyc_filter_2016$Vehicle_Make))

head(arrange(nyc_vm_2016, desc(nyc_vm_2016$count)), n=5)

#Top 5 vehicle make got a parking ticket for year 2016
#----------------------------
# Vehicle_Make   count
#----------------------------
# FORD           1293656
# TOYOT          1126329
# HONDA          987863
# NISSA          813695
# CHEVR          741641



################   2017   ################
# Below commands returns top 5 vehicle body types got a parking ticket for year 2017

nyc_vbt_2017 <- summarize(groupBy(nyc_filter_2017, nyc_filter_2017$Vehicle_Body_Type), 
                          count= n(nyc_filter_2017$Vehicle_Body_Type))

head(arrange(nyc_vbt_2017, desc(nyc_vbt_2017$count)), n=5)

#Top 5 vehicle body types got a parking ticket for year 2017
#----------------------------
# Vehicle_Body_Type   count
#----------------------------
# SUBN                3624913
# 4DSD                3016847
# VAN                 1380288
# DELV                668485
# SDN                 405170


# Below commands returns top 5 vehicle make got a parking ticket for year 2017

nyc_vm_2017 <- summarize(groupBy(nyc_filter_2017, nyc_filter_2017$Vehicle_Make), 
                         count= n(nyc_filter_2017$Vehicle_Make))

head(arrange(nyc_vm_2017, desc(nyc_vm_2017$count)), n=5)

#Top 5 vehicle make got a parking ticket for year 2017
#----------------------------
# Vehicle_Make   count
#----------------------------
# FORD           1247606
# TOYOT          1176814
# HONDA          1049257
# NISSA          893226
# CHEVR          696596



#-------------------------------------------------------------------------------------------------------------------------------
##  Q3. A precinct is a police station that has a certain zone of the city under its command. Find the (5 highest) frequencies of:
##  Violating Precincts (this is the precinct of the zone where the violation occurred)
##  Issuing Precincts (this is the precinct that issued the ticket)
#-------------------------------------------------------------------------------------------------------------------------------

#------------
# ASSUMPTION:
# Considered Precint '0' as valid Precint
#------------

################   2015   ################
# Below commands returns 5 highest frequencies of Violating Precincts for year 2015

nyc_vp_2015 <- summarize(groupBy(nyc_filter_2015, nyc_filter_2015$Violation_Precinct), 
                          count= n(nyc_filter_2015$Violation_Precinct))

head(arrange(nyc_vp_2015, desc(nyc_vp_2015$count)), n=5)

#5 highest frequencies of Violating Precincts for year 2015
#------------------------------
# Violation_Precinct   count
#------------------------------
# 0                    1602550
# 19                   588293
# 18                   419173
# 14                   400926
# 1                    322696


# Below commands returns 5 highest frequencies of Issuing Precincts for year 2015

nyc_ip_2015 <- summarize(groupBy(nyc_filter_2015, nyc_filter_2015$Issuer_Precinct), 
                         count= n(nyc_filter_2015$Issuer_Precinct))

head(arrange(nyc_ip_2015, desc(nyc_ip_2015$count)), n=5)

#5 highest frequencies of Issuing Precincts for year 2015
#----------------------------
# Issuer_Precinct   count
#----------------------------
# 0                 1828089
# 19                570884
# 18                409740
# 14                386171
# 1                 313469



################   2016   ################
# Below commands returns 5 highest frequencies of Violating Precincts for year 2016

nyc_vp_2016 <- summarize(groupBy(nyc_filter_2016, nyc_filter_2016$Violation_Precinct), 
                          count= n(nyc_filter_2016$Violation_Precinct))

head(arrange(nyc_vp_2016, desc(nyc_vp_2016$count)), n=5)

#5 highest frequencies of Violating Precincts for year 2016
#------------------------------
# Violation_Precinct   count
#------------------------------
# 0                    1806515
# 19                   544847
# 18                   324632
# 14                   316995
# 1                    298075


# Below commands returns 5 highest frequencies of Issuing Precincts for year 2016

nyc_ip_2016 <- summarize(groupBy(nyc_filter_2016, nyc_filter_2016$Issuer_Precinct), 
                         count= n(nyc_filter_2016$Issuer_Precinct))

head(arrange(nyc_ip_2016, desc(nyc_ip_2016$count)), n=5)

#5 highest frequencies of Issuing Precincts for year 2016
#----------------------------
# Issuer_Precinct   count
#----------------------------
# 0                 2061872
# 19                531651
# 18                316730
# 14                308893
# 1                 289897



################   2017   ################
# Below commands returns 5 highest frequencies of Violating Precincts for year 2017

nyc_vp_2017 <- summarize(groupBy(nyc_filter_2017, nyc_filter_2017$Violation_Precinct), 
                          count= n(nyc_filter_2017$Violation_Precinct))

head(arrange(nyc_vp_2017, desc(nyc_vp_2017$count)), n=5)

#5 highest frequencies of Violating Precincts for year 2017
#------------------------------
# Violation_Precinct   count
#------------------------------
# 0                    1949347
# 19                   527366
# 14                   346580
# 1                    325871
# 18                   301093


# Below commands returns 5 highest frequencies of Issuing Precincts for year 2017

nyc_ip_2017 <- summarize(groupBy(nyc_filter_2017, nyc_filter_2017$Issuer_Precinct), 
                         count= n(nyc_filter_2017$Issuer_Precinct))

head(arrange(nyc_ip_2017, desc(nyc_ip_2017$count)), n=5)

#5 highest frequencies of Issuing Precincts for year 2017
#----------------------------
# Issuer_Precinct   count
#----------------------------
# 0                 2248357
# 19                514061
# 14                340003
# 1                 316090
# 18                291563



#-------------------------------------------------------------------------------------------------------------------------------
##  Q4. Find the violation code frequency across 3 precincts which have issued the most number of tickets - do these precinct zones have an exceptionally high frequency of certain violation codes? Are these codes common across precincts?
#-------------------------------------------------------------------------------------------------------------------------------

#------------
# ASSUMPTION:
# Considered Top 5 violation codes for comparison across precincts.
#------------

################   2015   ################
# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2015, "nyc_view_2015")

# Below commands returns the violation code frequency across 3 precincts which have issued the most number of tickets for year 2015
nyc_vc_freq_2015 <- SparkR::sql("SELECT Violation_Code, COUNT(1) as count 
                                 FROM nyc_view_2015 p
                                 JOIN
                                 (SELECT Issuer_Precinct, COUNT(1) as count 
                                  FROM nyc_view_2015 
                                  GROUP BY Issuer_Precinct 
                                  ORDER BY count DESC
                                  LIMIT 3) c
                                 ON p.Issuer_Precinct = c.Issuer_Precinct
                                 GROUP BY Violation_Code 
                                 ORDER BY count DESC")

#Below command will return Top 5 violation codes across 3 precincts which have issued the most number of tickets for year 2015
head(nyc_vc_freq_2015, n = 5)

#Top 5 violation codes across 3 precincts which have issued the most number of tickets for year 2015
#------------------------
# Violation_Code   count
#------------------------
# 36               824610
# 7                619543
# 21               263242
# 14               195009
# 5                144352



################   2016   ################
# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2016, "nyc_view_2016")

# Below commands returns the violation code frequency across 3 precincts which have issued the most number of tickets for year 2016
nyc_vc_freq_2016 <- SparkR::sql("SELECT Violation_Code, COUNT(1) as count 
                                 FROM nyc_view_2016 p
                                 JOIN
                                 (SELECT Issuer_Precinct, COUNT(1) as count 
                                  FROM nyc_view_2016 
                                  GROUP BY Issuer_Precinct 
                                  ORDER BY count DESC
                                  LIMIT 3) c
                                 ON p.Issuer_Precinct = c.Issuer_Precinct
                                 GROUP BY Violation_Code 
                                 ORDER BY count DESC")

#Below command will return Top 5 violation codes across 3 precincts which have issued the most number of tickets for year 2016
head(nyc_vc_freq_2016, n = 5)

#Top 5 violation codes across 3 precincts which have issued the most number of tickets for year 2016
#------------------------
# Violation_Code   count
#------------------------
# 36               1232909
# 7                457855
# 21               283745
# 14               164411
# 5                106607


################   2017   ################

# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2017, "nyc_view_2017")

# Below command returns the violation code frequency across 3 precincts which have issued the most number of tickets for year 2017
nyc_vc_freq_2017 <- SparkR::sql("SELECT Violation_Code, COUNT(1) as count 
                                 FROM nyc_view_2017 p
                                 JOIN
                                 (SELECT Issuer_Precinct, COUNT(1) as count 
                                  FROM nyc_view_2017 
                                  GROUP BY Issuer_Precinct 
                                  ORDER BY count DESC
                                  LIMIT 3) c
                                 ON p.Issuer_Precinct = c.Issuer_Precinct
                                 GROUP BY Violation_Code 
                                 ORDER BY count DESC")

#Below command will return Top 5 violation codes across 3 precincts which have issued the most number of tickets for year 2017
head(nyc_vc_freq_2017, n = 5)

#Top 5 violation codes across 3 precincts which have issued the most number of tickets for year 2017
#------------------------
# Violation_Code   count
#------------------------
# 36               1345192
# 7                464676
# 21               310394
# 14               136277
# 5                130958



#--------------------------------------------
# Comparison between the 3 years
#--------------------------------------------



#-------------------------------------------------------------------------------------------------------------------------------
##  Q5. You’d want to find out the properties of parking violations across different times of the day:
##  The Violation Time field is specified in a strange format. Find a way to make this into a time attribute that you can use to divide into groups.
##  Find a way to deal with missing values, if any.
##  Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. For each of these groups, find the 3 most commonly occurring violations
##  Now, try another direction. For the 3 most commonly occurring violation codes, find the most common times of day (in terms of the bins from the previous part)
#-------------------------------------------------------------------------------------------------------------------------------


#------------
# ASSUMPTION:
# Ignored Missing Values, as the percentage of missing values compared to overall data set is very less.

# Considered Hour bins as below
# Bin 1 -- 0 to 3 hrs
# Bin 2 -- 4 to 7 hrs
# Bin 3 -- 8 to 11 hrs
# Bin 4 -- 12 to 15 hrs
# Bin 5 -- 16 to 19 hrs
# Bin 6 -- 20 to 23 hrs
#------------

################   2015   ################
# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2015, "nyc_view_2015");

# Below commands extracts hour in 24 hours format from Violation Time Column
nyc_filter_2015 <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN SUBSTR(TRIM(Violation_Time), -1) = 'P' 
                                         THEN IF(SUBSTR(TRIM(Violation_Time), 1, 2) = 12, SUBSTR(TRIM(Violation_Time), 1, 2) + 0, SUBSTR(TRIM(Violation_Time), 1, 2) + 12)
                                    WHEN SUBSTR(TRIM(Violation_Time), -1) = 'A' THEN SUBSTR(TRIM(Violation_Time), 1, 2) + 0
                                END  AS Violation_Time_24Hr
                                FROM nyc_view_2015")

# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2015, "nyc_view_2015");

# Below commands divides 24 hours into 6 equal discrete bins of time.
nyc_filter_2015 <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN (Violation_Time_24Hr >= 0 AND Violation_Time_24Hr <= 3) THEN 1
                                    WHEN (Violation_Time_24Hr >= 4 AND Violation_Time_24Hr <= 5) THEN 2
                                    WHEN (Violation_Time_24Hr >= 6 AND Violation_Time_24Hr <= 11) THEN 3
                                    WHEN (Violation_Time_24Hr >= 12 AND Violation_Time_24Hr <= 15) THEN 4
                                    WHEN (Violation_Time_24Hr >= 16 AND Violation_Time_24Hr <= 19) THEN 5
                                    WHEN (Violation_Time_24Hr >= 20 AND Violation_Time_24Hr <= 23) THEN 6
                                END  AS Hour_Bin
                                FROM nyc_view_2015")

# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2015, "nyc_view_2015");

#Below command will return  3 most commonly occurring violations for each discreate bins of time for year 2015
nyc_hour_1_top3_vc_2015 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count 
                                        FROM nyc_view_2015 
                                        WHERE Hour_Bin = 1
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_2_top3_vc_2015 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2015 
                                        WHERE Hour_Bin = 2
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_3_top3_vc_2015 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2015 
                                        WHERE Hour_Bin = 3
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_4_top3_vc_2015 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2015 
                                        WHERE Hour_Bin = 4
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_5_top3_vc_2015 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2015 
                                        WHERE Hour_Bin = 5
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_6_top3_vc_2015 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2015 
                                        WHERE Hour_Bin = 6
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

unions_2015 <- rbind(nyc_hour_1_top3_vc_2015, nyc_hour_2_top3_vc_2015, nyc_hour_3_top3_vc_2015, nyc_hour_4_top3_vc_2015, nyc_hour_5_top3_vc_2015, nyc_hour_6_top3_vc_2015)

head(unions_2015, n=20);


#3 most commonly occurring violations for each discrete bins of time for year 2015
#----------------------------------------
# Hour_Bin   Violation_Code   count
#---------------------------------------
#     1             21   67638
#     1             40   38350
#     1             78   35674

#     2             40   23190
#     2             14   18095
#     2              7   14933

#     3             21 1372141
#     3             38  476141
#     3             36  450539

#     4             38  600861
#     4             37  440471
#     4             36  350967

#     5             38  254773
#     5             37  184227
#     5             14  156727

#     6              7   76972
#     6             38   65109
#     6             40   48087




# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2015, "nyc_view_2015")

#Below commands return most common times of day, when 3 most commonly violation codes are occurring for year 2015
nyc_vc_hour_freq_2015 <- SparkR::sql("SELECT Hour_Bin, COUNT(1) as count 
                                 FROM nyc_view_2015 p
                                 JOIN
                                 (SELECT Violation_Code, COUNT(1) as count 
                                  FROM nyc_view_2015 
                                  GROUP BY Violation_Code 
                                  ORDER BY count DESC
                                  LIMIT 3) c
                                 ON p.Violation_Code = c.Violation_Code
                                 GROUP BY Hour_Bin 
                                 ORDER BY count DESC")

#Below commands return most common times of day, when 3 most commonly violation codes are occurring for year 2015
head(nyc_vc_hour_freq_2015, n = 6)

#Most common times of day, when 3 most commonly violation codes are occurring for year 2015
#------------------------
# Hour_Bin  count
#------------------------
#     3     2282575
#     4     1031802
#     5     412328
#     6     113602
#     1     96052
#     2     23266




################   2016   ################
# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2016, "nyc_view_2016");

# Below commands extracts hour in 24 hours format from Violation Time Column
nyc_filter_2016 <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN SUBSTR(TRIM(Violation_Time), -1) = 'P' 
                                         THEN IF(SUBSTR(TRIM(Violation_Time), 1, 2) = 12, SUBSTR(TRIM(Violation_Time), 1, 2) + 0, SUBSTR(TRIM(Violation_Time), 1, 2) + 12)
                                    WHEN SUBSTR(TRIM(Violation_Time), -1) = 'A' THEN SUBSTR(TRIM(Violation_Time), 1, 2) + 0
                                END  AS Violation_Time_24Hr
                                FROM nyc_view_2016")

# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2016, "nyc_view_2016");

# Below commands divides 24 hours into 6 equal discrete bins of time.
nyc_filter_2016 <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN (Violation_Time_24Hr >= 0 AND Violation_Time_24Hr <= 3) THEN 1
                                    WHEN (Violation_Time_24Hr >= 4 AND Violation_Time_24Hr <= 5) THEN 2
                                    WHEN (Violation_Time_24Hr >= 6 AND Violation_Time_24Hr <= 11) THEN 3
                                    WHEN (Violation_Time_24Hr >= 12 AND Violation_Time_24Hr <= 15) THEN 4
                                    WHEN (Violation_Time_24Hr >= 16 AND Violation_Time_24Hr <= 19) THEN 5
                                    WHEN (Violation_Time_24Hr >= 20 AND Violation_Time_24Hr <= 23) THEN 6
                                END  AS Hour_Bin
                                FROM nyc_view_2016")

# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2016, "nyc_view_2016");

#Below command will return  3 most commonly occurring violations for each discreate bins of time for year 2016
nyc_hour_1_top3_vc_2016 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count 
                                        FROM nyc_view_2016 
                                        WHERE Hour_Bin = 1
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_2_top3_vc_2016 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2016 
                                        WHERE Hour_Bin = 2
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_3_top3_vc_2016 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2016 
                                        WHERE Hour_Bin = 3
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_4_top3_vc_2016 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2016 
                                        WHERE Hour_Bin = 4
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_5_top3_vc_2016 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2016 
                                        WHERE Hour_Bin = 5
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_6_top3_vc_2016 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2016 
                                        WHERE Hour_Bin = 6
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

unions_2016 <- rbind(nyc_hour_1_top3_vc_2016, nyc_hour_2_top3_vc_2016, nyc_hour_3_top3_vc_2016, nyc_hour_4_top3_vc_2016, nyc_hour_5_top3_vc_2016, nyc_hour_6_top3_vc_2016)

head(unions_2016, n=20);

#3 most commonly occurring violations for each discrete bins of time for year 2016
#----------------------------------------
# Hour_Bin   Violation_Code   count
#---------------------------------------
#     1             21   65759
#     1             40   35611
#     1             78   28096

#     2             40   20961
#     2             14   18901
#     2             20   11308

#     3             21 1284358
#     3             36  655669
#     3             14  389865

#     4             36  536532
#     4             38  480483
#     4             37  378180

#     5             38  208553
#     5             37  159722
#     5             14  131900

#     6              7   56835
#     6             38   52566
#     6             40   43565
 
 


# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2016, "nyc_view_2016")

#Below commands return most common times of day, when 3 most commonly violation codes are occurring for year 2016
nyc_vc_hour_freq_2016 <- SparkR::sql("SELECT Hour_Bin, COUNT(1) as count 
                                 FROM nyc_view_2016 p
                                 JOIN
                                 (SELECT Violation_Code, COUNT(1) as count 
                                  FROM nyc_view_2016 
                                  GROUP BY Violation_Code 
                                  ORDER BY count DESC
                                  LIMIT 3) c
                                 ON p.Violation_Code = c.Violation_Code
                                 GROUP BY Hour_Bin 
                                 ORDER BY count DESC")

#Below commands return most common times of day, when 3 most commonly violation codes are occurring for year 2016
head(nyc_vc_hour_freq_2016, n = 6)

#Most common times of day, when 3 most commonly violation codes are occurring for year 2016
#------------------------
# Hour_Bin   count
#------------------------
#   3        2323764
#   4        1152077
#   5        249800
#   1        66076
#   6        52964
#   2        4907





################   2017   ################
# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2017, "nyc_view_2017");

# Below commands extracts hour in 24 hours format from Violation Time Column
nyc_filter_2017 <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN SUBSTR(TRIM(Violation_Time), -1) = 'P' 
                                         THEN IF(SUBSTR(TRIM(Violation_Time), 1, 2) = 12, SUBSTR(TRIM(Violation_Time), 1, 2) + 0, SUBSTR(TRIM(Violation_Time), 1, 2) + 12)
                                    WHEN SUBSTR(TRIM(Violation_Time), -1) = 'A' THEN SUBSTR(TRIM(Violation_Time), 1, 2) + 0
                                END  AS Violation_Time_24Hr
                                FROM nyc_view_2017")

# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2017, "nyc_view_2017");

# Below commands divides 24 hours into 6 equal discrete bins of time.
nyc_filter_2017 <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN (Violation_Time_24Hr >= 0 AND Violation_Time_24Hr <= 3) THEN 1
                                    WHEN (Violation_Time_24Hr >= 4 AND Violation_Time_24Hr <= 5) THEN 2
                                    WHEN (Violation_Time_24Hr >= 6 AND Violation_Time_24Hr <= 11) THEN 3
                                    WHEN (Violation_Time_24Hr >= 12 AND Violation_Time_24Hr <= 15) THEN 4
                                    WHEN (Violation_Time_24Hr >= 16 AND Violation_Time_24Hr <= 19) THEN 5
                                    WHEN (Violation_Time_24Hr >= 20 AND Violation_Time_24Hr <= 23) THEN 6
                                END  AS Hour_Bin
                                FROM nyc_view_2017")

# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2017, "nyc_view_2017");

#Below command will return  3 most commonly occurring violations for each discreate bins of time for year 2017
nyc_hour_1_top3_vc_2017 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count 
                                        FROM nyc_view_2017 
                                        WHERE Hour_Bin = 1
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_2_top3_vc_2017 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2017 
                                        WHERE Hour_Bin = 2
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_3_top3_vc_2017 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2017 
                                        WHERE Hour_Bin = 3
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_4_top3_vc_2017 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2017 
                                        WHERE Hour_Bin = 4
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_5_top3_vc_2017 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2017 
                                        WHERE Hour_Bin = 5
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

nyc_hour_6_top3_vc_2017 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As count
                                        FROM nyc_view_2017 
                                        WHERE Hour_Bin = 6
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY count DESC
                                        LIMIT 3")

unions_2017 <- rbind(nyc_hour_1_top3_vc_2017, nyc_hour_2_top3_vc_2017, nyc_hour_3_top3_vc_2017, nyc_hour_4_top3_vc_2017, nyc_hour_5_top3_vc_2017, nyc_hour_6_top3_vc_2017)

head(unions_2017, n=20);


#3 most commonly occurring violations for each discrete bins of time for year 2017
#----------------------------------------
# Hour_Bin   Violation_Code   count
#---------------------------------------
#     1             21   71306
#     1             40   44161
#     1             14   28470

#     2             40   25365
#     2             14   15176
#     2             20   13379

#     3             21 1268233
#     3             36  756556
#     3             14  393574

#     4             36  563548
#     4             38  456919
#     4             37  332335

#     5             38  200613
#     5             37  143521
#     5             14  141816

#     6              7   59133
#     6             38   46473
#     6             14   43635


# For using SQL, create a temporary view
createOrReplaceTempView(nyc_filter_2017, "nyc_view_2017")

#Below commands return most common times of day, when 3 most commonly violation codes are occurring for year 2017
nyc_vc_hour_freq_2017 <- SparkR::sql("SELECT Hour_Bin, COUNT(1) as count 
                                 FROM nyc_view_2017 p
                                 JOIN
                                 (SELECT Violation_Code, COUNT(1) as count 
                                  FROM nyc_view_2017 
                                  GROUP BY Violation_Code 
                                  ORDER BY count DESC
                                  LIMIT 3) c
                                 ON p.Violation_Code = c.Violation_Code
                                 GROUP BY Hour_Bin 
                                 ORDER BY count DESC")

#Below commands return most common times of day, when 3 most commonly violation codes are occurring for year 2017
head(nyc_vc_hour_freq_2017, n = 6)

#Most common times of day, when 3 most commonly violation codes are occurring for year 2017
#------------------------
# Hour_Bin   count
#------------------------
#   3        2369651
#   4        1169562
#   5        226193
#   1        71660
#   6        46812
#   2        5505






#-------------------------------------------------------------------------------------------------------------------------------
##  Q6. Let’s try and find some seasonality in this data
##  First, divide the year into some number of seasons, and find frequencies of tickets for each season.
##  Then, find the 3 most common violations for each of these season
#-------------------------------------------------------------------------------------------------------------------------------


#------------
# ASSUMPTION:
# Considered Seasons as below
# Season 1 -- January to March
# Season 2 -- April to June
# Season 3 -- July to September
# Season 4 -- October to December
#------------

################   2015   ################
# Below commands returns the violation code frequency across 3 precincts which have issued the most number of tickets for year 2015

# Create new column "Month" based on "Issue Date".
nyc_filter_2015 <- withColumn(nyc_filter_2015, "Month", month(nyc_filter_2015$Issue_Date_Parsed))

# For using SQL, you need to create a temporary view
createOrReplaceTempView(nyc_filter_2015, "nyc_view_2015");

# Below command divides data into season based on month.
nyc_filter_2015 <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN (Month >= 1 AND Month <= 3) THEN 1
                                    WHEN (Month >= 4 AND Month <= 6) THEN 2
                                    WHEN (Month >= 7 AND Month <= 9) THEN 3
                                    WHEN (Month >= 10 AND Month <= 12) THEN 4
                                END  AS bin_number
                                FROM nyc_view_2015")

# For using SQL, you need to create a temporary view
createOrReplaceTempView(nyc_filter_2015, "nyc_view_2015");

# Below commands returns frequencies of tickets for each season.
ticket_freq_season_2015 <- SparkR::sql("SELECT bin_number, COUNT(Summons_Number) AS total_tickets 
                                        FROM nyc_view_2015 
                                        GROUP BY bin_number 
                                        ORDER BY bin_number ASC")

head(ticket_freq_season_2015)

# Frequencies of tickets for each season for year 2015
#------------------------
# bin_number   total_tickets
#------------------------
# 1            2475936
# 2            3250418
# 3            2789425
# 4            2435478

#Below commands will return 3 most common violations for each season for Year 2015
top_3_vios_bin_1 <- SparkR::sql("select bin_number, Violation_Code, count(Summons_Number) as count_tckts from nyc_view_2015 where bin_number == 1 group by bin_number, Violation_Code order by bin_number, count_tckts desc limit 3")
top_3_vios_bin_2 <- SparkR::sql("select bin_number, Violation_Code, count(Summons_Number) as count_tckts from nyc_view_2015 where bin_number == 2 group by bin_number, Violation_Code order by bin_number, count_tckts desc limit 3")
top_3_vios_bin_3 <- SparkR::sql("select bin_number, Violation_Code, count(Summons_Number) as count_tckts from nyc_view_2015 where bin_number == 3 group by bin_number, Violation_Code order by bin_number, count_tckts desc limit 3")
top_3_vios_bin_4 <- SparkR::sql("select bin_number, Violation_Code, count(Summons_Number) as count_tckts from nyc_view_2015 where bin_number == 4 group by bin_number, Violation_Code order by bin_number, count_tckts desc limit 3")


# Combinining All Results
bin_1_2 <- UNION(top_3_vios_bin_1,top_3_vios_bin_2)
bin_3_4 <- UNION(top_3_vios_bin_3,top_3_vios_bin_4)
total_bin_15 <- UNION(bin_1_2,bin_3_4)

head(total_bin_15, n=15)

#3 Most common violations for each season for Year 2016
#--------------------------------------
#bin_number Violation_Code count_tckts
#------------------------------------------
#   1             38      336762
#   1             21      281600
#   1             14      220029

#   2             21      471580
#   2             38      346719
#   2             14      262595

#   3             21      397871
#   3             38      348466
#   3             14      234606

#   4             21      350563
#   4             38      292639
#   4             14      207397



################   2016   ################
# Below commands returns the violation code frequency across 3 precincts which have issued the most number of tickets for year 2016

# Create new column "Month" based on "Issue Date".
nyc_filter_2016 <- withColumn(nyc_filter_2016, "Month", month(nyc_filter_2016$Issue_Date_Parsed))

# For using SQL, you need to create a temporary view
createOrReplaceTempView(nyc_filter_2016, "nyc_view_2016");

# Below command divides data into season based on month.
nyc_filter_2016 <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN (Month >= 1 AND Month <= 3) THEN 1
                                    WHEN (Month >= 4 AND Month <= 6) THEN 2
                                    WHEN (Month >= 7 AND Month <= 9) THEN 3
                                    WHEN (Month >= 10 AND Month <= 12) THEN 4
                                END  AS bin_number
                                FROM nyc_view_2016")

# For using SQL, you need to create a temporary view
createOrReplaceTempView(nyc_filter_2016, "nyc_view_2016");

# Below commands returns frequencies of tickets for each season.
ticket_freq_season_2016 <- SparkR::sql("SELECT bin_number, COUNT(Summons_Number) AS total_tickets 
                                        FROM nyc_view_2016 
                                        GROUP BY bin_number 
                                        ORDER BY bin_number ASC")

head(ticket_freq_season_2016)

# Frequencies of tickets for each season for year 2016
#------------------------
# bin_number   total_tickets
#------------------------
# 1              2671331
# 2              2425877
# 3              2728663
# 4              2801028


#Below commands will return 3 most common violations for each season for Year 2016
top_3_vios_bin_1 <- SparkR::sql("select bin_number, Violation_Code, count(Summons_Number) as count_tckts from nyc_view_2016 where bin_number == 1 group by bin_number, Violation_Code order by bin_number, count_tckts desc limit 3")
top_3_vios_bin_2 <- SparkR::sql("select bin_number, Violation_Code, count(Summons_Number) as count_tckts from nyc_view_2016 where bin_number == 2 group by bin_number, Violation_Code order by bin_number, count_tckts desc limit 3")
top_3_vios_bin_3 <- SparkR::sql("select bin_number, Violation_Code, count(Summons_Number) as count_tckts from nyc_view_2016 where bin_number == 3 group by bin_number, Violation_Code order by bin_number, count_tckts desc limit 3")
top_3_vios_bin_4 <- SparkR::sql("select bin_number, Violation_Code, count(Summons_Number) as count_tckts from nyc_view_2016 where bin_number == 4 group by bin_number, Violation_Code order by bin_number, count_tckts desc limit 3")


# Combinining All Results
bin_1_2 <- UNION(top_3_vios_bin_1,top_3_vios_bin_2)
bin_3_4 <- UNION(top_3_vios_bin_3,top_3_vios_bin_4)
total_bin_16 <- UNION(bin_1_2,bin_3_4)

head(total_bin_16, n=15)

#3 Most common violations for each season for Year 2016
#--------------------------------------
#bin_number Violation_Code count_tckts
#------------------------------------------
#   1             21      349644
#   1             36      341787
#   1             38      308999

#   2             21      348473
#   2             36      294015
#   2             38      254909

#   3             21      403720
#   3             38      305360
#   3             14      234943

#   4             36      433966
#   4             21      429750
#   4             38      274428




################   2017   ################
# Below commands returns the violation code frequency across 3 precincts which have issued the most number of tickets for year 2017

# Create new column "Month" based on "Issue Date".
nyc_filter_2017 <- withColumn(nyc_filter_2017, "Month", month(nyc_filter_2017$Issue_Date_Parsed))

# For using SQL, you need to create a temporary view
createOrReplaceTempView(nyc_filter_2017, "nyc_view_2017");

# Below command divides data into season based on month.
nyc_filter_2017 <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN (Month >= 1 AND Month <= 3) THEN 1
                                    WHEN (Month >= 4 AND Month <= 6) THEN 2
                                    WHEN (Month >= 7 AND Month <= 9) THEN 3
                                    WHEN (Month >= 10 AND Month <= 12) THEN 4
                                END  AS bin_number
                                FROM nyc_view_2017")

# For using SQL, you need to create a temporary view
createOrReplaceTempView(nyc_filter_2017, "nyc_view_2017");

# Below commands returns frequencies of tickets for each season.
ticket_freq_season_2017 <- SparkR::sql("SELECT bin_number, COUNT(Summons_Number) AS total_tickets 
                                        FROM nyc_view_2017 
                                        GROUP BY bin_number 
                                        ORDER BY bin_number ASC")

head(ticket_freq_season_2017)

# Frequencies of tickets for each season for year 2017
#------------------------
# bin_number   total_tickets
#------------------------
# 1               2671332
# 2               3018840
# 3               2463936
# 4               2648920


#Below commands will return 3 most common violations for each season for Year 2017
top_3_vios_bin_1 <- SparkR::sql("select bin_number, Violation_Code, count(Summons_Number) as count_tckts from nyc_view_2017 where bin_number == 1 group by bin_number, Violation_Code order by bin_number, count_tckts desc limit 3")
top_3_vios_bin_2 <- SparkR::sql("select bin_number, Violation_Code, count(Summons_Number) as count_tckts from nyc_view_2017 where bin_number == 2 group by bin_number, Violation_Code order by bin_number, count_tckts desc limit 3")
top_3_vios_bin_3 <- SparkR::sql("select bin_number, Violation_Code, count(Summons_Number) as count_tckts from nyc_view_2017 where bin_number == 3 group by bin_number, Violation_Code order by bin_number, count_tckts desc limit 3")
top_3_vios_bin_4 <- SparkR::sql("select bin_number, Violation_Code, count(Summons_Number) as count_tckts from nyc_view_2017 where bin_number == 4 group by bin_number, Violation_Code order by bin_number, count_tckts desc limit 3")

# Combinining All Results
bin_1_2 <- UNION(top_3_vios_bin_1,top_3_vios_bin_2)
bin_3_4 <- UNION(top_3_vios_bin_3,top_3_vios_bin_4)
total_bin_17 <- UNION(bin_1_2,bin_3_4)

head(total_bin_17, n=15)

#3 Most common violations for each season for Year 2017
#--------------------------------------
#bin_number Violation_Code count_tckts
#------------------------------------------
#   1             21      374202
#   1             36      348240
#   1             38      287017

#   2             21      421184
#   2             36      369902
#   2             38      266909

#   3             21      385774
#   3             38      244985
#   3             36      239879

#   4             36      442593
#   4             21      347428
#   4             38      263393




#-------------------------------------------------------------------------------------------------------------------------------
##  Q7. The fines collected from all the parking violation constitute a revenue source for the NYC police department. Let’s take an example of estimating that for the 3 most commonly occurring codes.
##  Find total occurrences of the 3 most common violation codes
##  Then, search the internet for NYC parking violation code fines. You will find a website (on the nyc.gov URL) that lists these fines. They’re divided into two categories, one for the highest-density locations of the city, the other for the rest of the city. For simplicity, take an average of the two.
##  Using this information, find the total amount collected for all of the fines. State the code which has the highest total collection.
##  What can you intuitively infer from these findings?
#-------------------------------------------------------------------------------------------------------------------------------

################   2015   ################
# Below commands returns top 5 violation codes for year 2015
nyc_vc_2015 <- summarize(groupBy(nyc_filter_2015, nyc_filter_2015$Violation_Code), 
                              count= n(nyc_filter_2015$Violation_Code))
                        
head(arrange(nyc_vc_2015, desc(nyc_vc_2015$count)), n=3)


#Top 3 violation codes for year 2015 based on count
#------------------------
# Violation_Code   count
#------------------------
# 21               1592519
# 38               1397978
# 14               969177



# Below commands returns total amount collected for violation codes for year 2015
# For using SQL, you need to create a temporary view
createOrReplaceTempView(nyc_filter_2015, "nyc_view_2015");

# Below command divides data based on fine amount for respective Violation_Code.
nyc_filter_2015 <- sql("SELECT *, 
                           CASE WHEN Violation_Code = 21 THEN 55
                           WHEN Violation_Code = 38 THEN 50
                           WHEN Violation_Code = 14 THEN 115
                           WHEN Violation_Code = 36 THEN 50
                           END  as fine_amount FROM nyc_view_2015")

# For using SQL, you need to create a temporary view
createOrReplaceTempView(nyc_filter_2015, "nyc_view_2015");

# Below commands returns total fine amount collected for all of the fines.
total_fine_collected_2015 <- SparkR::sql("SELECT Violation_Code, SUM(fine_amount) AS total_fine_2015 
                                          FROM nyc_view_2015 
                                          GROUP BY Violation_Code 
                                          ORDER BY total_fine_2015 DESC ")

head(total_fine_collected_2015, n=3)

#Top 3 violation codes for year 2015 based on total fine collection
#----------------------------------
# Violation_Code   total_fine_2015
#------------------------------------
# 14              111455355
# 21              87588545
# 38              69898900

#Below command summarizes data by Violation_Code vs Total fine summary
total_fine_collected_2015_ggplot <- collect(SparkR::sql("SELECT Violation_Code, SUM(fine_amount) AS total_fine_2015 
                                                  FROM nyc_view_2015 
                                                  GROUP BY Violation_Code 
                                                  ORDER BY total_fine_2015 DESC
                                                  LIMIT 3"))

#Below command plot bar chart to show Violation_Code vs Total fine summary
ggplot(total_fine_collected_2015_ggplot, aes(x = factor(Violation_Code), y = total_fine_2015)) + 
  geom_bar(stat = "identity") +
  xlab("Violation Code") + 
  ylab("total fine 2015") +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) +
  geom_text(aes(label = total_fine_2015),  angle=90 , hjust=0.1)



################   2016   ################
# Below command returns total number of tickets having Violation Location as NULL for year 2016
nyc_vc_2016 <- summarize(groupBy(nyc_filter_2016, nyc_filter_2016$Violation_Code), 
                         count= n(nyc_filter_2016$Violation_Code))

head(arrange(nyc_vc_2016, desc(nyc_vc_2016$count)), n=3)

#Top 3 violation codes for year 2016 based on count
#------------------------
# Violation_Code   count
#------------------------
# 21               1490775
# 36               1232910
# 38               1125950


# Below commands returns total amount collected for violation codes for year 2016
# For using SQL, you need to create a temporary view
createOrReplaceTempView(nyc_filter_2016, "nyc_view_2016");

# Below command divides data based on fine amount for respective Violation_Code.
nyc_filter_2016 <- sql("SELECT *, 
                           CASE WHEN Violation_Code = 21 THEN 55
                           WHEN Violation_Code = 38 THEN 50
                           WHEN Violation_Code = 14 THEN 115
                           WHEN Violation_Code = 36 THEN 50
                           END  as fine_amount FROM nyc_view_2016")

# For using SQL, you need to create a temporary view
createOrReplaceTempView(nyc_filter_2016, "nyc_view_2016");

# Below commands returns total fine amount collected for all of the fines.
total_fine_collected_2016 <- SparkR::sql("SELECT Violation_Code, SUM(fine_amount) AS total_fine_2016 
                                          FROM nyc_view_2016 
                                          GROUP BY Violation_Code 
                                          ORDER BY total_fine_2016 DESC ")

head(total_fine_collected_2016, n=3)

#Top 3 violation codes for year 2016 based on total fine collection
#----------------------------------
# Violation_Code   total_fine_2016
#------------------------------------
# 14              98614915
# 21              81992625
# 36              61645500

#Below command summarizes data by Violation_Code vs Total fine summary
total_fine_collected_2016_ggplot <- collect(SparkR::sql("SELECT Violation_Code, SUM(fine_amount) AS total_fine_2016 
                                                  FROM nyc_view_2016 
                                                  GROUP BY Violation_Code 
                                                  ORDER BY total_fine_2016 DESC 
                                                  LIMIT 3"))

#Below command plot bar chart to show Violation_Code vs Total fine summary
ggplot(total_fine_collected_2016_ggplot, aes(x = factor(Violation_Code) , y = total_fine_2016)) + 
  geom_bar(stat = "identity") +
  xlab("Violation Code") + 
  ylab("total fine 2016") +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5)) +
  geom_text(aes(label = total_fine_2016),  angle=90 , hjust=0.1)



################   2017   ################
# Below command returns total number of tickets having Violation Location as NULL for year 2017
nyc_vc_2017 <- summarize(groupBy(nyc_filter_2017, nyc_filter_2017$Violation_Code), 
                         count= n(nyc_filter_2017$Violation_Code))

head(arrange(nyc_vc_2017, desc(nyc_vc_2017$count)), n=3)

#Top 3 violation codes for year 2017 based on count
#------------------------
# Violation_Code   count
#------------------------
# 21               1494775
# 36               1345192
# 38               1049457


# Below commands returns total amount collected for violation codes for year 2017
# For using SQL, you need to create a temporary view
createOrReplaceTempView(nyc_filter_2017, "nyc_view_2017");

# Below command divides data based on fine amount for respective Violation_Code.
nyc_filter_2017 <- sql("SELECT *, 
                           CASE WHEN Violation_Code = 21 THEN 55
                           WHEN Violation_Code = 38 THEN 50
                           WHEN Violation_Code = 14 THEN 115
                           WHEN Violation_Code = 36 THEN 50
                           END  as fine_amount FROM nyc_view_2017")

# For using SQL, you need to create a temporary view
createOrReplaceTempView(nyc_filter_2017, "nyc_view_2017");

# Below commands returns total fine amount collected for all of the fines.
total_fine_collected_2017 <- SparkR::sql("SELECT Violation_Code, SUM(fine_amount) AS total_fine_2017 
                                          FROM nyc_view_2017 
                                          GROUP BY Violation_Code 
                                          ORDER BY total_fine_2017 DESC ")

head(total_fine_collected_2017, n=3)

#Top 3 violation codes for year 2017 based on total fine collection
#----------------------------------
# Violation_Code   total_fine_2017
#------------------------------------
# 14              100938950
# 21              82212625
# 36              67259600

#Below command summarizes data by Violation_Code vs Total fine summary
total_fine_collected_2017_ggplot <- collect(SparkR::sql("SELECT Violation_Code, SUM(fine_amount) AS total_fine_2017 
                                                         FROM nyc_view_2017 
                                                         GROUP BY Violation_Code 
                                                         ORDER BY total_fine_2017 DESC
                                                         LIMIT 3"))

#Below command plot bar chart to show Violation_Code vs Total fine summary
ggplot(total_fine_collected_2017_ggplot, aes(x = factor(Violation_Code) , y = total_fine_2017)) + 
  geom_bar(stat = "identity") +
  xlab("Violation Code") + 
  ylab("total fine 2017") +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5)) +
  geom_text(aes(label = total_fine_2017),  angle=90 , hjust=0.1)
  
  
  