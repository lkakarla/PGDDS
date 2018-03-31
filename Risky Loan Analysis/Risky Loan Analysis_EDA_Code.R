#clear Environment
rm(list = ls()) 

#-------------------------------------------------------------------------------
#load packages
#-------------------------------------------------------------------------------
library("dplyr")
library("ggplot2")
#library("stringr")
library("scales")
#library("lubridate")
#library("tidyr")
#library("gmodels")
library("gridExtra")
library("reshape2")




#-------------------------------------------------------------------------------
# Load the data and check its contents
#-------------------------------------------------------------------------------
# Run below code if loan.zip file is not extracted
# unzip(zipfile = "loan.zip", exdir = ".")

#read csv
loan_df <- read.csv("loan.csv", stringsAsFactors = F)
str(loan_df)
summary(loan_df)
head(loan_df)

#print column names
names(loan_df)

#print no of unique values under each column
as.data.frame(rapply(loan_df,function(x)length(unique(x))))


#-------------------------------------------------------------------------------
#Check for duplicates
#-------------------------------------------------------------------------------
sum(duplicated(loan_df)) # no duplicate rows
sum(duplicated(loan_df$id)) 
sum(duplicated(loan_df$member_id))



#-------------------------------------------------------------------------------
#Missing values
#-------------------------------------------------------------------------------

#Check for Missing values
sum(is.na(loan_df))
#length(which(is.na(loan_df)))


#Get column names having NA
colnames(loan_df)[colSums(is.na(loan_df)) > 0]
na_col <- sapply(loan_df, function(x) length(which(is.na(x))))
na_col
table(na_col)


#Remove all columns for which all rows are 'NA'
loan_df <- loan_df[ ,colSums(is.na(loan_df)) != nrow(loan_df)]


#Print column names having same value for all rows
names(loan_df[sapply(loan_df, function(x) length(unique(x))==1)])


#Remove all columns having same value for all rows
loan_df <- loan_df[sapply(loan_df, function(x) length(unique(x))>1)]


#Print columns having unique values for each row
names(loan_df[sapply(loan_df, function(x) length(unique(x))==nrow(loan_df))])


#loan_df$zip_code <-  strtrim(loan_df$zip_code,3)

# Remove unnecessary columns
# "id", "member_id", "url" columns  ==>  contains unique values. since we do analysis on collection, these values are of no use
# "desc" column ==> contains a long text explanation
# "next_pymnt_d", "mths_since_last_delinq" & "mths_since_last_record" columns ==> contains too many missing values
# "last_pymnt_d" and "last_credit_pull_d", "out_prncp", "out_prncp_inv","collection_recovery_fee", "recoveries", "last_pymnt_amnt" columns ==> not required for analysis
# "total_pymnt", "total_pymnt_inv", "total_rec_int", "total_rec_late_fee", "total_rec_prncp" columns ==> not required for analysis
# "collections_12_mths_ex_med", "tax_liens" and "chargeoff_within_12_mths" columns ==> contains same or missing values
# "zip_code"  ==> contains invalid values
# "sub_grade" ==> removed as retrained "Grade" columns for analysis
remove_col <- c("id","member_id", "url", "desc", "next_pymnt_d","mths_since_last_delinq", "mths_since_last_record", "collections_12_mths_ex_med", "tax_liens", "chargeoff_within_12_mths", "zip_code", "last_pymnt_d", "last_credit_pull_d", "out_prncp", "out_prncp_inv", "total_pymnt", "total_pymnt_inv", "total_rec_int", "total_rec_late_fee", "total_rec_prncp", "collection_recovery_fee", "recoveries","last_pymnt_amnt","sub_grade")
loan_df <- loan_df[ , -which(names(loan_df) %in% remove_col)]


# Check again for NA values
na_col <- sapply(loan_df, function(x) length(which(is.na(x))))
na_col
table(na_col)


# Checking for blank "" values
blank_col <- sapply(loan_df, function(x) length(which(x == "")))
blank_col
table(blank_col)


# replace blank values by NA
#loan_df$last_credit_pull_d[which(loan_df$last_credit_pull_d == "")] <- NA
#loan_df$last_pymnt_d[which(loan_df$last_pymnt_d == "")] <- NA
#loan_df$last_pymnt_d[which(loan_df$next_pymnt_d == "")] <- NA
loan_df$revol_util[which(loan_df$revol_util == "")] <- NA
loan_df$title[which(loan_df$title == "")] <- NA
loan_df$emp_title[which(loan_df$emp_title == "")] <- NA

# Check again for blank values
blank_col <- sapply(loan_df, function(x) length(which(x == "")))
blank_col
table(blank_col)

sum(is.na(loan_df))



#-------------------------------------------------------------------------------
# Check Individual Columns and Derice New Metrics
#-------------------------------------------------------------------------------
# issue_d
# --------
summary(loan_df$issue_d)
loan_df$issue_d[1:10]

# convert it to proper date
loan_df$issue_d <- as.Date(paste("01-",loan_df$issue_d, sep=""), "%d-%b-%y")


# int_rate
# ----------
# remove percent symbols and convert to numeric
loan_df$int_rate <-  gsub("%", "", loan_df$int_rate)
loan_df$int_rate <-  as.numeric(loan_df$int_rate)
summary(loan_df$int_rate)



# revol_util 
# ----------
# remove percent symbols and and convert to numeric
loan_df$revol_util <-  gsub("%", "", loan_df$revol_util)
loan_df$revol_util <-  as.numeric(loan_df$revol_util)
summary(loan_df$revol_util)

# replace missing values as 0
loan_df$revol_util <- replace(loan_df$revol_util,is.na(loan_df$revol_util),0)

loan_df$revol_util[1:10]


# earliest_cr_line
# --------
summary(loan_df$earliest_cr_line)
loan_df$earliest_cr_line[1:10]

# convert earliest_cr_line to proper date format - below function will handle dates before 1970
loan_df$earliest_cr_line <- paste("01-",loan_df$earliest_cr_line, sep="") %>% 
  as.Date(paste("01-",loan_df$earliest_cr_line, sep=""), format= "%d-%b-%y") %>% 
  format("%y%m%d") %>%
  (function(d){
    paste0(ifelse(d>format(Sys.Date(),"%y%m%d"),"19","20"),d)
  }) %>% 
  as.Date("%Y%m%d")


# Term
# ----------
unique(loan_df$term)
# remove leading whitespace and "months"
loan_df$term <- gsub("[^0-9]+", "", loan_df$term)
loan_df$term <- factor(loan_df$term)
summary(loan_df$term)


# Grade
# ----------
unique(loan_df$grade)
summary(factor(loan_df$grade))

#order the grades
loan_df$grade <- factor(loan_df$grade, ordered = TRUE, levels = c('A', 'B', 'C', 'D', 'E', 'F', 'G'))


# ----------
summary(factor(loan_df$emp_length))

# order emp_length
# loan_df$emp_length <- factor(loan_df$emp_length, ordered = TRUE, levels = c('0 years', '1 year', '2 years', '3 years', '4 years', '5 years', '6 years', '7 years', '8 years', '9 years', '10+ years'))

# emp_length can be ordered categorical variable. converting it to ordinal variable to use intervals

#Replace 'n/a' and '< 1 year' as 0 
loan_df <-  loan_df %>% 
  mutate(emp_length = replace(emp_length, emp_length == "n/a" | emp_length == "< 1 year", 0))
#Remove all the non-numeric stuff
loan_df$emp_length <- gsub("[^0-9]+", "", loan_df$emp_length)
#Convert to numeric
loan_df$emp_length <- as.numeric(loan_df$emp_length)

summary(factor(loan_df$emp_length))


# home_ownership
# ----------
summary(factor(loan_df$home_ownership))


# verification_status
# -------------------
unique(loan_df$verification_status)
loan_df$verification_status <- toupper(loan_df$verification_status)  #convert to upper case
summary(factor(loan_df$verification_status))


# loan_status
# ----------
loan_df$loan_status <- toupper(loan_df$loan_status) #convert to upper case
summary(factor(loan_df$loan_status))


# purpose
# ----------
loan_df$purpose <- toupper(loan_df$purpose)  #convert to upper case
summary(factor(loan_df$purpose))


# emp_title
# ----------
loan_df$emp_title <- toupper(loan_df$emp_title) # convert to upper case
summary(factor(loan_df$emp_title))
# table(loan_df$emp_title)
# unique(loan_df$emp_title)

# emp_title is a categoric variable. Removing it from data set as it contains too many levels.
#loan_df$emp_title <- NULL
loan_df <- loan_df[ , -which(names(loan_df) == "emp_title") ] 


# title
# ----------
loan_df$title <- toupper(loan_df$title) #convert to upper case
summary(factor(loan_df$title))
# table(loan_df$title)
# unique(loan_df$title)

# title is a categoric variable. Removing it from data set as it contains too many levels.
loan_df <- loan_df[ , -which(names(loan_df) == "title") ] 


#addr_state
# ----------
unique(loan_df$addr_state)
summary(factor(loan_df$addr_state))


# loan_amnt
# ----------
summary(loan_df$loan_amnt)


# funded_amnt
# ----------
summary(loan_df$funded_amnt)


# funded_amnt_inv
# ----------
loan_df$funded_amnt_inv <- round(loan_df$funded_amnt_inv, digits=2) # round to nearest whole
summary(loan_df$funded_amnt_inv)


# annual_inc
# ----------
summary(loan_df$annual_inc)


# dti
# ----------
summary(loan_df$dti)


# delinq_2yrs
# -----------
table(loan_df$delinq_2yrs)


#open_acc
# ----------
unique(loan_df$open_acc)
summary(loan_df$open_acc)


#pub_rec
# ----------
unique(loan_df$pub_rec)
summary(loan_df$pub_rec)


# revol_bal
# ----------
summary(loan_df$revol_bal)


# pub_rec_bankruptcies
# ----------
summary(loan_df$pub_rec_bankruptcies)
table(loan_df$pub_rec_bankruptcies)

#replace missing values as 0
loan_df$pub_rec_bankruptcies <- replace(loan_df$pub_rec_bankruptcies,is.na(loan_df$pub_rec_bankruptcies),0)

# Check again for NA values
na_col <- sapply(loan_df, function(x) length(which(is.na(x))))
na_col
table(na_col)


str(loan_df)


#-------------------------------------------------------------------------------
# Derive New Metrics
#-------------------------------------------------------------------------------

#consider observations with load status either 'FULLY PAID' or 'CHARGED OFF'. Ignore 'CURRENT'.
loan_sub <- filter(loan_df, loan_status!="CURRENT")


# derive month, year and quarter from issue_d column
loan_sub$issue_month <- format(loan_sub$issue_d, "%b")
#loan_sub$issue_month_num = format(loan_sub$issue_d, "%m")
loan_sub$issue_year <- format(loan_sub$issue_d, "%Y")
loan_sub$issue_quarter <- quarters(loan_sub$issue_d)


# derive year from earliest_cr_line column
loan_sub$earliest_cr_line_yr <- format(loan_sub$earliest_cr_line, "%Y")

#summary(factor(loan_sub$issue_year))

#order the months
loan_sub$issue_month <- factor(loan_sub$issue_month, ordered = TRUE, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

# remove outliers based on annual_income
# check annual_income distribution
quantile(loan_sub$annual_inc,seq(0,1, 0.01))

# considered annual_income upto 98th percentile
loan_sub <- loan_sub[!loan_sub$annual_inc > quantile(loan_sub$annual_inc,0.98), ]


# cut the annual income into ranges
loan_sub$annual_inc.bucket <- cut(loan_sub$annual_inc, seq(0,200000,20000), dig.lab = 7)

# remove outliers based on dti(debt-to-income) 
# check dti(debt-to-income)  distribution
quantile(loan_sub$dti,seq(0,1, 0.01))

#considered dti(debt-to-income) upto 98th percentile
loan_sub <- loan_sub[!loan_sub$dti > quantile(loan_sub$dti,0.98), ]

# considered annual_income upto 98th percentile
loan_sub$dti.bucket <- cut(loan_sub$dti, seq(0,30,5), dig.lab = 4, include.lowest = T)


str(loan_sub)

# check interest rate distribution
# quantile(loan_sub$int_rate,seq(0,1, 0.01))

# options("scipen"=100, "digits"=4)
# write to CSV
#write.csv(loan_sub, file = "loan_clean_1.csv", row.names = F)

# -------------------------------
# Univariate Analysis
# -------------------------------

# Income Range Of Borrowers
ggplot(loan_sub, aes(x=annual_inc.bucket, y= ..count.., fill = I('dodgerblue4'))) +
  geom_bar(size = 1.5) +
  ggtitle("Income Range Of Borrowers") +
  ylab('Number of Loan Requests') +
  xlab("Annual Income Buckets") +
  coord_flip()

#most of the borrowers have income ranging from $20,000 to $80,000.


# Number of Loan Requests Based on Purpose
ggplot(loan_sub, aes(x=reorder(purpose, purpose, length), y= ..count.., fill = I('dodgerblue4'))) +
  geom_bar(size = 1.5) +
  ggtitle("Number of Loan Requests Based on Purpose") +
  ylab('Number of Loan Requests') +
  xlab("Purpose") +
  coord_flip()

# Most loans taken for Debt Consolidation purpose, followed by Credit Card.


# Number of Loan Requests Based on verification Status
ggplot(loan_sub, aes(x=reorder(verification_status, verification_status, length), y= ..count.., fill = I('dodgerblue4'))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), size = 1.5) +
  ggtitle("Number of Loan Requests Based on Verification Status") +
  ylab('Number of Loan Requests') +
  xlab("Verification Status") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent)

#table(loan_sub$verification_status)

# Number of Loans Issued by Employment Length
ggplot(loan_sub, aes(x=emp_length, y= ..count.., fill = I('dodgerblue4'))) +
  geom_bar(size = 1.5) +
  ggtitle("Number of Loans Issued by Employment Length") +
  ylab('Number of Loan Requests') +
  xlab("Employment Length(in years)") +
  scale_x_continuous(
    breaks = c(0:10)
  ) 

# Number of Loans Issued by Term
ggplot(loan_sub, aes(x=reorder(term, loan_status, length), y= ..count.., fill = I('dodgerblue4'))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), size = 1.5) +
  ggtitle("Number of Loans Issued by Term") +
  ylab('Number of Loan Requests') +
  xlab("Term(Months)")  +
  scale_y_continuous(labels = scales::percent)

# Majority of the loans has the length of 36 months. Around 25% of loans have length of 60 months. 


# Number of Loans Issued by Home Ownership
ggplot(loan_sub, aes(x=reorder(home_ownership, home_ownership, length), y= ..count.., fill = I('dodgerblue4'))) +
  geom_bar(size = 1.5) +
  ggtitle("Number of Loans Issued by Home Ownership") +
  ylab('Number of Loan Requests') +
  xlab("Home Ownership") 

#compare to number of people with loan owning a home are signficantly lesser than mortgage and rent


# Percentage of Loans Issued by Loan Status
ggplot(loan_sub, aes(x=reorder(loan_status, loan_status, length), y= ..count.., fill = I('dodgerblue4'))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), size = 1.5) +
  ggtitle("Percentage of Loans Issued by Loan Status") +
  ylab('Loan Requests(%)') +
  xlab("Loan Status") +
  scale_y_continuous(labels = scales::percent)

# Clearly indicates that, most of the investors were able to receive their funds(with interest).

# Number of Loans Issued by State
ggplot(loan_sub, aes(x=reorder(addr_state, addr_state, length), y= ..count.., fill = I('dodgerblue4'))) +
  geom_bar(size = 1.5) +
  ggtitle("Number of Loans Issued by Home Ownership") +
  ylab('Number of Loan Requests') +
  xlab("Home Ownership") 


# The number of borrowers of different states varies a lot and California, Texas, New York and Florida are the top 4 on the list of state with big borrower market.

# Number of Loans Issued by Year and Quarter
ggplot(loan_sub, aes(x=issue_quarter, y= ..count.., fill=I('dodgerblue4'))) +
  facet_grid(~ issue_year) +
  geom_bar(size = 1.5) +
  ggtitle("Number of Loans Issued by Year and Quarter") +
  theme(axis.text.x = element_text(angle = -90, hjust = 1)) +
  ylab('Number of Loan Requests') +
  xlab("Quarter")

# In year 2011, majority of loan requests issued in the year 2011 compare other years.
# And consistently, majority of the loan requests processed in Q4.


# Number of Loans Issued by Year and Month
ggplot(loan_sub, aes(x=issue_month, y= ..count.., fill=I('dodgerblue4'))) +
  facet_grid(~ issue_year) +
  geom_bar(size = 1.5) +
  ggtitle("Number of Loans Issued by Year and Month") +
  theme(axis.text.x = element_text(angle = -90, hjust = 1)) +
  ylab('Number of Loan Requests') +
  xlab("Month")

# Percentage of Loans Issued by Grade
loan_grade <- loan_sub %>% 
  group_by(grade) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(per)

ggplot(data=loan_grade)+
  geom_bar(aes(x="", y=per, fill=grade), stat="identity", width = 1) +
  coord_polar("y", start=0, direction = -1) +
  theme_void() +
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=scales::percent(loan_grade$per)), size =3) +
  ggtitle("Percentage of Loans Issued by Grade") + 
  guides( fill = guide_legend(title = "Loan Grade"))


# Interest Rate Distribution
ggplot(loan_sub, aes(x=int_rate, y= ..count.., fill=I('dodgerblue4'))) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), bins = 25, binwidth = 0.5, alpha = .7, col="red") +
  ggtitle("Interest Rate Distribution") +
  ylab('Probability of Loan Requests(%)') +
  xlab("Interest Rate") +
  scale_y_continuous(labels = scales::percent)


# Loan Amount Distribution
ggplot(filter(loan_sub, loan_status=="CHARGED OFF") , aes(x=loan_amnt, y= ..count.., fill=I('dodgerblue4'))) +
  geom_density(alpha = .7, col="red") +
  ggtitle("Loan Amount Distribution (for CHARGED OFF)") +
  ylab('Frequency of Loan Requests') +
  xlab("Loan Amount")


# Past 2 years Delinquency Distribution
ggplot(loan_sub, aes(x=delinq_2yrs, y= ..count.., fill=I('dodgerblue4'))) +
  geom_histogram(bins = 3, binwidth = 1, alpha = .7, col="red") +
  ggtitle("Delinquency(Past 2 years) Distribution") +
  ylab('Number of Loan Requests') +
  xlab("Delinquency(Past 2 years)") +
  scale_x_continuous(
    breaks = c(0:11)
  ) 


# Revolving Line Utilization Rate Distribution
ggplot(loan_sub, aes(x=revol_util, y= ..count.., fill=I('dodgerblue4'))) +
  geom_histogram(bins = 30, binwidth = 1, alpha = .7, col="red") +
  ggtitle("Revolving Line Utilization Rate Distribution") +
  ylab('Number of Loan Requests') +
  xlab("Revolving Line Utilization Rate") 


# Revolving Credit Balance Distribution
ggplot(loan_sub, aes(x=revol_bal, y= ..count.., fill=I('dodgerblue4'))) +
  geom_histogram(bins = 10, binwidth = 5000, alpha = .7, col="red") +
  ggtitle("Revolving Credit Balance Distribution") +
  ylab('Number of Loan Requests') +
  xlab("Revolving Credit Balance") 


# Debt to Income Ratio Distribution
ggplot(loan_sub, aes(x=dti, y= ..count.., fill=I('dodgerblue4'))) +
  geom_histogram(bins = 20, binwidth = 0.5, alpha = .7, col="red") +
  ggtitle("Debt to Income Ratio Distribution") +
  ylab('Number of Loan Requests') +
  xlab("Debt to Income") 





# -------------------------------
# Segmented Univariate Analysis
# -------------------------------

# Interest Rate Distribution by Grade
ggplot(loan_sub, aes(x = int_rate, fill = grade)) + 
  geom_density(alpha = 0.6) +
  ylab(NULL) +
  xlab("Interest Rate") + 
  guides( fill = guide_legend(title = "Loan Grade")) + 
  ggtitle("Interest Rate Distribution by Grade")


# Percentage of Loans Sanctioned Based on Purpose
ggplot(loan_sub, aes(x = purpose, y= ..count.., fill = loan_status)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.3, position=position_dodge(), colour="black")+
  ylab("Percentage of Loans") +
  xlab("Purpose") + 
  guides( fill = guide_legend(title = "Loan Status")) + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  ggtitle("Percentage of Loans Sanctioned Based on Purpose Categories") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top")


# Percentage of Loans Sanctioned Based on Home Ownership Categories
ggplot(loan_sub, aes(x = home_ownership, y= ..count.., fill = loan_status)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.3, position=position_dodge(), colour="black")+
  ylab("Percentage of Loans") +
  xlab("Home Ownership") + 
  guides( fill = guide_legend(title = "Loan Status")) + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  ggtitle("Percentage of Loans Sanctioned Based on Home Ownership Categories") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top")


# Percentage of Loans Sanctioned Based on Loan Grade
ggplot(loan_sub, aes(x = grade, y= ..count.., fill = loan_status)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.3, position=position_dodge(), colour="black")+
  ylab("Percentage of Loans") +
  xlab("Loan Grade") + 
  guides( fill = guide_legend(title = "Loan Status")) + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  ggtitle("Percentage of Loans Sanctioned Based on Lan Grade") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top")


# Percentage of Loans Sanctioned Based on Loan Term
ggplot(loan_sub, aes(x = term, y= ..count.., fill = loan_status)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.3, position=position_dodge(), colour="black")+
  ylab("Percentage of Loans") +
  xlab("Loan Term") + 
  guides( fill = guide_legend(title = "Loan Status")) + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  ggtitle("Percentage of Loans Sanctioned Based on Loan Term") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top")


# Percentage of Loans Sanctioned Based on Verification Status
ggplot(loan_sub, aes(x = verification_status, y= ..count.., fill = loan_status)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.3, position=position_dodge(), colour="black")+
  ylab("Percentage of Loans") +
  xlab("Verification Status") + 
  guides( fill = guide_legend(title = "Loan Status")) + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  ggtitle("Percentage of Loans Sanctioned Based on Verification Status") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top")


# Percentage of Loans Sanctioned Based on Employment Length
ggplot(loan_sub, aes(x = emp_length, y= ..count.., fill = loan_status)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.3, position=position_dodge(), colour="black")+
  ylab("Percentage of Loans") +
  xlab("Employment Length") + 
  guides( fill = guide_legend(title = "Loan Status")) + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  ggtitle("Percentage of Loans Sanctioned Based on Employment Length") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top")


# Percentage of Loans Sanctioned Based on State
ggplot(loan_sub, aes(x = addr_state, y= ..count.., fill = loan_status)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.3, position=position_dodge(), colour="black")+
  ylab("Percentage of Loans") +
  xlab("State") + 
  guides( fill = guide_legend(title = "Loan Status")) + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  ggtitle("Percentage of Loans Sanctioned Based on State") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top")


# -------------------------------
# Bivariate Analysis
# -------------------------------

#correlation heatmap
nums <- sapply(loan_sub, is.numeric)
loan_numeric <- loan_sub[ , nums]
loan_cormat <- round(cor(loan_numeric, use="na.or.complete"), 2)
loan_cormat <- melt(loan_cormat)

ggplot(loan_cormat, aes(x=Var1, y=Var2, fill = value)) + geom_tile() +
  theme(axis.text.x = element_text(angle = -90))


# Number of Loans Issued by Grade
plot1 <- ggplot(loan_sub, aes(x=grade, y= ..count..)) +
  geom_bar(size = 1.5) +
  ggtitle("Number of Loans Issued by Grade") +
  ylab('Number of Loan Requests') +
  xlab("Grade") +
  scale_fill_discrete(name = "Grade") + 
  theme(legend.background = element_rect(fill="lightblue", 
                                         size=0.5, linetype="solid", colour ="darkblue"))

# Grade vs Interest Rate Distribution
plot2 <- ggplot(loan_sub, aes(x=grade, y=int_rate)) +
  geom_boxplot() +
  ggtitle("Grade vs Interest Rate Distribution") +
  ylab('Interest Rate') +
  xlab("Grade")

grid.arrange(plot1, plot2)

# Loan Status vs Debt to Income Ratio
ggplot(loan_sub, aes(x=dti.bucket, y= ..count.., fill = loan_status)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.3, position=position_dodge(), colour="black")+
  ggtitle("Loan Status vs Debt to Income Ratio") +
  ylab('Loan Requests(%)') +
  xlab("Debt to Income Ratio Buckets") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top") +
  guides( fill = guide_legend(title = "Loan Status")) 


#indicates a steady increase in the charged off loans ratio (compare to fully paid loans) as DTI increases.


#Percentage of Loans Sanctioned Based on Purpose and Loan Grade
ggplot(loan_sub, aes(x = purpose, y= ..count.., fill = loan_status)) + 
  facet_grid(~ grade) +
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  ylab("Percentage of Loans") +
  xlab("Purpose") + 
  guides( fill = guide_legend(title = "Loan Status")) + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  ggtitle("Percentage of Loans Sanctioned Based on Purpose and Loan Grade") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top")


# As loan grade increases, the number of loans opted for 'Debt Consolidation' purpose significantly decreases 
# Majority of charged off loans noticed with Loan Grades 'B','C', 'D'


# Percentage of Loans Sanctioned Based on Home Ownership Categories
ggplot(loan_sub, aes(x = grade, y= ..count.., fill = loan_status)) + 
  facet_grid(~ home_ownership) +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.3, position=position_dodge(), colour="black")+
  ylab("Percentage of Loans") +
  xlab("Home Ownership") + 
  guides( fill = guide_legend(title = "Loan Status")) + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  ggtitle("Percentage of Loans Sanctioned Based on Home Ownership Categories") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top")


# Percentage of Loans Sanctioned Based on Home Ownership Categories
ggplot(loan_sub, aes(x = grade, y= ..count.., fill = loan_status)) + 
  facet_grid(~ verification_status) +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.3, position=position_dodge(), colour="black")+
  ylab("Percentage of Loans") +
  xlab("Home Ownership") + 
  guides( fill = guide_legend(title = "Loan Status")) + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  ggtitle("Percentage of Loans Sanctioned Based on Home Ownership Categories") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top")


# Percentage of Loans Sanctioned Based on Annual Income Buckets
ggplot(loan_sub, aes(x = annual_inc.bucket, y= ..count.., fill = loan_status)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.3, position=position_dodge(), colour="black")+
  ylab("Percentage of Loans") +
  xlab("Home Ownership") + 
  guides( fill = guide_legend(title = "Loan Status")) + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  ggtitle("Percentage of Loans Sanctioned Based on Annual Income Buckets") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top")


# Bivariate Analysis Based on Loan Grade and Loan Status
bi_grade_status<-xtabs(~grade+loan_status, data=loan_sub)
ftable(bi_grade_status)

bi_grade_status_prop <- replace(bi_grade_status, , sprintf("%.1f%%",prop.table(bi_grade_status,2)*100))
bi_grade_status_prop
#data.frame(bi_grade_status_prop)

# Bivariate Analysis Based on Purpose of the loan and Loan Status
bi_purpose_status<-xtabs(~purpose+loan_status, data=loan_sub)
ftable(bi_purpose_status)

bi_purpose_status_prop <- replace(bi_purpose_status, , sprintf("%.1f%%",prop.table(bi_purpose_status,2)*100))
bi_purpose_status_prop
#data.frame(bi_purpose_status_prop)


#Frequency of Loan Status Based on Loan Grade
ggplot(data = loan_sub, aes(x = loan_status, y = grade)) +
  geom_tile(aes(fill = home_ownership))

#Frequency of Loan Status Based on Loan Grade
ggplot(data = loan_sub, aes(x = loan_status, y = grade)) +
  geom_tile(aes(fill = annual_inc.bucket))

