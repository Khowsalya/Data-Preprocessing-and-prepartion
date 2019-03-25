######################## Data preprocessing - Data prepartion case study##################

#Suppose you are a part of Marketing team in a company and your manager wants you to analyse the data sets,"Campaign_File.txt",
#"Customers_File.txt","Products_File.txt" and "Transactions_File.txt,,to answer the following questions.


#Think of a situation where your manager wants to understand the patterns in purchase of product categories, mode of payment, time of transactions,
#customer behaviour towards different products according to their age and gender. 
#Also the response from customers for certain campaign.

#WorkingDirectory Path Setting

setwd("D:\\Kowsi_DataScience_R\\Class-Datasets\\Campaign_customers_products_transactions")


#Reading given text file for analysis

Campaign <- read.table("Campaign_File.txt",header = T,sep ="\t")
customer <- read.table("Customers_File.txt", header = T, sep ="\t")
product <- read.table("Products_File.txt", header = T, sep = "\t")
Transcation <- read.table( "Transactions_File.txt",header = T, sep ="\t")


#Checking  missing values
sum(is.na(Campaign$Campaign_Responce))



#Which product category dominates in terms of total purchase amount in dollars and 
#Which product category has lowest contribution in total purchase amount in dollars?

tr_product <- merge( x = Transcation, y = product , by = "Product_Code")

#using SQL so loading sqldf package here

library(sqldf)
category <- sqldf("select sum(Items_Amount), Product_Category from tr_product group by Product_Category")
category

#Answer:Entertainment product category dominates in terms of total purchase amount in dollars
#and Software product category has lowest contribution in total purchase amount in dollars




#Which mode of payment is most popular?

paymentmode1 <- table(tr_product$Payment_Method)
paymentmode1

#Answer:CreditCard is the most popular mode of payment based on the proportion


#Extracting desired  time information using the packages "dplyr" and "lubridate".

library(lubridate)

str(tr_product)

#How many transactions were carried out during 18:00 hour?

tr_product$Timestamp <- as.POSIXlt(tr_product$Timestamp)
Timestmp <- tr_product$Timestamp
tr_product$hours <- Timestmp$hour
Timestmp1 <- table(tr_product$hours)
Timestmp1

#Answer:798 transactions were carried out during 18:00 hour


#Is Mode of Payment affected by time of transaction?

effect <- table(tr_product$hours,tr_product$Payment_Method)
effect

?chisq.test
chisq.test(tr_product$hours,tr_product$Payment_Method,correct = FALSE)


#Answer:No ,because the p-value is more than 0.5 so we fail to reject the null hypothesis.


#A merge between "Customers_File.txt" and "Transactions_File.txt" files is required
tr_cust <- merge( x = customer, y = Transcation , by = "Card_ID")
unique(tr_cust$Card_ID)
?dim
?obs


#Find Age of customers as on '1-Jan-2017' using "Birth_Date" variable then perform a suitable age grouping and 
#analyse which group has maximum contribution in terms of amount spent in dollars.

str(tr_cust)
tr_cust$Birth_Date <- as.Date(tr_cust$Birth_Date)
tr_cust$specific <- dmy("01-jan-2017")
tr_cust$specific <- as.Date(tr_cust$specific)

tr_cust$age <- difftime(tr_cust$specific,tr_cust$Birth_Date,units = "days")/365
tr_cust$age <- round(tr_cust$age)
tr_cust$age <- as.numeric(tr_cust$age)

library(dplyr)

tr_cust$agegp <- cut(tr_cust$age,breaks = seq(25,115,by=15))
mg <- sqldf("select sum(Items_Amount),agegp from tr_cust group by agegp")
mg

#Answer:(40,55] age group has maximum contribution in terms of amount spent in dollars.



#Based on Age and Gender, which group of customers dominate amount spent in dollars?

mg1 <- sqldf("select sum(Items_Amount),agegp,Gender from tr_cust group by agegp , Gender")
mg1

#Answer: (40,55], M this age group and gender  of customers dominate amount spent in dollars

#What is the proportion of customers who responded(TRUE) to the campaign? 
#rounded off to three decimal points.

#A merge between "Customers_File.txt" and "Campaign_File.txt" files is required 
cc <- merge( x = Campaign, y = customer , by = "Card_ID")
proportion <- table(cc$Campaign_Responce)
proportion
(325)/(325+5632)

#Answer:0.0550 is the proportion of customers who responded(TRUE) to the campaign, 




#Identify the 'Tenure' group of customers where response rate is high. 
#Tenure will be defined as the time period between the Date of Registration and 31/12/2004


str(cc)
cc$Registration_Date <- as.Date(cc$Registration_Date)
cc$specific1 <- dmy("31/12/2004")
cc$specific1 <- as.Date(cc$specific1)

cc$tenure <- difftime(cc$specific1,cc$Registration_Date,units = "days")/365
cc$tenure <- round(cc$tenure)
cc$tenure <- as.numeric(cc$tenure)
cc$tenuregp <- cut(cc$tenure,breaks = seq(3,7.5,by=1.5))
cc$tenuregp

mg1 <- sqldf("select count(Campaign_Responce),tenuregp from cc group by tenuregp")
mg1

#  (4.5,6] this 'Tenure' group of customers where response rate is high. 
