Load the package
library(RODBC)
library(ggplot2)
library(raster)
library(dplyr)


# Connect to you new data source

db = odbcConnect("mysql_odbc_connection", uid="root", pwd="")

# Load the first 1000 rows of a table into a data frame
query = "SELECT * FROM ma_charity_small.acts LIMIT 1000"
df = sqlQuery(db, query)

# If embedded in a function, use print(head(df)) instead
print(head(df))

# Close the connection
odbcClose(db)

####################################################################################
### How many "new" donors were acquired every year? ################################

db = odbcConnect("mysql_odbc_connection", uid="root", pwd="")


query = "Select a.year_first_donation, count(a.contact_id) as Number_of_new_donors from
(Select contact_id, min(YEAR(act_date)) as year_first_donation from ma_charity_small.acts group by 1 order by 2) as a
where a.year_first_donation < '2018'
and a.year_first_donation > '1925'
group by 1
order by 1;"
df = sqlQuery(db, query)
print(df)

gr = ggplot(data = df[df$year_first_donation>1983,], aes(x = year_first_donation, y = Number_of_new_donors)) +
  geom_point() +
  geom_line() 

print(gr)

odbcClose(db)


###################################################################################
############# for the moment 2018 only 2998 new doners, is this year so bad ? ########
###### looking for new doners for each year from 1/01 to 25/06 ####################


db = odbcConnect("mysql_odbc_connection", uid="root", pwd="")

# Load the first 1000 rows of a table into a data frame
query = "Select a.year_first_donation, count(a.contact_id) as Number_of_new_donors from
(Select contact_id, min(YEAR(act_date)) as year_first_donation, act_date from ma_charity_small.acts group by 1 order by 2) as a
where month(a.act_date)<'6' or (month(a.act_date)<'7'and day(a.act_date)<'26')
group by 1
order by 1;"
df = sqlQuery(db, query)
print(df)
# If embedded in a function, use print(head(df)) instead

gr = ggplot(data = df[df$year_first_donation>1972,], aes(x = year_first_donation, y = Number_of_new_donors)) +
  geom_point() +
  geom_line() 

print(gr)


odbcClose(db)
#################################################################
#################################################################



db = odbcConnect("mysql_odbc_connection", uid="root", pwd="")
sqlQuery(db, "USE ma_charity_small")


query = "SELECT SUBSTRING(c.zip_code, 1, 2) as department,
COUNT(c.id) AS nb_donors, d.deparment_name
FROM ma_charity_small.contacts c
INNER JOIN departments d
ON d.num_department = SUBSTRING(c.zip_code, 1, 2)
WHERE SUBSTRING(c.zip_code, 1, 2) is not null
GROUP BY deparment_name
ORDER BY department;"

data = sqlQuery(db, query)

head(data)

#Obtenir les formes
formes <- getData(name="GADM", country="FRA", level=2)
plot(formes)
names(formes)

#?tablissement de l'index
idx <- match(formes$NAME_2, as.character(data$deparment_name))
print(data$deparment_name)
concordance <- data[idx, "nb_donors"]
formes$nb_donors <- concordance

#Tracage de la carte
couleurs <- colorRampPalette(c('white', 'blue'))
spplot(formes, "nb_donors",col.regions=couleurs(30),  main=list(label="Number of donors by department",cex=.8))



###############################################
###############################################

query = "SELECT SUBSTRING(c.zip_code, 1, 2) as department,
AVG(a.amount) as average_amount,
d.deparment_name
from  ma_charity_small.acts a
INNER JOIN  ma_charity_small.contacts c
ON c.id = a.contact_id
INNER JOIN ma_charity_small.departments d
ON d.num_department = SUBSTRING(c.zip_code, 1, 2)
WHERE SUBSTRING(c.zip_code, 1, 2) is not null
GROUP BY deparment_name
ORDER BY department;"

data = sqlQuery(db, query)

head(data)

#Obtenir les formes
formes <- getData(name="GADM", country="FRA", level=2)
plot(formes)
names(formes)

#?tablissement de l'index
idx <- match(formes$NAME_2, as.character(data$deparment_name))

concordance <- data[idx, "average_amount"]
formes$average_amount <- concordance

#Tracage de la carte

couleurs <- colorRampPalette(c('white', 'black'))
spplot(formes, "average_amount",col.regions=couleurs(30),  main=list(label="Average amount of donation by department",cex=.8))


 
library(RODBC)
db = odbcConnect("mysql_odbc_connection", uid="root", pwd="Pengtongyu1")
sqlQuery(db, "USE ma_charity_small")
query = "SELECT contact_id,
                DATEDIFF(20180625, MAX(act_date)) / 365 AS 'recency',
                COUNT(amount) AS 'frequency',
                AVG(amount) AS 'avgamount',
                DATEDIFF(20180625, MIN(act_date)) / 365 AS 'firstdonation'
         FROM acts
         WHERE act_type_id = 'DO'
         GROUP BY 1
         ORDER BY RAND()
         LIMIT 2000"
data = sqlQuery(db, query)
odbcClose(db)
# Assign contact id as row names, remove id from data
rownames(data) = data$contact_id
data = data[, -1] #remove the first column

# Compute distance metrics on standardized data
d = dist(scale(data))

# Perform hierarchical clustering on distance metrics
c = hclust(d, method="ward.D2") #centroid of clusters to compute distance between them

# Plot de dendogram
plot(c) #an object
# Cut at 5 segments
members = cutree(c, k=5)

# Show 50 first donors, frequency table
print(members[1:50])
print(table(members))

# Show profile of each segment
for (i in 1:5) {
  print(colMeans(data[members == i, ]))
}

library(ggplot2)
db = odbcConnect("mysql_odbc_connection", uid="root", pwd="Pengtongyu1")
sqlQuery(db, "USE ma_charity_small")
query1 = "SELECT YEAR(act_date) AS Year, SUM(amount) AS Total amount
        FROM acts
        GROUP BY 1 ORDER BY 1"
data1 = sqlQuery(db, query1)

query2 = "SELECT YEAR(act_date), SUM(amount) 
FROM acts
WHERE act_type_id='PA'
GROUP BY 1 ORDER BY 1"
data2 = sqlQuery(db, query2)

query3 = "SELECT YEAR(act_date), SUM(amount) 
FROM acts
WHERE act_type_id='DO'
GROUP BY 1 ORDER BY 1"
data3 = sqlQuery(db, query3)

ggplot()+
  geom_line(data1[2:28,],mapping=aes(x=`YEAR(act_date)`,y=`SUM(amount)`,color="Total"))+
  geom_line(data2[1:13,],mapping=aes(x=`YEAR(act_date)`,y=`SUM(amount)`,color="DO"))+
  geom_line(data3[2:28,],mapping=aes(x=`YEAR(act_date)`,y=`SUM(amount)`,color="PA"))+
  scale_x_continuous(breaks=seq(1991, 2017, 2))+
  xlab("year")+
  ylab("total")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_color_discrete(name="act_type",labels = c("DO","PA","Total"))+
  theme(legend.position = c(0.2, 0.8))
  
query4="SELECT id, amount
FROM acts
WHERE ACT_DATE>='2017-01-01' AND act_type_id='PA'"
data4=sqlQuery(db, query4)
ggplot(data4, aes(x=amount))+
  geom_histogram(bins=50)+
  scale_x_continuous(limits=c(0,200),breaks=seq(0, 200, 20))+
  ggtitle("Histogram of amount per act for PA")

query5="SELECT id, amount
FROM acts
WHERE ACT_DATE>='2017-01-01' AND act_type_id='DO'
ORDER BY 2"
data5=sqlQuery(db, query5)
ggplot(data5, aes(x=amount))+
  geom_histogram(bins = 50)+
  ggtitle("Histogram of amount per act for DO")+
  scale_x_continuous(limits = c(0,200))

query6="SELECT SUBSTR(zip_code,1,2) as department, AVG(amount), SUM(amount)
FROM contacts
LEFT JOIN acts
ON contacts.id=acts.contact_id
WHERE acts.amount IS NOT NULL
GROUP BY department
ORDER BY department"
data6=sqlQuery(db,query6)
data6=data6[1:100,]
query6.1="SELECT SUBSTR(zip_code,1,2) as department, AVG(amount), SUM(amount)
FROM contacts
LEFT JOIN acts
ON contacts.id=acts.contact_id
WHERE acts.amount IS NOT NULL
GROUP BY department
ORDER BY SUM(amount) DESC
LIMIT 10"
data6.1=sqlQuery(db,query6.1)

query7="SELECT MONTH(act_date), SUM(amount) 
FROM acts
WHERE act_date>='2017-01-01'AND act_date<'2018-01-01'
GROUP BY 1 "
data7=sqlQuery(db,query7)
query7.1="SELECT MONTH(act_date), SUM(amount) 
FROM acts
WHERE act_date>='2017-01-01'AND act_date<'2018-01-01' AND act_type_id='PA'
GROUP BY 1 "
data7.1=sqlQuery(db,query7.1)
query7.2="SELECT MONTH(act_date), SUM(amount) 
FROM acts
WHERE act_date>='2017-01-01'AND act_date<'2018-01-01' AND act_type_id='DO'
GROUP BY 1 "
data7.2=sqlQuery(db,query7.2)
ggplot(data7,mapping=aes(`MONTH(act_date)`,y=`SUM(amount)`,color="Total"))+
  geom_line()+
  geom_line(data7.1,mapping=aes(`MONTH(act_date)`,y=`SUM(amount)`,color="PA"))+
  geom_line(data7.2,mapping=aes(`MONTH(act_date)`,y=`SUM(amount)`,color="DO"))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_color_discrete(name="act_type")

# the average of 2008-2017
query7="SELECT MONTH(act_date), SUM(amount) 
FROM acts
WHERE act_date>='2008-01-01'AND act_date<'2018-01-01'
GROUP BY 1 "
data7=sqlQuery(db,query7)
query7.1="SELECT MONTH(act_date), SUM(amount) 
FROM acts
WHERE act_date>='2008-01-01'AND act_date<'2018-01-01' AND act_type_id='PA'
GROUP BY 1 "
data7.1=sqlQuery(db,query7.1)
query7.2="SELECT MONTH(act_date), SUM(amount) 
FROM acts
WHERE act_date>='2008-01-01'AND act_date<'2018-01-01' AND act_type_id='DO'
GROUP BY 1 "
data7.2=sqlQuery(db,query7.2)
ggplot(data7,mapping=aes(`MONTH(act_date)`,y=`SUM(amount)`,color="Total"))+
  geom_line()+
  geom_line(data7.1,mapping=aes(`MONTH(act_date)`,y=`SUM(amount)`,color="PA"))+
  geom_line(data7.2,mapping=aes(`MONTH(act_date)`,y=`SUM(amount)`,color="DO"))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_color_discrete(name="act_type")

query8="SELECT prefix_id as prefix, AVG(amount), SUM(amount)
FROM contacts
LEFT JOIN acts
ON contacts.id=acts.contact_id
WHERE acts.act_date>='2017-06-25' 
GROUP BY prefix
ORDER BY SUM(amount) DESC"
data8=sqlQuery(db,query8)
ggplot(data8[1:4,], mapping=aes(x=`SUM(amount)`,y=`prefix`,fill=`AVG(amount)`))+
  geom_bar(stat='identity',width=0.5)+
  scale_fill_gradientn(limits = c(20,30), colours=c("navyblue", "darkmagenta", "darkorange1"))+
  theme(aspect.ratio = 1/2)
  
query="SELECT YEAR(acts.act_date), COUNT(contacts.id)
FROM contacts
LEFT JOIN acts
ON contacts.id=acts.contact_id
WHERE contacts.active=1 AND YEAR(acts.act_date)>1990 AND YEAR(acts.act_date)<2018
GROUP BY 1
ORDER BY 1"
data=sqlQuery(db,query)
ggplot(data,mapping=aes(x=`YEAR(acts.act_date)`,y=`COUNT(contacts.id)`))+
  geom_line()+
  scale_x_continuous(breaks=seq(1991, 2017, 2))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

 
#install.packages("RODBC")
library(RODBC)
odbcDataSources()
library(ggplot2)

# Connect to you new data source
# Mine is “mysql_odbc_64”, with “root”/”” for credentials 
db = odbcConnect("mysqlodbc", uid="jonny", pwd="puangelo.scn92")

# Load the first 1000 rows of a table into a data frame
query = "SELECT * FROM ma_charity_small.contacts LIMIT 1000" 
df = sqlQuery(db, query)

# If embedded in a function, use print(head(df)) instead
head(df)

# Close the connection
odbcClose(db)

#How much money is collected every year?
query2 = "SELECT YEAR(act_date) AS DonationYear, SUM(amount) AS CollectedSum
FROM acts
GROUP BY DonationYear
ORDER BY DonationYear DESC"
df2 = sqlQuery(db, query2)
head(df2)
plot(df2)

#How much money is collected through classic donations (ActType=’DO’) and automatic deductions (ActType=’PA’)?
query3 = "SELECT YEAR(act_date) AS DonationYear, SUM(amount) AS CollectedSum, 
act_type_id
FROM acts
GROUP BY act_type_id, DonationYear
ORDER BY act_type_id, DonationYear DESC;"
df3 = sqlQuery(db, query3)
head(df3)
#plot(df3[1], df3[2], col=df3[3])
plot(df3$DonationYear,df3$CollectedSum, col=df3$act_type_id)

#How many donors?
query4 = "SELECT YEAR(act_date) AS DonationYear,
COUNT(DISTINCT(id)) AS Count
FROM acts
GROUP BY DonationYear
ORDER BY DonationYear DESC;;"
df4 = sqlQuery(db, query4)
head(df4)
plot(df4)

# Is the average donation amount increasing or decreasing? How much is it?
query5 = "SELECT YEAR(act_date) AS DonationYear, AVG(amount) AS AvgAmount
FROM acts
GROUP BY DonationYear
ORDER BY DonationYear DESC;"
df5 = sqlQuery(db, query5)
head(df5)
plot(df5)

# How many “new” donors were acquired every year?
query6 = "SELECT a.AcquiryYear, 
COUNT(a.contact_id) AS COUNT
FROM
(
  SELECT contact_id,
  MIN(act_date) AS AcquiryDate,
  YEAR(act_date) AS AcquiryYear
  FROM acts 
  GROUP BY contact_id, AcquiryYear) a
GROUP BY a.AcquiryYear
ORDER BY a.AcquiryYear DESC;"
df6 = sqlQuery(db, query6)
head(df6)
plot(df6)

# development of channels for donations through the years (e.g. street, event, telephone, ...)
# in terms of amount and number of transactions
query7 = "SELECT YEAR(act_date) AS DonationYear, 
SUM(amount) AS CollectedSum, 
channel_id,
COUNT(*) AS Count
FROM acts
GROUP BY DonationYear, channel_id
ORDER BY DonationYear DESC;"
df7 = sqlQuery(db, query7)
head(df7)
plot(df7$DonationYear, df7$CollectedSum, col=df7$channel_id)

#df7$DonationYear <- c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
df7$DonationYear <- as.Date(ISOdate(df7$DonationYear, 1, 1))
head(df7)
ggplot(df7,mapping=aes(DonationYear,y=CollectedSum,color=channel_id))+
  geom_line()+
  scale_x_date(limits = as.Date(c('2008-01-01','2017-12-31')))+
  labs(title="Development of channels for donations", x="Years", y="Amount")



# development of payment methods for donations through the years 
# in terms of amount
query8 = "SELECT YEAR(act_date) AS DonationYear, SUM(amount) AS CollectedSum, 
payment_method_id,
COUNT(*) AS Count
FROM acts
GROUP BY DonationYear, payment_method_id
ORDER BY DonationYear DESC;"
df8 = sqlQuery(db, query8)
head(df8)
plot(df8$DonationYear, df8$CollectedSum, col=df8$payment_method_id)
#legend(x=1, y=NULL, legend="Legend")
df8$DonationYear <- as.Date(ISOdate(df8$DonationYear, 1, 1))
head(df8)
ggplot(df8,mapping=aes(DonationYear,y=CollectedSum,color=payment_method_id))+
  geom_line()+
  scale_x_date(limits = as.Date(c('2008-01-01','2017-12-31')))+
  labs(title="Development of payment methods for donations", x="Years", y="Amount")


# When were the campaigns made?
query9 = "SELECT MIN(action_date) AS CampaignDate, campaign_id
FROM ma_charity_small.actions
GROUP BY campaign_id
ORDER BY CampaignDate;"
df9 = sqlQuery(db, query9)
head(df9)
plot(df9)

# number of contacts approached per campaign
query10 = "SELECT campaign_id, 
MIN(action_date) AS CampaignStart, 
MONTH(action_date) AS CampaignMonth, 
YEAR(action_date) AS CampaignYear, 
COUNT(contact_id) AS Count
FROM actions
GROUP BY campaign_id, CampaignMonth, CampaignYear
ORDER BY CampaignStart;"
df10 = sqlQuery(db, query10)
head(df10)
plot(df10$CampaignStart, df10$Count)

# how many messages receive low-value donors vs. high-value donors after their last donation
query11 = "SELECT b.*, MAX(a.action_date) AS LatestMessage, 
a.message_id, 
DATEDIFF(MAX(a.action_date), b.LastDonation) AS Datedifference,
COUNT(action_date) AS MessagesAfterLastDonation,
DATEDIFF(MAX(a.action_date), b.LastDonation)/COUNT(action_date) AS MessageEveryXDays,
SUM(a2.amount) AS DonatedAmount
FROM 
(SELECT contact_id, 
  MAX(act_date) AS LastDonation
  FROM ma_charity_small.acts
  GROUP BY contact_id) b
JOIN actions a
ON a.contact_id = b.contact_id
JOIN acts a2
ON a2.contact_id = b.contact_id
WHERE a.action_date > b.LastDonation
GROUP BY b.contact_id
;"
df11 = sqlQuery(db, query11)
head(df11)
#plot(df11$MessageEveryXDays, df11$DonatedAmount, xlim=c(0, 200), ylim=c(2000, 10000))
ggplot(df11,mapping=aes(df11$MessageEveryXDays,y=df11$DonatedAmount,color="Total"))+
  geom_point()+
  xlim(c(0,150))+
  ylim(c(0,5000))+
  labs(title="Message frequency after last donation vs. donated total amount", x="Donor Messaged Every X Days", y="Donated Amount")


# number of people contacted by campaigns by year
query12 = "SELECT YEAR(action_date) AS CampaignYear, COUNT(*) AS Count
FROM ma_charity_small.actions
GROUP BY CampaignYear
ORDER BY CampaignYear;"
df12 = sqlQuery(db, query12)
query12.1 = "# how many campaigns per year
SELECT a.CampaignYear, COUNT(*) AS CountOfCampaigns
FROM
(SELECT YEAR(MIN(action_date)) AS CampaignYear, MONTH(MIN(action_date)) AS CampaignMonth, campaign_id
FROM ma_charity_small.actions
GROUP BY campaign_id
ORDER BY CampaignYear, CampaignMonth) a
GROUP BY a.CampaignYear;"
df12.1 = sqlQuery(db, query12.1)
head(df12)
df12$CampaignYear <- c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
df12$CampaignYear <- as.Date(ISOdate(df12$CampaignYear, 1, 1))
df12.1$CampaignYear <- c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
df12.1$CampaignYear <- as.Date(ISOdate(df12.1$CampaignYear, 1, 1))
options(scipen=5)
#install.packages("patchwork")
#install.packages("hrbrthemes")
#install.packages("dplyr")
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(dplyr)
p1 <- ggplot(df12,mapping=aes(CampaignYear,y=Count,color="Amount"))+
  geom_line()+
  geom_line(df12.1,mapping=aes(CampaignYear,y=CountOfCampaigns,color="Campaigns"))+
  scale_x_date(limits = as.Date(c('2008-01-01','2017-12-31')))+
  scale_y_continuous(
    "Amount", 
    sec.axis = sec_axis(~ . / 10000, name = "People"))+
  labs(title="Number of people contacted by campaigns each year", x="Years", y="Amount")+
  theme(
    axis.title.y = element_text(color = "grey"),
    axis.title.y.right = element_text(color = "blue"))

p1

p2 <- ggplot(df12.1,mapping=aes(CampaignYear,y=CountOfCampaigns))+
  geom_line()+
  scale_x_date(limits = as.Date(c('2008-01-01','2017-12-31')))+
  labs(title="Number of campaigns each year", x="Years", y="Amount")

p2
