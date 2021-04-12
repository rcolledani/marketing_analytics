
# number of people contacted by campaigns by month
query13 = "SELECT MONTH(action_date) AS CampaignMonth, COUNT(*) AS Count
FROM ma_charity_small.actions
GROUP BY CampaignMonth
ORDER BY CampaignMonth;"
df13 = sqlQuery(db, query13)
head(df13)
ggplot(df13,mapping=aes(df13$CampaignMonth,y=df13$Count,color="Total"))+
  geom_line()+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  labs(title="Number of people contacted by campaigns per month", x="Months", y="Amount")

# how many campaigns per month
query13.1 = "SELECT a.CampaignMonth, COUNT(*) AS CountOfCampaigns
FROM
(SELECT YEAR(MIN(action_date)) AS CampaignYear, MONTH(MIN(action_date)) AS CampaignMonth, campaign_id
FROM ma_charity_small.actions
GROUP BY campaign_id
ORDER BY CampaignYear, CampaignMonth) a
GROUP BY a.CampaignMonth
ORDER BY a.CampaignMonth;"
df13.1 = sqlQuery(db, query13.1)
head(df13.1)
ggplot(df13.1,mapping=aes(CampaignMonth,y=CountOfCampaigns))+
  geom_line()+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  labs(title="Number of campaigns per month", x="Months", y="Amount")

# how long after the last donation does the charity still message their donors
query14 = "SELECT b.*, a.action_date, a.message_id, DATEDIFF(a.action_date, b.LastDonation) AS Datedifference
FROM 
(SELECT contact_id, 
MAX(act_date) AS LastDonation
FROM ma_charity_small.acts
GROUP BY contact_id) b
JOIN actions a
ON a.contact_id = b.contact_id
WHERE a.action_date > b.LastDonation"
df14 = sqlQuery(db, query14)
head(df14)
ggplot(df14,mapping=aes(x=Datedifference))+
  geom_bar()+
  xlim(c(0,2200))+
  labs(title="Number of days between last donation and message", x="Days", y="Count")


# what's the latest after the last donation that the charity still message their donors and how many messages?
query15 = "SELECT b.*, MAX(a.action_date) AS LatestMessage, 
a.message_id, 
DATEDIFF(MAX(a.action_date), b.LastDonation) AS Datedifference,
COUNT(action_date) AS NumberOfMessagesAfterLastDonation
FROM 
(SELECT contact_id, 
  MAX(act_date) AS LastDonation
  FROM ma_charity_small.acts
  GROUP BY contact_id) b
JOIN actions a
ON a.contact_id = b.contact_id
WHERE a.action_date > b.LastDonation
GROUP BY b.contact_id
;"
df15 = sqlQuery(db, query15)
head(df15)
ggplot(df15,mapping=aes(x=Datedifference,y=NumberOfMessagesAfterLastDonation))+
  geom_point()+
  xlim(c(0,3000))+
  labs(title="Relationship of message frequency vs. time difference of last donation to last message", x="Days between last donation and last message", y="Messages")


# frequency of follow-up messages after last donation and value-type according to total donation amount
query16 = "SELECT c.*,
SUM(a2.amount) AS DonatedAmount,
CASE 
WHEN SUM(a2.amount) >= 1000 THEN 'HV'
WHEN SUM(a2.amount) < 1000 THEN 'LV'
END AS ValueType
FROM
(
  SELECT b.*, MAX(a.action_date) AS LatestMessage, 
  a.message_id, 
  DATEDIFF(MAX(a.action_date), b.LastDonation) AS Datedifference,
  COUNT(a.action_date) AS MessagesAfterLastDonation,
  DATEDIFF(MAX(a.action_date), b.LastDonation)/COUNT(action_date) AS MessageEveryXDays
  FROM 
  (SELECT contact_id, 
    MAX(act_date) AS LastDonation
    FROM ma_charity_small.acts
    GROUP BY contact_id) b
  JOIN actions a
  ON a.contact_id = b.contact_id
  WHERE a.action_date > b.LastDonation
  GROUP BY a.contact_id) c
JOIN acts a2
ON a2.contact_id = c.contact_id
GROUP BY c.contact_id
;"
df16 = sqlQuery(db, query16)
head(df16)
ggplot(df16,mapping=aes(x=MessageEveryXDays,y=DonatedAmount, color=ValueType))+
  geom_point()+
  xlim(c(0,2000))+
  labs(title="Donated Amount vs. a message every X days", x="a message every X days", y="Donated Amount")


# what share of messages and campaigns was followed by donations?
query17 = "SELECT c.*, 
COUNT(a2.campaign_id) AS CountOfActions,
ROUND(NumberOfDonations/COUNT(a2.campaign_id),2) AS DonationsPerMessages
FROM
(SELECT campaign_id, 
  MIN(act_date) AS EarliestDonation,
  SUM(amount) AS AmountDonated,
  COUNT(amount) AS NumberOfDonations,
  ROUND(SUM(amount)/COUNT(amount),2) AS AverageDonationAmount,
  act_type_id
  FROM ma_charity_small.acts
  WHERE campaign_id IS NOT NULL
  GROUP BY campaign_id, act_type_id
  ORDER BY campaign_id, MIN(act_date)) c
RIGHT JOIN actions a2
ON c.campaign_id = a2.campaign_id
GROUP BY a2.campaign_id, act_type_id;"
df17 = sqlQuery(db, query17)
head(df17)
ggplot(df17,mapping=aes(x=EarliestDonation,y=AverageDonationAmount, color=act_type_id))+
  geom_point()+
  scale_x_date(limits = as.Date(c("2008-01-01","2017-12-31")))+
  geom_smooth(method = "lm")+
  labs(title="Average Donations per Campaign", x="Date", y="Average Donated Amount")

ggplot(df17,mapping=aes(x=EarliestDonation,y=DonationsPerMessages, color=act_type_id))+
  geom_point()+
  scale_x_date(limits = as.Date(c("2008-01-01","2017-12-31")))+
  geom_smooth(method = "lm")+
  labs(title="Number of Donations per Campaign Messages", x="Date", y="Donations Per Messages")
 
#count of active donors
SELECT active,
COUNT(*) AS Count  
FROM ma_charity_small.contacts c
GROUP BY c.active
ORDER BY Count DESC
LIMIT 100;

#count of donors by zip code
SELECT town_clean, zip_code,
COUNT(*) AS Count  
FROM ma_charity_small.contacts c
GROUP BY c.town_clean, zip_code
ORDER BY Count DESC
LIMIT 100;

#count of donors by city
SELECT town_clean,
COUNT(*) AS Count  
FROM ma_charity_small.contacts c
GROUP BY c.town_clean
ORDER BY Count DESC
LIMIT 100;

# development of channels for donations through the years (e.g. street, event, telephone, ...)
# in terms of amount and number of transactions
SELECT SUM(amount) AS CollectedSum, 
channel_id,
YEAR(act_date) AS DonationYear,
COUNT(*) AS Count
FROM acts
GROUP BY DonationYear, channel_id
ORDER BY DonationYear DESC;

# development of payment methods for donations through the years 
# in terms of amount
SELECT YEAR(act_date) AS DonationYear, SUM(amount) AS CollectedSum, 
payment_method_id,
COUNT(*) AS Count
FROM acts
GROUP BY DonationYear, payment_method_id
ORDER BY DonationYear DESC;

# number of campagins made
SELECT COUNT(DISTINCT(campaign_id)) AS Count
FROM ma_charity_small.actions;

# number of messages sent
SELECT COUNT(DISTINCT(message_id)) AS Count
FROM ma_charity_small.actions;

# number of people contacted by campaigns
SELECT campaign_id, COUNT(*) AS Count
FROM ma_charity_small.actions
GROUP BY campaign_id
ORDER BY campaign_id;

# number of people contacted by campaigns by year
SELECT YEAR(action_date) AS CampaignYear, COUNT(*) AS Count
FROM ma_charity_small.actions
GROUP BY CampaignYear
ORDER BY CampaignYear;

# number of people contacted by campaigns by month
SELECT MONTH(action_date) AS CampaignMonth, COUNT(*) AS Count
FROM ma_charity_small.actions
GROUP BY CampaignMonth
ORDER BY CampaignMonth;

# When were the campaigns made?
SELECT YEAR(MIN(action_date)) AS CampaignYear, MONTH(MIN(action_date)) AS CampaignMonth, campaign_id
FROM ma_charity_small.actions
GROUP BY campaign_id
ORDER BY CampaignYear, CampaignMonth;

# how many campaigns per year
SELECT a.CampaignYear, COUNT(*) AS CountOfCampaigns
FROM
(SELECT YEAR(MIN(action_date)) AS CampaignYear, MONTH(MIN(action_date)) AS CampaignMonth, campaign_id
FROM ma_charity_small.actions
GROUP BY campaign_id
ORDER BY CampaignYear, CampaignMonth) a
GROUP BY a.CampaignYear;

# how many campaigns per month
SELECT a.CampaignMonth, COUNT(*) AS CountOfCampaigns
FROM
(SELECT YEAR(MIN(action_date)) AS CampaignYear, MONTH(MIN(action_date)) AS CampaignMonth, campaign_id
FROM ma_charity_small.actions
GROUP BY campaign_id
ORDER BY CampaignYear, CampaignMonth) a
GROUP BY a.CampaignMonth
ORDER BY a.CampaignMonth;

#SET GLOBAL sql_mode=(SELECT REPLACE(@@sql_mode,'ONLY_FULL_GROUP_BY',''));

# number of contacts approached per campaign
SELECT campaign_id, 
MIN(action_date) AS CampaignStart, 
MAX(action_date) AS CampaignEnd,
MONTH(action_date) AS CampaignMonth, 
YEAR(action_date) AS CampaignYear, 
COUNT(DISTINCT(contact_id)) AS Count
FROM actions
GROUP BY campaign_id, CampaignMonth, CampaignYear
ORDER BY CampaignStart;

# how many actions did each contact "receive" (= how many messages?)
SELECT contact_id, COUNT(*) AS Count 
FROM ma_charity_small.actions
GROUP BY contact_id;

# average number of messages per contact
SELECT ROUND(AVG(Count),2) AS AverageMessages 
FROM
(
SELECT contact_id, COUNT(*) AS Count 
FROM ma_charity_small.actions
GROUP BY contact_id) a;

# average number of messages per year per contact
SELECT a.MessageYear, ROUND(AVG(Count),2) AS AverageMessages 
FROM
(
SELECT contact_id, 
COUNT(*) AS Count, 
YEAR(action_date) AS MessageYear 
FROM ma_charity_small.actions
GROUP BY contact_id, MessageYear) a
GROUP BY a.MessageYear
ORDER BY a.MessageYear DESC;

# average number of messages per month per contact
SELECT a.MessageMonth, ROUND(AVG(Count),2) AS AverageMessages 
FROM
(
SELECT contact_id, 
COUNT(*) AS Count, 
MONTH(action_date) AS MessageMonth 
FROM ma_charity_small.actions
GROUP BY contact_id, MessageMonth) a
GROUP BY a.MessageMonth
ORDER BY a.MessageMonth ASC;

# do messages also go to inactive contacts
SELECT a.*, c.active
#, COUNT(*) AS Count 
FROM ma_charity_small.actions a
JOIN ma_charity_small.contacts c
ON a.contact_id = c.id
WHERE c.active=0
AND a.action_date > "2018-01-01"
#GROUP BY contact_id
;

# how long after the last donation does the charity still message their donors
SELECT b.*, a.action_date, a.message_id, DATEDIFF(a.action_date, b.LastDonation) AS Datedifference
FROM 
(SELECT contact_id, 
MAX(act_date) AS LastDonation
FROM ma_charity_small.acts
GROUP BY contact_id) b
JOIN actions a
ON a.contact_id = b.contact_id
WHERE a.action_date > b.LastDonation
;

# what's the latest after the last donation that the charity still message their donors and how many messages?
SELECT b.*, MAX(a.action_date) AS LatestMessage, 
a.message_id, 
DATEDIFF(MAX(a.action_date), b.LastDonation) AS Datedifference,
COUNT(action_date) AS NumberOfMessagesAfterLastDonation
FROM 
(SELECT contact_id, 
MAX(act_date) AS LastDonation
FROM ma_charity_small.acts
GROUP BY contact_id) b
JOIN actions a
ON a.contact_id = b.contact_id
WHERE a.action_date > b.LastDonation
GROUP BY b.contact_id
;

# what's the average latest message after the last donation and how many messages on average?
SELECT ROUND(AVG(c.DateDifference),2) AS AverageLastMessageDays, 
ROUND(AVG(c.NumberOfMessagesAfterLastDonation),2) AS AverageNumMessagesAfterLastDonation
FROM
(SELECT b.*, MAX(a.action_date) AS LatestMessage, 
a.message_id, 
DATEDIFF(MAX(a.action_date), b.LastDonation) AS Datedifference,
COUNT(action_date) AS NumberOfMessagesAfterLastDonation
FROM 
(SELECT contact_id, 
MAX(act_date) AS LastDonation
FROM ma_charity_small.acts
GROUP BY contact_id) b
JOIN actions a
ON a.contact_id = b.contact_id
WHERE a.action_date > b.LastDonation
GROUP BY b.contact_id) c
;

# frequency of follow-up messages after last donation and value-type according to total donation amount
SELECT c.*,
SUM(a2.amount) AS DonatedAmount,
CASE 
WHEN SUM(a2.amount) >= 1000 THEN "HV"
WHEN SUM(a2.amount) < 1000 THEN "LV"
END AS ValueType
FROM
(
SELECT b.*, MAX(a.action_date) AS LatestMessage, 
a.message_id, 
DATEDIFF(MAX(a.action_date), b.LastDonation) AS Datedifference,
COUNT(a.action_date) AS MessagesAfterLastDonation,
DATEDIFF(MAX(a.action_date), b.LastDonation)/COUNT(action_date) AS MessageEveryXDays
FROM 
(SELECT contact_id, 
MAX(act_date) AS LastDonation
FROM ma_charity_small.acts
GROUP BY contact_id) b
JOIN actions a
ON a.contact_id = b.contact_id
WHERE a.action_date > b.LastDonation
GROUP BY a.contact_id) c
JOIN acts a2
ON a2.contact_id = c.contact_id
GROUP BY c.contact_id
;

# how many messages receive low-value donors vs. high-value donors after their last donation
SELECT d.ValueType, 
ROUND(AVG(MessageEveryXDays),2) AS AverageMessagesEveryXDays,
ROUND(AVG(DonatedAmount),2) AS AverageDonatedAmount
FROM
(SELECT c.*,
SUM(a2.amount) AS DonatedAmount,
CASE 
WHEN SUM(a2.amount) >= 1000 THEN "HV"
WHEN SUM(a2.amount) < 1000 THEN "LV"
END AS ValueType
FROM
(
SELECT b.*, MAX(a.action_date) AS LatestMessage, 
a.message_id, 
DATEDIFF(MAX(a.action_date), b.LastDonation) AS Datedifference,
COUNT(a.action_date) AS MessagesAfterLastDonation,
DATEDIFF(MAX(a.action_date), b.LastDonation)/COUNT(action_date) AS MessageEveryXDays
FROM 
(SELECT contact_id, 
MAX(act_date) AS LastDonation
FROM ma_charity_small.acts
GROUP BY contact_id) b
JOIN actions a
ON a.contact_id = b.contact_id
WHERE a.action_date > b.LastDonation
GROUP BY a.contact_id) c
JOIN acts a2
ON a2.contact_id = c.contact_id
GROUP BY c.contact_id
) d
GROUP BY d.ValueType
;

# (average) number of messages for "old" and for "new" contacts


# what was the donation outcome of campaigns?
SELECT campaign_id, 
MIN(act_date) AS EarliestDonation,
SUM(amount) AS AmountDonated,
COUNT(amount) AS NumberOfDonations,
ROUND(SUM(amount)/COUNT(amount),2) AS AverageDonationAmount,
act_type_id
FROM ma_charity_small.acts
WHERE campaign_id IS NOT NULL
GROUP BY campaign_id, act_type_id
ORDER BY campaign_id, MIN(act_date);

# what share of messages and campaigns was followed by donations?
SELECT c.*, 
COUNT(a2.campaign_id) AS CountOfActions,
ROUND(NumberOfDonations/COUNT(a2.campaign_id),2) AS DonationsPerMessages
FROM
(SELECT campaign_id, 
MIN(act_date) AS EarliestDonation,
SUM(amount) AS AmountDonated,
COUNT(amount) AS NumberOfDonations,
ROUND(SUM(amount)/COUNT(amount),2) AS AverageDonationAmount,
act_type_id
FROM ma_charity_small.acts
WHERE campaign_id IS NOT NULL
GROUP BY campaign_id, act_type_id
ORDER BY campaign_id, MIN(act_date)) c
RIGHT JOIN actions a2
ON c.campaign_id = a2.campaign_id
GROUP BY a2.campaign_id, act_type_id;

# what was the donation outcome of messages?
SELECT campaign_id, message_id, 
MIN(act_date) AS EarliestDonation,
SUM(amount) AS AmountDonated,
COUNT(amount) AS NumberOfDonations,
ROUND(SUM(amount)/COUNT(amount),2) AS AverageDonationAmount,
act_type_id
FROM ma_charity_small.acts
WHERE campaign_id IS NOT NULL
GROUP BY message_id
ORDER BY MIN(act_date);

# what share of messages and campaigns was followed by donations?
SELECT a1.contact_id, a1.campaign_id, a1.message_id, 
a2.amount, a2.contact_id, a2.campaign_id, a2.message_id
FROM actions a1
LEFT JOIN acts a2
ON a1.contact_id=a2.contact_id
AND a1.campaign_id=a2.campaign_id
AND a1.message_id=a2.message_id;
#UNION
SELECT a1.contact_id, a1.campaign_id, a1.message_id, 
a2.amount, a2.contact_id, a2.campaign_id, a2.message_id
FROM actions a1
RIGHT JOIN acts a2
ON a1.contact_id=a2.contact_id
AND a1.campaign_id=a2.campaign_id
AND a1.message_id=a2.message_id
;
 
USE ma_charity_small;
# Is the average donation amount increasing or decreasing? How much is it?
SELECT AVG(amount), 
YEAR(act_date) AS DonationYear,act_type_id
FROM acts
GROUP BY DonationYear, act_type_id
ORDER BY DonationYear DESC;
#how 2018 is going on in terms of new donors comparing to the other years on the same period
SELECT a.year_first_donation, count(a.contact_id) 
from
	(Select contact_id, min(YEAR(act_date)) as year_first_donation, ANY_VALUE(act_date) x
	from acts 
	group by 1 
    having month(x)<'6' 
	or (month(x)<'7' and day(x)<'26')
	order by 2) as a
group by 1
order by 1;
# create temporary table which only contains smae time perid as 2018
CREATE TEMPORARY TABLE IF NOT EXISTS temp_tb
AS 
SELECT
contact_id,  act_date, amount, act_type_id
FROM
acts
where month(act_date)<'6' 
or (month(act_date)='6' and day(act_date)<'26')
order by contact_id;
# how 2018 is going on in terms of donation frequency comparing to the other years on the same period
SELECT YEAR(act_date) AS DonationYear, COUNT(act_date), act_type_id
FROM temp_tb
GROUP BY DonationYear, act_type_id
ORDER BY DonationYear;
# how 2018 is going on in terms of donation amount comparing to the other years on the same period
SELECT YEAR(act_date) AS DonationYear, SUM(amount) AS CollectedSum, act_type_id
FROM temp_tb
GROUP BY DonationYear, act_type_id
ORDER BY DonationYear;
# how 2018 is going on in terms of average donation amount comparing to the other years on the same period
SELECT YEAR(act_date) AS DonationYear, AVG(amount) AS avgamount, act_type_id
FROM temp_tb
GROUP BY DonationYear, act_type_id
ORDER BY DonationYear;
