# Load the package
library(RODBC)
library(ggplot2)
library(raster)
library(dplyr)
library(tidyverse)


# Connect to you new data source

db = odbcConnect("mysql_odbc_connection", uid="root", pwd="")
sqlQuery(db, "USE ma_charity_full")


# Load the first 1000 rows of a table into a data frame
query = "SELECT d.contact_id,
	   d.calibration,
       d.donation,
       d.amount,
       g.active,
       CASE
            WHEN a.act_type_id in ('DO') THEN 1
            ELSE 0
       END AS act_type,
                DATEDIFF(20180626, MAX(a.act_date)) / 365 AS 'recency',
                COUNT(a.amount) AS 'frequency',
                AVG(a.amount) AS 'avgamount',
                DATEDIFF(20180626, MIN(a.act_date)) / 365 AS 'firstdonation',
                MAX(a.amount) AS 'maxamount',
                IF(c.counter IS NULL, 0, c.counter) AS 'nb_donation',
                e.gift_by_campaign,
                IF(f.channel_id in ('MA','WW','TE'),1,0) AS 'channel',
                IF(f.payment_method_id in ('PR','AU'),0,1) AS 'payment',
                CASE
                    WHEN g.prefix_id in ('MMME','MME','MLLE') THEN 1
                    WHEN g.prefix_id in ('MR','ME','DR') THEN 2
                    WHEN g.prefix_id in ('AU','NA') THEN 3
                    ELSE 0
				END AS 'prefix'
                
                
                
		 FROM assignment2 d
         LEFT JOIN acts a
         ON d.contact_id=a.contact_id
         LEFT JOIN (SELECT contact_id, COUNT(amount) AS counter
                    FROM Acts
                    WHERE (act_date >= 20170626) AND
                    (act_date <  20180626) AND
                    (act_type_id = 'DO')
                    GROUP BY contact_id) AS c
         ON c.contact_id = a.contact_id
         LEFT JOIN (SELECT contact_id, COUNT(campaign_id) AS gift_by_campaign
				    FROM Acts
                    WHERE campaign_id is not null
                    GROUP BY contact_id) AS e
         ON e.contact_id=a.contact_id
         LEFT JOIN (SELECT contact_id, channel_id, payment_method_id, MAX(act_date)
					from acts
					group by contact_id
					order by contact_id, act_date desc) f
		 ON f.contact_id=a.contact_id
         
         LEFT JOIN ( SELECT id, prefix_id, active 
					 FROM contacts) g
		 ON g.id = a.contact_id
         
		
         GROUP BY 1"

data1 = sqlQuery(db, query)


query2= "SELECT contact_id, 
CASE
	WHEN segment in ('COLD') THEN 1
	WHEN segment in ('LOST') THEN 2
	WHEN segment in ('NEW') THEN 3
	WHEN segment in ('WARM') THEN 4
	WHEN segment in ('BOTTOM') THEN 5
	WHEN segment in ('TOP') THEN 6
	ELSE 0
END AS segment
FROM segments
WHERE period_id=0"
data2= sqlQuery(db,query2)


query5="SELECT contact_id, COUNT(amount) AS last_counter
              FROM Acts
                    WHERE (act_date >= 20160626) AND
                    (act_date <  20170626) AND
                    (act_type_id = 'DO')
                    GROUP BY contact_id"
data5= sqlQuery(db,query5)
data6 = left_join(data1, data2, by ="contact_id")
data = left_join(data6, data5, by ="contact_id")
data_predict = left_join(data6, data5, by ="contact_id")

odbcClose(db)



rownames(data) = data$contact_id
data = data[, -1]


#############################################
#############################################
#############################################


library(nnet)

k1 = which(is.na(data$last_counter))
data$last_counter[k1]=0

k2 = which(is.na(data_predict$last_counter))
data_predict$last_counter[k2]=0


z=which(data$calibration == '1')
train = data[z,]




#############################################
#############################################



# In-sample, donation amount model
# Note that the amount model only applies to a subset of donors...


nfold = 10
nobs  = nrow(train)
index = rep(1:nfold, length.out = nobs)
probs = rep(0, nobs)
formula = "donation ~ (recency:frequency) + log(recency) + log(frequency) + firstdonation + segment+ channel + payment + active + nb_donation+(nb_donation:recency)+(nb_donation*last_counter)"

prob.model = multinom(formula, data)

for (i in 1:nfold) {
  
  # Assign in-sample and out-of-sample observations
  insample  = which(index != i)
  outsample = which(index == i)
  
  # Run model on in-sample data only
  submodel = multinom(formula, train[insample, ])
  
  # Obtain predicted probabilities on out-of-sample data
  probs[outsample] = predict(object = submodel, newdata = train[outsample, ], type = "probs")
  
}

# Print cross-validated probabilities
#print(probs)

# How many loyal donors among the top 2000
# in terms of (out-of-sample) probabilities?
pred = data.frame(model = probs, truth = train$donation)
pred = pred[order(pred$model, decreasing = TRUE), ]
#print(pred)
print(sum(pred$truth[1:2000]) / 2000)

# vs. full model used to make actual predictions
probs = predict(object = prob.model, newdata = train, type = "probs")
pred = data.frame(model = probs, truth = train$donation)
pred = pred[order(pred$model, decreasing = TRUE), ]
print(sum(pred$truth[1:2000]) / 2000)



#########################################################
#########################################################


# Get coefficients, standard errors
coeff = t(summary(prob.model)$coefficients)
stder = t(summary(prob.model)$standard.errors)
zvalues = coeff / stder
pvalues = (1 - pnorm(abs(zvalues), 0, 1)) * 2

# Print results
print("coefficients:")
print(coeff)
print("standard deviations:")
print(stder)
print("p-values")
print(pvalues)


#########################################################
#########################################################


# In-sample, donation amount model
# Note that the amount model only applies to a subset of donors...
z2 = which(!is.na(train$amount))
train2 = train[z2, ]
print(head(train2))
print(head(train[z2, ]))


nfold2 = 5
nobs2  = nrow(train[z2,])
index2 = rep(1:nfold2, length.out = nobs2)
prediction = rep(0, nobs2)

formula = "log(amount) ~ log(avgamount) + log(maxamount)+ segment + channel + act_type"
amount.model = lm(formula ,data = train2)

for (i in 1:nfold2) {
  
  # Assign in-sample and out-of-sample observations
  insample  = which(index2 != i)
  outsample = which(index2 == i)
  
  # Run model on in-sample data only
  submodel = lm(formula, train2[insample, ])
  
  # Obtain predicted probabilities on out-of-sample data
  prediction[outsample] = exp(predict(object = submodel, newdata = train2[outsample,]))
  
}

# Print cross-validated probabilities
#print(probs)

# How many loyal donors among the top 2000
# in terms of (out-of-sample) probabilities?
pred = data.frame(model = prediction, truth = train2$amount)
pred = pred[order(pred$model, decreasing = TRUE), ]
#print(pred)
check<-0
for (i in 1:2000){
  check = check + (pred$model[i]-pred$truth[i])^2
}
check=sqrt(check)
print(check)


###################################################
###################################################

# Get coefficients, standard errors
coeff = t(summary(amount.model)$coefficients)

print("coefficients:")
print(coeff)

###################################################
###################################################

z3=which(data_predict$calibration == '0')
newdata = data_predict[z3,]
print(head(newdata))

newdata = newdata[,-3]
newdata = newdata[,-3]
print(head(newdata))

k2 = which(is.na(newdata$last_counter))
print(k2)



# Out-of-sample predictions
# Do NOT forget to re-transform "log(amount)" into "amount"
out = data.frame(contact_id = newdata$contact_id)
out$probs  = predict(object = prob.model, newdata = newdata, type = "probs")
out$amount = exp(predict(object = amount.model, newdata = newdata))
out$score  = out$probs * out$amount

# Show results
print(head(out))

# Who is likely to be worth more than 5 EUR?
z1 = which(out$score > 2)
z2 = which(out$score <= 2)
print(nrow(out))
print(out)



result<- data.frame(contact_id = out$contact_id, sollicit=out$score)
result$sollicit[z1] = 1
result$sollicit[z2] = 0


print(result[z1,])

write.table(result, file="result4.txt", sep="\t", row.names=FALSE, col.names=FALSE)


#####################################################
#####################################################
