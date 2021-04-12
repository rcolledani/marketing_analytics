
### 00 Settings ------------------

# install.packages('CLVTools')
library(CLVTools)
library(RODBC)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)

odbcCloseAll()
# db_full = odbcConnect("mysql_server_64", uid="root", pwd="")
# db_full = odbcConnect("my_server_full", uid = "root", pwd = "rooTuseR$")
# db_full <- odbcConnect("mysql_odbc_connector", uid="root", pwd="")
db_full = odbcConnect("mysql_odbc_connection", uid="root", pwd="")



### 01 Queries ----------------

check = sqlQuery(db_full, "SELECT * FROM acts LIMIT 100")

mails <- sqlQuery(db_full, 'SELECT contact_id, message_id, action_date 
                  FROM ma_charity_full.actions
                  WHERE year(action_date) > 2012')

donations <- sqlQuery(db_full, "SELECT contact_id, message_id, 
                      act_date, amount, act_type_id
                      FROM ma_charity_full.acts")

donors <- sqlQuery(db_full, 'SELECT id as "contact_id", active 
                   FROM ma_charity_full.contacts')


# Responsiveness
# Donor responded if at least one donation for a message id
df_resp0 <- mails %>%
  left_join(distinct(donations, contact_id, message_id, .keep_all = T), 
            by = c('contact_id', 'message_id'), 
            na_matches = "never") %>%
  arrange(contact_id, message_id)

df_resp1 <- df_resp0 %>%
  group_by(contact_id) %>%
  dplyr:: summarise(responsiveness = sum(!is.na(amount)) / n(), 
                    emails_total = n()) %>%
  ungroup()
hist(df_resp1$responsiveness)

# Initiative
# Donor has shown initiative if a donation has no link to a valid message
df_init0 <- donations %>%
  filter(lubridate::year(act_date) > 2012 & act_type_id == 'DO') %>%
  left_join(distinct(mails, contact_id, message_id, .keep_all = T),
            by = c('contact_id', 'message_id'),
            na_matches = "never")

df_init1 <- df_init0 %>%
  group_by(contact_id) %>%
  mutate(no_id = is.na(action_date), 
         not_first = act_date > min(act_date)) %>%
  dplyr:: summarise(initiative = sum(no_id & not_first) / (n()-1),        # exclude first donations as 'initiative' donations
                    donations_total = n(), 
                    .groups = 'keep') %>%
  ungroup()
hist(df_init1$initiative)

# Number of donations per month
df_months <- donations %>%
  filter(lubridate::year(act_date) > 2012 & act_type_id == 'DO') %>%
  mutate(month = factor(str_c("don_month_", lubridate::month(act_date)), 
                        levels = str_c('don_month_', 1:12))) %>%
  group_by(contact_id, month) %>%
  dplyr:: summarise(donations = n(), .groups = 'keep') %>%
  ungroup() %>%
  spread(key = month, value = donations, fill = 0)


df = df_resp1 %>%
  left_join(df_init1, by = c("contact_id")) %>%
  left_join(df_months, by = c("contact_id")) %>%
  mutate_at(.vars = vars(matches('don_month|initiative|donations_total')), 
            .funs = ~ ifelse(is.na(.x), 0, .x))



# check <- donations %>%
#   left_join(mails, by = c("contact_id", "message_id")) %>%
#   mutate(date_diff = as.numeric(act_date - action_date)) %>%
#   filter(year(act_date) > 2015) %>%
#   filter(date_diff < 100, date_diff > 0) %>%
#   filter(lubridate::month(act_date) == 1)
# 
# 
# hist(check$date_diff, breaks = 30)
# table(round(check$date_diff, -1))
# 
# lubridate::month(donations$act_date) %>% table() %>% plot()

check2 <- df_allocation %>%
  mutate(clv_rounded = round(predicted.CLV, -1)) %>%
  group_by(clv_rounded) %>%
  arrange(desc(clv_rounded)) %>%
  filter(sum(responsiveness < 0.1) > 1 & sum(initiative > 0.9) > 1) %>%
  dplyr::select(contact_id, predicted.CLV, responsiveness, initiative, emails_total, donations_total, score)


### 02 CLV ------------
## Data
df1 <- sqlQuery(db_full, "SELECT contact_id, act_date, amount
                FROM ma_charity_full.acts 
                WHERE  act_type_id = 'DO'")


df1 <- filter_all(df1, .vars_predicate = ~all(!is.na(.x)))
colnames(df1) = c('Id', 'Date', 'Price')


df1_clv <- clvdata(df1, date.format = 'ymd', time.unit = "weeks") 
#summary(df1_clv)

## Estimating the PNBD model parameters
est.pnbd <- pnbd(clv.data = df1_clv, optimx.args = list(control=list(trace=5), method="Nelder-Mead"))
est.pnbd
#summary(est.pnbd)
#coef(est.pnbd)
#confint(est.pnbd)

## Predicting customers' behavior
results.pnbd <- predict(est.pnbd, prediction.end = 1040, predict.spending = TRUE, continuous.discount.factor = 0.001)

#plot(results.pnbd$predicted.CLV)

## Plot donations
#plot(est.pnbd)

## CLV
#results.pnbd[is.na(results.pnbd$predicted.CLV)]$predicted.CLV <- 0
#mean(results.pnbd$predicted.CLV)
#plot(results.pnbd$predicted.CLV)
#plot(results.pnbd[results.pnbd$predicted.CLV < 400]$predicted.CLV)
#plot(results.pnbd[results.pnbd$predicted.CLV > 400]$predicted.CLV)
#hist(log(results.pnbd$predicted.CLV), breaks = 100)

## PAlive
#plot(results.pnbd$PAlive)



### 03 Scoring model ------------

colnames(results.pnbd) <- c("contact_id", "period.first", "period.last", "period.length", "PAlive", "CET", "DERT",
                            "predicted.mean.spending", "predicted.CLV")
results.pnbd$contact_id <- as.integer(results.pnbd$contact_id)

df_scoring <- results.pnbd %>%
  left_join(df, by = c("contact_id")) %>%
  left_join(donors, by = c("contact_id"))  %>%
  mutate_at(.vars = vars(matches('emails_total|initiative|donations_total|^don_')), 
            .funs = ~ ifelse(is.na(.x), 0, .x)) %>%
  mutate(responsiveness = ifelse(is.na(responsiveness), 1, responsiveness)) %>%
  mutate(active = ifelse(is.na(active), 0, active))

# SET THIS VALUE
small_alpha <- 0.05     

df_scoring$score <- (df_scoring$responsiveness * (1 - df_scoring$initiative) + small_alpha) * df_scoring$predicted.CLV * df_scoring$active
#hist(df_scoring[df_scoring$score < 30]$score, breaks = 100)

#hist(log(df_scoring$score), breaks = 1000)

#plot(ecdf(df_scoring$score))

#ggplot(df_scoring, aes(log(score)) )+ stat_ecdf(geom = "point")

#ggplot(df_scoring[df_scoring$predicted.CLV > 2], aes(log(score)) )+ stat_ecdf(geom = "point")
#length(df_scoring[df_scoring$predicted.CLV > 2]$contact_id)



### 04 Allocation of Frequency Brackets ----------------

# Set Parameters
total_campaigns <- 9          # total number of campaigns to run - keep at 9
remaining_budget <- 500000    # remaining budget for the rest of the year
min_clv <- 2                  # minimum predicted CLV for any solicitation

campaigns_finished_index <- c()   # indices of finished campaigns - empty if none finished

min_year_comparison <- 2015    # minimum year to calculate the distribution of frequency brackets
max_year_comparison <- 2017    # maximum year to calculate the distribution of frequency brackets

# based on remaining budget, min_clv, and the already finished campaigns
# we can compute the optimal allocation

campaigns_finished <- n_distinct(campaigns_finished_index) 
months_finished <- data.frame(months_finished_df = 1:12, 
                              campaigns_finished_df = c(8, 9, 9, 1, 2, 3, 4, 4, 5, 5, 6, 7)) %>%
  filter(campaigns_finished_df %in% campaigns_finished_index) %>%
  pull(months_finished_df)

campaigns_remaining <- total_campaigns - campaigns_finished
remaining_solicitations <- floor((remaining_budget - 
                                (total_campaigns-campaigns_finished)*25000)/0.85)

# when a month is over, we specify it
# then we disregard campaign messages from this month for further analysis
# now we can repeat the analysis for the rest of the year

# compute frequency of solicitation brackets
allocation <- mails %>%
  filter(!(lubridate::month(action_date)) %in% months_finished) %>%    
  mutate(year = lubridate::year(action_date)) %>%
  filter(year >= min_year_comparison, year <= max_year_comparison) %>%
  group_by(contact_id, year) %>%
  summarise(solicitation_bracket = n(), .groups = 'keep') %>%
  ungroup() %>%
  mutate(solicitation_bracket = ifelse(solicitation_bracket > campaigns_remaining, 
                                       campaigns_remaining, 
                                       solicitation_bracket)) %>%
  group_by(solicitation_bracket) %>%
  summarise(freq_of_bracket = n(), .groups = 'keep') %>%
  ungroup() %>%
  mutate(total_solicitations_bracket = solicitation_bracket*freq_of_bracket, 
         allocation = round(total_solicitations_bracket/sum(total_solicitations_bracket), 3), 
         plan_solicitations = floor(allocation*remaining_solicitations), 
         num_donors = floor(plan_solicitations/solicitation_bracket), 
         max_rank_donor = rev(cumsum(rev(num_donors))))

plot(allocation$allocation, type = "l", 
     ylab = "rel frequency", xlab = "allocation_bracket")

# assign bracket to each donor
rank_vec <- c()
solicitation_vec <- for (i in NROW(allocation):1) {
  rank_vec <- c(rank_vec, 
                rep(allocation$solicitation_bracket[i], 
                    allocation$num_donors[i]))
}

# Join back to Scoring Dataframe
df_allocation <- df_scoring %>%
  arrange(desc(score)) %>%
  filter(predicted.CLV >= min_clv) 

# pad with zeroes so the vector is long enough
rank_vec <- c(rank_vec, rep(0, NROW(df_allocation)))

df_allocation <- df_allocation %>%
  mutate(number_solicitations = rank_vec[1:NROW(df_allocation)])
 
# plot the disttribution of the scores below 50
df_allocation %>% 
  filter(predicted.CLV >= min_clv, score <= 50) %>% pull(score) %>% hist()

# plot probabilities of being alive
df_allocation$PAlive %>% hist()


# minimum scores/CLVs solicited
min_clv <- df_allocation %>%
  filter(number_solicitations > 0) %>%
  filter(predicted.CLV == min(predicted.CLV)) %>% 
  slice(1) %>%
  pull(score) 

min_score <- df_allocation %>%
  filter(number_solicitations > 0) %>%
  filter(score == min(score)) %>% 
  slice(1) %>%
  pull(score) 


# Feedback
print(paste('min_CLV solicited: ', round(min_clv, 3)))
print(paste('min_score solicited: ', round(min_score, 3)))

budget_perc_spent <- round((sum(df_allocation$number_solicitations)*0.85 + 
                              25000*campaigns_remaining)/remaining_budget, 2)
print(paste0("Budget spent: ", budget_perc_spent*100, "%"))


# SOLUTION 
# #### ### 
# unwanted_months <- c(1,2,3)
# 
# check <- donations %>% 
#   mutate(month_num = lubridate::month(act_date), 
#          month_solicitation = case_when(month_num == 1 ~ 12, 
#                                         month_num == 5 ~ 4, 
#                                         T ~ month_num)) %>%
#   filter(!month_solicitation %in% unwanted_months) %>%
#   ungroup() %>%
#   group_by(contact_id, month_solicitation) %>%
#   summarise(amount = sum(amount, na.rm = T)) %>%
#   arrange(contact_id, desc(amount)) %>%
#   ungroup()
# 
# check2 <- check %>%
#   spread(key = month_solicitation, value = amount, fill = 0) %>%
#   gather(key = 'month_solicitation', value = 'amount', matches('^\\d+$')) %>%
#   arrange(contact_id, desc(amount)) %>%
#   mutate(num_solicitations = (contact_id %% 9) +1) %>%       # remove this: here you join the actual number from the other dataframe
#   group_by(contact_id) %>%
#   mutate(num_solicitations = max(num_solicitations)) %>%        
#   mutate(row_id = row_number()) %>%
#   filter(row_id <= num_solicitations)


  



### 05 Assign customer to campaigns ----------------

db_full = odbcConnect("mysql_odbc_connection", uid="root", pwd="")

query = "select contact_id, sum(amount), count(id), month(act_date) AS period,
         ROW_NUMBER() OVER(PARTITION BY contact_id ORDER BY sum(amount) desc, count(id) desc) as RN
         from ma_charity_full.acts
         where act_type_id='DO' group by contact_id,  month(act_date)
         order by contact_id, sum(amount) desc, count(id) desc  "


data = sqlQuery(db_full, query)
odbcClose(db_full)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Creation of the dataframe reporting the number of solicitation needed per customer
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

how_many = data.frame(contact_id = df_allocation$contact_id, nb_solit=df_allocation$number_solicitations)
how_many <- how_many%>%arrange(contact_id)
nb_contact = length(how_many$contact_id)


data <- data%>%filter(data$contact_id %in% how_many$contact_id)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

prefs = data.frame(contact_id = how_many$contact_id, p1=0, p2=0 ,p3=0, p4=0, p5=0, p6=0 ,p7=0, p8=0, p9=0, p10=0 ,p11=0, p12=0 )

j<-1
for (i in 1:nb_contact){
  
  while (j<= length(data$contact_id) & data$contact_id[j]==how_many$contact_id[i]){
    
    if (data$RN[j]==1){
      prefs$p1[i]=data$period[j]
    }else if (data$RN[j]==2){
      prefs$p2[i]=data$period[j]
    }else if (data$RN[j]==3){
      prefs$p3[i]=data$period[j]
    }else if (data$RN[j]==4){
      prefs$p4[i]=data$period[j]
    }else if (data$RN[j]==5){
      prefs$p5[i]=data$period[j]
    }else if (data$RN[j]==6){
      prefs$p6[i]=data$period[j]
    }else if (data$RN[j]==7){
      prefs$p7[i]=data$period[j]
    }else if (data$RN[j]==8){
      prefs$p8[i]=data$period[j]
    }else if (data$RN[j]==9){
      prefs$p9[i]=data$period[j]
    }else if (data$RN[j]==10){
      prefs$p10[i]=data$period[j]
    }else if (data$RN[j]==11){
      prefs$p11[i]=data$period[j]
    }else if (data$RN[j]==12){
      prefs$p12[i]=data$period[j]
    }
    
    j=j+1
  }
  
  
}

#print(head(prefs))

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

campaigns = data.frame(contact_id = how_many$contact_id, camp_mar=0, camp_apr=0 ,camp_may=0, camp_jun=0, camp_aug=0, camp_oct=0 ,camp_nov=0, camp_decA=0, camp_decB=0)

for (i in 1:nb_contact){
  
  k=how_many$nb_solit[i]
  
  l<-campaigns_finished_index
  for (p in 1:length(l)){
    l[p] = l[[p]]+1
  }
  l<-c(l, list(c(1)))
  
  if (k>0){
    j<-1
    while (j <= k){
      
      if ((prefs[i,j+1]==2) | (prefs[i,j+1]==3)){
        if (!(10 %in% l )){
          campaigns$camp_decB[i]=1
          l<-c(l, list(c(10)))
        }else {
          k=k+1
        }
        
      }else if (prefs[i,j+1]==1){
        if (!(9 %in% l )){
          campaigns$camp_decA[i]=1
          l<-c(l, list(c(9)))
        }else {
          k=k+1
        }
        
      }else if (prefs[i,j+1]==4){
        if (!(2 %in% l )){
          campaigns$camp_mar[i]=1
          l<-c(l, list(c(2)))
        }else {
          k=k+1
        }
        
      }else if (prefs[i,j+1]==5){
        if (!(3 %in% l )){
          campaigns$camp_apr[i]=1
          l<-c(l, list(c(3)))
        }else {
          k=k+1
        }
        
      }else if ((prefs[i,j+1]==7) | (prefs[i,j+1]==8)){
        if (!(5 %in% l )){
          campaigns$camp_jun[i]=1
          l<-c(l, list(c(5)))
        }else {
          k=k+1
        }
        
      }else if (prefs[i,j+1]==6){
        if (!(4 %in% l )){
          campaigns$camp_may[i]=1
          l<-c(l, list(c(4)))
        }else {
          k=k+1
        }
        
      }else if (prefs[i,j+1]==9){
        if (!(6 %in% l )){
          campaigns$camp_aug[i]=1
          l<-c(l, list(c(6)))
        }else {
          k=k+1
        }
        
      }else if (prefs[i,j+1]==10){
        if (!(6 %in% l )){
          campaigns$camp_aug[i]=1
          l<-c(l, list(c(6)))
        }else {
          k=k+1
        }
        
      }else if (prefs[i,j+1]==11){
        if (!(7 %in% l )){
          campaigns$camp_oct[i]=1
          l<-c(l, list(c(7)))
        }else {
          k=k+1
        }
        
      }else if (prefs[i,j+1]==12){
        if (!(8 %in% l )){
          campaigns$camp_nov[i]=1
          l<-c(l, list(c(8)))
        }else {
          k=k+1
        }
        
      }else if (prefs[i,j+1]==0){
        a<-sample.int(10, 1)
        while (a %in% l ){
          a<-sample.int(10, 1)
        }
        l<-c(l, list(c(a)))
        campaigns[i,a]=1
      }
      j=j+1
    }
  }
}

#print(head(campaigns))



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

C_march = sum(campaigns$camp_mar)
C_april = sum(campaigns$camp_apr)
C_may = sum(campaigns$camp_may)
C_june = sum(campaigns$camp_jun)
C_August = sum(campaigns$camp_aug)
C_october = sum(campaigns$camp_oct)
C_november = sum(campaigns$camp_nov)
C_decemberA = sum(campaigns$camp_decA)
C_decemberB = sum(campaigns$camp_decB)




result_howmany = data.frame( March = C_march, April = C_april, May=C_may, June = C_june, August = C_August, October= C_october, November = C_november, DecemberA= C_decemberA, DecemberB = C_decemberB)

customer_to_contact = data.frame(contact_id = campaigns$contact_id, March = NA, April = NA, May=NA, June = NA, August = NA, October= NA, November = NA, DecemberA= NA, DecemberB = NA)

for (i in 1:9){
  z = which(campaigns[,(i+1)] ==1)
  if (length(z)!=0){
    customer_to_contact[1:length(z),i+1]=campaigns[(z),1]
  }
}

customer_to_contact = customer_to_contact[, -1]
