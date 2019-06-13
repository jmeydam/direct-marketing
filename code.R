library(tidyverse)
library(caret)
library(gbm)
library(ROCR)

rm(list = ls())

# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
RED = "#F8766D"
GREEN = "#00BFC4"

###############################################################################
# Downloading data
###############################################################################

# Link zip archive data: 
# http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip

csv_file <- "bank-additional/bank-additional-full.csv"

# Uncomment to download data:

# dl <- tempfile()
# download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip", dl)
# unzip(dl, csv_file)


###############################################################################
# Verifying basic properties of data set, adding features with transformations
###############################################################################

# Info in comments from: http://archive.ics.uci.edu/ml/datasets/Bank+Marketing

# bank-additional-full.csv with all examples (41188) and 20 inputs, ordered by date 
# (from May 2008 to November 2010), very close to the data analyzed in [Moro et al., 2014].

d <- read.table(csv_file, header = TRUE, sep = ";", stringsAsFactors = TRUE)

dim(d)
nrow(d) == 41188
ncol(d) == 20 + 1 # 20 input variables + outcome

str(d)

# Missing attribute values: There are several missing values in some categorical attributes, all coded with the "unknown" label. These missing values can be treated as a possible class label or using deletion or imputation techniques. 

# Attributes from bank client data

#  1. age (numeric)
class(d$age)
sum(is.na(d$age)) == 0
min(d$age)
max(d$age)
boxplot(d$age)
hist(d$age)
ggplot(d, aes(age)) + geom_histogram()

# Add feature log_age (log transform of age) with more symmetric distribution
d$log_age <- log(d$age)
class(d$log_age)
sum(is.na(d$log_age)) == 0
min(d$log_age)
max(d$log_age)
boxplot(d$log_age)
hist(d$log_age)
ggplot(d, aes(log_age)) + geom_histogram()

#  2. job: type of job (categorical: "admin.", "blue-collar", "entrepreneur", "housemaid", "management", "retired", "self-employed", "services", "student", "technician", "unemployed", "unknown")
class(d$job)
sum(is.na(d$job)) == 0
sort(table(d$job), decreasing = TRUE)
job_counts <- d %>% 
  group_by(job) %>% summarize(count = n()) %>% arrange(desc(count))
job_counts
ggplot(job_counts, aes(reorder(job, -count), count)) + 
  geom_col() + 
  xlab("job") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  3. marital: marital status (categorical: "divorced", "married", "single", "unknown"; note: "divorced" means divorced or widowed)
class(d$marital)
sum(is.na(d$marital)) == 0
sort(table(d$marital), decreasing = TRUE)
marital_counts <- d %>% 
  group_by(marital) %>% summarize(count = n()) %>% arrange(desc(count))
marital_counts
ggplot(marital_counts, aes(reorder(marital, -count), count)) + 
  geom_col() + 
  xlab("marital") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  4. education (categorical: "basic.4y", "basic.6y", "basic.9y", "high.school", "illiterate", "professional.course", "university.degree", "unknown")
class(d$education)
sum(is.na(d$education)) == 0
sort(table(d$education), decreasing = TRUE)
education_counts <- d %>% 
  group_by(education) %>% summarize(count = n()) %>% arrange(desc(count))
education_counts
ggplot(education_counts, aes(reorder(education, -count), count)) + 
  geom_col() + 
  xlab("education") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  5. default: has credit in default? (categorical: "no", "yes", "unknown")
class(d$default)
sum(is.na(d$default)) == 0
sort(table(d$default), decreasing = TRUE)
default_counts <- d %>% 
  group_by(default) %>% summarize(count = n()) %>% arrange(desc(count))
default_counts
ggplot(default_counts, aes(reorder(default, -count), count)) + 
  geom_col() + 
  xlab("default") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  6. housing: has housing loan? (categorical: "no", "yes", "unknown")
class(d$housing)
sum(is.na(d$housing)) == 0
sort(table(d$housing), decreasing = TRUE)
housing_counts <- d %>% 
  group_by(housing) %>% summarize(count = n()) %>% arrange(desc(count))
housing_counts
ggplot(housing_counts, aes(reorder(housing, -count), count)) + 
  geom_col() + 
  xlab("housing") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  7. loan: has personal loan? (categorical: "no", "yes", "unknown")
class(d$loan)
sum(is.na(d$loan)) == 0
sort(table(d$loan), decreasing = TRUE)
loan_counts <- d %>% 
  group_by(loan) %>% summarize(count = n()) %>% arrange(desc(count))
loan_counts
ggplot(loan_counts, aes(reorder(loan, -count), count)) + 
  geom_col() + 
  xlab("loan") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Attributes related to the last contact of the current campaign

#  8. contact: contact communication type (categorical: "cellular", "telephone") 
class(d$contact)
sum(is.na(d$contact)) == 0
sort(table(d$contact), decreasing = TRUE)
contact_counts <- d %>% 
  group_by(contact) %>% summarize(count = n()) %>% arrange(desc(count))
contact_counts
ggplot(contact_counts, aes(reorder(contact, -count), count)) + 
  geom_col() + 
  xlab("contact") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  9. month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
class(d$month)
sum(is.na(d$month)) == 0
sort(table(d$month), decreasing = TRUE)
month_counts <- d %>% 
  group_by(month) %>% summarize(count = n()) %>% arrange(desc(count))
month_counts
ggplot(month_counts, aes(reorder(month, -count), count)) + 
  geom_col() + 
  xlab("month") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 10. day_of_week: last contact day of the week (categorical: "mon", "tue", "wed", "thu", "fri")
class(d$day_of_week)
sum(is.na(d$day_of_week)) == 0
sort(table(d$day_of_week), decreasing = TRUE)
day_of_week_counts <- d %>% 
  group_by(day_of_week) %>% summarize(count = n()) %>% arrange(desc(count))
day_of_week_counts
ggplot(day_of_week_counts, aes(reorder(day_of_week, -count), count)) + 
  geom_col() + 
  xlab("day_of_week") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 11. duration: last contact duration, in seconds (numeric). 
# Important note:  this attribute highly affects the output target (e.g., if duration=0 then y="no"). Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.
# (Will not use this feature, therefore no log transform)
class(d$duration)
sum(is.na(d$duration)) == 0
min(d$duration)
max(d$duration)
boxplot(d$duration)
hist(d$duration)
ggplot(d, aes(duration)) + geom_histogram()

# Other attributes

# 12. campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
class(d$campaign)
sum(is.na(d$campaign)) == 0
min(d$campaign)
max(d$campaign)
boxplot(d$campaign)
hist(d$campaign)
ggplot(d, aes(campaign)) + geom_histogram()

min(d$campaign) == 1
# Add feature log_campaign (log transform of campaign) to add more weight 
# to difference between small values
d$log_campaign = log(d$campaign)
class(d$log_campaign)
sum(is.na(d$log_campaign)) == 0
min(d$log_campaign)
max(d$log_campaign)
boxplot(d$log_campaign)
hist(d$log_campaign)
ggplot(d, aes(log_campaign)) + geom_histogram()

# 13. pdays: number of days that passed by after the client was last contacted from a previous campaign 
#            (numeric; 999 means client was not previously contacted)
class(d$pdays)
sum(is.na(d$pdays)) == 0
min(d$pdays)
max(d$pdays)
boxplot(d$pdays)
hist(d$pdays)
ggplot(d, aes(pdays)) + geom_histogram()

# Without 999 (not previously contacted)
min(d$pdays[d$pdays < 999])
max(d$pdays[d$pdays < 999])
boxplot(d$pdays[d$pdays < 999])
hist(d$pdays[d$pdays < 999])
ggplot(d[d$pdays < 999,], aes(pdays)) + geom_histogram()

# Add feature log_pdays (log transform of pdays + 1) to add more weight 
# to difference between small values and reduce distance to outlier
# Note:
# - Negligible number of observations with pdays = 0, and these are really 
#   rounded down to 0, since some time has passed (fraction of day)
# - 999 means client was not previously contacted
min(d$pdays + 1) == 1
d$log_pdays <- log(d$pdays + 1)
class(d$log_pdays)
sum(is.na(d$log_pdays)) == 0
min(d$log_pdays)
max(d$log_pdays)
boxplot(d$log_pdays)
hist(d$log_pdays)
ggplot(d, aes(log_pdays)) + geom_histogram()

# 14. previous: number of contacts performed before this campaign and for this client (numeric)
class(d$previous)
sum(is.na(d$previous)) == 0
min(d$previous)
max(d$previous)
boxplot(d$previous)
hist(d$previous)
ggplot(d, aes(previous)) + geom_histogram()

# 15. poutcome: outcome of the previous marketing campaign (categorical: "failure", "nonexistent", "success")
class(d$poutcome)
sum(is.na(d$poutcome)) == 0
sort(table(d$poutcome), decreasing = TRUE)
poutcome_counts <- d %>% 
  group_by(poutcome) %>% summarize(count = n()) %>% arrange(desc(count))
poutcome_counts
ggplot(poutcome_counts, aes(reorder(poutcome, -count), count)) + 
  geom_col() + 
  xlab("poutcome") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Social and economic context attributes

# 16. emp.var.rate: employment variation rate - quarterly indicator (numeric)
class(d$emp.var.rate)
sum(is.na(d$emp.var.rate)) == 0
min(d$emp.var.rate)
max(d$emp.var.rate)
boxplot(d$emp.var.rate)
hist(d$emp.var.rate)
ggplot(d, aes(emp.var.rate)) + geom_histogram()

# 17. cons.price.idx: consumer price index - monthly indicator (numeric)     
class(d$cons.price.idx)
sum(is.na(d$cons.price.idx)) == 0
min(d$cons.price.idx)
max(d$cons.price.idx)
boxplot(d$cons.price.idx)
hist(d$cons.price.idx)
ggplot(d, aes(cons.price.idx)) + geom_histogram()

# 18. cons.conf.idx: consumer confidence index - monthly indicator (numeric)     
class(d$cons.conf.idx)
sum(is.na(d$cons.conf.idx)) == 0
min(d$cons.conf.idx)
max(d$cons.conf.idx)
boxplot(d$cons.conf.idx)
hist(d$cons.conf.idx)
ggplot(d, aes(cons.conf.idx)) + geom_histogram()

# 19. euribor3m: euribor 3 month rate - daily indicator (numeric)
class(d$euribor3m)
sum(is.na(d$euribor3m)) == 0
min(d$euribor3m)
max(d$euribor3m)
boxplot(d$euribor3m)
hist(d$euribor3m)
ggplot(d, aes(euribor3m)) + geom_histogram()

# 20. nr.employed: number of employees - quarterly indicator (numeric)
class(d$nr.employed)
sum(is.na(d$nr.employed)) == 0
min(d$nr.employed)
max(d$nr.employed)
boxplot(d$nr.employed)
hist(d$nr.employed)
ggplot(d, aes(nr.employed)) + geom_histogram()

# Output variable (target)

# 21. y: has the client subscribed a term deposit? (binary: "yes", "no")
class(d$y)
sum(is.na(d$y)) == 0
sort(table(d$y), decreasing = TRUE)
y_counts <- d %>% 
  group_by(y) %>% summarize(count = n()) %>% arrange(desc(count))
y_counts
ggplot(y_counts, aes(reorder(y, -count), count)) + 
  geom_col() + 
  xlab("y") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add y_bernoulli for GBM, otherwise error message:
# Bernoulli requires the response to be in {0,1}
table(d$y)
table(unclass(d$y))
d$y_bernoulli <- unclass(d$y) - 1
table(d$y_bernoulli)

# Conclusion: data ready for use in models
# - No obviously wrong values, e.g., distribution of age as expected
# - No NAs; missing values are encoded ("unknown")
# - The proportion of missing values is small
# - Fact that value is missing may carry information - for now no imputation

# Three log transformations added:
# - log_age
# - log_campaign
# - log_pdays

# Also added for GBM:
# - y_bernoulli (0 = "no", 1 = "yes")


###############################################################################
# Exploratory data analysis
###############################################################################

str(d)

# Social and Economic Context Attributes

# 16. emp.var.rate: employment variation rate - quarterly indicator (numeric)
# 17. cons.price.idx: consumer price index - monthly indicator (numeric)     
# 18. cons.conf.idx: consumer confidence index - monthly indicator (numeric)     
# 19. euribor3m: euribor 3 month rate - daily indicator (numeric)
# 20. nr.employed: number of employees - quarterly indicator (numeric)

# The date is not given, but, according to data set documentation,
# the inputs are ordered by date, from May 2008 to November 2010.
# Therefore, plotting the data in sequence will give an approximation 
# to a proper time series.
#
# Note:
#
# - The observations are not evenly distributed in time. 
# - The observations cover the period from just before to two years 
#   after the onset of the global financial crisis. 
#   Lehman Brothers collapsed on September 15, 2008.
#   (https://en.wikipedia.org/wiki/Financial_crisis_of_2007%E2%80%932008)

# nr.employed: solid downward trend after onset of crisis
plot(d$nr.employed)
d %>% mutate(observation = 1:n()) %>%
  ggplot(aes(observation, nr.employed)) + geom_line()

# emp.var.rate: similar to employed, but uptick towards the end 
# (though still negative)
plot(d$emp.var.rate)
d %>% mutate(observation = 1:n()) %>%
  ggplot(aes(observation, emp.var.rate)) + geom_line()

# d$emp.var.rate and d$nr.employed are strongly correlated
plot(d$emp.var.rate, d$nr.employed)
cor(d$emp.var.rate, d$nr.employed)

# euribor3m: approximately constant before and after onset of crisis, 
# with jump discontinuity after onset
plot(d$euribor3m)

# cons.price.idx: slow downward trend for most of the time, with 
# strong upwards movement at the end
plot(d$cons.price.idx)

# cons.conf.idx: at first glance similar to cons.price.idx, but 
# more variability
plot(d$cons.conf.idx)

# Although there appears to be a (nonlinear) pattern, 
# cons.conf.idx and cons.price.idx are barely correlated
plot(d$cons.price.idx, d$cons.conf.idx)
cor(d$cons.price.idx, d$cons.conf.idx)

# Both nr.employed and emp.var.rate are weakly correlated with cons.conf.idx
plot(d$nr.employed, d$cons.conf.idx)
cor(d$nr.employed, d$cons.conf.idx)
plot(d$emp.var.rate, d$cons.conf.idx)
cor(d$emp.var.rate, d$cons.conf.idx)

# Both nr.employed and emp.var.rate are relatively strongly correlated 
# with cons.price.idx
plot(d$nr.employed, d$cons.price.idx)
cor(d$nr.employed, d$cons.price.idx)
plot(d$emp.var.rate, d$cons.price.idx)
cor(d$emp.var.rate, d$cons.price.idx)

# At one glance
pairs(~ nr.employed + emp.var.rate + euribor3m + cons.price.idx + cons.conf.idx, 
      data = d, 
      main = "Scatterplot Matrix Economic Indicators")

# Median and mean of nr.employed are lower for y = "yes",
# so it seems that after the onset of the financial crisis 
# customers were more likely to accept the offer
ggplot(d, aes(y, nr.employed)) + geom_boxplot() 
d %>% group_by(y) %>% summarize(median(nr.employed))
d %>% group_by(y) %>% summarize(mean(nr.employed))

# Given correlations, similar result for emp.var.rate;
# in fact, 75% of the "yes" responses coincide with negative
# employment variation rate
ggplot(d, aes(y, emp.var.rate)) + geom_boxplot() 
d %>% group_by(y) %>% summarize(median(emp.var.rate))
d %>% group_by(y) %>% summarize(mean(emp.var.rate))

# Given correlations, similar result for euribor3m
ggplot(d, aes(y, euribor3m)) + geom_boxplot() 
d %>% group_by(y) %>% summarize(median(euribor3m))
d %>% group_by(y) %>% summarize(mean(euribor3m))

# Given correlations, similar result for cons.price.idx
ggplot(d, aes(y, cons.price.idx)) + geom_boxplot() 
d %>% group_by(y) %>% summarize(median(cons.price.idx))
# However, difference between means is negligible
d %>% group_by(y) %>% summarize(mean(cons.price.idx))

# However, median and mean of cons.conf.idx slightly higher for y = "yes",
# so after onset of financial crisis customers seemed to be more likely 
# to accept the offer in particular when consumer confidence was improving
ggplot(d, aes(y, cons.conf.idx)) + geom_boxplot() 
d %>% group_by(y) %>% summarize(median(cons.conf.idx))
d %>% group_by(y) %>% summarize(mean(cons.conf.idx))
plot(d$cons.conf.idx)

# Assessing combinations of cons.conf.idx with other indicators:
# How well and in which way do these combinations separate y = yes vs. no?
# Note that some indicators change quarterly, some monthly, so there are 
# clusters. These clusters can be visualized by jittering the points.

# cons.conf.idx with nr.employed
ggplot(d, aes(x = nr.employed, y = cons.conf.idx, color = y)) + 
  geom_point(position = position_jitter(width = 20, height = 2),
             alpha = 0.6) 

# Increasing jitter and transparency to reduce effect of overplotting
ggplot(d, aes(x = nr.employed, y = cons.conf.idx, color = y)) + 
  geom_point(position = position_jitter(width = 80, height = 8),
             alpha = 0.3) 

# Increasing jitter and transparency even further - now position is 
# very inaccurate
ggplot(d, aes(x = nr.employed, y = cons.conf.idx, color = y)) + 
  geom_point(position = position_jitter(width = 120, height = 12),
             alpha = 0.15) 

# cons.conf.idx with emp.var.rate
ggplot(d, aes(x = emp.var.rate, y = cons.conf.idx, color = y)) + 
  geom_point(position = position_jitter(width = 0.5, height = 2),
             alpha = 0.6) 

# Increasing jitter and transparency - position is now very inaccurate
ggplot(d, aes(x = emp.var.rate, y = cons.conf.idx, color = y)) + 
  geom_point(position = position_jitter(width = 1, height = 12),
             alpha = 0.15) 

# cons.conf.idx with cons.price.idx
ggplot(d, aes(x = cons.price.idx, y = cons.conf.idx, color = y)) + 
  geom_point(position = position_jitter(width = 0.25, height = 2),
             alpha = 0.6) 

# cons.conf.idx with euribor3m
ggplot(d, aes(x = euribor3m, y = cons.conf.idx, color = y)) + 
  geom_point(position = position_jitter(width = 0.5, height = 2),
             alpha = 0.6) 

# Looking at change over time may help to understand patterns.
#
# Remember that observations are ordered, but not evenly distributed
# in time. Higher variability after observation 36,000 is partly due 
# to fewer observations per time unit (fewer/lower intensity campaigns)

plot(d$nr.employed)
plot(d$cons.conf.idx)

# 20. nr.employed: number of employees - quarterly indicator (numeric)
d %>% mutate(observation = 1:n()) %>% 
  ggplot(aes(x = observation, y = nr.employed, color = y)) + 
  geom_point(position = position_jitter(width = 0, height = 20),
             alpha = 0.4)

# Increasing jitter and transparency to reduce effect of overplotting
d %>% mutate(observation = 1:n()) %>% 
  ggplot(aes(x = observation, y = nr.employed, color = y)) + 
  geom_point(position = position_jitter(width = 0, height = 40),
             alpha = 0.2)

# Increasing jitter and transparency even further - now position is 
# very inaccurate
d %>% mutate(observation = 1:n()) %>% 
  ggplot(aes(x = observation, y = nr.employed, color = y)) + 
  geom_point(position = position_jitter(width = 0, height = 100),
             alpha = 0.15)

# reducing jitter and transparency again, but now only showing
# observations with y = "yes"
d %>% mutate(observation = 1:n()) %>% 
  filter(y == "yes") %>%
  ggplot(aes(x = observation, y = nr.employed)) + 
  geom_point(position = position_jitter(width = 0, height = 40),
             alpha = 0.2,
             color = GREEN)

# Now only showing observations with y = "no"
d %>% mutate(observation = 1:n()) %>% 
  filter(y == "no") %>%
  ggplot(aes(x = observation, y = nr.employed)) + 
  geom_point(position = position_jitter(width = 0, height = 40),
             alpha = 0.2,
             color = RED)

# 16. emp.var.rate: employment variation rate - quarterly indicator (numeric)
d %>% mutate(observation = 1:n()) %>% 
  ggplot(aes(x = observation, y = emp.var.rate, color = y)) + 
  geom_point(position = position_jitter(width = 0, height = 0.5),
             alpha = 0.4)

# Increasing jitter and transparency to reduce effect of overplotting
d %>% mutate(observation = 1:n()) %>% 
  ggplot(aes(x = observation, y = emp.var.rate, color = y)) + 
  geom_point(position = position_jitter(width = 0, height = 1),
             alpha = 0.2)

# Increasing jitter and transparency even further - now position is 
# very inaccurate
d %>% mutate(observation = 1:n()) %>% 
  ggplot(aes(x = observation, y = emp.var.rate, color = y)) + 
  geom_point(position = position_jitter(width = 0, height = 1.5),
             alpha = 0.15)

# Reducing jitter and transparency again, but now only showing
# observations with y = "yes"
d %>% mutate(observation = 1:n()) %>%
  filter(y == "yes") %>%
  ggplot(aes(x = observation, y = emp.var.rate)) + 
  geom_point(position = position_jitter(width = 0, height = 1),
             alpha = 0.2,
             color = GREEN)

# Now only showing observations with y = "no"
d %>% mutate(observation = 1:n()) %>%
  filter(y == "no") %>%
  ggplot(aes(x = observation, y = emp.var.rate)) + 
  geom_point(position = position_jitter(width = 0, height = 1),
             alpha = 0.2,
             color = RED)

# 17. cons.price.idx: consumer price index - monthly indicator (numeric)     
d %>% mutate(observation = 1:n()) %>% 
  ggplot(aes(x = observation, y = cons.price.idx, color = y)) + 
  geom_point(position = position_jitter(width = 0, height = 0.25),
             alpha = 0.4)

# 19. euribor3m: euribor 3 month rate - daily indicator (numeric)
d %>% mutate(observation = 1:n()) %>% 
  ggplot(aes(x = observation, y = euribor3m, color = y)) + 
  geom_point(position = position_jitter(width = 0, height = 0.5),
             alpha = 0.4)

# 18. cons.conf.idx: consumer confidence index - monthly indicator (numeric)     
d %>% mutate(observation = 1:n()) %>% 
  ggplot(aes(x = observation, y = cons.conf.idx, color = y)) + 
  geom_point(position = position_jitter(width = 0, height = 5),
             alpha = 0.4)

# These two plots seem to be most revealing:

# 20. nr.employed: number of employees - quarterly indicator (numeric)
# Reducing jitter and transparency again, but now only showing
# observations with y = "yes"
d %>% mutate(observation = 1:n()) %>% 
  filter(y == "yes") %>%
  ggplot(aes(x = observation, y = nr.employed)) + 
  geom_point(position = position_jitter(width = 0, height = 40),
             alpha = 0.2,
             color = GREEN)

# 16. emp.var.rate: employment variation rate - quarterly indicator (numeric)
# Reducing jitter and transparency again, but now only showing
# observations with y = "yes"
d %>% mutate(observation = 1:n()) %>%
  filter(y == "yes") %>%
  ggplot(aes(x = observation, y = emp.var.rate)) + 
  geom_point(position = position_jitter(width = 0, height = 1),
             alpha = 0.2,
             color = GREEN)

# Two additional plots:

# Plot density curves of "yes" and "no" responses
d %>% mutate(observation = 1:n()) %>%
  ggplot(aes(x = observation, color = y)) + 
  geom_line(stat = "density")

# Bin observations in groups of 2,500, then plot "yes", "no"
d %>% mutate(observation = 1:n()) %>%
  mutate(group = (observation %/% 2500) * 2500) %>%
  group_by(group) %>%
  ggplot(aes(x = group, fill = y)) + 
  geom_bar(position = position_stack())

# In the first visual displays of the data, until about observation 36,000, 
# the "no" responses drowned out the "yes" responses  - even using high 
# levels of jitter and transparency left some uncertainty concerning
# the changing frequency of "yes" responses. After observation 36,000 
# the "no" responses drop dramatically, making the "yes" responses clearly
# visible even in the first plots.
#
# Further analysis confirmed that "yes" responses, while present from the 
# beginning, became much more frequent starting around observation 27'500,
# and ended up being more frequent than "no" responses.
# 
# If we measure the efficiency of the campaigns in terms of the ratio of 
# "yes" to "no" responses, campaigns became daramatically more efficient
# - and in that sense more successful - starting around observation 27'500.
# 
# The most efficient campaigns towards the end can also be identified via
# nr.employed, since the number of employees is decreasing for the second 
# half of the observations and is significantly lower towards the end.
# The employment variation rate (emp.var.rate) can also be used to detect 
# observations from 27'500 onwards.
# 
# There might be a causal connection between the efficiency / success 
# of campaigns and a combination of economic indicators (in particular, 
# the number of employees and the employment variation rate), but the 
# improvement of the success rate might also be due to a learning effect: 
# the campaigns towards the end may have learned lessons from previous 
# campaigns and this might be an important reason why later campaigns 
# were more efficient/successful.
#
# It is worth noting that after an initial dramatic improvement around
# observation 27'500 performance first went back to the previous level,
# then returned to the improved level, and then improved even further.
# It is conceivable that this discontinuity and further accelerated 
# improvement is a result of different campaigns using different 
# approaches, with systematic optimization of the approach after a
# brief delay after a first breakthrough around observation 27'500. 
# This new, optimized approach might have exploited changing attitudes 
# in a country in crisis. In particular, it is conceivable that certain 
# identifiable subgroups among the customers may have become more susceptible 
# to sales pitches emphasizing security and wealth protection.


# Attributes from Bank Client Data

#  1. age (numeric)
#     log_age
#  2. job: type of job (categorical: "admin.", "blue-collar", "entrepreneur", "housemaid", "management", "retired", "self-employed", "services", "student", "technician", "unemployed", "unknown")
#  3. marital: marital status (categorical: "divorced", "married", "single", "unknown"; note: "divorced" means divorced or widowed)
#  4. education (categorical: "basic.4y", "basic.6y", "basic.9y", "high.school", "illiterate", "professional.course", "university.degree", "unknown")
#  5. default: has credit in default? (categorical: "no", "yes", "unknown")
#  6. housing: has housing loan? (categorical: "no", "yes", "unknown")
#  7. loan: has personal loan? (categorical: "no", "yes", "unknown")

#  1. age (numeric)

# togehter with job and education

ggplot(d, aes(reorder(job, age, median), age)) + 
  geom_boxplot() + 
  xlab("job") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(d, aes(reorder(job, log_age, median), log_age)) + 
  geom_boxplot() + 
  xlab("job") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(d, aes(age)) + geom_histogram() +
  ylab("") + facet_wrap(~job, ncol = 1)

ggplot(d, aes(age)) + geom_histogram() +
  ylab("") + facet_wrap(~education, ncol = 1)

# "before" vs. "after", with response y

ggplot(d[1:27500,], aes(x = age, fill = y)) +
  geom_histogram(bins = 20)

ggplot(d[27501:nrow(d),], aes(x = age, fill = y)) +
  geom_histogram(bins = 20)

d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  ggplot(aes(x = age, fill = y)) + 
  geom_histogram() + 
  xlab("age") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  ggplot(aes(x = age, fill = y)) + 
  geom_histogram(bins = 20) + 
  xlab("age") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

# log_age
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  ggplot(aes(x = log_age, fill = y)) + 
  geom_histogram(bins = 20) + 
  xlab("log_age") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

#  2. job: type of job (categorical: "admin.", "blue-collar", "entrepreneur", "housemaid", "management", "retired", "self-employed", "services", "student", "technician", "unemployed", "unknown")
d %>%
  group_by(job) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(job, -count), fill = y)) + 
  geom_bar() + 
  xlab("job") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

d[1:27500,] %>%
  group_by(job) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(job, -count), fill = y)) + 
  geom_bar() + 
  xlab("job") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

d[27501:nrow(d),] %>%
  group_by(job) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(job, -count), fill = y)) + 
  geom_bar() + 
  xlab("job") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  group_by(job) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(job, -count), fill = y)) + 
  geom_bar() + 
  xlab("job") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

#  4. education (categorical: "basic.4y", "basic.6y", "basic.9y", "high.school", "illiterate", "professional.course", "university.degree", "unknown")
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  group_by(education) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(education, -count), fill = y)) + 
  geom_bar() + 
  xlab("education") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

#  3. marital: marital status (categorical: "divorced", "married", "single", "unknown"; note: "divorced" means divorced or widowed)
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  group_by(marital) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(marital, -count), fill = y)) + 
  geom_bar() + 
  xlab("marital") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

#  5. default: has credit in default? (categorical: "no", "yes", "unknown")
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  group_by(default) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(default, -count), fill = y)) + 
  geom_bar() + 
  xlab("default") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

#  6. housing: has housing loan? (categorical: "no", "yes", "unknown")
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  group_by(housing) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(housing, -count), fill = y)) + 
  geom_bar() + 
  xlab("housing") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

#  7. loan: has personal loan? (categorical: "no", "yes", "unknown")
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  group_by(loan) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(loan, -count), fill = y)) + 
  geom_bar() + 
  xlab("loan") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)


# Attributes Related to the Last Contact of the Current Campaign

#  8. contact: contact communication type (categorical: "cellular", "telephone") 
#  9. month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
# 10. day_of_week: last contact day of the week (categorical: "mon", "tue", "wed", "thu", "fri")
# 11. duration: last contact duration, in seconds (numeric). Important note:  this attribute highly affects the output target (e.g., if duration=0 then y="no"). Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.

#  8. contact: contact communication type (categorical: "cellular", "telephone") 
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  group_by(contact) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(contact, -count), fill = y)) + 
  geom_bar() + 
  xlab("contact") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

#  9. month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  group_by(month) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(month, -count), fill = y)) + 
  geom_bar() + 
  xlab("month") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

# 10. day_of_week: last contact day of the week (categorical: "mon", "tue", "wed", "thu", "fri")
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  group_by(day_of_week) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(day_of_week, -count), fill = y)) + 
  geom_bar() + 
  xlab("day_of_week") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)


# Other Attributes

# 12. campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
#     log_campaign
# 13. pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)
#     log_pdays
# 14. previous: number of contacts performed before this campaign and for this client (numeric)
# 15. poutcome: outcome of the previous marketing campaign (categorical: "failure", "nonexistent", "success")

# 12. campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  ggplot(aes(x = campaign, fill = y)) + 
  geom_histogram() + 
  xlab("campaign") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

# log_campaign
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  ggplot(aes(x = log_campaign, fill = y)) + 
  geom_histogram() + 
  xlab("log_campaign") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

# 13. pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  ggplot(aes(x = pdays, fill = y)) + 
  geom_histogram() + 
  xlab("pdays") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

# log_pdays
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  ggplot(aes(x = log_pdays, fill = y)) + 
  geom_histogram() + 
  xlab("log_pdays") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

# 14. previous: number of contacts performed before this campaign and for this client (numeric)
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  ggplot(aes(x = previous, fill = y)) + 
  geom_histogram() + 
  xlab("previous") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

# 15. poutcome: outcome of the previous marketing campaign (categorical: "failure", "nonexistent", "success")
d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  group_by(poutcome) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(poutcome, -count), fill = y)) + 
  geom_bar() + 
  xlab("poutcome") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)


###############################################################################
# Given the obvious changes both in the economy and in the targeting of the 
# campaigns after the onset of the financial crisis, a model based on the whole
# data set is bound to give misleading results. The data set is therefore split
# into two data sets, "before" and "after" the onset of the financial crisis, 
# using observation 27,500 as the cut-off. Each of these new data sets is then
# split into a training and a test set.
###############################################################################

# index of observations in orginal data set before and after observation 27,500
index_before <- 1:27500
index_after <- 27501:nrow(d)
length(index_before)
length(index_after)
length(index_before) + length(index_after) == nrow(d)

# Splitting orginal data set into "before" and "after" set
train_test_before <- d[index_before,]
train_test_after <- d[index_after,]

# new training and test set "before"

# test set to be used for model selection, 10% of remaining data
test_index_before <- createDataPartition(y = train_test_before$y, 
                                         times = 1, p = 0.1, 
                                         list = FALSE)
train_before <- train_test_before[-test_index_before,]
test_before <- train_test_before[test_index_before,]

nrow(test_before) / (nrow(test_before) + nrow(train_before))

# new training and test set "after"

# test set to be used for model selection, 10% of remaining data
test_index_after <- createDataPartition(y = train_test_after$y, 
                                        times = 1, p = 0.1, 
                                        list = FALSE)
train_after <- train_test_after[-test_index_after,]
test_after <- train_test_after[test_index_after,]

nrow(test_after) / (nrow(test_after) + nrow(train_after))

nrow(d) == nrow(train_before) + nrow(test_before) + 
           nrow(train_after) + nrow(test_after)


###############################################################################
# Since there are many categorical variables and at least some effects of
# continuous variables (age!) may be nonlinear, a tree-based model is a natural
# choice. We will fit a simple boosted model using the gbm package:
#
# browseVignettes(package = "gbm")
# 
# The emphasis here is on understanding what is roughly possible with the
# data before and after the onset of the financial crisis. Tuning the models
# by optimizing paramters would be a natural next step.
###############################################################################

set.seed(5)

###############################################################################
# "Before"
###############################################################################

colnames(train_before)

# Leaving out all economic variables and month to reduce effect of 
# beginning financial crisis on model. Also leaving out log transformations 
# (not used by gbm model) and duration, as well as the orginal y.
boost_before_3 <- gbm(y_bernoulli ~ ., 
                      data = subset(train_before, 
                                    select = -c(duration, month, euribor3m, emp.var.rate, 
                                                cons.price.idx, cons.conf.idx, nr.employed,
                                                log_age, log_campaign, log_pdays, y)),
                      distribution = "bernoulli", 
                      n.trees = 5000, 
                      interaction.depth = 4)

summary(boost_before_3)
#                     var      rel.inf
# job                 job 27.152056729
# age                 age 19.571323650
# education     education 16.539990870
# day_of_week day_of_week 13.536900141
# campaign       campaign  7.795835207
# marital         marital  4.682135452
# contact         contact  3.290501427
# housing         housing  2.764413041
# loan               loan  2.445246409
# default         default  1.525721577
# poutcome       poutcome  0.506492675
# previous       previous  0.181584149
# pdays             pdays  0.007798672

# job 27.152056729
plot(boost_before_3, i = "job")

d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  group_by(job) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(job, -count), fill = y)) + 
  geom_bar() + 
  xlab("job") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

# age 19.571323650
plot(boost_before_3, i = "age")

d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  ggplot(aes(x = age, fill = y)) + 
  geom_histogram() + 
  xlab("age") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

# education 16.539990870
plot(boost_before_3, i = "education")

d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  group_by(education) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(education, -count), fill = y)) + 
  geom_bar() + 
  xlab("education") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

plot(boost_before_3, i = "day_of_week")
plot(boost_before_3, i = "campaign")
plot(boost_before_3, i = "marital")
plot(boost_before_3, i = "housing")
plot(boost_before_3, i = "contact")
plot(boost_before_3, i = "loan")

# test_before

# ?predict.gbm
yhat_boost_before <- predict(boost_before_3,
                             type = "response",
                             newdata = test_before,
                             n.trees = 5000)

summary(test_before$y_bernoulli == 1)
summary(yhat_boost_before)
# using cut-off for probability to predict a reasonable number of true "yes"
summary(yhat_boost_before > 0.05)
actual_before <- ifelse(test_before$y_bernoulli == 1, "yes", "no")
predicted_before <- ifelse(yhat_boost_before > 0.05, "yes", "no")
mean(actual_before == predicted_before)
table(actual_before, predicted_before)

# ?plot.performance
plot(performance(prediction(predictions = yhat_boost_before, 
                            labels = ifelse(test_before$y_bernoulli == 1, "yes", "no")), 
                 "tpr", 
                 "fpr"), 
     print.cutoffs.at = c(0.05, 0.10, 0.15),
     main = "GBM, Before")

# This model is basically useless.

###############################################################################
# "After"
###############################################################################

colnames(train_after)

# Leaving out all economic variables and month to reduce effect of 
# beginning financial crisis on model. Also leaving out log transformations 
# (not used by gbm model) and duration, as well as the orginal y.
boost_after_3 <- gbm(y_bernoulli ~ ., 
                      data = subset(train_after, 
                                    select = -c(duration, month, euribor3m, emp.var.rate, 
                                                cons.price.idx, cons.conf.idx, nr.employed,
                                                log_age, log_campaign, log_pdays, y)),
                      distribution = "bernoulli", 
                      n.trees = 5000, 
                      interaction.depth = 4)

summary(boost_after_3)
#                     var      rel.inf
# job                 job 24.7270879
# age                 age 21.2481151
# education     education 13.6982947
# day_of_week day_of_week 11.4210767
# pdays             pdays  8.8720524
# campaign       campaign  4.7749730
# poutcome       poutcome  3.5969610
# marital         marital  3.3450231
# housing         housing  2.4191063
# previous       previous  2.3089277
# loan               loan  1.8543587
# contact         contact  0.9871851
# default         default  0.7468385

# job 24.7270879
plot(boost_after_3, i = "job")

d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  group_by(job) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(job, -count), fill = y)) + 
  geom_bar() + 
  xlab("job") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

# age 21.2481151
plot(boost_after_3, i = "age")

d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  ggplot(aes(x = age, fill = y)) + 
  geom_histogram() + 
  xlab("age") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

# education 13.6982947
plot(boost_after_3, i = "education")

d %>%
  mutate(time = ifelse(row_number() <= 27500, "before", "after")) %>%
  mutate(time = factor(time, levels = c("before", "after"))) %>%
  group_by(education) %>% 
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(education, -count), fill = y)) + 
  geom_bar() + 
  xlab("education") + 
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~time, ncol = 1)

plot(boost_after_3, i = "day_of_week")
plot(boost_after_3, i = "campaign")
plot(boost_after_3, i = "marital")
plot(boost_after_3, i = "housing")
plot(boost_after_3, i = "contact")
plot(boost_after_3, i = "loan")

# test_after

yhat_boost_after <- predict(boost_after_3,
                             type = "response",
                             newdata = test_after,
                             n.trees = 5000)

summary(test_after$y_bernoulli == 1)
summary(yhat_boost_after)
# using cut-off for probability to predict a reasonable number of true "yes"
summary(yhat_boost_after > 0.20)
actual_after <- ifelse(test_after$y_bernoulli == 1, "yes", "no")
predicted_after <- ifelse(yhat_boost_after > 0.20, "yes", "no")
mean(actual_after == predicted_after)
table(actual_after, predicted_after)

# ?plot.performance
plot(performance(prediction(predictions = yhat_boost_after, 
                            labels = ifelse(test_after$y_bernoulli == 1, "yes", "no")), 
                 "tpr", 
                 "fpr"), 
     print.cutoffs.at = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35),
     main = "GBM, After")

# This looks much better. It looks like improved performance of the campaigns
# was at least enabled by the financial crisis. The data after the onset of the
# crisis were much more useful, and are likely to have informed the campaigns.
