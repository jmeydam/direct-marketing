library(tidyverse)
library(caret)

###############################################################################
# Downloading data
###############################################################################

rm(list = ls())

# Link zip archive data: 
# http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip

dl <- tempfile()
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip", dl)
csv_file <- "bank-additional/bank-additional-full.csv"
unzip(dl, csv_file)


###############################################################################
# Exploratory data analysis, verifying basic properties of data set 
###############################################################################

# Info in comments from: http://archive.ics.uci.edu/ml/datasets/Bank+Marketing

# bank-additional-full.csv with all examples (41188) and 20 inputs, ordered by date 
# (from May 2008 to November 2010), very close to the data analyzed in [Moro et al., 2014].

d <- read.table(csv_file, header = TRUE, sep = ";", stringsAsFactors = FALSE)

dim(d)
nrow(d) == 41188
ncol(d) == 20 + 1 # 20 input variables + outcome

str(d)

# Missing Attribute Values: There are several missing values in some categorical attributes, all coded with the "unknown" label. These missing values can be treated as a possible class label or using deletion or imputation techniques. 

# Attributes from Bank Client Data

#  1. age (numeric)
class(d$age)
sum(is.na(d$age))
min(d$age)
max(d$age)
boxplot(d$age)
hist(d$age)
ggplot(d, aes(age)) + geom_histogram()

#  2. job: type of job (categorical: "admin.", "blue-collar", "entrepreneur", "housemaid", "management", "retired", "self-employed", "services", "student", "technician", "unemployed", "unknown")
class(d$job)
sum(is.na(d$job))
sort(table(d$job), decreasing = TRUE)
job_counts <- d %>% group_by(job) %>% summarize(count = n()) %>% arrange(desc(count))
job_counts
# jobs_in_order <- factor(job_counts$job, levels = job_counts$job)
# jobs_in_order
# job_counts$job <- jobs_in_order
ggplot(job_counts, aes(reorder(job, -count), count)) + 
  geom_col() + 
  xlab("job") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(job_counts, aes(count, reorder(job, count))) +
  geom_point(size = 3) +
  ylab("job") +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")
  )

#  3. marital: marital status (categorical: "divorced", "married", "single", "unknown"; note: "divorced" means divorced or widowed)
class(d$marital)
sum(is.na(d$marital))
sort(table(d$marital), decreasing = TRUE)
marital_counts <- d %>% group_by(marital) %>% summarize(count = n()) %>% arrange(desc(count))
marital_counts
ggplot(marital_counts, aes(reorder(marital, -count), count)) + 
  geom_col() + 
  xlab("marital") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  4. education (categorical: "basic.4y", "basic.6y", "basic.9y", "high.school", "illiterate", "professional.course", "university.degree", "unknown")
class(d$education)
sum(is.na(d$education))
sort(table(d$education), decreasing = TRUE)
education_counts <- d %>% group_by(education) %>% summarize(count = n()) %>% arrange(desc(count))
education_counts
ggplot(education_counts, aes(reorder(education, -count), count)) + 
  geom_col() + 
  xlab("education") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  5. default: has credit in default? (categorical: "no", "yes", "unknown")
class(d$default)
sum(is.na(d$default))
sort(table(d$default), decreasing = TRUE)
default_counts <- d %>% group_by(default) %>% summarize(count = n()) %>% arrange(desc(count))
default_counts
ggplot(default_counts, aes(reorder(default, -count), count)) + 
  geom_col() + 
  xlab("default") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  6. housing: has housing loan? (categorical: "no", "yes", "unknown")
class(d$housing)
sum(is.na(d$housing))
sort(table(d$housing), decreasing = TRUE)
housing_counts <- d %>% group_by(housing) %>% summarize(count = n()) %>% arrange(desc(count))
housing_counts
ggplot(housing_counts, aes(reorder(housing, -count), count)) + 
  geom_col() + 
  xlab("housing") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  7. loan: has personal loan? (categorical: "no", "yes", "unknown")
class(d$loan)
sum(is.na(d$loan))
sort(table(d$loan), decreasing = TRUE)
loan_counts <- d %>% group_by(loan) %>% summarize(count = n()) %>% arrange(desc(count))
loan_counts
ggplot(loan_counts, aes(reorder(loan, -count), count)) + 
  geom_col() + 
  xlab("loan") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Attributes Related to the Last Contact of the Current Campaign

#  8. contact: contact communication type (categorical: "cellular", "telephone") 
class(d$contact)
sum(is.na(d$contact))
sort(table(d$contact), decreasing = TRUE)
contact_counts <- d %>% group_by(contact) %>% summarize(count = n()) %>% arrange(desc(count))
contact_counts
ggplot(contact_counts, aes(reorder(contact, -count), count)) + 
  geom_col() + 
  xlab("contact") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  9. month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
class(d$month)
sum(is.na(d$month))
sort(table(d$month), decreasing = TRUE)
month_counts <- d %>% group_by(month) %>% summarize(count = n()) %>% arrange(desc(count))
month_counts
ggplot(month_counts, aes(reorder(month, -count), count)) + 
  geom_col() + 
  xlab("month") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 10. day_of_week: last contact day of the week (categorical: "mon", "tue", "wed", "thu", "fri")
class(d$day_of_week)
sum(is.na(d$day_of_week))
sort(table(d$day_of_week), decreasing = TRUE)
day_of_week_counts <- d %>% group_by(day_of_week) %>% summarize(count = n()) %>% arrange(desc(count))
day_of_week_counts
ggplot(day_of_week_counts, aes(reorder(day_of_week, -count), count)) + 
  geom_col() + 
  xlab("day_of_week") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 11. duration: last contact duration, in seconds (numeric). Important note:  this attribute highly affects the output target (e.g., if duration=0 then y="no"). Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.
class(d$duration)
sum(is.na(d$duration))
min(d$duration)
max(d$duration)
boxplot(d$duration)
hist(d$duration)
ggplot(d, aes(duration)) + geom_histogram()

# Other Attributes

# 12. campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
class(d$campaign)
sum(is.na(d$campaign))
min(d$campaign)
max(d$campaign)
boxplot(d$campaign)
hist(d$campaign)
ggplot(d, aes(campaign)) + geom_histogram()

# 13. pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)
class(d$pdays)
sum(is.na(d$pdays))
min(d$pdays)
max(d$pdays)
boxplot(d$pdays)
hist(d$pdays)
ggplot(d, aes(pdays)) + geom_histogram()

# 14. previous: number of contacts performed before this campaign and for this client (numeric)
class(d$previous)
sum(is.na(d$previous))
min(d$previous)
max(d$previous)
boxplot(d$previous)
hist(d$previous)
ggplot(d, aes(previous)) + geom_histogram()

# 15. poutcome: outcome of the previous marketing campaign (categorical: "failure", "nonexistent", "success")
class(d$poutcome)
sum(is.na(d$poutcome))
sort(table(d$poutcome), decreasing = TRUE)
poutcome_counts <- d %>% group_by(poutcome) %>% summarize(count = n()) %>% arrange(desc(count))
poutcome_counts
ggplot(poutcome_counts, aes(reorder(poutcome, -count), count)) + 
  geom_col() + 
  xlab("poutcome") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Social and Economic Context Attributes

# 16. emp.var.rate: employment variation rate - quarterly indicator (numeric)
class(d$emp.var.rate)
sum(is.na(d$emp.var.rate))
min(d$emp.var.rate)
max(d$emp.var.rate)
boxplot(d$emp.var.rate)
hist(d$emp.var.rate)
ggplot(d, aes(emp.var.rate)) + geom_histogram()

# 17. cons.price.idx: consumer price index - monthly indicator (numeric)     
class(d$cons.price.idx)
sum(is.na(d$cons.price.idx))
min(d$cons.price.idx)
max(d$cons.price.idx)
boxplot(d$cons.price.idx)
hist(d$cons.price.idx)
ggplot(d, aes(cons.price.idx)) + geom_histogram()

# 18. cons.conf.idx: consumer confidence index - monthly indicator (numeric)     
class(d$cons.conf.idx)
sum(is.na(d$cons.conf.idx))
min(d$cons.conf.idx)
max(d$cons.conf.idx)
boxplot(d$cons.conf.idx)
hist(d$cons.conf.idx)
ggplot(d, aes(cons.conf.idx)) + geom_histogram()

# 19. euribor3m: euribor 3 month rate - daily indicator (numeric)
class(d$euribor3m)
sum(is.na(d$euribor3m))
min(d$euribor3m)
max(d$euribor3m)
boxplot(d$euribor3m)
hist(d$euribor3m)
ggplot(d, aes(euribor3m)) + geom_histogram()

# 20. nr.employed: number of employees - quarterly indicator (numeric)
class(d$nr.employed)
sum(is.na(d$nr.employed))
min(d$nr.employed)
max(d$nr.employed)
boxplot(d$nr.employed)
hist(d$nr.employed)
ggplot(d, aes(nr.employed)) + geom_histogram()

# Output Variable (Target)

# 21. y: has the client subscribed a term deposit? (binary: "yes", "no")
class(d$y)
sum(is.na(d$y))
sort(table(d$y), decreasing = TRUE)
y_counts <- d %>% group_by(y) %>% summarize(count = n()) %>% arrange(desc(count))
y_counts
ggplot(y_counts, aes(reorder(y, -count), count)) + 
  geom_col() + 
  xlab("y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
