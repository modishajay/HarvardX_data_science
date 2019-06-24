library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
library(e1071)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#Question 1
inclass <- dat %>% filter(type == "inclass")
inclass_females <- dat %>% filter(sex == "Female" & type == "inclass")

online <- dat %>% filter(type == "online")
online_females <- dat %>% filter(sex == "Female" & type == "online")

count(inclass_females) / count(inclass)
count(online_females) / count(online)

#Question 2
y_hat <- ifelse(x == "inclass", "Female", "Male") %>% factor(levels = levels(y))
mean(y_hat == y)

#Question 3
table(y_hat, y)

#Question 4
#confusionMatrix(data = y_hat, reference = y)
sensitivity(data = y_hat, reference = y)

#Question 5
specificity(data = y_hat, reference = y)

#Question 6
females <- dat %>% filter(sex == "Female") 
count(females) / count(dat)

