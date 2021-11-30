setwd("~/Google Drive/My Drive/Trinity/MSc Business Analytics 2021/Business Data Mining/GroupProject_02/Data")

library(ggplot2)

#semicolon seperated dataset
data_raw <- read.csv("bank-full.csv", sep = ';')
View(data_raw)
summary(data_raw)
str(data_raw)
head(data_raw,10)

# Checking for any missing data, it turns out 0
sum(!complete.cases(data_raw))

ggplot(data_raw, aes(x=duration)) +
  geom_histogram(aes(y=..density..), colour="grey", fill="lightblue", linetype="dashed", bins=10)+
  geom_density(alpha=.2)+ 
  xlim(0,800)
geom_vline(aes(xintercept= mean(duration)), color="blue",
           linetype="dashed", size=1)

ggplot(data_raw, aes(x=duration, fill=y)) + xlim(0,800) + geom_density(alpha=.3)


# As can be seen, my assumption is that the one who says yes are informed a bit further, that's why duration is longer.
# Duration is not something identified before someone accepts or not, so I'll drop it.

data_raw$duration <- NULL

summary(data_raw$pdays)
str(data_raw$pdays)

# these column shows mostly -1's which means they were not 
sum(data_raw$pdays == -1)

# 36954 rows (TOTAL 45221) shows -1, which means they are not contacted, so I'll drop this column as well.
data_raw$pdays <- NULL
data_raw$month <- NULL
data_raw$day <- NULL

data_raw$y <- ifelse(data_raw$y == "yes", 1, 0)
data_raw_1 <- na.omit(data_raw)

# Joining some close jobs
data_raw$job <- as.character(data_raw$job)
data_raw$job[data_raw$job %in% c("adata_rawin.","management")] <- "managerial"
data_raw$job[data_raw$job %in% c('services','housemaid')] <- "houseAndServicing"

set.seed(1)
train.index <- sample(c(1:dim(data_raw)[1]),
                      0.6*dim(data_raw)[1])
valid.index <- setdiff(c(1:dim(data_raw)[1]), train.index)

train.df <- data_raw[train.index, ]
valid.df <- data_raw[valid.index, ]

library(caret)

mod_fit <- glm(y ~ ., data = train.df, family = "binomial")
summary(mod_fit)

sum(valid.df$y==1)

anova(mod_fit, test = 'Chisq')
outcome <- exp((predict(mod_fit, newdata=valid.df)))>0.5
sum(outcome==TRUE)

probabilities <- mod_fit %>% predict(valid.df, type = "prob")

hist(data_raw$age)
hist(data_raw$balance)

library("dplyr")

colnames("data_raw")

