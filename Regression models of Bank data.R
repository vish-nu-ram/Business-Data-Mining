library(leaps) #for subset selection
library(corrgram) #for producing a graphical display of a correlation matrix
library(caret)
library(ggplot2)
library(Hmisc)
library(reshape)
library(leaps) #for subset selection
library(gains) #for gains and lift chart
library(forecast) #for accuracy measures
library(corrgram) #for producing a graphical display of a correlation matrix
dm <- read.csv("BankData.csv")
head(bank.df)

unique(dm$marital)

#factoring categorical variables to numerical variables 
dm$education = factor(dm$education,levels = c('primary', 'secondary', 'tertiary', 'unknown'),labels = c(1, 2, 3,4))
dm$job = factor(dm$job,levels = c('management' ,  'technician',    'entrepreneur' , 'blue-collar',   'unknown'  ,'retired'   ,'admin',
                                  'services' ,  'self-employed' , 'unemployed'  ,  'housemaid'  , 'student' ),
                labels = c(1, 2, 3,4,5,6,7,8,9,10,11,12))
dm$housing = factor(dm$housing, levels = c('yes','no'), labels = c(1,2))
dm$loan = factor(dm$loan, levels = c('yes','no'), labels = c(1,2))
dm$y = factor(dm$y, levels = c('yes','no'), labels = c(1,2))
dm$poutcome = factor(dm$poutcome, levels = c('success', 'failure', 'other', 'unknown'),labels = c(1, 2, 3,4))
dm$marital = factor(dm$marital, levels = c('married', 'single','divorced' ), labels = c(1,2,3))
head(dm)

#removed 
dm_new = dm[, -c(5,9,10,11,12,14,15)]
summary(dm_new)
head(dm_new)
View(dm_new)


unique(train.df$y)

#changing values in y to 0 & 1 from 1&2
train.df$y_new <- as.numeric(train.df$y) - 1
valid.df$y_new <- as.numeric(valid.df$y) -1

#regression to predict y  
regression <- lm(y_new ~ age + job + marital+ education +	balance	+ housing +loan + poutcome + duration, data = train.df)

regression <- lm(y_new ~ .-y, data = train.df)

regression <- lm(y_new ~  duration +  housing + poutcome, data = train.df)

summary(regression)

#validation set regression 
reg_valid <- lm(y_new ~age + job + marital+ education +	balance	+ housing +loan + poutcome, data = valid.df)

summary (reg_valid)

#Stepwise regression for training and validation set

step(regression, direction = "both")
step(reg, direction = "both")

step(regression, direction = "forward")
step(reg, direction = "forward")

step(regression, direction = "backward")
step(reg, direction = "backward")

#plotting residulas of training and validation set
hist(reg$residuals)
plot(regression$residuals, regression$fitted.values)

#calculating the accuracy
pred.train <- predict(regression, train.df)
accuracy(pred.train, train.df$y_new)

pred.valid <- predict(reg, valid.df)
accuracy(pred.valid, valid.df$y_new)

#scatter plots
plot(train.df$housing ~ train.df$y_new)













#predictions <- predict(regression, train.df)
#confusionMatrix(predictions$class, valid.df)


#plot(age ~ y, data=train.df)
#hist(train.df$y)

#corr <- cor(train.df$y)

#fit4=lm(y_new~age*poutcome*balance,data=train.df)
#summary(fit4)x

#library(ggiraphExtra)
#ggPredict(fit4,interactive = TRUE)

#ggplot(train.df,aes(y=y_new,x=balance,color=age))+geom_point()+stat_smooth(method="lm",se=FALSE)
0
par(mfrow = c(2, 1))
plot(train.df$y, train.df$age)
     #, ylim = c(0, 150), ylab = "age", xlab = "Y",     bty = "l", xaxt = "n", main = "", flty = 2)
