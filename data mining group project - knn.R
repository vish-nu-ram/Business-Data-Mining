library(caret)
library(readxl)
dm = read_excel("/Users/anushree/Downloads/bank-full excel.xlsx")
dim(dm)
summary(dm)
colnames(dm)
#View(dm)


#conversion of categorical variables to binary variables
dm$education = factor(dm$education,levels = c('primary', 'secondary', 'tertiary', 'unknown'),labels = c(1, 2, 3,4))
dm$default = factor(dm$default, levels = c('yes','no'), labels = c(1,2))
dm$housing = factor(dm$housing, levels = c('yes','no'), labels = c(1,2))
dm$loan = factor(dm$loan, levels = c('yes','no'), labels = c(1,2))
dm$y = factor(dm$y, levels = c('yes','no'), labels = c(1,2))
dm$poutcome = factor(dm$poutcome, levels = c('success', 'failure', 'other', 'unknown'),labels = c(1, 2, 3,4))

#removed job, marital, contact, month from the data frame
dm_new = dm[, -c(2,3,9,11)]
summary(dm_new)
colnames(dm_new)
head(dm_new)
#View(dm_new)
table(dm_new$y)

#partioning the data into training, validation and test set
set.seed(111)
train_index = sample(row.names(dm_new),0.6*dim(dm_new))
valid_index = sample(setdiff(row.names(dm_new), train_index), 0.39*dim(dm_new))
test_index = setdiff(row.names(dm_new), c(train_index, valid_index)) 

train_df = dm_new[train_index,]
valid_df = dm_new[valid_index,]
test_df = dm_new[test_index,]
View(train_df)


#normalizing the data
#train_norm_df = train_df[,-c(2,3,5,6,8,12,13)]
#valid_norm_df = valid_df[,-c(2,3,5,6,8,12,13)]
#test_norm_df = test_df[,-c(2,3,5,6,8,12,13)]

norm_values = preProcess(train_norm_df, method=c("center", "scale"))
train_norm_df_new = predict(norm_values, train_df)
valid_norm_df_new = predict(norm_values, valid_df)
test_norm_df_new = predict(norm_values, test_df)

length(train_norm_df_new)

##############################################################
##Under sampling to balance the data

y_values <- which(train_df$y == 1)
n_values <- which(train_df$y == 2)

b = length(train_norm_y)

pick_y <- sample(y_values, b)
pick_n <- sample(n_values, b)

new_data <- train_norm_df_new[c(pick_y, pick_n), ]

summary(new_data)
##############################################################

# optimal k
accuracy.df = data.frame(k = seq(1, 30, 1), overallaccurace = rep(0, 30))
for(i in 1:30) {
  knn_pred = class::knn(train = new_data, 
                         test = valid_norm_df_new, 
                         cl = new_data$y, k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn_pred, 
                                       as.factor(valid_df$y))$overall[1]
}

which(accuracy.df[,2] == max(accuracy.df[,2])) 

accuracy.df   
#k =3 gives the best results

#knn with k = 3
knn_pred_1 = class::knn(train = train_norm_df_new, 
                         test = valid_norm_df_new, 
                         cl = train_df$y, k = 3)
confusionMatrix(knn_pred_1, as.factor(valid_df$y), positive = "1")

#cm = as.matrix(table(Actual = train_norm_df_new, Predicted = knn_pred_1))

# prediction of test set with k = 3
knn_pred_test <- class::knn(train = train_norm_df_new, 
                       test = test_norm_df_new, 
                       cl = train_df$y, k = 3)

knn_pred_test



