library(readxl)
#install.packages("varhandle")
library(varhandle)
library(caret)
library(neuralnet)
library(forecast)
library(gains)
library(fastDummies)

dm = read_excel("bank-full excel.xlsx")
dim(dm)
summary(dm)
colnames(dm)

print(dm)
binary_Education <- to.dummy(dm$education, "education")

#conversion of categorical variables to binary variables
#dm$education = factor(dm$education,levels = c('primary', 'secondary', 'tertiary', 'unknown'),labels = c(1, 2, 3,4))
dm$default = factor(dm$default, levels = c('yes','no'), labels = c(1,0))
dm$housing = factor(dm$housing, levels = c('yes','no'), labels = c(1,0))
dm$loan = factor(dm$loan, levels = c('yes','no'), labels = c(1,0))
dm$y = factor(dm$y, levels = c('yes','no'), labels = c(1,0))
#dm$poutcome = factor(dm$poutcome, levels = c('success', 'failure', 'other', 'unknown'),labels = c(1, 2, 3,4))

dm

#dm<- dummy_cols(dm, select_columns = 'job')
dm<- dummy_cols(dm, select_columns = 'marital')
dm<- dummy_cols(dm, select_columns = 'education')
colnames(dm)
#removed job, marital, contact, month from the data frame
dm_new = dm[, -c(2,3,4,9,10,11,13,14,15,16)]
colnames(dm_new)
head(dm_new)

dm_new$loan <- as.integer(as.character(dm_new$loan))
dm_new$default <- as.integer(as.character(dm_new$default))
dm_new$housing <- as.integer(as.character(dm_new$housing))
dm_new$y <- as.integer(as.character(dm_new$y))
dm_new

#partioning the data into training, validation and test set
set.seed(111)
train_index = sample(row.names(dm_new),0.6*dim(dm_new))
valid_index = sample(setdiff(row.names(dm_new), train_index), 0.39*dim(dm_new))
test_index = setdiff(row.names(dm_new), c(train_index, valid_index))


train_df = dm_new[train_index,]
valid_df = dm_new[valid_index,]
test_df = dm_new[test_index,]

#scale
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

train_norm <- as.data.frame(lapply(train_df, min_max_norm))
valid_norm <- as.data.frame(lapply(valid_df[,-7], min_max_norm))
valid_df <- as.data.frame(lapply(valid_df[,-7], min_max_norm))
valid_df <- cbind(valid_df,dm[valid_index,'y'])
valid_df$y <- as.factor(valid_df$y)
head(train_norm)
summary(train_norm)
dim(train_norm)

##Under sampling to balance the data

train_norm_y <- which(train_norm$y == 1)
train_norm_n <- which(train_norm$y == 0)

length(train_norm_y)

pick_y <- sample(train_norm_y, length(train_norm_y))
pick_n <- sample(train_norm_n, length(train_norm_y))

new_data <- train_norm[c(pick_y, pick_n), ]

summary(new_data)

nn <- neuralnet(y ~ balance +
                  duration+
                  default+
                  housing+
                  age+
                  education_unknown,
                data = new_data, linear.output = F,
                hidden = 3)
plot(nn)

# predictions on training and validation data
options(scipen = 0)
# training prediction probabilities
train_pred <- compute(nn, new_data)
train_pred <- train_pred$net.result[,1]
# convert probabilities to classes
train.class <- (1* (train_pred>0.5))
confusionMatrix(factor(train.class), factor(new_data$y), positive = "1")


# validation prediction probabilities
valid.pred <- compute(nn, valid_norm)
valid.pred <- valid.pred$net.result[,1]
# convert probabilities to classes
valid.class <- (1* (valid.pred>0.5))
# confusion matrix 
valid_norm['y']<- valid_df$y
confusionMatrix(factor(valid.class), factor(valid_norm$y), positive = "1")

#On a balanced validation set
####################################################

##Under sampling to balance the data

valid_norm_y <- which(valid_norm$y == 1)
valid_norm_n <- which(valid_norm$y == 0)

length(valid_norm_y)

pick_y_valid <- sample(valid_norm_y, length(valid_norm_y))
pick_n_valid <- sample(valid_norm_n, length(valid_norm_y))

new_data_valid <- valid_norm[c(pick_y_valid, pick_n_valid), ]

valid.pred <- compute(nn, new_data_valid)
valid.pred <- valid.pred$net.result[,1]
# convert probabilities to classes
valid.class <- (1* (valid.pred>0.5))
# confusion matrix 
confusionMatrix(factor(valid.class), factor(new_data_valid$y), positive = "1")


############################## Random forest #############

#install.packages("randomForest")
#install.packages("Boruta")
library(randomForest)
library(Boruta)

### finding the varibale importance
dm_new1 = dm[, -c(2,3,4,9,10,11,13,14,15,16)]
bor <- Boruta(y~.,data = dm_new1, doTrace=2, maxRuns = 20)
set.seed(111)
train_index = sample(row.names(dm_new1),0.6*dim(dm_new1))
valid_index = sample(setdiff(row.names(dm_new1), train_index), 0.39*dim(dm_new1))
test_index = setdiff(row.names(dm_new1), c(train_index, valid_index))

train_df = dm_new1[train_index,]
valid_df = dm_new1[valid_index,]
test_df = dm_new1[test_index,]

#######Random forest###############
new_data$y <- as.factor(new_data$y)
valid_df$y <- as.factor(valid_df$y)
model_RF <- randomForest(getNonRejectedFormula(bor),data = new_data,importance=T,proximity=T)
print(model_RF)

p1_RF <- predict(model_RF,new_data)
confusionMatrix(p1_RF,new_data$y)

p2_RF <- predict(model_RF,valid_norm)
confusionMatrix(p2_RF,valid_norm$y)

#confMatrix_1_RF <- table(Predicted=p1_RF,Actual=train_df$y)
#w_RF1 = sum(diag(confMatrix_1_RF ))/nrow(train_df)

#confMatrix_2_RF <- table(Predicted=p2_RF,Actual=valid_norm$y)
#w_RF2 = sum(diag(confMatrix_2_RF ))/nrow(valid_norm)


#rocPlot(trainData,as.numeric(p1_RF),'RF')


############## Tree ##############
library(e1071)
#install.packages("party")
library(party)
#install.packages("ROCR")
library(ROCR)
library(pROC)

model_tree <- ctree(getNonRejectedFormula(bor),data = new_data, controls = ctree_control(mincriterion = 0.9,minsplit = 300))
plot(model_tree)

p1_Tree <- predict(model_tree,new_data,type = 'response')
confMatrix_1_Tree <- table(p1_Tree,new_data$y)
confMatrix_1_Tree
sum(diag(confMatrix_1_Tree))/nrow(new_data)
w_Tree1 = sum(diag(confMatrix_1_Tree))/nrow(new_data)

p2_Tree <- predict(model_tree,valid_norm,type = 'response')
confMatrix_2_Tree <- table(p2_Tree,valid_df$y)
confMatrix_2_Tree 
w_Tree2 = sum(diag(confMatrix_2_Tree))/nrow(valid_df)


rocPlot(trainData,as.numeric(p1_Tree),'Tree')
