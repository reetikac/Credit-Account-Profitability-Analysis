#Credit profitabaility 
setwd("C:\\reetika\\useful study material\\ms sem 2\\data")
library(gplots)
library(ROCR)
#1a
cd <- read.csv("Credit_Dataset.csv")
#1b
cd$PROFITABLE[cd$PROFIT>0] <- 1 #if no net loss then 1 
cd$PROFITABLE[cd$PROFIT<0] <- 0 #if net loss then 0 
#1c
cd$CHK_ACCT <- as.factor(cd$CHK_ACCT)
cd$SAV_ACCT <- as.factor(cd$SAV_ACCT)
cd$HISTORY <- as.factor(cd$HISTORY)
cd$JOB <- as.factor(cd$JOB)
cd$TYPE <- as.factor(cd$TYPE)
#1d
set.seed(11217)
test_instn = sample(nrow(cd), 0.3*nrow(cd))#partition 30% as test 
cd_test <- cd[test_instn,] #make a new dataframe with test data in it 
cd_rest <- cd[-test_instn,] #this is the remaining data left that is 70%
#1e
valid_instn = sample(nrow(cd_rest), 0.25*nrow(cd_rest))#partition 25% of remaining as validation
cd_valid <- cd_rest[valid_instn,]
cd_train <- cd_rest[-valid_instn,]

########################

mod_logistic <- glm(PROFITABLE ~ AGE+DURATION+RENT+TELEPHONE+FOREIGN+CHK_ACCT+SAV_ACCT+HISTORY+JOB+TYPE,family="binomial",cd_train)
summary(mod_logistic)
#2a
logistic_preds <- predict(mod_logistic,newdata=cd_valid)
logistic_preds
logistic_predsontrain <- predict(mod_logistic, newdata=cd_train)
pred <- prediction(logistic_preds,cd_valid$PROFITABLE)
trai <- prediction(logistic_predsontrain,cd_train$PROFITABLE)

acc.perf = performance(pred, measure = "acc")#accuracy
tpr.perf = performance(pred, measure = "tpr")#sensitivity-tpr
tnr.perf = performance(pred, measure = "tnr")#specificity-tnr

plot(tpr.perf,ylim=c(0,1))#plot tpr values wrt cutoff values
plot(tnr.perf, add=T, col="blue")#add tnr to the plot
plot(acc.perf, add=T, col="red") #add accuracy to the plot 
par(xpd=TRUE)
legend(0.05,1.4,legend= c("TPR","TNR","Accuracy") , c("black","blue","red"))
 
#below are the steps to find best accuracy at the particular cutoff point
best = which.max(slot(acc.perf,"y.values")[[1]])
max.acc = slot(acc.perf,"y.values")[[1]][best]
max.cutoff = slot(acc.perf,"x.values")[[1]][best]
print(c(accuracy= max.acc, cutoff = max.cutoff))

#2b 
roc.valid = performance(pred, measure = "tpr", x.measure = "fpr")
roc.train = performance(trai, measure= "tpr", x.measure = "fpr")
plot(roc.valid,colorize=T)
plot(roc.train, add=T)
abline(a=0,b=1,lty=3)

################
#3
library(tree)
mod_tree = tree(PROFITABLE ~ AGE+DURATION+RENT+TELEPHONE+FOREIGN+CHK_ACCT+SAV_ACCT+HISTORY+JOB+TYPE, cd_train)
summary(mod_tree)
plot(mod_tree)
text(mod_tree, pretty=1)#full unpruned tree
tree_pred_full_v <- predict(mod_tree,newdata=cd_valid)#predict the values in validation
tree_pred_full_t <- predict(mod_tree, newdata=cd_train)#predict the values in train 
class_valid_full_v <- ifelse(tree_pred_full_v>.5,1,0)
class_train_full_t <- ifelse(tree_pred_full_t>.5,1,0)
acc_full_v = sum(ifelse(cd_valid$PROFITABLE==class_valid_full_v,1,0))/nrow(cd_valid)
acc_full_t = sum(ifelse(cd_train$PROFITABLE==class_train_full_t,1,0))/nrow(cd_train)

mod_tree_pruned_1 =prune.tree(mod_tree,best=1)# for terminal node=1
plot(mod_tree_pruned_1)
text(mod_tree_pruned_1,pretty=1)
tree_pred_1v <- predict(mod_tree_pruned_1,newdata=cd_valid)#predict the values in validation
tree_pred_1t <- predict(mod_tree_pruned_1, newdata=cd_train)#predict the values in train 
class_valid_1v <- ifelse(tree_pred_1v>.5,1,0)
class_train_1t <- ifelse(tree_pred_1t>.5,1,0)
acc_1v = sum(ifelse(cd_valid$PROFITABLE==class_valid_1v,1,0))/nrow(cd_valid)
acc_1t = sum(ifelse(cd_train$PROFITABLE==class_train_1t,1,0))/nrow(cd_train)

mod_tree_pruned_2 =prune.tree(mod_tree,best=2)# for terminal node=2
plot(mod_tree_pruned_2)
text(mod_tree_pruned_2,pretty=1)
tree_pred_2v <- predict(mod_tree_pruned_2,newdata=cd_valid)#predict the values in validation
tree_pred_2t <- predict(mod_tree_pruned_2, newdata=cd_train)#predict the values in train 
class_valid_2v <- ifelse(tree_pred_2v>.5,1,0)
class_train_2t <- ifelse(tree_pred_2t>.5,1,0)
acc_2v = sum(ifelse(cd_valid$PROFITABLE==class_valid_2v,1,0))/nrow(cd_valid)
acc_2t = sum(ifelse(cd_train$PROFITABLE==class_train_2t,1,0))/nrow(cd_train)

mod_tree_pruned_5 =prune.tree(mod_tree,best=5)# for terminal node=5
plot(mod_tree_pruned_5)
text(mod_tree_pruned_5,pretty=1)
tree_pred_5v <- predict(mod_tree_pruned_5,newdata=cd_valid)#predict the values in validation
tree_pred_5t <- predict(mod_tree_pruned_5, newdata=cd_train)#predict the values in train 
class_valid_5v <- ifelse(tree_pred_5v>.5,1,0)
class_train_5t <- ifelse(tree_pred_5t>.5,1,0)
acc_5v = sum(ifelse(cd_valid$PROFITABLE==class_valid_5v,1,0))/nrow(cd_valid)
acc_5t = sum(ifelse(cd_train$PROFITABLE==class_train_5t,1,0))/nrow(cd_train)

mod_tree_pruned_10 =prune.tree(mod_tree,best=10)# for terminal node=10
plot(mod_tree_pruned_10)
text(mod_tree_pruned_10,pretty=1)
tree_pred_10v <- predict(mod_tree_pruned_10,newdata=cd_valid)#predict the values in validation
tree_pred_10t <- predict(mod_tree_pruned_10, newdata=cd_train)#predict the values in train 
class_valid_10v <- ifelse(tree_pred_10v>.5,1,0)
class_train_10t <- ifelse(tree_pred_10t>.5,1,0)
acc_10v = sum(ifelse(cd_valid$PROFITABLE==class_valid_10v,1,0))/nrow(cd_valid)
acc_10t = sum(ifelse(cd_train$PROFITABLE==class_train_10t,1,0))/nrow(cd_train)

plot(c(23,3,9,21), c(acc_full_v,acc_2v,acc_5v,acc_10v), col = rep(c('black','red', 'forestgreen','blue')), main="Tree size vs Accuracy of validation")
plot(c(23,3,9,21), c(acc_full_t,acc_2t,acc_5t,acc_10t), col = rep(c('black','red', 'forestgreen','blue')), main="Tree size vs Accuracy of training")

#3c
table(cd_valid$PROFITABLE,class_valid_full_v)

##################
#4

cd$CHK_ACCT <- as.numeric(cd$CHK_ACCT)
cd$SAV_ACCT <- as.numeric(cd$SAV_ACCT)
cd$HISTORY <- as.numeric(cd$HISTORY)
cd$JOB <- as.numeric(cd$JOB)
cd$TYPE <- as.numeric(cd$TYPE)

library(class)

train.X=cd_train[,c(2,3,4,6,7,10,12,17,18,20)]
#train.X=cd_train[,c(2,6,12,17,18)]
valid.X=cd_valid[,c(2,3,4,6,7,10,12,17,18,20)]
test.X=cd_test[,c(2,3,4,6,7,10,12,17,18,20)]

train.profitable=as.numeric(cd_train$PROFITABLE)
valid.profitable=as.numeric(cd_valid$PROFITABLE)
test.profitable=as.numeric(cd_test$PROFITABLE)

#using k=1 
knn.pred1v <- knn(train.X,valid.X,train.profitable,k=1)
cm1v=as.matrix(table(valid.profitable,knn.pred1v))#actual, predicted
acc1v=sum(diag(cm1v))/length(valid.profitable)

knn.pred1t <- knn(train.X,train.X,train.profitable,k=1)
cm1t=as.matrix(table(train.profitable,knn.pred1t))#actual, predicted
acc1t=sum(diag(cm1t))/length(train.profitable)

#using k=3 
knn.pred3v <- knn(train.X,valid.X,train.profitable,k=3)
cm3v=as.matrix(table(valid.profitable,knn.pred3v))#actual, predicted
acc3v=sum(diag(cm3v))/length(valid.profitable)

knn.pred3t <- knn(train.X,train.X,train.profitable,k=3)
cm3t=as.matrix(table(train.profitable,knn.pred3t))#actual, predicted
acc3t=sum(diag(cm3t))/length(train.profitable)

#using k=5 
knn.pred5v <- knn(train.X,valid.X,train.profitable,k=5)
cm5v=as.matrix(table(valid.profitable,knn.pred5v))#actual, predicted
acc5v=sum(diag(cm5v))/length(valid.profitable)

knn.pred5t <- knn(train.X,train.X,train.profitable,k=5)
cm5t=as.matrix(table(train.profitable,knn.pred5t))#actual, predicted
acc5t=sum(diag(cm5t))/length(train.profitable)

#using k=10 
knn.pred10v <- knn(train.X,valid.X,train.profitable,k=10)
cm10v=as.matrix(table(valid.profitable,knn.pred10v))#actual, predicted
acc10v=sum(diag(cm10v))/length(valid.profitable)

knn.pred10t <- knn(train.X,train.X,train.profitable,k=10)
cm10t=as.matrix(table(train.profitable,knn.pred10t))#actual, predicted
acc10t=sum(diag(cm10t))/length(train.profitable)

#using k=25 
knn.pred25v <- knn(train.X,valid.X,train.profitable,k=25)
cm25v=as.matrix(table(valid.profitable,knn.pred25v))#actual, predicted
acc25v=sum(diag(cm25v))/length(valid.profitable)

knn.pred25t <- knn(train.X,train.X,train.profitable,k=25)
cm25t=as.matrix(table(train.profitable,knn.pred25t))#actual, predicted
acc25t=sum(diag(cm25t))/length(train.profitable)

plot(c(1,3,5,10,25), c(acc1t,acc3t,acc5t,acc10t,acc25t), col = rep(c('black','red','forestgreen','blue',"pink")), main="k vs Accuracy of training")
plot(c(1,3,5,10,25), c(acc1v,acc3v,acc5v,acc10v,acc25v), col = rep(c('black','red','forestgreen','blue',"pink")), main="k vs Accuracy of validation")

#########################
#5

#logistic model on  test data
final_pred_logistic <- predict(mod_logistic,newdata=cd_test)
class_test <- ifelse(final_pred_logistic>.5,1,0)
table(cd_test$PROFITABLE, class_test)
accuracy_test_logistic = sum(ifelse(cd_test$PROFITABLE==class_test,1,0))/nrow(cd_test)
rmse.logistic <- sqrt(mean((cd_test$PROFITABLE- final_pred_logistic)^2))

#full tree on test data 
tree_pred_full_test <- predict(mod_tree,newdata=cd_test)#predict the values in test
class_test_full <- ifelse(tree_pred_full_test>.5,1,0)
accuracy_test_tree = sum(ifelse(cd_test$PROFITABLE==class_test_full,1,0))/nrow(cd_test)

#knn k=3 on test data 
knn.pred3test <- knn(train.X,test.X,train.profitable,k=3)
cm3test=as.matrix(table(test.profitable,knn.pred3test))#actual, predicted
accuracy_test_knn=sum(diag(cm3test))/length(test.profitable)






