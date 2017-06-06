#install.packages("RCurl")
library("RCurl")
urlfile <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/blood-transfusion/transfusion.data'
downloaded <- getURL(urlfile, ssl.verifypeer=FALSE)
connection <- textConnection(downloaded)
blood <- read.csv(connection, header=TRUE)
attach(blood)
head(blood)
colnames(blood)[which(names(blood)=="whether.he.she.donated.blood.in.March.2007")] <- "donated"
head(blood)

library("tree")
tree.blood <- tree(donated~., data = blood)
summary(tree.blood)
plot(tree.blood)
text(tree.blood, pretty = 0)

blood$donated <- ifelse(blood$donated==1, "Yes", "No")
blood$donated <- as.factor(blood$donated)
set.seed(2)
train <- sample(1:nrow(blood), nrow(blood)*.7) #random selection of observation/row numbers#
head(blood)
str(blood)

### just a note -- could bootstrap sample manually with
###(x, size, replace = FALSE)
train_bootstrap = sample(1:nrow(blood), nrow(blood), replace = TRUE)
length(unique(train2))

tree.blood.test <- blood[-train, "donated"]
tree.blood.train <- tree(donated~., data = blood, subset = train)
###in code below blood[-train] is missing the column Recency..months. Why?
tree.blood.pred <- predict(tree.blood.train, newdata=blood[-train,], type="class")
table(tree.blood.pred, tree.blood.test)


###What's this?
pred.df <- data.frame(tree.blood.pred)
pred.df
(157+19)/nrow(pred.df)


#k-fold validation
set.seed(2)
cv.tree.blood <- cv.tree(tree.blood.train, FUN=prune.misclass)
par(mfrow=c(1,2))
plot(cv.tree.blood$size, cv.tree.blood$dev, type = "b")
plot(cv.tree.blood$k, cv.tree.blood$dev, type = "b")

#prune
prune.tree.blood <- prune.misclass(tree.blood.train, best=2)
summary(prune.tree.blood)
plot(prune.tree.blood)
text(prune.tree.blood, pretty=0)
prune.blood.pred <- predict(prune.tree.blood, newdata=blood[-train,], type="class")
table(prune.blood.pred, tree.blood.test)

prune.pred.df <- data.frame(prune.blood.pred)
(157+19)/nrow(prune.pred.df)


#random forest
#install.packages("randomForest")
library(randomForest)
set.seed(2)
rf.blood <- randomForest(donated~., data = blood, subset = train, mtry=4, importance = TRUE)
rf.blood.pred <- predict(rf.blood, newdata = blood[-train,], type = "class")
table(rf.blood.pred, tree.blood.test)
rf.pred.df <- data.frame(rf.blood.pred)
(150+16)/nrow(rf.pred.df)

set.seed(2)
rf.blood2 <- randomForest(donated~., data = blood, subset = train, mtry=1, importance = TRUE)
rf.blood2.pred <- predict(rf.blood2, newdata = blood[-train,], type = "class")
table(rf.blood2.pred, tree.blood.test)
rf.pred.df2 <- data.frame(rf.blood2.pred)
(158+16)/nrow(rf.pred.df2) #best performance of three random forests performed
importance(rf.blood2)
varImpPlot(rf.blood2)

set.seed(2)
rf.blood3 <- randomForest(donated~., data = blood, subset = train, mtry=2, importance = TRUE)
rf.blood3.pred <- predict(rf.blood3, newdata = blood[-train,], type = "class")
table(rf.blood3.pred, tree.blood.test)
rf.pred.df3 <- data.frame(rf.blood3.pred)
(153+16)/nrow(rf.pred.df3)

#boosting
library("gbm")
set.seed(2)
blood$donated <- ifelse(blood$donated=="Yes", 1, 0) #have to turn the factors back into numeric 1s and 0s for the 'bernoulli distribution' in the gbm package
blood$donated <- as.numeric(blood$donated)
boost.blood <- gbm(donated~., data=blood[train,], distribution = "bernoulli", n.trees = 5000, interaction.depth = 4)
summary(boost.blood)
plot(boost.blood, i = "Time..months.")
plot(boost.blood, i = "Frequency..times.")
#could also test out difference lambda i.e. shrinkage values