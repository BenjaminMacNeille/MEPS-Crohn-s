library("RCurl")
urlfile <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/blood-transfusion/transfusion.data'
downloaded <- getURL(urlfile, ssl.verifypeer=FALSE)
connection <- textConnection(downloaded)
blood <- read.csv(connection, header=TRUE)
attach(blood)
head(blood)
colnames(blood)[which(names(blood)=="whether.he.she.donated.blood.in.March.2007")] <- "donated"
head(blood)
blood$y.donated <- ifelse(blood$donated==1, "Yes", "No")
str(blood)
unique(blood$donated)
sum(is.na(blood))
View(blood)
library("tree")
tree.blood <- tree(y.donated~.-donated, data = blood)
summary(tree.blood)
plot(tree.blood)
text(tree.blood, pretty = 0)
set.seed(2)
train <- sample(1:nrow(blood), nrow(blood)*.7) #random selection of observation/row numbers#
blood.test <- blood[-train,]
tree.blood.train <- tree(donated~., blood, subset = train)
tree.blood.pred <- predict(tree.blood.train, blood.test, type="class")
