##1. Read the file
playstore<- read.csv("googleplaystore.csv")
dim(playstore)
summary(playstore)
colSums(is.na(playstore))

##2. Data Cleaning
#2.1 Remove duplicate values from App
library(dplyr)
newdata<-distinct(playstore,App, .keep_all= TRUE)
dim(newdata)

#2.2 Convert Type = Nan as Free
newdata$Type <- as.factor(gsub("NaN","Free", as.factor(newdata$Type)))

#2.3 Convert Content.Rating Unrated with Everyone
newdata$Content.Rating <- as.factor(gsub("Unrated","Everyone", as.factor(newdata$Content.Rating)))

#2.4 Clean Size by Removing  M from Size
newdata$Size<- gsub("M", "", as.character(newdata$Size))
newdata$Size[1:5]

#2.5 Clean Size by removing k and convert it to Mb and then convert to Numeric
newdata$Size <- as.character(ifelse(grepl("k", newdata$Size),
                                    as.numeric(as.character(gsub("k","",newdata$Size)))/1024,
                                    newdata$Size))
newdata$Size <- as.numeric(newdata$Size)
newdata$Size<- round(newdata$Size, 2)

#2.6 Convert Last.Updated date as %d-%b-%Y
x<- newdata$Last.Updated
newdata$Last.Updated<-as.Date(x,"%d-%b-%Y")
newdata$Last.Updated[1:5]

#2.7 Clean Installs by removing + from installs then convert to Numeric
newdata$Installs <- gsub("\\+","", as.character(newdata$Installs))
newdata$Installs <- gsub("\\,","", as.character(newdata$Installs))
newdata$Installs <- as.numeric(newdata$Installs)

#2.8 Clean Price column by removing $ and converting it to numeric
newdata$Price <- gsub("\\$","", as.character(newdata$Price))
newdata$Price <- as.numeric(newdata$Price)

#2.9 Remove missing data (na) 
playstore1 <- newdata[complete.cases(newdata),]
nrow(playstore1)

##3. Data Visualization
install.packages("ggplot2")
library(ggplot2)
str(playstore1)
nrow(playstore1)
qplot(Size,Rating,data=playstore1)
#3.1 different scatter plots
install.packages("car")
library(dplyr)
library(car)
scatterplot(Rating~Size|Installs,data=playstore1,
            smoother=FALSE,grid=FALSE,Frame=FALSE,cex=0.8)
#3.2 3D plot
library(scatterplot3d)
x<-playstore1$Size
y<-playstore1$Rating
z<-playstore1$Installs
grps<-as.factor(playstore1$Category)
scatterplot3d(x,y,z,pch=16)

#3.3 3D Scatterplot with Coloring and Vertical Lines and Regression Plane
attach(mtcars)
s3d <-scatterplot3d(x,y,z, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")
fit <- lm(z ~ x+y)
s3d$plane3d(fit)
frequency(playstore1)

#3.4 Scatter plot
qplot(Installs,Category,data=playstore1)
qplot(Price,Category,data=playstore1)

#3.5 boxplot
boxplot(Installs~Type, data=playstore1,
        xlab="The type of payment",
        ylab="The number of Downloads",
        main="The number of Installs if the app is Paid/Free",
        col=c("cyan"),
        log="y")
#3.6 ggplot
ggplot(playstore1)+aes(x=Category,cex=0.1,fill=Type)+geom_bar()

#3.7 Piechart         
x <- c(1972,1144,843,463,460,424,392,387,384,382,366,342,336,295,283,260,258,234,231,175,156,149,137,127,88,85,85,82,65,64,60,60,53)
lbls<-c("FAMILY","GAME","TOOLS","MEDICAL","BUSINESS","PRODUCTIVITY","PERSONALIZATION","COMMUNICATION","SPORTS", "LIFESTYLE,FINANCE","HEALTH_AND_FITNESS","PHOTOGRAPHY","SOCIAL","NEWS_AND_MAGAZINE,SHOPPING","TRAVEL_AND_LOCAL,DATING","BOOKS_AND_REFERENCE","VIDEO_PLAYER,EDUCATION","ENTERTAINMENT","MAPS_AND_NAVIGATION","FOOD_AND_DRINK","HOUSE_AND_HOME","LIBRARIES_AND_DEMO","AUTO_AND_VEHICLES","WEATHER","ART_AND_VEHICLES","WEATHER","ART_AND_DESIGN","EVENTS","PARENTING","COMICS","BEAUTY")
piepercent<-round(100*x/sum(x),1)
pie(x,labels=piepercent,main="Pie Chart of count of Category",col=rainbow(length(x)))
legend("right",c("FAMILY","GAME","TOOLS","MEDICAL","BUSINESS","PRODUCTIVITY","PERSONALIZATION","COMMUNICATION","SPORTS", "LIFESTYLE,FINANCE","HEALTH_AND_FITNESS","PHOTOGRAPHY","SOCIAL","NEWS_AND_MAGAZINE,SHOPPING","TRAVEL_AND_LOCAL,DATING","BOOKS_AND_REFERENCE","VIDEO_PLAYER,EDUCATION","ENTERTAINMENT","MAPS_AND_NAVIGATION","FOOD_AND_DRINK","HOUSE_AND_HOME","LIBRARIES_AND_DEMO","AUTO_AND_VEHICLES","WEATHER","ART_AND_VEHICLES","WEATHER","ART_AND_DESIGN","EVENTS","PARENTING","COMICS","BEAUTY"),
       cex=0.4,fill = rainbow(length(x)))

#3.8 pairwise correlation matrix
library(GGally)
pairs(playstore1[3:6]) 
pairs(playstore1[3:7])

##4.Convert sample data in training, validating and testing data sets
## 70% Train, 20% Validate, 10% test
total.rows <- nrow(playstore1)
train.size <- floor(.7*total.rows)

#4.1 Choose rows at random
set.seed(42)
validation.size <- floor(.2*total.rows)
train.rows <- sample(1:total.rows, train.size)
#4.2 Create train set
playstore1.train <- playstore1[train.rows, ] #
playstore1.remaining <- playstore1[-train.rows, ]
remaining.rows <- nrow(playstore1.remaining)
validation.rows <- sample(1:remaining.rows, validation.size)
#4.3 Create Validation set
playstore1.validation <- playstore1.remaining[validation.rows, ] 
#4.4 Create test set
playstore1.test <- playstore1.remaining[-validation.rows, ]
#4.5 Rows of each set
nrow(playstore1.train) ## 4919
nrow(playstore1.validation) ## 1405
nrow(playstore1.test) ## 704

##5. Write csv files for train, validate and test sets
write.csv(playstore1.train, file = "playstore1-train.csv", row.names = FALSE)
write.csv(playstore1.validation, file = "playstore1-validate.csv", row.names = FALSE)
write.csv(playstore1.test, file = "playstore1-test.csv", row.names = FALSE)
sapply(playstore1.train, class)

##6. Linear Regression Model
#6.1 model1 with all variables except App name
playstore1.model1 <- lm(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating
                        + Last.Updated + Genres + Current.Ver + Android.Ver, data = playstore1.train)
summary(playstore1.model1) 
#Rsquare - 91.46, adjusted Rsquare  85.32

#6.2 model2 with significant variables
playstore1.model2 <- lm(Installs ~ Category + Reviews + Size + Type 
                        + Genres + Current.Ver, data = playstore1.train)
summary(playstore1.model2) 
#Rsquare - 91.32, adjusted Rsquare 85.25

#6.3 model3 with the most significant variables
playstore1.model3 <- lm(Installs ~ Category + Reviews  + Current.Ver, data = playstore1.train)
summary(playstore1.model3) ##Rsquare 91.1

##7 Logistic Regression
#7.1 convert Installs into two categories (Below 50k = 0, Above 50k = 1) using cut function
#playstore1.train
PS.train.Installs.Above50k <- cut(playstore1.train$Installs, breaks = c(0, 50000, Inf),
                                  labels = c("0","1"))
playstore1.train$Installs <- as.character(PS.train.Installs.Above50k)
playstore1.train$Installs <- as.factor(playstore1.train$Installs)
sapply(playstore1.train, class)
playstore1.train$Installs[1:5]

#playstore1.validation
PS.validation.Installs.Above50k <- cut(playstore1.validation$Installs, breaks = c(0, 50000, Inf),
                                       labels = c("0","1"))
playstore1.validation$Installs <- as.character(PS.validation.Installs.Above50k)
playstore1.validation$Installs <- as.factor(playstore1.validation$Installs)
sapply(playstore1.validation, class)
playstore1.validation$Installs[1:5]

#playstore1.test
PS.test.Installs.Above50k <- cut(playstore1.test$Installs, breaks = c(0, 50000, Inf),
                                 labels = c("0","1"))
playstore1.test$Installs <- as.character(PS.test.Installs.Above50k)
playstore1.test$Installs <- as.factor(playstore1.test$Installs)
sapply(playstore1.test, class)
playstore1.test$Installs[1:5]

#7.2 fit1 with variables Category, rating, reiews, size, type, price, content.rating
playstore1.fit1<- glm (Installs ~ Category+ Rating + Reviews + Size + Type + Price+ Content.Rating , data = playstore1.train, family = "binomial")
summary (playstore1.fit1)

actual <- playstore1.train$Installs
## P Scores from our model
predicted.probability <- predict(playstore1.fit1, type = "response") ## Convert Logit score to Prob
## Define threshold for that we need ROC Curve
## Collect data in data frame
roc.table <- data.frame(Threshold = seq(from = 0, to = 1, by = 0.01),
                        FPR = NA,
                        Sensitivity = NA)
## For Loop
for ( i in 1:nrow(roc.table)) {
  ## Calculate FPR
  roc.table$FPR[i] <- sum(predicted.probability > roc.table$Threshold[i] & actual == 0)/sum(actual ==0)
  
  ## Calculate Sensitivity
  roc.table$Sensitivity[i] <- sum(predicted.probability > roc.table$Threshold[i] & actual == 1)/ sum(actual == 1)
}
roc.table
#7.2.1 Graph ROC Curve
plot(Sensitivity ~ FPR, data = roc.table, type = "s")
## 45 degree line, lty = line type 2 dashed line, 1= solid line
abline(a = 0, b = 1, lty = 2)

###Threshold 0.5
threshold <-0.5
actual <- playstore1.train$Installs
predicted <- ifelse(predicted.probability > threshold, 1, 0)
## Confusion Matrix
table(actual, predicted)

#7.2.2 Accuracy Calculation
##Calculate Accuracy Train
sum(actual == predicted)/nrow(playstore1.train) ##  92.60%
##Sensitivity Train
sum(predicted == 1 & actual ==1) / sum(actual == 1) ## 89.73%
## Specificity Train
sum(predicted == 0 & actual ==0) / sum(actual == 0) ## 96.31%

#7.2.3 Test on Validate Data
actual <- playstore1.validation$Installs
predicted <- ifelse(predict(playstore1.fit1, newdata = playstore1.validation, type = "response") > threshold, 1, 0)
## Confucion Matrix
table(actual, predicted)
## Accuracy
sum(actual == predicted)/nrow(playstore1.validation) ## 93.23%
## Sensitivity
sum(predicted == 1 & actual ==1) / sum(actual == 1) ## 90.74%
## Specificity
sum(predicted == 0 & actual ==0) / sum(actual == 0) ## 96.42%

#7.2.4 Test on Test Data
actual <- playstore1.test$Installs
predicted <- ifelse(predict(playstore1.fit1, newdata = playstore1.test, type = "response") > threshold, 1, 0)
## Confucion Matrix
table(actual, predicted)
## Accuracy
sum(actual == predicted)/nrow(playstore1.test) ## 93.46%
## Sensitivity
sum(predicted == 1 & actual ==1) / sum(actual == 1) ## 89.24%
## Specificity
sum(predicted == 0 & actual ==0) / sum(actual == 0) ## 98.19%

#7.4 fit2 with variables rating, reiews, type
playstore1.fit2<- glm (Installs ~  Category + Rating + Reviews + Type, data = playstore1.train, family = "binomial")
summary (playstore1.fit2)

actual <- playstore1.train$Installs
## P Scores from our model
predicted.probability <- predict(playstore1.fit2, type = "response") 
## Define threshold for that we need ROC Curve
## Collect data in data frame
roc.table <- data.frame(Threshold = seq(from = 0, to = 1, by = 0.01),
                        FPR = NA,
                        Sensitivity = NA)
## For Loop
for ( i in 1:nrow(roc.table)) {
  ## Calculate FPR
  roc.table$FPR[i] <- sum(predicted.probability > roc.table$Threshold[i] & actual == 0)/sum(actual ==0)
  
  ## Calculate Sensitivity
  roc.table$Sensitivity[i] <- sum(predicted.probability > roc.table$Threshold[i] & actual == 1)/ sum(actual == 1)
}
roc.table
## Graph ROC Curve
plot(Sensitivity ~ FPR, data = roc.table, type = "s")
## 45 degree line, lty = line type 2 dashed line, 1= solid line
abline(a = 0, b = 1, lty = 2)
## ROC table 0.5 wasnt best but use it for simplicity
threshold <-.5
##Accuracy Calculation
actual <- playstore1.train$Installs
predicted <- ifelse(predicted.probability > threshold, 1, 0)
## Confusion Matrix
table(actual, predicted)

##Calculate Accuracy Train
sum(actual == predicted)/nrow(playstore1.train) ## Accuracy 92.62%
##Sensitivity Train
sum(predicted == 1 & actual ==1) / sum(actual == 1) ## 89.77%
## Specificity Train
sum(predicted == 0 & actual ==0) / sum(actual == 0) ## 96.31%

## Test on Validate Data
actual <- playstore1.validation$Installs
predicted <- ifelse(predict(playstore1.fit2, newdata = playstore1.validation, type = "response") > threshold, 1, 0)
## Confucion Matrix
table(actual, predicted)
## Accuracy
sum(actual == predicted)/nrow(playstore1.validation) ## 93.23%
## Sensitivity
sum(predicted == 1 & actual ==1) / sum(actual == 1) ## 90.62%
## Specificity
sum(predicted == 0 & actual ==0) / sum(actual == 0) ## 96.59%

## Test on Test Data
actual <- playstore1.test$Installs
predicted <- ifelse(predict(playstore1.fit2, newdata = playstore1.test, type = "response") > threshold, 1, 0)
## Confucion Matrix
table(actual, predicted)
## Accuracy
sum(actual == predicted)/nrow(playstore1.test) ## 93.46%
## Sensitivity
sum(predicted == 1 & actual ==1) / sum(actual == 1) ## 89.24%
## Specificity
sum(predicted == 0 & actual ==0) / sum(actual == 0) ## 98.19%

###Threshold 0.7
threshold <-.7
actual <- playstore1.train$Installs
predicted <- ifelse(predicted.probability > threshold, 1, 0)
## Confusion Matrix
table(actual, predicted)

##Calculate Accuracy Train
sum(actual == predicted)/nrow(playstore1.train) ## Accuracy 91.50%
##Sensitivity Train
sum(predicted == 1 & actual ==1) / sum(actual == 1) ## 86.82%
## Specificity Train
sum(predicted == 0 & actual ==0) / sum(actual == 0) ## 97.57%

## Test on Validate Data
actual <- playstore1.validation$Installs
predicted <- ifelse(predict(playstore1.fit2, newdata = playstore1.validation, type = "response") > threshold, 1, 0)
## Confucion Matrix
table(actual, predicted)
## Accuracy
sum(actual == predicted)/nrow(playstore1.validation) ## 92.31%
## Sensitivity
sum(predicted == 1 & actual ==1) / sum(actual == 1) ## 88.08%
## Specificity
sum(predicted == 0 & actual ==0) / sum(actual == 0) ## 97.72%

## Test on Test Data
actual <- playstore1.test$Installs
predicted <- ifelse(predict(playstore1.fit2, newdata = playstore1.test, type = "response") > threshold, 1, 0)
## Confucion Matrix
table(actual, predicted)
## Accuracy
sum(actual == predicted)/nrow(playstore1.test) ## 92.33%
## Sensitivity
sum(predicted == 1 & actual ==1) / sum(actual == 1) ## 86.83%
## Specificity
sum(predicted == 0 & actual ==0) / sum(actual == 0) ## 98.49%

##8 K-nearest neighbors   
library(class)
x.train <- sapply(playstore1.train[,-c(1,6,10,11,12,13)], as.numeric)
y.train <- playstore1.train[,6]
predicted<- knn(x.train, x.train, y.train, 3)
predicted

## Calculate the accuracy
sum(y.train == predicted)/nrow(x.train)
## 94.69% percent accuracy

## Let's find the best value of k to use
## The function knn.cv uses cross-validation
## (Leave-one-out cross validation)
## Argument: x, y, and k
## Loop through k = 1, 2, ..., 10
for (k in 1:10) {
  predicted <- knn.cv(x.train, y.train, k)
  print(paste("With", k, "neighbors the accuracy is", sum(y.train == predicted)/nrow(x.train)))
}
##9 neighbors

## Let's normalize the data and do things over
normalize <- function(numbers) {
  (numbers - mean(numbers))/sd(numbers)
}
## Apply this to our x data
x.normalized <- apply(x.train, 2, normalize)
## Check that it worked
apply(x.normalized, 2, mean)
apply(x.normalized, 2, sd)

## Let's apply knn to the normalized data
#Let's start by finding the optimal value of k

## Loop through k = 1, 2, ..., 10
for (k in 1:10) {
  predicted <- knn.cv(x.normalized, y.train, k)
  print(paste("With", k, "neighbors the accuracy is", sum(y.train == predicted)/nrow(x.normalized)))
}
##10 neighbors

## Let's see how well nearest neighbor does on our testing data
x.test <- sapply(playstore1.test[,-c(1,6,10,11,12,13)], as.numeric)
y.test <- playstore1.test[,6]
## Make predictions
predicted <- knn(x.train, x.test, y.train, 1)
sum(predicted == y.test)/nrow(x.test) ##90.625%

##Normalise test data
x.test.normalized <- apply(x.test, 2, normalize)
predicted <- knn(x.normalized, x.test.normalized, y.train, 1)
sum(y.test == predicted)/nrow(x.test.normalized) ##66.19%

##9. CART #################################
library(rpart)
library(rpart.plot)
playstore1.tree <- rpart(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                         data = playstore1.train, method = "class",
                         control = rpart.control(minsplit=0,cp=0))
##rpart.plot(playstore.tree)
prp(playstore1.tree, faclen = 2)
printcp(playstore1.tree)
cp<-playstore1.tree$cptable[which.min(playstore1.tree$cptable[,"xerror"]),"CP"]
cp
pruned.tree <- prune(playstore1.tree, cp = cp)
library(rattle)
fancyRpartPlot(pruned.tree) ##6 splits
##predict
##predict(pruned.tree, newdata = data.frame(Category= "6", Rating =2.8, Reviews = 4, Size =4.2, Type = "2",Price = 0,
##                                         Content.Rating= "3"))

## Let's see how accurate our tree is-
##Train
actual <- playstore1.train$Installs
predicted <- predict(playstore1.tree, type = "class") ## specify type = "class"
## Confusion matrix
table(actual, predicted)
## Calculate the accuracy
sum(actual == predicted)/nrow(playstore1.train) ##100%
## Sensitivity
sum(predicted == 1 & actual == 1)/sum(actual == 1) ##100%

##Validate
actual <- playstore1.validation$Installs
predicted <- predict(playstore1.tree, type = "class", newdata = playstore1.validation)
## Confusion matrix
table(actual, predicted)
## Accuracy
sum(actual == predicted)/nrow(playstore1.validation) ##92.31%
## Sensitivity
sum(predicted == 1 & actual == 1)/sum(actual == 1) ##94.29%

##Test
actual <- playstore1.test$Installs
predicted <- predict(playstore1.tree, type = "class", newdata = playstore1.test)
## Confusion matrix
table(actual, predicted)
## Accuracy
sum(actual == predicted)/nrow(playstore1.test) ##91.90%
## Sensitivity
sum(predicted == 1 & actual == 1)/sum(actual == 1) ##93.27%

##10. ensemble methods ############################
#10.1 Bagging#########
library (ipred)
str(playstore1.train)
nrow(playstore1.train)
#playstore1.train$Installs <- as.factor(playstore1.train$Installs)
#playstore1.validation$Installs <- as.factor(playstore1.validation$Installs)
#playstore1.test$Installs <- as.factor(playstore1.test$Installs)
bag.model <- bagging(Installs ~ Category + Rating+ Reviews+Size  + Type + Price + Content.Rating ,
                     data = playstore1.train, coob = TRUE)

## Accuracy on Train data
actual <- playstore1.train$Installs
predicted <- predict(bag.model)

sum(actual == predicted)/nrow(playstore1.train) ## Accuracy 93.21%
predicted <- predict(bag.model, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ## Accuracy 99.89%

## Accuracy on validate data
actual <- playstore1.validation$Installs
predicted <- predict(bag.model, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ## 95.02% accuracy on validation

## Accuracy on test data
actual <- playstore1.test$Installs
predicted <- predict(bag.model, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ## 94.74% accuracy on test

## nbagg =5
bag.model.5 <- bagging(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                       data = playstore1.train, nbagg = 5)
## Accuracy on Train
actual <- playstore1.train$Installs
predicted <- predict(bag.model.5, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ## 99.08% accuracy on training

## Accuracy on validate data
actual <- playstore1.validation$Installs
predicted <- predict(bag.model.5, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ##  93.80% accuracy on validation

## Accuracy on test data
actual <- playstore1.test$Installs
predicted <- predict(bag.model.5, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ##  92.75% accuracy on test


## nbagg = 25
bag.model.25 <- bagging(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                        data = playstore1.train, nbagg = 25)
## Accuracy on Train
actual <- playstore1.train$Installs
predicted <- predict(bag.model.25, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ## 99.87% accuracy on training

## Accuracy on validate data
actual <- playstore1.validation$Installs
predicted <- predict(bag.model.25, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ##  94.87% accuracy on validation

## Accuracy on test data
actual <- playstore1.test$Installs
predicted <- predict(bag.model.25, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ##  94.03% accuracy on test


## nbagg = 101
bag.model.101 <- bagging(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                         data = playstore1.train, nbagg = 101)
## Accuracy on Train
actual <- playstore1.train$Installs
predicted <- predict(bag.model.101, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ## 100% accuracy on training

## Accuracy on validate data
actual <- playstore1.validation$Installs
predicted <- predict(bag.model.101, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ##  94.59% accuracy on validation

## Accuracy on test data
actual <- playstore1.test$Installs
predicted <- predict(bag.model.101, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ##  93.89% accuracy on test

## nbagg = 401
bag.model.401 <- bagging(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                         data = playstore1.train, nbagg = 401)
## Accuracy on Train
actual <- playstore1.train$Installs
predicted <- predict(bag.model.401, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ## 100% accuracy on training

## Accuracy on validate data
actual <- playstore1.validation$Installs
predicted <- predict(bag.model.401, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ##  94.51% accuracy on validation

## Accuracy on test data
actual <- playstore1.test$Installs
predicted <- predict(bag.model.401, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ##  94.03% accuracy on test

## nbagg = 1601
bag.model.1601 <- bagging(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                          data = playstore1.train, nbagg = 1601)
## Accuracy on Train
actual <- playstore1.train$Installs
predicted <- predict(bag.model.1601, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ## 100% accuracy on training

## Accuracy on validate data
actual <- playstore1.validation$Installs
predicted <- predict(bag.model.1601, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ##  94.37% accuracy on validation

## Accuracy on test data
actual <- playstore1.test$Installs
predicted <- predict(bag.model.1601, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ##  94.31% accuracy on test

#10.2 Boosting ########################
library(ada)
boost.model <- ada(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating, data = playstore1.train)
##plot.boundary(boost.model, playstore.train)

##Accuray on train
actual <- playstore1.train$Installs
predicted <- predict(boost.model, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ## 95.71%

##Accuray on validation
actual <- playstore1.validation$Installs
predicted <- predict(boost.model, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ## 95.51%

##Accuray on Test
actual <- playstore1.test$Installs
predicted <- predict(boost.model, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ## 94.74%

## iter = 5
boost.model.5 <- ada(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                     data = playstore1.train, iter = 5)

##Accuray on train
actual <- playstore1.train$Installs
predicted <- predict(boost.model.5, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ## 94.06%

##Accuray on validation
actual <- playstore1.validation$Installs
predicted <- predict(boost.model.5, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ## 94.30%

##Accuray on Test
actual <- playstore1.test$Installs
predicted <- predict(boost.model.5, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ## 94.31%

## iter = 25
boost.model.25 <- ada(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                      data = playstore1.train, iter = 25)

##Accuray on train
actual <- playstore1.train$Installs
predicted <- predict(boost.model.25, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ## 94.99%

##Accuray on validation
actual <- playstore1.validation$Installs
predicted <- predict(boost.model.25, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ## 94.73%

##Accuray on Test
actual <- playstore1.test$Installs
predicted <- predict(boost.model.25, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ## 95.02%

## iter = 101
boost.model.101 <- ada(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                       data = playstore1.train, iter = 101)

##Accuray on train
actual <- playstore1.train$Installs
predicted <- predict(boost.model.101, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ## 96.88%

##Accuray on validation
actual <- playstore1.validation$Installs
predicted <- predict(boost.model.101, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ## 95.16%

##Accuray on Test
actual <- playstore1.test$Installs
predicted <- predict(boost.model.101, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ## 94.31%

## iter = 1601
boost.model.1601 <- ada(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                        data = playstore1.train, iter = 1601)

##Accuray on train
actual <- playstore1.train$Installs
predicted <- predict(boost.model.1601, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ## 100%

##Accuray on validation
actual <- playstore1.validation$Installs
predicted <- predict(boost.model.1601, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ## 94.51%

##Accuray on Test
actual <- playstore1.test$Installs
predicted <- predict(boost.model.1601, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ## 94.60%

##10.3 random forest
library(randomForest)
str(playstore1.train)
str(playstore1.validation)
str(playstore1.test)
forest.model <- randomForest(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                             data = playstore1.train)
##varImpPlot(forest.model)
##importance(forest.model)
## Accuracy on train data
actual <- playstore1.train$Installs
predicted <- predict(forest.model)
sum(actual == predicted)/nrow(playstore1.train) ##94.10%

predicted <- predict(forest.model, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ##97.92%

## Accuracy on validation data
actual <- playstore1.validation$Installs
predicted <- predict(forest.model, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ##95.08%

##Accuracy on test data
actual <- playstore1.test$Installs
predicted <- predict(forest.model, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ##95.03%

## ntree= 5
forest.model.5 <- randomForest(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                               data = playstore1.train, ntree = 5)
## Accuracy on train data
actual <- playstore1.train$Installs
predicted <- predict(forest.model.5, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ##96.82%

## Accuracy on validation data
actual <- playstore1.validation$Installs
predicted <- predict(forest.model.5, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ##93.66%

##Accuracy on test data
actual <- playstore1.test$Installs
predicted <- predict(forest.model.5, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ##92.61%

## ntree= 25
forest.model.25 <- randomForest(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                                data = playstore1.train, ntree = 25)
## Accuracy on train data
actual <- playstore1.train$Installs
predicted <- predict(forest.model.25, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ##97.70%

## Accuracy on validation data
actual <- playstore1.validation$Installs
predicted <- predict(forest.model.25, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ##94.66%

##Accuracy on test data
actual <- playstore1.test$Installs
predicted <- predict(forest.model.25, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ##94.60%

## ntree= 101
forest.model.101 <- randomForest(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                                 data = playstore1.train, ntree = 101)
## Accuracy on train data
actual <- playstore1.train$Installs
predicted <- predict(forest.model.101, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ##98.35%

## Accuracy on validation data
actual <- playstore1.validation$Installs
predicted <- predict(forest.model.101, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ##94.94%

##Accuracy on test data
actual <- playstore1.test$Installs
predicted <- predict(forest.model.101, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ##94.74%

## ntree= 1601
forest.model.1601 <- randomForest(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                                  data = playstore1.train, ntree = 1601)
## Accuracy on train data
actual <- playstore1.train$Installs
predicted <- predict(forest.model.1601, newdata = playstore1.train)
sum(actual == predicted)/nrow(playstore1.train) ##98.19%

## Accuracy on validation data
actual <- playstore1.validation$Installs
predicted <- predict(forest.model.1601, newdata = playstore1.validation)
sum(actual == predicted)/nrow(playstore1.validation) ##95.01%

##Accuracy on test data
actual <- playstore1.test$Installs
predicted <- predict(forest.model.1601, newdata = playstore1.test)
sum(actual == predicted)/nrow(playstore1.test) ##95.02%

library(randomForest)

## Logistic Model Fit 2
playstore1.fit1<- glm (Installs ~ Category+ Rating + Reviews + Size + Type + Price+ Content.Rating , data = playstore1.train, family = "binomial")
lr_prediction <- predict(playstore1.fit1, playstore1.test, type = "response")

## RF model
forest.model <- randomForest(Installs ~ Category + Rating + Reviews + Size + Type + Price + Content.Rating,
                             data = playstore1.train)
rf_prediction <- predict(forest.model, playstore1.test, type = 'prob')

# ROC curves
library(pROC)
ROC_rf <- roc(playstore1.test$Installs, rf_prediction[,2])
ROC_lr <- roc(playstore1.test$Installs, lr_prediction)
# Area Under Curve (AUC) for each ROC curve (higher -> better)
ROC_rf_auc <- auc(ROC_rf)
ROC_lr_auc <- auc(ROC_lr)

# plot ROC curves
plot(ROC_rf, col = "green", main = "ROC For Random Forest (GREEN) vs Logistic Regression (RED)")
lines(ROC_lr, col = "red")
# print the performance of each model
paste("Accuracy % of random forest: ", mean(playstore1.test$Installs == round(rf_prediction[,2], digits = 0)))
paste("Accuracy % of logistic regression: ", mean(playstore1.test$Installs == round(lr_prediction, digits = 0)))
paste("Area under curve of random forest: ", ROC_rf_auc)
paste("Area under curve of logistic regression: ", ROC_lr_auc)

##neural network
nn<- newdata
str(nn)
nn <- 



