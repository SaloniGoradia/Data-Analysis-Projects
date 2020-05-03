##Install Packages
install.packages("rmarkdown")
install.packages("interp")
install.packages("zoo")
install.packages("dplyr")
install.packages("DataExplorer")
install.packages("ggplot2")
install.packages("plot3D")
install.packages("tidyverse")
install.packages("GGally")
install.packages("gganimate")
install.packages("plotly")
install.packages("maps")
install.packages("ggthemes")
install.packages("statebins")
install.packages("openintro")
install.packages("glmnet")
install.packages("Matrix")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("randomForest")

##Load Libraries
library(rmarkdown)
library(interp)
library(zoo)
library(dplyr)
library(DataExplorer)
library(ggplot2)
library(plot3D)
library(tidyverse)
library(GGally)
library(gganimate)
library(plotly)
library(maps)
library(ggthemes)
library(statebins)
library(openintro)
library(glmnet)
library(Matrix)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)

##Set working directory
setwd("/Users/salonigoradia/Desktop/SJSU/Sem4- Sp20/235-A/Project/us-education-datasets-unification-project")

#Read the .csv file
States<- read.csv("states_all.csv")
dim(States)
sapply(States,class)
colSums(is.na(States))
introduce(States)
plot_missing(States)
summary(States)

#dataframe of last 15 years
States_15years<-States[States$YEAR >2001 &States$YEAR <= 2016, ]
dim(States_15years)
colSums(is.na(States_15years))
introduce(States_15years)
plot_missing(States_15years)
summary(States_15years)
attach(States_15years)

############ Data Cleaning #############
#remove "_" from state names
States_15years$STATE <- gsub("\\_", " ",States_15years$STATE)
nrow(distinct(States_15years,STATE))

#Year wise missing values
MissCount<- rowSums(is.na(States_15years))
MissingValues_Year<- tapply( MissCount, States_15years$YEAR, sum)
MissingValues_Year

#State wise missing values
MissingValues_State <- tapply(MissCount, States_15years$STATE, sum)
MissingValues_State

## Missing Values - Replaced with interpolation

#GRADES_PK_G
States_15years$GRADES_PK_G <- na.approx(States_15years$GRADES_PK_G, rule = 2)

## AVG_MATH_4_SCORE
States_15years$AVG_MATH_4_SCORE  <- na.approx(States_15years$AVG_MATH_4_SCORE, rule = 2)

## AVG_MATH_8_SCORE
States_15years$AVG_MATH_8_SCORE  <- na.approx(States_15years$AVG_MATH_8_SCORE, rule = 2)

## AVG_READING_4_SCORE
States_15years$AVG_READING_4_SCORE  <- na.approx(States_15years$AVG_READING_4_SCORE, rule = 2)

## AVG_READING_8_SCORE
States_15years$AVG_READING_8_SCORE  <- na.approx(States_15years$AVG_READING_8_SCORE, rule = 2)

summary(States_15years)
dim(States_15years)
colSums(is.na(States_15years))

## Transform the Columns
## Revenue Per Pupil
States_15years$Total_Revenue_Per_Pupil <- ( States_15years$TOTAL_REVENUE  /   States_15years$GRADES_ALL_G )

## Expenditure Per Pupil
States_15years$Total_Expenditure_Per_Pupil <- ( States_15years$TOTAL_EXPENDITURE  / States_15years$  GRADES_ALL_G )

## Average Math 4 Score- Average Reading 4 Score → Average Score_4G
States_15years$Avg_4_G <- ( States_15years$AVG_MATH_4_SCORE  + States_15years$ AVG_READING_4_SCORE )/ 2

## Average Math 8 Score - Average Reading 8 Score → Average Score_8G
States_15years$Avg_8_G <- ( States_15years$AVG_MATH_8_SCORE  +  States_15years$AVG_READING_8_SCORE )/ 2

## Average Reading 4+8 Score → Average Reading Score
States_15years$Avg_Reading_Score <- ( States_15years$AVG_READING_4_SCORE  +  States_15years$AVG_READING_8_SCORE )/ 2

## Average Math 4+8 Score → Average Reading Score
States_15years$Avg_Math_Score <- ( States_15years$AVG_MATH_4_SCORE  +  States_15years$AVG_MATH_8_SCORE )/ 2

###### Visualize Columns #############

##STATE
State_Frequency <- table(States_15years$STATE)
State_Frequency

##YEAR
Year_Frequency <- table(States_15years$YEAR)
Year_Frequency

## GRADES_ALL_G
States_15years$GRADES_ALL_G  <- States_15years$GRADES_ALL_G /1000000
p.grades <- States_15years %>%
  ggplot( aes(x=GRADES_ALL_G)) +
  geom_histogram( binwidth=0.5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Total number of students in all grades in million") 
p.grades+ labs(x= "Number of Students in million")

#Total_Revenue_Per_Pupil
p.revenue <- States_15years %>%
  ggplot( aes(x=Total_Revenue_Per_Pupil)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Total Revenue per Pupil") 
p.revenue + labs(x= "Total Revenue per pupil")
summary(States_15years$Total_Revenue_Per_Pupil)

#Total_Expenditure_Per_Pupil
p.expenditure <- States_15years %>%
  ggplot( aes(x=Total_Expenditure_Per_Pupil)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Total Expenditure per Pupil") 
p.expenditure + labs(x= "Total Expenditure per pupil")
summary(States_15years$Total_Expenditure_Per_Pupil)

#Grade - 4G
States_15years$GRADES_4_G  <- States_15years$GRADES_4_G /100000
p.4g <- States_15years %>%
  ggplot( aes(x=GRADES_4_G)) +
  geom_histogram( binwidth=0.5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Number of Students in Grade 4") 
p.4g + labs(x= "Number of Students in hundred thousands")

#Grade - 8G
States_15years$GRADES_8_G  <- States_15years$GRADES_8_G /100000
p.8g <- States_15years %>%
  ggplot( aes(x=GRADES_8_G)) +
  geom_histogram( binwidth=0.5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Number of Students in Grade 8") 
p.8g + labs(x= "Number of Students in hundred thousands")

#Average Math 4 Score- Average Reading 4 Score → Average Score_4G
p.4s<- States_15years %>%
  ggplot( aes(x=Avg_4_G)) +
  geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Average Maths and Reading score in Grade 4") 
p.4s + labs(x = "Average Maths & Reading Score")
summary(States_15years$Avg_4_G)

#Average Math 8 Score - Average Reading 8 Score → Average Score_8G
p.8s<- States_15years %>%
  ggplot( aes(x=States_15years$Avg_8_G)) +
  geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Average Math and Reading score in Grade 8") 
p.8s + labs(x = "Average Maths & Reading Score")
summary(States_15years$Avg_8_G)

#Average Maths 4+8  Score → Average Maths Score
p.maths<- States_15years %>%
  ggplot( aes(x=States_15years$Avg_Math_Score)) +
  geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Average Maths score in Grade 4 and 8") 
p.maths+ labs(x = "Average Maths Score")
summary(States_15years$Avg_Math_Score)

#Average Reading 4+8 Score → Average Reading Score
p.reading<- States_15years %>%
  ggplot( aes(x=States_15years$Avg_Reading_Score)) +
  geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Average Reading score in Grade 4 and 8") 
p.reading+ labs(x = "Average Reading Score")
summary(States_15years$Avg_Reading_Score)

###############pairwise comparision ####################
par(mfrow = c(1,1))

#Expenditure per pupil vs Year
ggplot(States_15years, aes(x=as.factor(YEAR), y=Total_Expenditure_Per_Pupil)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Year")+ ylab("Total Expenditure per Pupil")+ggtitle("Total Expenditure per Pupil Vs Year")

#Revenue per pupil vs Year
ggplot(States_15years, aes(x=as.factor(YEAR), y=Total_Revenue_Per_Pupil)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Year")+ ylab("Total Revenue per Pupil")+ggtitle("Total Revenue per Pupil Vs Year")

States_15years$abrState <- state2abbr(States_15years$STATE)
States_15years$abrState

#Grades_All_G Vs State
class(States_15years$abrState)

States_15years %>%
  mutate(statename = States_15years$abrState) %>%
  mutate(enroll = States_15years$GRADES_ALL_G) %>%
  statebins_continuous(state_data = ., state_col = "statename",
                       text_color = "white", value_col = "enroll", legend_title = "Enrollment in millions",
                       brewer_pal="Spectral")

## Maths Vs States
States_15years %>%
  mutate(statename = States_15years$abrState) %>%
  mutate(avg_maths_score = Avg_Math_Score ) %>%
  statebins_continuous(state_data = ., state_col = "statename",
                       text_color = "Black", value_col = "avg_maths_score", legend_title = "Maths Score",
                       brewer_pal="Spectral")

## Reading Vs States
States_15years %>%
  mutate(statename = States_15years$abrState) %>%
  mutate(avg_reading_score = Avg_Reading_Score ) %>%
  statebins_continuous(state_data = ., state_col = "statename",
                       text_color = "Black", value_col = "avg_reading_score", legend_title = "Reading Score",
                       brewer_pal="Spectral")


## Avg_4_G Vs States
States_15years %>%
  mutate(statename = States_15years$abrState) %>%
  mutate(avg_4g_score = Avg_4_G ) %>%
  statebins_continuous(state_data = ., state_col = "statename",
                       text_color = "Black", value_col = "avg_4g_score", legend_title = "Score in Grade 4",
                       brewer_pal="Spectral")

## Avg_8_G Vs States
States_15years %>%
  mutate(statename = States_15years$abrState) %>%
  mutate(avg_8g_score = Avg_8_G ) %>%
  statebins_continuous(state_data = ., state_col = "statename",
                       text_color = "Black", value_col = "avg_8g_score", legend_title = "Score in Grade 8",
                       brewer_pal="Spectral")

#########################Linear Regression###############################
#Split data into training and testing data
States_model<- States_15years
set.seed(42)
test.rows <- sample(1:nrow(States_model), size=0.20*nrow(States_model))
States_train <- States_model[-test.rows, ]
States_test <- States_model[test.rows, ]
nrow(States_train)
nrow(States_test)

###Y = AVG_MATH_4_SCORE

Fit1 <- lm(AVG_MATH_4_SCORE ~  STATE + YEAR + GRADES_4_G + TOTAL_REVENUE + FEDERAL_REVENUE +
             STATE_REVENUE + LOCAL_REVENUE + TOTAL_EXPENDITURE + INSTRUCTION_EXPENDITURE +
             SUPPORT_SERVICES_EXPENDITURE + OTHER_EXPENDITURE + CAPITAL_OUTLAY_EXPENDITURE +
             GRADES_PK_G + GRADES_KG_G + GRADES_4_G  + GRADES_8_G  + GRADES_12_G + GRADES_1_8_G +
             GRADES_9_12_G + GRADES_ALL_G  + AVG_READING_4_SCORE + AVG_MATH_8_SCORE + AVG_READING_8_SCORE,
           data =  States_train)

summary(Fit1)

##Significant variables
Fit2 <- lm(AVG_MATH_4_SCORE ~  STATE + YEAR + TOTAL_REVENUE +  AVG_READING_4_SCORE + GRADES_4_G+ 
             AVG_MATH_8_SCORE, data =  States_train)
summary(Fit2)

#Correlation for fit2
cor(States_train[,c(3,5,24,16,23)])
pairs(States_train[,c(3,5,24,16,23)])
#ggpairs(States_train, columns = c(3,5,24,16,23), title = "",  
 #       axisLabels = "show", columnLabels = colnames(States_train[, c(3,5,24,16,23)]))


#Removing AVG_MATH_8_SCORE in Fit3
Fit3 <- lm(AVG_MATH_4_SCORE ~  STATE + YEAR  + TOTAL_REVENUE +AVG_READING_4_SCORE ,
           data =  States_train)
summary(Fit3)

#Removing TOTAL_REVENUE
Fit4<- lm(AVG_MATH_4_SCORE ~  STATE + YEAR +AVG_READING_4_SCORE, data =  States_train)
summary(Fit4)

#### Fit5
Fit5<-lm(AVG_MATH_4_SCORE ~  STATE + YEAR   + FEDERAL_REVENUE +
           STATE_REVENUE + LOCAL_REVENUE  + INSTRUCTION_EXPENDITURE +
           SUPPORT_SERVICES_EXPENDITURE + OTHER_EXPENDITURE + CAPITAL_OUTLAY_EXPENDITURE +
           GRADES_PK_G + GRADES_KG_G + GRADES_4_G  + GRADES_8_G  + GRADES_12_G,
         data =  States_train)
summary(Fit5)

###### Fit6
Fit6<- lm(AVG_MATH_4_SCORE ~  STATE + YEAR + Total_Revenue_Per_Pupil + 
            Total_Expenditure_Per_Pupil +GRADES_ALL_G+ AVG_MATH_8_SCORE + AVG_READING_8_SCORE,
          data =  States_train)
summary(Fit6)
cor(States_train[,c(3,26,27,21,23,25)])

### Final model: Fit7
Fit7<-lm(AVG_MATH_4_SCORE ~  STATE + YEAR  + Total_Expenditure_Per_Pupil + 
           AVG_MATH_8_SCORE + AVG_READING_8_SCORE,
         data =  States_train)
summary(Fit7)

cor(States_train[,c(3,27,21,23,25)])

###Fitted values plot
par(mfrow=c(1,1))
plot(x= States_train$AVG_MATH_4_SCORE, y= Fit7$fitted.values, ylab = "Fitted Values", xlab ="Actual Values"
     , main = "Fitted Vs Actual Values", xlim = c(200,260), ylim = c(200,260))
abline(0,1)

#Residual Diagnostics
#Studentized Residuals
plot(density(rstudent(Fit7)),main="Studentized Residuals")

## Plot Residuals against fitted values
plot(Fit7$residuals ~ Fit7$fitted.values,
     xlab = "Fitted Values", ylab = "Residuals", main = "Residuals against Fitted Values")

## Plot Residuals against log of fitted values
plot(Fit7$residuals ~ log(Fit7$fitted.values),
     xlab = "log(Fitted Values)", ylab = "Residuals", main = "Residuals against
                                                              Fitted Values")

## Residuals against each x variable
par(mfrow = c(2,2))

## Residuals against YEAR
plot(Fit7$residuals ~ States_train$YEAR,
     xlab = "YEAR", ylab = "Residuals", main = "Residuals Vs YEAR")

## Residuals against AVG_READING_8_SCORE
plot(Fit7$residuals ~ States_train$AVG_READING_8_SCORE,
     xlab = "AVG_READING_8_SCORE", ylab = "Residuals", 
     main = "Residuals Vs 
             AVG_READING_8_SCORE")

## Residuals against AVG_MATH_8_SCORE
plot(Fit7$residuals ~ States_train$AVG_MATH_8_SCORE,
     xlab = "AVG_MATH_8_SCORE", ylab = "Residuals", 
     main = "Residuals Vs 
             AVG_MATH_8_SCORE")

## Residuals against Total_Expenditure_Per_Pupil
plot(Fit7$residuals ~ States_train$Total_Expenditure_Per_Pupil,
     xlab = "Total_Expenditure_Per_Pupil", ylab = "Residuals", 
     main = "Residuals Vs 
          Total_Expenditure_Per_Pupil")

##cook's distance
par(mfrow=c(1,1))
cook1<-cooks.distance(Fit7)
cook1
plot(cook1, pch="*", cex=2, main="Influential Observation : Cooks distance")  # plot cook's distance
abline(h = 0.02, col="red")  # add cutoff line
# add labels
text(x=1:length(cook1)+1, y=cook1, labels = ifelse(cook1 > 0.02, names(cook1), ""),pos = 4, col="red")  
points<- which(cook1>0.020)
points
States_train[c(314), ]
States_train1<- States_train[-c(314), ]
##outlierTest
car::outlierTest(Fit7)
##After removing influential point
Fit8<-lm(AVG_MATH_4_SCORE ~  STATE + YEAR  + 
           Total_Expenditure_Per_Pupil +GRADES_ALL_G+ AVG_MATH_8_SCORE + AVG_READING_8_SCORE,
         data =  States_train1)
summary(Fit8)

##Comparing plots of fit7 and fit8
par(mfrow=c(1,2))
plot(x= States_train$AVG_MATH_4_SCORE, y= Fit7$fitted.values, ylab = "Fitted Values", xlab ="Actual Values"
     , main = "Fitted Vs Actual Values
               with Influential
               points", xlim = c(200,260), ylim = c(200,260))
abline(0,1)

plot(x= States_train1$AVG_MATH_4_SCORE, y= Fit8$fitted.values, ylab = "Fitted Values", xlab ="Actual Values"
     , main = "Fitted Vs Actual Values
               without Influential 
               points", xlim = c(200,260), ylim = c(200,260))
abline(0,1)

#####Step function
stepmodel<- step(Fit1)
names(States_train)
cor(States_train[,c(15,14,5,11,18,21,16,3,24,23)])

### Impact of coefficients
attach((States_train))
sd(YEAR)
sd(YEAR)*Fit7$coefficients["YEAR"]

sd(States_train$Total_Expenditure_Per_Pupil)
sd(States_train$Total_Expenditure_Per_Pupil)*Fit7$coefficients["Total_Expenditure_Per_Pupil"]

sd(AVG_MATH_8_SCORE)
sd(AVG_MATH_8_SCORE)*Fit7$coefficients["AVG_MATH_8_SCORE"]

sd(AVG_READING_8_SCORE)
sd(AVG_READING_8_SCORE)*Fit7$coefficients["AVG_READING_8_SCORE"]

##Prediction on train - Linear model
avg_math_4_score_Pred_train <- predict(Fit7, States_train)  # predict AVG Math 4 Score
actuals_preds <- data.frame(cbind(actuals = States_train$AVG_MATH_4_SCORE, 
                                  predicteds = avg_math_4_score_Pred_train))  

# make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
head(actuals_preds)
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

#Prediction on test- linear model
avg_math_4_score_Pred_test <- predict(Fit7, States_test)  # predict AVG Math 4 Score
actuals_preds <- data.frame(cbind(actuals = States_test$AVG_MATH_4_SCORE, predicteds = avg_math_4_score_Pred_test))  

# make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
head(actuals_preds)

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

###########RMSE: Fit7
##RMSE training
predictions.train <- predict(Fit7, newdata=States_train)
sqrt(mean((predictions.train - States_train$AVG_MATH_4_SCORE)^2))	

#RMSE testing
predictions.test <- predict(Fit7, newdata=States_test)
sqrt(mean((predictions.test - States_test$AVG_MATH_4_SCORE)^2))	

######lasso model

x_vars <- model.matrix(AVG_MATH_4_SCORE~ STATE + YEAR + GRADES_4_G + TOTAL_REVENUE + FEDERAL_REVENUE +
                         STATE_REVENUE + LOCAL_REVENUE + TOTAL_EXPENDITURE + INSTRUCTION_EXPENDITURE +
                         SUPPORT_SERVICES_EXPENDITURE + OTHER_EXPENDITURE + CAPITAL_OUTLAY_EXPENDITURE +
                         GRADES_PK_G + GRADES_KG_G + GRADES_4_G  + GRADES_8_G  + GRADES_12_G + 
                         GRADES_1_8_G +
                         GRADES_9_12_G + GRADES_ALL_G  + AVG_READING_4_SCORE + AVG_MATH_8_SCORE + AVG_READING_8_SCORE, 
                       States_train)

y_var <- States_train$AVG_MATH_4_SCORE
lambda_seq <- 10^seq(2, -2, by = -.1)

# Splitting the data into test and train
set.seed(42)
train = sample(1:nrow(x_vars), nrow(x_vars)/2)
test = (-train)
y_test = y_var[test]
cv_output <- cv.glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = lambda_seq)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam
lasso.mod <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = lambda_seq)
lasso.pred <- predict(lasso.mod, s = best_lam, newx = x_vars[test,])
mean((lasso.pred-y_test)^2)

lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = best_lam)
lasso.coef 
## Significant variables: STATE,YEAR, AVG_READING_4_SCORE, AVG_MATH_8_SCORE, AVG_READING_8_SCORE

#################### Logistic Regression ########################

## Train data split
State.train.Above236 <- cut(States_train$AVG_MATH_4_SCORE, breaks = c(0, 236, Inf),
                            labels = c("0","1"))
States_train$AVG_MATH_4_SCORE <- as.character(State.train.Above236)
States_train$AVG_MATH_4_SCORE <- as.factor(State.train.Above236)
sapply(States_train, class)

## Test data split
State.test.Above236 <- cut(States_test$AVG_MATH_4_SCORE, breaks = c(0, 236, Inf),
                           labels = c("0","1"))
States_test$AVG_MATH_4_SCORE <- as.character(State.test.Above236)
States_test$AVG_MATH_4_SCORE <- as.factor(State.test.Above236)
sapply(States_test, class)

## Model Fit
log.model <- glm (AVG_MATH_4_SCORE ~  STATE + YEAR  + Total_Expenditure_Per_Pupil + 
                    AVG_MATH_8_SCORE + AVG_READING_8_SCORE,
                  data =  States_train, family = "binomial")
summary (log.model)

actual <- States_train$AVG_MATH_4_SCORE
predicted.probability <- predict(log.model, type = "response")

roc.table <- data.frame(Threshold = seq(from = 0, to = 1, by = 0.01),
                        FPR = NA, Sensitivity = NA)

for ( i in 1:nrow(roc.table)) {
  ## Calculate FPR
  roc.table$FPR[i] <- sum(predicted.probability > roc.table$Threshold[i] & actual == 0) / sum(actual == 0)
  ## Calculate Sensitivity
  roc.table$Sensitivity[i] <- sum(predicted.probability > roc.table$Threshold[i] & actual == 1)/ sum(actual == 1)
}
roc.table
par(mfrow=c(1,1))
plot(Sensitivity ~ FPR, data = roc.table, type = "s")
## 45 degree line, lty = line type 2 dashed line, 1= solid line
abline(a = 0, b = 1, lty = 2)

###Threshold 0.5
threshold <-0.5
actual <- States_train$AVG_MATH_4_SCORE
predicted <- ifelse(predicted.probability > threshold, 1, 0) ## Confusion Matrix
table(actual, predicted)

## Accuracy on Train Data
sum(actual == predicted)/nrow(States_train)
##Sensitivity Train
sum(predicted == 1 & actual == 1) / sum(actual == 1)
## Specificity Train
sum(predicted == 0 & actual == 0) / sum(actual == 0)

##Accuracy on Test Data
actual <- States_test$AVG_MATH_4_SCORE
predicted <- ifelse(predict(log.model, newdata = States_test, type = "response") > threshold, 1, 0)
##Confusion Matrix
table(actual, predicted)
##Accuracy
sum(actual == predicted)/nrow(States_test) 
##Sensitivity
sum(predicted == 1 & actual ==1) / sum(actual == 1) 
##Specificity
sum(predicted == 0 & actual ==0) / sum(actual == 0) 

##################################Accuracy of Linear regression####################
##Train
avg_math_4_score_Pred_train
Factor_avg_math_4_score_Pred_train <- cut(avg_math_4_score_Pred_train, breaks = c(0, 236, Inf),
                                          labels = c("0","1"))

actual <- States_train$AVG_MATH_4_SCORE
predicted <- Factor_avg_math_4_score_Pred_train 
table(actual, predicted)

## Accuracy on Train Data
sum(actual == predicted)/nrow(States_train)
##Sensitivity Train
sum(predicted == 1 & actual == 1) / sum(actual == 1)
## Specificity Train
sum(predicted == 0 & actual == 0) / sum(actual == 0)

###Test
avg_math_4_score_Pred_test
Factor_avg_math_4_score_Pred_test <- cut(avg_math_4_score_Pred_test, breaks = c(0, 236, Inf),
                                         labels = c("0","1"))

actual <- States_test$AVG_MATH_4_SCORE
predicted <- Factor_avg_math_4_score_Pred_test
table(actual, predicted)
## Accuracy
sum(actual == predicted)/nrow(States_test) 
## Sensitivity
sum(predicted == 1 & actual ==1) / sum(actual == 1)
## Specificity
sum(predicted == 0 & actual ==0) / sum(actual == 0) ##

#########################Classification Tree###################################
States.tree<- rpart(AVG_MATH_4_SCORE ~  STATE + YEAR  + Total_Expenditure_Per_Pupil + 
                      AVG_MATH_8_SCORE + AVG_READING_8_SCORE,
                    data =  States_train, method = "class", 
                    control = rpart.control(minsplit=0,cp=0))
prp(States.tree, faclen = 2)
printcp(States.tree)
cp<-States.tree$cptable[which.min(States.tree$cptable[,"xerror"]),"CP"]
cp
pruned.tree <- prune(States.tree, cp = 0.010)
fancyRpartPlot(pruned.tree)

###########################Prediction on linear and logistic ########################
##Prediction on linear model
predict(Fit7, data.frame(STATE= "CALIFORNIA", YEAR= 2015, Total_Expenditure_Per_Pupil= 9,
                         AVG_MATH_8_SCORE= 270, AVG_READING_8_SCORE=260))

predict(log.model, data.frame(STATE= "CALIFORNIA", YEAR= 2015, Total_Expenditure_Per_Pupil= 9,
                              AVG_MATH_8_SCORE= 270, AVG_READING_8_SCORE=260), type = "response")


