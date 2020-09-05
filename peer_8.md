---
title: "Prediction Assignment Writeup"
author: "kishan bhat"
---

# Overview
The goal of this project is to predict the manner in which they did the exercise. This is the “classe” variable in the training set. This report describes how data was cleaned, how I split “pml-training.csv” into train set and test set, and some of models are investigated.

# Exercise

# 1. Loading add-on package and set seed
```{r echo=FALSE}
set.seed(12345)
library(caret)
```
# 2. Download rawdata and submit_data
```{r echo=FALSE}
atr <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
ate <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
rawdata <- read.csv(atr, na.strings = c("", "NA"))
submit_data<-  read.csv(ate, na.strings = c("", "NA"))
View(rawdata)
View(submit_data)
```
# 3. Cleaning data
There are lot of  columns where there are lots of irrelevant data, we need to clean that data first. 
lets start with deleting those columns,intotal there are 60 columns 
```{r echo=TRUE}
#Remove NA cols
colname <- colnames(rawdata)[!colSums(is.na(rawdata)) > 0]
colname
```

```{r echo=TRUE}
#Slice data related with exercise
colname <- colname[8: length(colname)]
df_wo_NA <- rawdata[colname]
#Check the colnames of df_wo_NA is in submit_data.
#The last colname is "classe"
is.element(colname, colnames(submit_data))
```
# 4. Split data into random train and test
```{r echo=TRUE}
inTrain = createDataPartition(df_wo_NA$classe, p = 3/4)[[1]]
training = df_wo_NA[ inTrain,]
testing = df_wo_NA[-inTrain,]
```
# 5. Random Forest
It takes a very long time for training, but it has a high accuracy.
```{r echo=TRUE}
model_rf <- train(classe ~ ., data = training, method = "rf")
pred_rf <- predict(model_rf, testing)
confusionMatrix(testing$classe, pred_rf)
```

# 6. Liner Discriminant Analysis
It takes a short time but poor accuracy.
```{r echo=TRUE}
model_lda <- train(classe ~ ., data = training, method = "lda")
pred_lda <- predict(model_lda, testing)
confusionMatrix(testing$classe, pred_lda)
```

# 7. Recursive Partitioning and Regression Trees
The results can be confirmed visually, but poor accuracy.
```{r echo=TRUE}
model_rpart <- train(classe ~ ., data = training, method = "rpart")
pred_rpart<- predict(model_rpart, testing)
confusionMatrix(testing$classe, pred_rpart)
```

```{r echo=TRUE}
library(rattle)
fancyRpartPlot(model_rpart$finalModel)
```



# 8. Submit data with Random Forest
We can use the high accuracy model to submit data. In this report the Random Forest accuracy has the highest value 99.45. We can show the prediction.
```{r echo=TRUE}
submit_rf <- predict(model_rf, submit_data)
submit_rf
```




