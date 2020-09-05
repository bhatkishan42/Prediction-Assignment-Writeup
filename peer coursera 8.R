library(caret)
library(ggplot2)
set.seed(123)
atr <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
ate <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
rawdata <- read.csv(atr, na.strings = c("", "NA"))
submit_data<-  read.csv(ate, na.strings = c("", "NA"))
View(rawdata)
#Remove NA cols
colname <- colnames(rawdata)[!colSums(is.na(rawdata)) > 0]
colname
#Slice data related with exercise
colname <- colname[8: length(colname)]
df_wo_NA <- rawdata[colname]
#Check the colnames of df_wo_NA is in submit_data.
#The last colname is "classe"
is.element(colname, colnames(submit_data))
inTrain = createDataPartition(df_wo_NA$classe, p = 3/4)[[1]]
training = df_wo_NA[ inTrain,]
testing = df_wo_NA[-inTrain,]
model_rf <- train(classe ~ ., data = training, method = "rf")
pred_rf <- predict(model_rf, testing)
confusionMatrix(testing$classe, pred_rf)
model_lda <- train(classe ~ ., data = training, method = "lda")
pred_lda <- predict(model_lda, testing)
confusionMatrix(testing$classe, pred_lda)
odel_rpart <- train(classe ~ ., data = training, method = "rpart")
pred_rpart<- predict(model_rpart, testing)
confusionMatrix(testing$classe, pred_rpart)
library(rattle)
fancyRpartPlot(model_rpart$finalModel)
submit_rf <- predict(model_rf, submit_data)
submit_rf










