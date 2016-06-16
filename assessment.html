library(caret)
#load data
train_csv<-read.csv("./pml-training.csv")
test_csv <-read.csv("./pml-testing.csv")
#inspect data
dim(train_csv); dim(test_csv)
# str(train_csv); str(test_csv)
#clean data
##remove variables with only NA data
train_noNA <- train_csv[,colSums(is.na(train_csv))==0]
test_noNA  <- test_csv[,colSums(is.na(test_csv))==0]
##remove variables not movement related
# keep classe 
classe <- train_csv$classe
remove <- grepl("^X|timestamp|window", names(train_noNA))
train_noNA <- train_noNA[, !remove]
train_final <- train_noNA[, sapply(train_noNA, is.numeric)]
train_final$classe <- classe
# same for test data
remove <- grepl("^X|timestamp|window", names(test_noNA))
test_noNA <- test_noNA[, !remove]
test_final <- test_noNA[, sapply(test_noNA, is.numeric)]
# check dimensions
dim(train_final);dim(test_final)
# slice the data
# you can include a set.seed() for reproducibile purposes
inTrain <- createDataPartition(train_final$classe, p=0.70, list=F)
train <- train_final[inTrain, ]
test  <- train_final[-inTrain, ]
# model and cross-validation (10-fold)
control <- trainControl(method="cv", 10) 
model_rf <- train(classe ~ ., data=train, method="rf", trControl=control, ntree=250)
model_rf
# model predictions
predict_rf<-predict(model_rf,test)
confusionMatrix(test$classe,predict_rf)
accuracy <- postResample(predict_rf, test$classe)
accuracy
oose <- 1 - as.numeric(confusionMatrix(test$classe, predict_rf)$overall[1])
oose
result <- predict(model_rf, test_final[, -length(names(test_final))])
result

#[1] B A B A A E D B A A B C B A E E A B B B
#Levels: A B C D E

