### Practical Machine-Learning Assessment:


We first load the package "caret" that will be used for data preparation and modeling
```
> library(caret)
```

Let's now load the data, supposed to be in the working directory
```
>train_csv<-read.csv("./pml-training.csv")
>test_csv <-read.csv("./pml-testing.csv")
```

It is always a good idea to inspect the data
```
>dim(train_csv); dim(test_csv)
>str(train_csv); str(test_csv)
```

Now it is time to clean the data

First removing all variables with only NA data
```
>train_noNA <- train_csv[,colSums(is.na(train_csv))==0]
>test_noNA  <- test_csv[,colSums(is.na(test_csv))==0]
```
and secondly removing variables that are not movement-related
(we secure the variable "classe" to be restored afterwards)
```
>classe <- train_csv$classe                                  #keep "classe"
>remove <- grepl("^X|timestamp|window", names(train_noNA))   #select variables
>train_noNA <- train_noNA[, !remove]
>train_final <- train_noNA[, sapply(train_noNA, is.numeric)]
>train_final$classe <- classe
```

We must do the same for the test data
```
>remove <- grepl("^X|timestamp|window", names(test_noNA))
>test_noNA <- test_noNA[, !remove]
>test_final <- test_noNA[, sapply(test_noNA, is.numeric)]
```
Let's check dimensions now of the data
```
>dim(train_final);dim(test_final)
```

Now we slice the data
```
>inTrain <- createDataPartition(train_final$classe, p=0.70, list=F)
>train <- train_final[inTrain, ]
>test  <- train_final[-inTrain, ]
```

And we can now build the random-forest model and the cross-validation (10-fold)
```
>control <- trainControl(method="cv", 10) 
>model_rf <- train(classe ~ ., data=train, method="rf", trControl=control, ntree=250)
>model_rf
```

The model predictions and their accuracy are
```
>predict_rf<-predict(model_rf,test)
>confusionMatrix(test$classe,predict_rf)
>accuracy <- postResample(predict_rf, test$classe)
>accuracy
>oose <- 1 - as.numeric(confusionMatrix(test$classe, predict_rf)$overall[1])
>oose
```
Finally, the results:
```
>result <- predict(model_rf, test_final[, -length(names(test_final))])
>result
[1] B A B A A E D B A A B C B A E E A B B B
Levels: A B C D E
```
