library(tree)
library(randomForest)
#library(ggplot2)
load('samsungData.rda')

missClass = function(values, prediction){
  sum((prediction != values) * 1 / length(values))
}

# reformat names and cure duplicates, activity as factor
names(samsungData) <- gsub('[[:punct:]]', "_", names(samsungData))
dyn_vars <- dim(samsungData)[2]-2
suff <- paste0("_v", 1:dyn_vars)
names(samsungData)[1:dyn_vars] <- paste0(names(samsungData)[1:dyn_vars], suff)
samsungData$activity <- as.factor(samsungData$activity)

# split data in train, test and validate sets
trainData <- subset(samsungData, subject==1 | subject==3 | subject==5 | subject==6)
testData <- subset(samsungData, subject==27 | subject==28 | subject==29 | subject==30)

# decision tree prediction
library(plyr)
samsung_tree <- tree(activity ~ ., data=trainData)
tree_preds <- predict(samsung_tree, testData)
activ_labels <- c("laying", "sitting", "standing", "walk", "walkdown", "walkup")
best_act <- aaply(tree_preds, 1, which.max)
predicted_activ <- activ_labels[best_act]
missClass(predicted_activ, testData$activity)

# random forest prediction
samsung_rf <- randomForest(activity ~ ., data=trainData, importance=TRUE,
                           proximity=TRUE, do.trace=TRUE)
preds <- predict(rf, testData)
missClass(preds, testData$activity)
MDSplot(rf, trainData$activity)