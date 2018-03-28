library(dplyr)
library(caret)

setwd("C:/Users/admin/Desktop/R/My NMAP Project/Datasets")


# read both datasets - 9pm and 11pm
# NOTE - now, I'm gonna use only the combi dataset
elevenDf <- read.csv("elevenPmDataset.csv", stringsAsFactors = FALSE)
nineDf <- read.csv("ninePmDataset.csv", stringsAsFactors = FALSE)
combi <- rbind(nineDf, elevenDf)


# Remember, Day is an engineered feature.

# 1. Convert to Binary Classification Problem.

combi$FreeInd <- 1
combi$FreeInd[combi$Status != "Free"] <- 0
# nineDf <- combi[c(1:6096), ]
# elevenDf <- combi[c(6097:16764), ]

# check nineDf and elevenDf - to see if unnecessary levels added
table(nineDf$Date)
# Yes, all the levels are there. One lesson - Convert to factor only right before modeling. Keep 
# as character in all the other cases. -- stringsAsFactors added to read.csv()
# Now, only the required levels.

# Now - Converted to Binary Classification Problem.



# 2. Check if data is skewed.
# table(as.factor(nineDf$FreeInd))    # 0 - 2050, 1 - 4046
# table(as.factor(elevenDf$FreeInd))  # 0 - 4083, 1 - 6585
table(as.factor(combi$FreeInd))     # 0 - 6133, 1 - 10631

# So, not skewed or unbalanced at all! Accuracy can be a fine metric here.



# 3. Building a model, training and prediction
# Can try multiple models, example - LR, RF, etc. using caret.

str(nineDf)
toFactor <- c("Day", "Status", "FreeInd")
# nineDf[toFactor] <- lapply(nineDf[toFactor], as.factor)
# elevenDf[toFactor] <- lapply(elevenDf[toFactor], as.factor)
combi[toFactor] <- lapply(combi[toFactor], as.factor)

set.seed(234)
# I will use DateOnly as a predictor only in combi. NO
tr <- trainControl(method = "cv", number = 10, verboseIter = TRUE)


# 1. Logistic Regression
lrmodel_combi <- train(x = combi[, c(1, 3, 6)], y = combi$FreeInd, trControl = tr, 
                       method = "glm", family = "binomial")

print(lrmodel_combi) # 0.6341566

# 2. LDA (Linear Discriminant Analysis)
# ldamodel_combi <- train(x = combi[, c(1, 3, 6)], y = combi$FreeInd, trControl = tr,
#                         method = "lda")

# IMPORTANT NOTE - LDA needs numerical predictors, I guess. So, in order to convert categorical predictors 
# into dummy variables, will use the formula interface in caret.
ldamodel_combi <- train(FreeInd ~ IpAddress, data = combi,
                        trControl = tr, method = "lda")

print(ldamodel_combi) # 0.6341566 (same as above)

# 3. KNN
knn_combi <- train(FreeInd ~ IpAddress + Day + DateOnly, data = combi,
                   trControl = tr, method = "knn")

print(knn_combi) # gives 0.6273563 with k = 9 (< previous scores)

# 4. NB (Naive Bayes)
nb_combi <- train(x = combi[, c(1, 3)], y = combi$FreeInd,
                  trControl = tr, method = "nb")

print(nb_combi) # 0.6343354 (The best yet!)

# 5. CART (Decision Tree)
cart_combi <- train(x = combi[, c(1, 3, 6)], y = combi$FreeInd,
                    trControl = tr, method = "rpart")

print(cart_combi) # 0.6422099

# 6. SVM (Support Vector Machine)
svm_combi <- train(FreeInd ~ IpAddress, data = combi,
                   trControl = tr, method = "svmLinear")

print(svm_combi) # 0.6341566

# Ran so many algorithms, but still got so low accuracies in all. 

# 7. Trying running one Random Forest also!

rf_combi <- train(x = combi[, c(1, 3, 6)], y = combi$FreeInd,
                       trControl = tr, method = "rf")

print(rf_combi) # 0.6195433

##### RUNNING A TEsT PREDICTION FOR A SPECIFIC DATE, LET'S TAKE THE
##### LAST ONE IN THE COMBI DATASET
testData <- tail(combi[, c(1, 3, 6)], 254)
pred <- predict(rf_combi, testData)
confusionMatrix(pred, tail(combi$FreeInd, 254))
predProb <- predict(rf_combi, testData, type = "prob")

# Mostly all the algorithms are heavily biased towards the 1 (Free) class, except maybe KNN.
# Although, KNN has similar accuracy, it's not heavily biased as can be seen from the confusion matrix and
# the prediction probability. Can move ahead with it and tune it also. 
# RF also is quite superior here, even if its training accuracy is bad.
# Accuracy for test data in case of:
# 1. KNN = 0.7441
# 2. RF = 0.8937

# I was also confused whether to take 3 predictors or only a single predictor (IP). But since, KNN and RF
# are giving fairly good results, I will happily use all the 3 predictors.


# Can do some more tests like splitting into stratified samples of train and test and then checking 
# performance of both KNN and RF. This will be true testing of unseen data. After that, can go for
# their tuning, and if required, any advanced technique like ensembling.
# Well, don't really need true testing of unseen data because eventually when I train my model using 
# all the data, all the examples that will have to be predicted will already be seen by the algo, just 
# like in my previous testing scenario. Then KNN, and RF will be nice to work with!

# Stratified sampling
set.seed(21)
trainIndex <- createDataPartition(combi$FreeInd, p = 0.8, list = FALSE)
combi_train <- combi[trainIndex, ]
combi_test <- combi[-trainIndex, ]

# 1. KNN
knn_model2 <- train(FreeInd ~ IpAddress + Day + DateOnly, data = combi_train,
                    method = "knn")
print(knn_model2)
# 2. RF
rf_model2 <- train(x = combi_train[, c(1, 3, 6)], y = combi_train$FreeInd,
                   method = "rf")
print(rf_model2)

# And now, the test...
pred <- predict(rf_model2, combi_test)
confusionMatrix(pred, combi_test$FreeInd)
predProb <- predict(rf_model2, combi_test, type = "prob")

# Well, here KNN gave = 0.6238 and RF = 0.6196

# Train more models: (so, remember, usually try to create separate train and test sets first to evaluate 
# different algo)

# 3. LR - This is just wasted in our case, won't do it again. Gave all predictions as 1 once again!

lr_model2 <- train(x = combi_train[, c(1, 3, 6)], y = combi_train$FreeInd,
                   method = "glm", family = "binomial")
print(lr_model2)

# 4. LDA - Similar to LR, very bad.

lda_model2 <- train(FreeInd ~ IpAddress + Day + DateOnly, data = combi_train,
                    method = "lda")
print(lda_model2)

# testing 3 and 4 (very bad!)
pred <- predict(lda_model2, combi_test)
confusionMatrix(pred, combi_test$FreeInd)
predProb <- predict(lda_model2, combi_test, type = "prob")





######################
### FOR ONCE TRY OVERSAMPLING TO SEE IF IT HELPS HERE TOO FOR IMBALANCE IN CLASSES ###
allZeros <- combi[combi$FreeInd == '0', ]
temp <- createDataPartition(allZeros$Day, p = 0.4, list = FALSE)
temp1 <- allZeros[temp, ]
# Now adding these samples which are randomly but chosen in a stratified fashion into the combi dataset
# to increase the pop of class 0.
# First make another combi copy
combi_overSampled <- combi
combi_overSampled <- rbind(combi_overSampled, temp1)
table(combi_overSampled$FreeInd)
# 0: 8589, 1: 10631




## Start creating training and test set again and then make models out of those data sets.

# Stratified sampling
set.seed(21)
trainIndex <- createDataPartition(combi_overSampled$FreeInd, p = 0.8, list = FALSE)
combi_train <- combi_overSampled[trainIndex, ]
combi_test <- combi_overSampled[-trainIndex, ]

# 1. KNN
knn_model2 <- train(FreeInd ~ IpAddress + Day + DateOnly, data = combi_train,
                    method = "knn")
print(knn_model2)
# 2. RF
rf_model2 <- train(x = combi_train[, c(1, 3, 6)], y = combi_train$FreeInd,
                   method = "rf")
print(rf_model2)

# And now, the test...
pred <- predict(rf_model2, combi_test)
confusionMatrix(pred, combi_test$FreeInd)
predProb <- predict(rf_model2, combi_test, type = "prob")

# Accuracy: KNN = 0.6001, RF = 0.6825 ( So, apparantly, accuracy increased a bit, but only for RF.)
# So, it's final. I will move with RF now.
# And I am not going to go for tuning anymore. mtry is not possible and increasing ntree will make the 
# model really slow to train. 

#####
#####

# Will train my RF with the whole oversampled data and test on already seen examples, like I did before,
# like I'm going to get in the future!

set.seed(35)
tr <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

rf_combi <- train(x = combi_overSampled[, c(1, 3, 6)],
                  y = combi_overSampled$FreeInd,
                  trControl = tr,
                  method = "rf")

knn_combi <- train(FreeInd ~ IpAddress + Day + DateOnly,
                    data = combi_overSampled,
                    trControl = tr,
                    method = "knn")

print(rf_combi) # 0.6195433, after overSampling: 0.6927680 (mtry = 3)
print(knn_combi) # 0.5968280 after oversampling

##### RUNNING A TEsT PREDICTION FOR A SPECIFIC DATE, LET'S TAKE THE
##### LAST ONE IN THE COMBI DATASET, ####(Doing this once more!)####
testData <- tail(combi[, c(1, 3, 6)], 10000)
pred <- predict(rf_combi, testData)
confusionMatrix(pred, tail(combi$FreeInd, 10000))
predProb <- predict(rf_combi, testData, type = "prob")

# RF - Above, got 100% accuracy, since all the examples are already seen by the model. 
# REMARK - Also, when I am trying to predict the whole combi_overSampled dataset, it gives 100%
# accuracy, while there was some training error since training accuracy was = 0.6927680
# Well, at least, it doesn't give probabilities as 0 and 1. Can still use it.

# KNN - This model doesn't seem to remember all the examples. So, it doesn't give 100% accuracy. 



# FINAL - The model is not useful for any unseen examples. Although all the examples I will get as input
# even in the future, say, they all will be already seen by my model as the predictors are only 3 and with
# quite limited values (Not really though, eg - if Date = 12 if Friday, it can be Sunday in a future example
# which will be unseen by my model. Then the results may be unusual, which won't be verifiable since 
# I will not have the actual classes, or at least the probabilities may be unusual), but the predictions will
# be bad for future examples, if I had any way of knowing the actual classes. Still it doesn't matter because
# the exact situation we had in our hostel can't be recreated and so this model can't work anywhere else,
# only for our hostel in those days. So, a predictive model, even though not helpful elsewhere, is good
# for representational purposes on Shiny. 

# Will wrap this modeling part up now and take the final oversampled RF model and use it to make
# predictions for date input in Shiny. This model will be helpful where the same hostel conditions
# exist for eternity! 

# Next task - Deploy on Shiny



