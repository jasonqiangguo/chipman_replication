options(java.parameters = "-Xmx300g")
rm(list = ls())

library(bartMachine)
library(data.table)
library(dplyr)
library(caret)

path <- "/scratch/qg251/dbart_mid/chipman_replication"
# setwd("~/Dropbox/dbart_mid/chipman_replication/tmp_storage")

###### dataset with a binary dependent variable used in Chipman et al is from https://www.niss.org/research/datasets
###### 266 predictors and 29374 obs
# setwd("/home/jason/Dropbox/dbart_mid/chipman_replication")
# load("training.RData")
# train <- training[[1]]
# 
# load("test.RData")
# test <- test[[1]]
# 

load(paste0(path, "/training.RData"))
train <- training[[1]]

load(paste0(path, "/test.RData"))
test <- test[[1]]

# tc<-trainControl(method="cv",
#                  number=5,#creates CV folds - 5 for this data
#                  summaryFunction=twoClassSummary, # provides ROC summary stats in call to model
#                  classProb=T)


?bartMachine

train_covariates <- as.data.frame(select(train, -Potency))
bart1 <- bartMachine(X = train_covariates, y = train$Potency, num_trees = 1000, num_burn_in = 20000, num_iterations_after_burn_in = 20000,
            alpha = 0.95, k = 2, nu = 3, beta = 2, serialize = T)

test_covariates <- as.data.frame(select(test, -Potency))
bart1_test <- bart_predict_for_test_data(bart1, test_covariates, test$Potency, prob_rule_class = NULL)

save(bart1, file = paste0(path, "/bart1_spam.RData"))
save(bart1_pred, file = paste0(path, "/bart1_spam_test.RData"))

# bartGrid <- expand.grid(num_trees = c(500, 1000), k = 2, alpha = 0.95, beta = 2, nu = 3)
# model.bt <- train(as.factor(Potency)~., data=data, metric = "ROC", method = "bartMachine",
#                   tuneGrid = bartGrid, trControl = tc,  num_burn_in = 20000, num_iterations_after_burn_in = 20000, serialize = T)
# md <- model.bt$finalModel
# save(md, file=paste0(path, "/final_model_5cv.RData"))