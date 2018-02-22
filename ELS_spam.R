options(java.parameters = "-Xmx300g")
rm(list = ls())

path <- "/scratch/qg251/dbart_mid/chipman_replication"

library(foreign)
library(bartMachine)
library(data.table)
library(dplyr)
library(caret)



###### dataset with a binary dependent variable used in ESL https://web.stanford.edu/~hastie/ElemStatLearn/
###### 57 predictors and 4601 obs
spam_data <- read.table("spam.data")

probit <- glm(V58 ~., data = spam_data, family = binomial(link = "probit"), control = list(maxit = 1000))
summary(probit)

spam_data$V58 <- factor(spam_data$V58,
                        levels = c(0, 1),
                        labels = c("not", "spam"))

tc<-trainControl(method="cv",
                 number=5,#creates CV folds - 5 for this data
                 summaryFunction=twoClassSummary, # provides ROC summary stats in call to model
                 classProb=T)

bartGrid <- expand.grid(num_trees = c(500, 1000), k = 2, alpha = 0.95, beta = 2, nu = 3)
model.bt <- train(as.factor(V58)~., data=spam_data, metric = "ROC", method = "bartMachine",
                  tuneGrid = bartGrid, trControl = tc,  num_burn_in = 2000, num_iterations_after_burn_in = 5000, serialize = T)


md <- model.bt$finalModel

save(md, file=paste0(path, "/spam_final_model_5cv.RData"))