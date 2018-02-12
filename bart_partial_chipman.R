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
nci_topo <- fread(paste0(path, "/tmp_storage//NCI/topo_2.txt"), stringsAsFactors = F)
data <- nci_topo[,-c(1, 269)]
# data <- select(data, -c(AEigZ, AEigm, AEigv, AEige, AEigp)) # all these variables have constant value 0
data$Potency <- ifelse(data$Potency == 0, 0, 1)
probit <- glm(Potency ~., data = data, family = binomial(link = "probit"))

data$Potency<-factor(
  data$Potency,
  levels=c(0,1),
  labels=c("none", "noneactive"))
tc<-trainControl(method="cv",
                 number=5,#creates CV folds - 5 for this data
                 summaryFunction=twoClassSummary, # provides ROC summary stats in call to model
                 classProb=T)

bartGrid <- expand.grid(num_trees = c(500, 1000), k = 2, alpha = 0.95, beta = 2, nu = 3)
model.bt <- train(as.factor(Potency)~., data=data, metric = "ROC", method = "bartMachine",
                  tuneGrid = bartGrid, trControl = tc,  num_burn_in = 20000, num_iterations_after_burn_in = 20000, serialize = T)


md <- model.bt$finalModel

save(md, file=paste0(path, "/final_model_5cv.RData"))