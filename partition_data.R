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
# nci_topo <- fread("NCI/topo_2.txt", stringsAsFactors = F)
nci_topo <- fread(paste0(path, "/tmp_storage//NCI/topo_2.txt"), stringsAsFactors = F)
data <- nci_topo[,-c(1, 269)]
data$Potency <- ifelse(data$Potency == 0, 0, 1)

set.seed(2018)

flds <- createFolds(data$Potency, k = 5, returnTrain = FALSE)
flds

data$Potency<-factor(
  data$Potency,
  levels=c(0,1),
  labels=c("none", "active"))


training <- list()
test<- list()

training[[1]] <- data[-flds$Fold1, ]
test[[1]] <- data[flds$Fold1, ]

training[[2]] <- data[-flds$Fold2, ]
test[[2]] <- data[flds$Fold2, ]

training[[3]] <- data[-flds$Fold3, ]
test[[3]] <- data[flds$Fold3, ]

training[[4]] <- data[-flds$Fold4, ]
test[[4]] <- data[flds$Fold4, ]

training[[5]] <- data[-flds$Fold5, ]
test[[5]] <- data[flds$Fold5, ]


# save(training, file = "/home/jason/Dropbox/dbart_mid/chipman_replication/training.RData")
# save(test, file = "/home/jason/Dropbox/dbart_mid/chipman_replication/test.RData")

save(training, file = paste0(path, "/training.RData"))
save(test, file = paste0(path, "/test.RData"))




