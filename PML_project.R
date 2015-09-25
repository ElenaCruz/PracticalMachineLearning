#Practical Machine Learning Project R files
# Elena Cruz Martín, 2015

library(dplyr)
library(caret)
library(corrplot)
library(rattle)
library(parallel, quietly=T)
library(doParallel, quietly=T)


data_dir <- "."
training_file_name <- "pml-training.csv"
testing_file_name <- "pml-testing.csv"

training <- read.table(file=file.path(data_dir,training_file_name),sep=",", header=TRUE)
testing <- read.table(file=file.path(data_dir,testing_file_name),sep=",", header=TRUE)

# Some preliminary analysis on the data
names(training)
# table(training$user_name)
# table(training$X)
# table(training$raw_timestamp_part_1)
# table(training$raw_timestamp_part_1)
# table(training$cvtd_timestamp)
# table(training$new_window)
# table(training$num_window)

setdiff(names(training),names(testing))
setdiff(names(testing), names(training))

#See how the data looks like
head(training)

##Clean summary columns
summary_patterns <- c("max", "min","stddev", "var", "avg", "kurtosis", "skewness")
summary_columns <- unlist(sapply(summary_patterns, grep,names(training)))
training_redux <- select(training, -summary_columns)
summary(training_redux)

amp_yaw_pattern <- c("amplitude_")
amp_yaw_columns <- grep(amp_yaw_pattern, names(training_redux))
training_redux <- select(training_redux, -amp_yaw_columns)

summary(training_redux)

#Data checking
t1 <- training_redux[complete.cases(training_redux),]
nacases <- sum(is.na(training_redux))

#Remove not needed vars 
removed_names <-names(training_redux)[1:7]
training_redux <- training_redux[,8:ncol(training_redux)]

head(training_redux)

#See correlated vars
corMatrix_entry <- select(training_redux, -classe) #We remove the classe column
corMatrix <- cor(corMatrix_entry)
correlatedCols <- findCorrelation(corMatrix, names=TRUE)
corrplot(corMatrix, method = "circle")

#Remove correlated cols 
training_redux <- training_redux[,!(names(training_redux) %in% correlatedCols)]

##Get a set of samples in order to test the models
# training_redux_1 <- sample_n(training_redux, 1000)
# training_redux <- training_redux_1

## Turn on PP (leave at least 1 core free so your machine is still usable when doing the calc)
cluster             <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

#PCA Model
preProc <- train(classe ~ ., data = training_redux, preProcess="pca")

#Trees
modFit_tree <- train(classe~., data = training_redux, method="rpart")
print(modFit_tree$finalModel)
fancyRpartPlot(modFit_tree$finalModel)

#Random forest
modFit_rf <- train(classe~., data = training_redux, method="rf")
print(modFit_rf$finalModel)

## Turn off PP
stopCluster(cluster)

answers_pca <- predict(preProc,testing)
answers_tree <- predict(modFit_tree,testing)
answers_rf <- predict(modFit_tree,testing)

print("answers_pca:")
answers_pca

print("answers_tree:")
answers_pca

print("answers_rf:")
answers_pca

#Predict testing values
# answers = answers_pca ##To be changed once we have the test vector
# pml_write_files = function(x){
#   n = length(x)
#   for(i in 1:n){
#     filename = paste0("./Results/problem_id_",i,".txt")
#     write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
#   }
# }
# pml_write_files(answers)


