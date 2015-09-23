#Practical Machine Learning Project R files
# Elena Cruz Martín, 2015

library(dplyr)

data_dir <- "."
training_file_name <- "pml-training.csv"
testing_file_name <- "pml-testing.csv"

training <- read.table(file=file.path(data_dir,training_file_name),sep=",", header=TRUE)
testing <- read.table(file=file.path(data_dir,testing_file_name),sep=",", header=TRUE)

# Some preliminary analysis on the data
names(training)
table(training$user_name)
table(training$X)
table(training$raw_timestamp_part_1)
table(training$raw_timestamp_part_1)
table(training$cvtd_timestamp)
table(training$new_window)
table(training$num_window)

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



#Clean NAs:
t1 <- training_redux[complete.cases(training_redux),] #See how many rows are complete --> Only 460 of them
summary(training)
