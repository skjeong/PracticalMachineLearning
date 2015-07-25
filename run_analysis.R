# Load library
library(randomForest)

# Read training data file (remove NA values, remove irrelevant variables)
path = getwd()
training = read.csv(file.path(path, "pml-training.csv"), na.strings=c("NA","#DIV/0!", " "))
training = training[, -c(1:7)]

summary(training)
# Removing columns that have only NAs
NAcols = colSums(is.na(training)) != 0
training = training[, !NAcols]

# Read teesting data file, remove NA values, remove irrelevant variables
testing = read.csv(file.path(path, "pml-testing.csv"), na.strings=c("NA","#DIV/0!", " "))
testing = testing[, -c(1:7)]
NAcols = colSums(is.na(testing)) != 0
testing = testing[, !NAcols]

# Build RandomForest model
model1 = randomForest(classe~., data = training, method = "class")

# Predict test data set
modelPredict = predict(model1, newdata = testing, type = "class")

# Write prediction results
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
