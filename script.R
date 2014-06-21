#install.packages("rpart.plot")
setwd("/home/mario/pml/pa01")
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")
summary(training)
summary(testing)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
set.seed(1234)

# Number of folds
tr.control = trainControl(method = "cv", number = 10)
# cp values
cp.grid = expand.grid( .cp = (0:10)*0.001)
# Cross-validation
tr = train(classe ~ num_window+roll_belt+pitch_belt+yaw_belt+total_accel_belt+ 
             gyros_belt_x+gyros_belt_y+gyros_belt_z+accel_belt_x+accel_belt_y+accel_belt_z+magnet_belt_x+magnet_belt_y+magnet_belt_z+roll_arm+pitch_arm+yaw_arm+total_accel_arm+
             gyros_arm_x+gyros_arm_y+gyros_arm_z+accel_arm_x+accel_arm_y+accel_arm_z+magnet_arm_x+magnet_arm_y+magnet_arm_z+
             roll_dumbbell+pitch_dumbbell+yaw_dumbbell+
             total_accel_dumbbell+gyros_dumbbell_x+gyros_dumbbell_y+gyros_dumbbell_z+accel_dumbbell_x+accel_dumbbell_y+accel_dumbbell_z+magnet_dumbbell_x+
             magnet_dumbbell_y+magnet_dumbbell_z+roll_forearm+pitch_forearm+yaw_forearm+
             total_accel_forearm+gyros_forearm_x+gyros_forearm_y+gyros_forearm_z+accel_forearm_x+accel_forearm_y+
             accel_forearm_z+magnet_forearm_x+magnet_forearm_y+magnet_forearm_z,data = training, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
# Extract tree
best.tree = tr$finalModel
prp(best.tree)

# Test over training data
best.tree.pred = predict(best.tree, newdata=training)
answers = NULL
for(i in 1:length(best.tree.pred[,1])){
  answers <- rbind(answers,names(which.max(best.tree.pred[i,])))
}
table(training$classe,answers)
sum(training$classe==answers)/length(training$classe)

# Make predictions
best.tree.pred = predict(best.tree, newdata=testing)
answers = NULL
for(i in 1:20){
  answers <- rbind(answers,names(which.max(best.tree.pred[i,])))
}
real.answers <- c("B","A","B","A","A","E","D","B","A","A","B","C","B","A","E","E","A","B","B","B")
table(real.answers,answers)
sum(real.answers==answers)/length(real.answers)

#Save answers
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)