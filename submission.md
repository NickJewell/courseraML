Course Project: Submission

Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

Data 


The training data for this project are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

Report

How I built the model

# Setting a Seed (for re-testing)

set.seed(90210)

# Importing the necessary libraries

library(caret)
library(randomForest)
library(rattle)
library(rpart)
library(rpart.plot)

# Wrangling data from the URLs

trainingDataUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
validationDataUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainingSet <- read.csv(
						url(trainingDataUrl), 
						na.strings=c("NA","#DIV/0!",""))
						
validationSet <- read.csv(
						url(validationDataUrl), 
						na.strings=c("NA","#DIV/0!",""))

# Producing a Training Set and Test Set from a single Training Set (70%/30%)
# (as per Jeff's lectures)

inTrain <- createDataPartition(y=trainingSet$classe, p=0.7, list=FALSE)
splitTrain <- trainingSet[inTrain, ]
splitTest <- trainingSet[-inTrain, ]

# Near Zero Variance cleanup

trainingCleanup <- nearZeroVar(splitTrain, saveMetrics=TRUE)

colNames <- names(splitTrain) %in% 
c(
"new_window", 
"kurtosis_roll_belt", 
"kurtosis_picth_belt",
"kurtosis_yaw_belt", 
"skewness_roll_belt", 
"skewness_roll_belt.1", 
"skewness_yaw_belt",
"max_yaw_belt", 
"min_yaw_belt", 
"amplitude_yaw_belt", 
"avg_roll_arm", 
"stddev_roll_arm",
"var_roll_arm", 
"avg_pitch_arm", 
"stddev_pitch_arm", 
"var_pitch_arm", 
"avg_yaw_arm",
"stddev_yaw_arm", 
"var_yaw_arm", 
"kurtosis_roll_arm", 
"kurtosis_picth_arm",
"kurtosis_yaw_arm", 
"skewness_roll_arm", 
"skewness_pitch_arm", 
"skewness_yaw_arm",
"max_roll_arm", 
"min_roll_arm", 
"min_pitch_arm", 
"amplitude_roll_arm", 
"amplitude_pitch_arm",
"kurtosis_roll_dumbbell", 
"kurtosis_picth_dumbbell", 
"kurtosis_yaw_dumbbell", 
"skewness_roll_dumbbell",
"skewness_pitch_dumbbell", 
"skewness_yaw_dumbbell", 
"max_yaw_dumbbell", 
"min_yaw_dumbbell",
"amplitude_yaw_dumbbell", 
"kurtosis_roll_forearm", 
"kurtosis_picth_forearm", 
"kurtosis_yaw_forearm",
"skewness_roll_forearm", 
"skewness_pitch_forearm", 
"skewness_yaw_forearm", 
"max_roll_forearm",
"max_yaw_forearm", 
"min_roll_forearm", 
"min_yaw_forearm", 
"amplitude_roll_forearm",
"amplitude_yaw_forearm", 
"avg_roll_forearm", 
"stddev_roll_forearm", 
"var_roll_forearm",
"avg_pitch_forearm", 
"stddev_pitch_forearm", 
"var_pitch_forearm", 
"avg_yaw_forearm",
"stddev_yaw_forearm", 
"var_yaw_forearm")

splitTrain <- splitTrain[!colNames]

# Remove the ID from the data set (it will be misinterpreted anyway)

splitTrain <- splitTrain[c(-1)]

# Remove variables that have more than half NA

trainingRemove <- splitTrain 
for(p in 1:length(splitTrain)) { 
        if( sum( 
				is.na( splitTrain[, p] ) ) /nrow(splitTrain) >= .5 ) { 
       			 	for(q in 1:length(trainingRemove)) {
            			if( length( grep(names(splitTrain[p]), names(trainingRemove)[q]) ) ==1)  { 
                			trainingRemove <- trainingRemove[ , -q] 
            }   
        } 
    }
}

splitTrain <- trainingRemove

# Make sure the Validation data set is up to the job - assign names, etc.

trainCols <- colnames(splitTrain)
trainColsRemoved <- colnames(splitTrain[, -58])
splitTest <- splitTest[trainCols]
validationSet <- validationSet[trainColsRemoved]


for (p in 1:length(validationSet) ) 
	{
        for(q in 1:length(splitTrain)) 
			{
        		if( length( grep(names(splitTrain[p]), names(validationSet)[q]) ) ==1)  
					{
            			class(validationSet[q]) <- class(splitTrain[p])
        			}      
    		}      
		}

validationSet <- rbind(splitTrain[2, -58] , validationSet)
validationSet <- validationSet[-1,]


## Build a Random Forest

randForest <- randomForest(classe ~.,data=splitTrain)

predictRF <- predict(randForest, splitTest, type="class")

## Did it work? Run a confusion matrix

confusionMatrix(predictRF, splitTest$classe)


# Running the Forest against the Validation Set

predictRF_Validation <- predict(randForest, validationSet, type="class")


## Submitting the Validation Set for Scoring

output = function(x)
{
  t = length(x)
  for(s in 1:t)
  	{
    	filename = paste0("problem_id_",s,".txt")
    	write.table(x[s],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  	}
}

output(predictRF_Validation)







