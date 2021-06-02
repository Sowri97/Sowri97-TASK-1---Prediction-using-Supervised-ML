#READING DATA FROM THE FILE
input<-read.csv("http://bit.ly/w-data")
input
#USING SCATTER PLOT TO VISUALISE RELATIONSHIP
scatter.smooth(x=input$Hours, y=input$Scores, main="Scores ~ Hours") 
#USING BOXPLOT FOR CHECKING OUTLIERS

# dividing graph area in 2 columns
par(mfrow=c(1, 2)) 
# box plot for 'Hours'
boxplot(input$Hours, main="Hours", sub=paste("Outlier rows: ", boxplot.stats(input$Hours)$out)) 
# box plot for 'Scores'
boxplot(input$Scores, main="Scores", sub=paste("Outlier rows: ", boxplot.stats(input$Scores)$out))  

#CORRELATION BETWEEN HOURS AND SCORES
cor(input$Hours, input$Scores)
#result displayed for cor is close to 1 which means there exists a strong correlation between hours and scores


# TESTING AND TRAINING DATA

# setting seed to reproduce results of random sampling
set.seed(100)  
# row indices for training data
trainingRowIndex <- sample(1:nrow(input), 0.8*nrow(input))  
# model training data
trainingData <- input[trainingRowIndex, ]
# test data
testData  <- input[-trainingRowIndex, ]   

# BUILDING MODEL ON TRAINING DATA

# build the model
lmMod <- lm(Scores ~ Hours, data=trainingData)
lmMod
summary(lmMod)
# TO PREDICT SCORES
ScorePred <- predict(lmMod, testData)  
ScorePred
#predicting the score of student given hours(which works as a predictor)
hrs<-data.frame(Hours=9.25)
res<-predict(lmMod,hrs)
res
summary(lmMod)
#COMPARING ACTUAL AND PREDICTED VALUES
actuals_preds <- data.frame(cbind(actuals=testData$Scores, predicteds=ScorePred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)# 82.7%
correlation_accuracy
head(actuals_preds)
