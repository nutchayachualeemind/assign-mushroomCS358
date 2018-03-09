#install.packages(lattice)
library( readr )
library( rpart )
library( RWeka )
library( caret )
library( ggplot2 )
library("rpart.plot")
mushrooms <- read.csv("mushrooms.csv")
#str(mushroomDf)
print(summary(mushrooms))
set.seed(4) # for reproducibility
train <- sample(1:nrow(mushrooms),size = ceiling(0.80*nrow(mushrooms)))
# training set
mushrooms_train <- mushrooms[train,]
# test set
mushrooms_test <- mushrooms[-train,]

gainRatioResult <- GainRatioAttributeEval( class ~ . , data = mushrooms_train )
#print( sort( gainRatioResult, decreasing = TRUE ))

hist <- ggplot( mushrooms, aes( x = odor, fill = class )) 
hist <- hist + geom_bar(stat='count', position='dodge') + labs(x = 'Odor', y = 'Count of Class')

dt.Model      <- rpart( mushrooms, data = mushrooms_train )

#dt.Model <- rpart(class~.,data=mushrooms_train)

dt.Prediction <- predict( dt.Model, newdata = mushrooms_test,type="class" )
rpart.plot(dt.Model)

test <- table(mushrooms_test$class,dt.Prediction)
print(confusionMatrix(test))

