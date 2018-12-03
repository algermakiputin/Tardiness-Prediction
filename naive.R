library(e1071)

train <- data.frame(type=c("spam","ham","ham","ham"), 
                    viagra=c("yes","no","no","yes"),
                    meet=c("yes","yes","yes", "no"),
                    age =c(10,15,10,15))

classifier<- naiveBayes(type ~ ., data = train)
 
testData<- data.frame(
    viagra=c("yes"),
    meet=c("yes"),
    age=c(15)
)

cl<- predict(classifier, testData, type = "class")

pred<- predict(classifier, testData, type = "raw")

print(pred[,c(cl)])
