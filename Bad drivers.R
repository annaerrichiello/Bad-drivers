library(tidyverse) #
library(car)#
library(corrplot)#
library(ridge)#
library(MASS)#

#import the dataset
bad_drivers <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/bad-drivers/bad-drivers.csv", sep = ",")
head(bad_drivers)
dir.create("data")
save(bad_drivers, file=file.path("data","bad_drivers.rda"))

a <- bad_drivers$Number.of.drivers.involved.in.fatal.collisions.per.billion.miles
b <- bad_drivers$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Speeding
c <- bad_drivers$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Alcohol.Impaired
d <- bad_drivers$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Were.Not.Distracted
e <- bad_drivers$Percentage.Of.Drivers.Involved.In.Fatal.Collisions.Who.Had.Not.Been.Involved.In.Any.Previous.Accidents
f <- bad_drivers$Car.Insurance.Premiums....
g <- bad_drivers$Losses.incurred.by.insurance.companies.for.collisions.per.insured.driver....

bad_drivers <- data.frame(a,b,c,d,e,f,g)
view(bad_drivers)

x <- bad_drivers[,-7] %>% as.matrix()
y <- bad_drivers$g
view(round(cor(x), 2)) # Correlation Test
corrplot(round(cor(x), 2)) #correlation graph

trainingIndex <- sample(nrow(bad_drivers), 0.80*nrow(bad_drivers)) # indices for 80%
trainingData <- bad_drivers[trainingIndex, ] # training data
x_train <- trainingData [,-7] %>% as.matrix()
y_train <- trainingData$g

testData <- bad_drivers[-trainingIndex, ] # test data
x_test <- testData[,-7] %>% as.matrix()
y_test <- testData$g

lmMod <- lm(g ~ ., trainingData)  # the linear reg model
summary (lmMod) # get summary
plot(lmMod)

predicted <- predict(lmMod, testData)  # predict on test data
compare <- cbind(actual=testData$g, predicted)  # combine actual and predicted
show (compare)

mean (apply(compare, 1, min)/apply(compare, 1, max)) # calculate accuracy

ridge_regr <- linearRidge(y_train ~ ., data = trainingData)  # the ridge regression model
summary(ridge_regr)

y_predicted <- predict(ridge_regr, testData)  # predict on test data
compare <- cbind (actual=testData$g, y_predicted)  # combine
compare
mean (apply(compare, 1, min)/apply(compare, 1, max))

# Sum of Squares Total and Error
sst <- sum((y_test - mean(y_test))^2)
sse <- sum((y_predicted - y_test)^2)

# R squared
rsq <- 1 - sse / sst
rsq

#ridge regression
lambdas <- c(1,5,10,20,50,100)
table <- data.frame()
{coef <- c((ridge1 <- lm.ridge(y_train ~ ., lambda = 1, data = trainingData))$coef[1], 
          (ridge2 <- lm.ridge(y_train ~ ., lambda = 5, data = trainingData))$coef[1],
          (ridge3 <- lm.ridge(y_train ~ ., lambda = 10, data = trainingData))$coef[1],
          (ridge4 <- lm.ridge(y_train ~ ., lambda = 20, data = trainingData))$coef[1],
          (ridge5 <- lm.ridge(y_train ~ ., lambda = 50, data = trainingData))$coef[1],
          (ridge6 <- lm.ridge(y_train ~ ., lambda = 100, data = trainingData))$coef[1])

plot(lambdas,coef)

  
  pred <- c(pred1 <-  x_test %*% matrix(c(ridge1$coef[1]), ncol=1,nrow=6),
          pred2 <-  x_test %*% matrix(c(ridge2$coef[1]), ncol=1,nrow=6),
          pred3 <-  x_test %*% matrix(c(ridge3$coef[1]), ncol=1,nrow=6),
          pred4 <-  x_test %*% matrix(c(ridge4$coef[1]), ncol=1,nrow=6),
          pred5 <-  x_test %*% matrix(c(ridge5$coef[1]), ncol=1,nrow=6),
          pred6 <-  x_test %*% matrix(c(ridge6$coef[1]), ncol=1,nrow=6))

mse <- c(mse1 <- mean((y_test-pred1)^(2)),
         mse2 <- mean((y_test-pred2)^(2)),
         mse3 <- mean((y_test-pred3)^(2)),
         mse4 <- mean((y_test-pred4)^(2)),
         mse5 <- mean((y_test-pred5)^(2)),
         mse6 <- mean((y_test-pred6)^(2)))
table <- data.frame(lambdas,mse=mse)
}
show(table)
