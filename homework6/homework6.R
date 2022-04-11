# MATH-3190
# Akhil Anand
# Homework 6 R Script

#Libraries
library(glmnet)
library(caret)
library(tidyverse)

# Getting the salary dataset
salary <- tibble(
  Salary = c(57310.00, 57380.00, 54135.00, 56985.00, 58715.00, 60620.00, 59200.00, 60320.00),
  Employment = c(10,5,3,6,8,20,8,14),
  Experience = c(2,6,1,5,8,0,4,6),
  Education = c(16,16,12,14,16,12,18,17)
)

knitr::kable(salary)
summary(princomp(salary[,-1]))
prcomp(salary[,-1])$rotation

# Fitting a least squares model
lm_fit1 <- lm(Salary~., data = salary)
summary(lm_fit1)

# Fitting a Lasso Model, lambda = 1000
lasso_fit1 <- glmnet(as.matrix(salary[,c(2,3,4)]), as.matrix(salary[,1]), lambda = 1000, alpha = 1)
lasso_fit1
coef(lasso_fit1)

# lambda = 800
lasso_fit2 <- glmnet(as.matrix(salary[,c(2,3,4)]), as.matrix(salary[,1]), lambda = 800, alpha = 1)
lasso_fit2$beta
coef(lasso_fit2)

# lambda = 500
lasso_fit3 <- glmnet(as.matrix(salary[,c(2,3,4)]), as.matrix(salary[,1]), lambda = 500, alpha = 1)
lasso_fit3$beta
coef(lasso_fit3)

# lambda = 1
lasso_fit4 <- glmnet(as.matrix(salary[,c(2,3,4)]), as.matrix(salary[,1]), lambda = 1, alpha = 1)
lasso_fit4
coef(lasso_fit4) 

# now lets cross validate
cv_lasso <- cv.glmnet(as.matrix(salary[,c(2,3,4)]), as.matrix(salary[,1]), alpha = 1)
best_lambda <- cv_lasso$lambda.min
plot(cv_lasso)

lasso_fit5 <- glmnet(as.matrix(salary[,c(2,3,4)]), as.matrix(salary[,1]), lambda = best_lambda, alpha = 1)
lasso_fit5
coef(lasso_fit5)


# Fitting Ridge models
ridge_fit1 <- glmnet(as.matrix(salary[,c(2,3,4)]), as.matrix(salary[,1]), lambda = 1000, alpha = 0)
ridge_fit1
coef(ridge_fit1)

# lambda = 800
ridge_fit2 <- glmnet(as.matrix(salary[,c(2,3,4)]), as.matrix(salary[,1]), lambda = 800, alpha = 0)
ridge_fit2
coef(ridge_fit2)

# lambda = 500
ridge_fit3 <- glmnet(as.matrix(salary[,c(2,3,4)]), as.matrix(salary[,1]), lambda = 500, alpha = 0)
ridge_fit3
coef(ridge_fit3)

# lambda = 1
ridge_fit4 <- glmnet(as.matrix(salary[,c(2,3,4)]), as.matrix(salary[,1]), lambda = 1, alpha = 0)
ridge_fit4
coef(ridge_fit4)

# Now cross validation to get best lambda
cv_ridge <- cv.glmnet(as.matrix(salary[,c(2,3,4)]), as.matrix(salary[,1]), alpha = 0)
best_lambda_ridge <- cv_ridge$lambda.min
plot(cv_ridge)

ridge_fit5 <- glmnet(as.matrix(salary[,c(2,3,4)]), as.matrix(salary[,1]), 
                     lambda = best_lambda_ridge, alpha = 0)
ridge_fit5
coef(ridge_fit5)


# Question 2
# Cereal dataset
cereal <- read.csv("~/MATH-3190/MATH_3190_HW/homework6/cereal.csv")

# lets create some indicator variables
cereal$name <- as.factor(cereal$name)
cereal$mfr <- as.factor(cereal$mfr)
cereal$type <- as.factor(cereal$type)
levels(cereal$name)
levels(cereal$mfr)
levels(cereal$type)

# gonna create indicator for type
# if "C" then 1, otherwise 0
cerealA <- ifelse(cereal$mfr == "A", 1, 0)
cerealA <- as.factor(cerealA)
cerealG <- ifelse(cereal$mfr == "G", 1, 0)
cerealG <- as.factor(cerealG)
cerealK <- ifelse(cereal$mfr == "K", 1, 0)
cerealK <- as.factor(cerealK)
cerealN <- ifelse(cereal$mfr == "N", 1, 0)
cerealN <- as.factor(cerealN)
cerealP <- ifelse(cereal$mfr == "P", 1, 0)
cerealP <- as.factor(cerealP)
cerealQ <- ifelse(cereal$mfr == "Q", 1, 0)
cerealQ <- as.factor(cerealQ)
cerealR <- ifelse(cereal$mfr == "R", 1, 0)
cerealR <- as.factor(cerealR)

# now we can add the indicator variables back to the dataset
cereal$mfrA <- cerealA
cereal$mfrG <- cerealG
cereal$mfrK <- cerealK
cereal$mfrN <- cerealN
cereal$mfrP <- cerealP
cereal$mfrQ <- cerealQ
cereal$mfrR <- cerealR

# we can drop mfr now
cereal <- cereal[, -2]

# reordering
rating <- cereal$rating
cereal <- cereal[,-15]
cereal$rating <- rating

# Now type
cereal$type <- ifelse(cereal$type == "C","1","0")
cereal$type <- as.numeric(cereal$type)

# Lasso modeling
# Lambda = 8
cereal_lasso1 <- glmnet(cereal[,c(1:21)], cereal[,c(22)], lambda = 8, alpha = 1)
cereal_lasso1
cereal_lasso1$beta

# Lambda = 5
cereal_lasso2 <- glmnet(cereal[,c(1:21)], cereal[,c(22)], lambda = 5, alpha = 1)
cereal_lasso2
cereal_lasso2$beta

# Lambda = 3
cereal_lasso3 <- glmnet(cereal[,c(1:21)], cereal[,c(22)], lambda = 3, alpha = 1)
cereal_lasso3
cereal_lasso3$beta

# Lambda = 1
cereal_lasso4 <- glmnet(cereal[,c(1:21)], cereal[,c(22)], lambda = 1, alpha = 1)
cereal_lasso4
cereal_lasso4$beta


# Cross validation 

cv_cereal <- cv.glmnet(as.matrix(cereal[,c(2:21)]), as.matrix(cereal[,c(22)]), alpha = 1)
best_cereal_lambda <- cv_cereal$lambda.min
plot(cv_cereal)

cereal_lasso5 <- glmnet(cereal[,c(2:21)], cereal[,c(22)], lambda = best_cereal_lambda, alpha = 1)
cereal_lasso5
cereal_lasso5$beta

# Question 3
# Car prediction dataset
car_price_prediction$year <- as.factor(car_price_prediction$year)
car_price_prediction$fuel <- as.factor(car_price_prediction$fuel)
car_price_prediction$seller_type <- as.factor(car_price_prediction$seller_type)
car_price_prediction$transmission <- as.factor(car_price_prediction$transmission)
car_price_prediction$owner <- as.factor(car_price_prediction$owner)

# will have to make indicator variables for all
# except year and name
# first fuel
car_price_prediction$fuelCNG <- ifelse(car_price_prediction$fuel == "CNG",1,0)
car_price_prediction$fuelDiesel <- ifelse(car_price_prediction$fuel == "Diesel",1,0)
car_price_prediction$fuelElectric <- ifelse(car_price_prediction$fuel == "Electric",1,0)
car_price_prediction$fuelLPG <- ifelse(car_price_prediction$fuel == "LPG",1,0)
car_price_prediction$fuelPetrol <- ifelse(car_price_prediction$fuel == "Petrol",1,0)
car_price_prediction <- car_price_prediction[,-5]

# now seller type
car_price_prediction$seller_typeD <- ifelse(car_price_prediction$seller_type == "Dealer",1,0)
car_price_prediction$seller_typeI <- ifelse(car_price_prediction$seller_type == "Individual",1,0)
car_price_prediction$seller_typeT <- ifelse(car_price_prediction$seller_type == "Trustmark Dealer",1,0)
car_price_prediction <- car_price_prediction[,-5]

# transmission, if Automatic then 1 else 0
car_price_prediction$transmission <- ifelse(car_price_prediction$transmission == "Automatic",1,0)

# finally owner
car_price_prediction$owner_FO <- ifelse(car_price_prediction$owner == "First Owner",1,0)
car_price_prediction$owner_FAO <- ifelse(car_price_prediction$owner == "Fourth & Above Owner",1,0)
car_price_prediction$owner_SO <- ifelse(car_price_prediction$owner == "Second Owner",1,0)
car_price_prediction$owner_TDC <- ifelse(car_price_prediction$owner == "Test Drive Car",1,0)
car_price_prediction$owner_TO <- ifelse(car_price_prediction$owner == "Third Owner",1,0)
car_price_prediction <- car_price_prediction[,-6]

# All indicators are now done. So we can model
# Elastic net model
x <- as.matrix(car_price_prediction[, c(4:18)])
y <- as.matrix(car_price_prediction[, 3])
car_elastic1 <- glmnet(x,y)
car_elastic1$call
plot(car_elastic)

# cross validation
cv_elastic <- cv.glmnet(x,y)
best_lambda_car <- cv_elastic$lambda.min
plot(cv_elastic)
The # final model 
elastic_car <- glmnet(x,y,lambda = best_lambda_car)
summary(elastic_car)
elastic_car$beta
elastic_car$a0
