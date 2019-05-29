library(ggplot2)

# Fit lm model: model
model <- lm(formula=price ~ ., data=diamonds)

# Predict on full data: p
p <- predict(model, diamonds)

# Compute errors: error
error<-p-diamonds$price

# Calculate RMSE
sqrt(mean(error^2))


##########################################################
######################Ejercicio 2#########################
##########################################################

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(diamonds))

# Randomly order data
shuffled_diamonds <- diamonds[rows, ]


##########################################################
######################Ejercicio 3#########################
##########################################################

# Determine row to split on: split
split <- round(nrow(diamonds) * .80)

# Create train
train<-diamonds[1:split, ]

# Create test
test<-diamonds[(split + 1):nrow(diamonds), ]


##########################################################
######################Ejercicio 4#########################
##########################################################

# Fit lm model on train: model
model <- lm(price ~ ., train)

# Predict on test: p
p <- predict(model, test)


##########################################################
######################Ejercicio 5#########################
##########################################################

# Compute errors: error
error<-p-test[["price"]]

# Calculate RMSE
sqrt(mean(error^2))

##########################################################
######################Ejercicio 6#########################
##########################################################
library(caret)
#Cross Validation
# Fit lm model using 10-fold CV: model
model <- train(
  price~., 
  diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  )
)


##########################################################
######################Ejercicio 7#########################
##########################################################
library(MASS)#Para cagar el dataset Boston

# Fit lm model using 5-fold CV: model
model <- train(
  medv~., 
  Boston,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 5,
    verboseIter = TRUE
  )
)

##########################################################
######################Ejercicio 8#########################
##########################################################

# Fit lm model using 5 x 5-fold CV: model
model <- train(
  medv ~ ., 
  Boston,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv", 
    number = 5,
    repeats = 5, 
    verboseIter = TRUE
  )
)
  
  
##########################################################
######################Ejercicio 9#########################
##########################################################
  
# Predict on full Boston dataset
predict(model, Boston)


##########################################################
######################Ejercicio 10########################
##########################################################




##########################################################
######################Ejercicio 11########################
##########################################################


