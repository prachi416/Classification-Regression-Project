library(MASS)
library(caret)
library (glmnet)
library(e1071)
library(class)

# Load your data
data <- read.csv("E://UB//EAS508 Statistical Learning and Data Mining-I//RLab//Project_2//project2.csv", header = TRUE)
colnames(data) <- c("X1", "X2", "X3", "X4", "Y")


# --------------------- Linear Discriminant Analysis (LDA) -------------------------------

# Splitting the data into training and testing sets
set.seed(1)
train <- sample(1:nrow(data), 0.8 * nrow(data))
train.data <- data[train, ]
test.data <- data[-train, ]

# Fit LDA model
lda.model <- lda(train.data[, -ncol(train.data)], grouping = train.data[, ncol(train.data)])
# Make predictions
lda.predictions <- predict(lda.model, test.data[, -ncol(test.data)])
# Evaluate the model
lda.accuracy <- sum(test.data[, ncol(test.data)] == lda.predictions$class) / nrow(test.data)
lda.accuracy
table(lda.predictions$class, test.data$Y)

# Cross-Validation for Misclassification Rate
# 10-fold cross-validation
control <- trainControl(method = "cv", number = 10)  
cv_model <- train(as.factor(Y) ~ ., data = data, method = "lda", trControl = control)
cv_results <- cv_model$results
# Classification Rate
cv_classification_rate <- mean(cv_results$Accuracy)
cv_classification_rate
# Misclassification Rate
misclassification_rate <- 1 - mean(cv_results$Accuracy)
misclassification_rate

# LDA Model Accuracy : 0.9672727
# Accuracy using Cross Validation with 10 folds : 0.9766529
# Misclassification Rate using Cross Validation : 0.02334709


# --------------------- Quadratic Discriminant Analysis (QDA) ----------------------------

# Splitting the data into training and testing sets
set.seed(1)
train <- sample(1:nrow(data), 0.8 * nrow(data))
train.data <- data[train, ]
test.data <- data[-train, ]

# Fit QDA model
qda.model <- qda(train.data[, -ncol(train.data)], grouping = train.data[, ncol(train.data)])
# Make predictions
qda.predictions <- predict(qda.model, test.data[, -ncol(test.data)])
# Evaluate the model
qda.accuracy <- sum(test.data[, ncol(test.data)] == qda.predictions$class) / nrow(test.data)
qda.accuracy
table(qda.predictions$class, test.data$Y)

# Cross-Validation for Misclassification Rate
# 10-fold cross-validation
control <- trainControl(method = "cv", number = 10)  
cv_model <- train(as.factor(Y) ~ ., data = data, method = "qda", trControl = control)
cv_results <- cv_model$results
# Classification Rate
cv_classification_rate <- mean(cv_results$Accuracy)
cv_classification_rate
# Misclassification Rate
misclassification_rate <- 1 - mean(cv_results$Accuracy)
misclassification_rate

# QDA Model Accuracy : 0.9854545
# Accuracy using Cross Validation with 10 folds : 0.9846821
# Misclassification Rate using Cross Validation : 0.01531789


# --------------------------------- Logistic Regression ---------------------------------

glm.fits <- glm(formula = Y ~ X1 + X2 + X3 + X4, data = data , family = "binomial")
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
predictions <- predict(glm.fits, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
conf_matrix <- table(Actual = data$Y, Predicted = predicted_classes)
Accuracy <- accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
Accuracy*100

# Cross-Validation for Misclassification Rate
X <- as.matrix(data[, c('X1', 'X2', 'X3', 'X4')])
Y <- data$Y
cv.glmnet.fit <- cv.glmnet(x = X, y = Y, family = "binomial", type.measure = "class", nfolds = 5)
best_lambda <- cv.glmnet.fit$lambda.min
plot(cv.glmnet.fit)
predictions <- predict(cv.glmnet.fit, newx = X, s = "lambda.min", type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
misclassification_error_rate <- mean(predicted_classes != as.numeric(Y))
misclassification_error_rate
1- misclassification_error_rate

# Accuracy for Logistic Regression : 99.19825%
# Acurracy Rate using Cross Validation : 0.9905248
# Misclassification Error Rate obtained : 0.009475219


# ------------------------------ Naive Bayes -------------------------------------------

set.seed(1)  
# Create a trainControl object for 5-fold cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Train the Naive Bayes model using cross-validation
nb_model <- train(
  x = data[, 1:4],
  y = as.factor(data$Y),
  method = "naive_bayes",
  trControl = ctrl
)
nb_model

pred_nb <- predict(nb_model, newdata = data[, 1:4])
conf_matrix_nb <- confusionMatrix(pred_nb, as.factor(data$Y))
conf_matrix_nb
misclassification_rate_nb <- 1 - conf_matrix_nb$overall["Accuracy"]
misclassification_rate_nb

# Accuracy of the Naive Bayes model : 92.57%.
# Misclassification rate using Cross Validation with 5 folds : 7.43%.
# Sensitivity (True Positive Rate): 95.28%
# #Specificity (True Negative Rate): 89.18%


# ------------------------------------- KNN --------------------------------------------

train_test_split <- sample(1:nrow(data), size = 0.8*nrow(data))
train.X <- data[train_test_split, c('X1', 'X2', 'X3', 'X4')]
test.X <- data[-train_test_split, c('X1', 'X2', 'X3', 'X4')]
train.Y <- data[train_test_split, 'Y']
test.Y <- data[-train_test_split, 'Y']
table(data$Y)
table(train.Y)
table(test.Y)
#KNN model with k=1
knn.pred <- knn (train.X, test.X, train.Y , k = 1)
table(knn.pred, test.Y)
(151+123)/275

#5-fold cross validation on KNN with k=1
data$Y <- as.factor(data$Y)
control <- trainControl(method="cv", number=5)
knnFit <- train(Y~., data=data, method="knn", trControl=control, tuneGrid=expand.grid(k=1))
print(knnFit)

#KNN model with k=3
knn.pred <- knn (train.X, test.X, train.Y , k = 3)
table(knn.pred, test.Y)
(152+123)/275

#5-fold cross validation on KNN with k=3
data$Y <- as.factor(data$Y)
control <- trainControl(method="cv", number=5)
knnFit <- train(Y~., data=data, method="knn", trControl=control, tuneGrid=expand.grid(k=3))
print(knnFit)

# Accuracy of KNN with k = 1 : 0.996
# Accuracy of KNN with k = 3 : 1
# Accuracy of 5-fold Cross Validation accuracy of KNN with k = 1 : 0.9992701
# Misclassification rate of 5-fold Cross Validation accuracy of KNN with k = 1 : 0.0007
# Accuracy of 5-fold Cross Validation accuracy of KNN with k = 3 : 0.9992727
# Misclassification rate of 5-fold Cross Validation accuracy of KNN with k = 3 : 0.0007


# ----------------------- Suppory Vector Machine (SVM) ----------------------------------

# Split the data into training and testing sets (80-20 split)
set.seed(1)  
i <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[i, ]
test_data <- data[-i, ]

# Split the training and testing sets into predictors (X) and response (y)
X_train <- train_data[, 1:4]
y_train <- train_data[, 5]
X_test <- test_data[, 1:4]
y_test <- test_data[, 5]

# Create a trainControl object for 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 2)

# Train the SVM model using cross-validation
svm_model_cv <- train(
  x = X_train,
  y = as.factor(y_train),
  method = "svmRadial",  # Use "svmRadial" for radial kernel
  trControl = ctrl,
  preProcess = c("center", "scale"),  # Center and scale the predictors
  tuneLength = 2  # Number of values to try for cost parameter C
)

# Print the cross-validated results
svm_model_cv

# Make predictions on the test set
svm_predictions_cv <- predict(svm_model_cv, newdata = X_test)

# Confusion matrix for SVM model
svm_conf_matrix_cv <- confusionMatrix(svm_predictions_cv, as.factor(y_test))
svm_conf_matrix_cv

# Accuracy with 10 fold Cross Validation: 1
# Misclassification with 10 fold Cross Validation: 0