# ------------------------------- HEALTH DATA SET -------------------------------


library (glmnet)
library(readxl)
library(leaps)
library ( MASS )
library(car)
library(Metrics)
library(boot)
library(mgcv)
library(caret)
library(boot)
Health <- read_excel("Health.xlsx")
healthdata = Health


# -------- Model 1 - The Lasso ---------

set.seed(1)
# Fit a lasso model in order to predict X1 (Death rate) on the Health data.
x <- model.matrix (X1 ~ ., healthdata)[, -1]
y <- healthdata$X1
grid <- 10 ^ seq (10, -2, length = 100)
train <- sample (1: nrow (x), nrow (x) / 1.25)
test <- (-train)
y.test <- y[test]
lasso.mod <- glmnet (x[train , ], y[train], alpha = 1, lambda = grid)
plot (lasso.mod)

# Evaluate using K-Fold Cross-Validation
# 10 fold cross validation - select best lambda value and find MSE
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1, nfolds = 10)
bestlam <- cv.out$lambda.min
bestlam 
lasso.pred <- predict (lasso.mod, s = bestlam, newx = x[test,])
mse <- mean ((lasso.pred - y.test)^2) 
mse 
out <- glmnet (x, y, alpha = 1, lambda = grid)
lasso.coef <- predict (out , type = "coefficients", s = bestlam)[1:5, ]
lasso.coef 

# Evaluate using LOOCV
# Select best lambda value and find MSE
set.seed(1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1, nfolds = nrow(healthdata[train,]))
bestlam <- cv.out$lambda.min
bestlam 
lasso.pred <- predict (lasso.mod, s = bestlam, newx = x[test,])
mse <- mean ((lasso.pred - y.test)^2) 
mse
out <- glmnet (x, y, alpha = 1, lambda = grid)
lasso.coef <- predict (out , type = "coefficients", s = bestlam)[1:5, ]
lasso.coef 


# -------- Model 2 - Ridge Regression --------

set.seed(1)
x <- model.matrix (X1 ~ ., healthdata)[, -1]
y <- healthdata$X1
grid <- 10 ^ seq (10, -2, length = 100)
# Perform Ridge Regression
ridge.mod <- glmnet (x, y, alpha = 0, lambda = grid)
plot (ridge.mod)

# Evaluate using K-Fold Cross-Validation
# 10 fold cross validation- select best lambda value and find MSE
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 0, nfolds = 10)
bestlam <- cv.out$lambda.min
bestlam 
ridge.pred <- predict (ridge.mod, s = bestlam, newx = x[test,])
mse <- mean ((ridge.pred - y.test)^2)
mse
out <- glmnet (x, y, alpha = 0, lambda = grid)
ridge.coef <- predict (out , type = "coefficients", s = bestlam)[1:5, ]
ridge.coef 

# Evaluate using LOOCV
# Select best lambda value and find MSE
set.seed(1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 0, nfolds = nrow(healthdata))
bestlam <- cv.out$lambda.min
bestlam 
ridge.pred <- predict (ridge.mod, s = bestlam, newx = x[test,])
mse <- mean ((ridge.pred - y.test)^2)
mse 
out <- glmnet (x, y, alpha = 0, lambda = grid)
ridge.coef <- predict (out , type = "coefficients", s = bestlam)[1:5, ]
ridge.coef


# -------- Model 3 - Multiple Linear Regression --------

set.seed(1)
model <- lm(X1 ~ X2 + X3 + X4 + X5, data = healthdata)
summary(model) 
model1 <- lm(X1 ~ X4 + X5, data = healthdata)
summary(model1) 
model2 <- lm(X1 ~ X4 + I(X5^2) + X5, data = healthdata)
summary(model2) 

#P-Values
p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
p_values 
p_values1 <- summary(model1)$coefficients[, "Pr(>|t|)"]
p_values1 
p_values2 <- summary(model2)$coefficients[, "Pr(>|t|)"]
p_values2 

# Adjusted R square
set.seed(1)
adjusted_r_squared <- summary(model)$adj.r.squared
adjusted_r_squared
adjusted_r_squared1 <- summary(model1)$adj.r.squared
adjusted_r_squared1 
adjusted_r_squared2 <- summary(model2)$adj.r.squared
adjusted_r_squared2 

#Residual Sum of Squares(RSS)
set.seed(1)
rss <- sum(residuals(model)^2)
rss 
rss1 <- sum(residuals(model1)^2)
rss1 
rss2 <- sum(residuals(model2)^2)
rss2 

# MSE
set.seed(1)
mse <- mean(residuals(model)^2)
mse 
mse1 <- mean(residuals(model1)^2)
mse1 
mse2 <- mean(residuals(model2)^2)
mse2 

#F-value/ANOVA
set.seed(1)
anova_table <- anova(model)
f_value <- anova_table$"F value"[1]
f_value 
anova_table1 <- anova(model1)
f_value1 <- anova_table1$"F value"[1]
f_value1 
anova_table2 <- anova(model2)
f_value2 <- anova_table2$"F value"[1]
f_value2 

#AIC and BIC
aic <- AIC(model)
aic 
aic1 <- AIC(model1)
aic1
aic2 <- AIC(model2)
aic2 
bic <- BIC(model)
bic 
bic1 <- BIC(model1)
bic1 
bic2 <- BIC(model2)
bic2 


# -------- Model 4 - GAMs --------

set.seed(1)
gam.m1 <- gam(X1 ~ s(X2) + s(X3) + s(X4) + s(X5), data = healthdata)
par(mfrow = c(1, 4))
plot(gam.m1, se = TRUE, col = "blue")
summary(gam.m1)
pred.m1 <- predict(gam.m1, newdata = healthdata)
rss.m1 <- sum((healthdata$X1 - pred.m1)^2)
rss.m1
mse.m1 <- mean((healthdata$X1 - pred.m1)^2)
mse.m1

gam.m2 <- gam(X1 ~ s(X2) + s(X3) + te(X4, X5), data = healthdata)
par(mfrow = c(1, 4))
plot(gam.m2, se = TRUE, col = "blue")
summary(gam.m2)
pred.m2 <- predict(gam.m2, newdata = healthdata)
rss.m2 <- sum((healthdata$X1 - pred.m2)^2)
rss.m2
mse.m2 <- mean((healthdata$X1 - pred.m2)^2)
mse.m2

gam.m3 <- gam(X1 ~  te(X2, X3) + te(X4, X5), data = healthdata)
par(mfrow = c(1, 4))
plot(gam.m3, se = TRUE, col = "blue")
summary(gam.m3)
pred.m3 <- predict(gam.m3, newdata = healthdata)
rss.m3 <- sum((healthdata$X1 - pred.m3)^2)
rss.m3
mse.m3 <- mean((healthdata$X1 - pred.m3)^2)
mse.m3

anova(gam.m1, gam.m2, gam.m3, test = "F")


# -------- Model 5 - Polynomial Regression --------

set.seed(1)
X <- healthdata[, -1] 
Y <- healthdata$X1
attach(healthdata)
train <- sample(1:nrow(healthdata),0.9*nrow(healthdata))
test <- (-train)
healthdata_train <- healthdata[train,]
healthdata_test <- healthdata[-train,]

degree <- 2
fit <- lm(X1 ~ poly(X2, degree) + poly(X3, degree) + poly(X4, degree) + poly(X5,degree), data = healthdata_train)
summary(fit)
coef(fit)
pred =  predict(fit, newdata = healthdata_test)
length(pred)
mse <- mean((Y[test]-pred)^2)
mse


# -------- Model 6 - Model selection --------

set.seed(1)
attach(healthdata)
# Fit model for all the variables and check summary
regfit.full <- regsubsets(X1~., healthdata)
reg.summary <- summary(regfit.full)
reg.summary

which.max(reg.summary$adjr2)
which.min(reg.summary$rss)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

num_of_coefficients <- which.max(reg.summary$adjr2)
coef(regfit.full,num_of_coefficients)

# Do forward and backward selection on the model
set.seed(1)
regfit.fwd <- regsubsets(X1~., data = healthdata, method="forward")
reg.summary <- summary(regfit.fwd)
which.max(reg.summary$adjr2)
which.min(reg.summary$rss)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

set.seed(1)
regfit.bwd <- regsubsets(X1~., data = healthdata, method = "backward")
reg.summary <- summary(regfit.bwd)
which.max(reg.summary$adjr2)
which.min(reg.summary$rss)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

num_of_coefficients <- which.max(reg.summary$adjr2)
coef(regfit.bwd,num_of_coefficients)

# Split into training and test set
set.seed(1)
train <- sample(1:nrow(healthdata),0.7*nrow(healthdata))
test <- (-train)
Health_train <- healthdata[train,]
Health_test <- healthdata[-train,]
# Since best subset and backward selection gave the same set of predictors (X2,
# X4 and X5),use it to calculate cross validation errors with 5 fold and 10 fold
regfit.best <- regsubsets(X1~X2+X4+X5,data = healthdata[train,])
test.mat<-model.matrix (X1~X2+X4+X5, data = healthdata[test,])
val.errors <- rep (NA, num_of_coefficients)
for (i in 1:num_of_coefficients) {
  coefi <- coef(regfit.best , id = i)
  pred <- test.mat[,names(coefi)] %*% coefi
  val.errors[i] <- mean((healthdata$X1[test] - pred)^2)
}
val.errors
num_of_predictors_mse <- which.min(val.errors)
num_of_predictors_mse

coef(regfit.best , num_of_predictors_mse)

# Predict function to predict the output X1 for k fold

predict.regsubsets <- function(object,newdata,id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

# Model evaluation with 10 fold
k <- 10
n <- nrow(Health)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, num_of_coefficients,dimnames = list (NULL , 
                                                                paste (1:num_of_coefficients)))

for  (j in 1:k){
  best.fit <- regsubsets(X1~X2+X4+X5,data=healthdata[folds!=j,])
  for (i in 1:num_of_coefficients){
    pred <- predict(regfit.best,healthdata[folds==j,],id=i)
    cv.errors[j,i] <- mean((healthdata$X1[folds==j]-pred)^2)
  }
}
cv.errors

# Display the cv errors matrix and the mean of errors
mean.cv.errors <- apply(cv.errors , 2, mean)
mean.cv.errors
min(mean.cv.errors)

# Model evaluation with 5 fold
k <- 5
n <- nrow(healthdata)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, num_of_coefficients,dimnames = list (NULL , paste (1:num_of_coefficients)))

for  (j in 1:k){
  best.fit <- regsubsets(X1~.,data=healthdata[folds!=j,])
  for (i in 1:num_of_coefficients){
    pred <- predict(regfit.best,healthdata[folds==j,],id=i)
    cv.errors[j,i] <- mean((healthdata$X1[folds==j]-pred)^2)
  }
}
cv.errors
mean.cv.errors <- apply (cv.errors , 2, mean)
mean.cv.errors
min(mean.cv.errors)




# ------------------------------- REAL ESTATE VALUATION -------------------------


RealEstate_Data <- read_excel("RealEstate.xlsx")
attach(RealEstate_Data)
# Remove the index column
RealEstate_Data$No <- NULL
head(RealEstate_Data)


# -------- Model 1 - The Lasso ---------

set.seed (1)
x <- model.matrix (Y ~ ., RealEstate_Data)[, -1]
y <- RealEstate_Data$Y
grid <- 10 ^ seq (10, -2, length = 100)
# Perform Lasso Regression
train <- sample (1: nrow (x), nrow (x) / 1.25)
test <- (-train)
y.test <- y[test]
lasso.mod <- glmnet (x[train,], y[train], alpha = 1, lambda = grid)
plot (lasso.mod)

# Evaluate using K-Fold Cross-Validation
# 10 fold cross validation- select best lambda value and find MSE
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1, nfolds = 10)
bestlam <- cv.out$lambda.min
bestlam 
lasso.pred <- predict (lasso.mod, s = bestlam, newx = x[test,])
mse <- mean ((lasso.pred - y.test)^2)
mse 
out <- glmnet (x, y, alpha = 0, lambda = grid)
lasso.coef <- predict (out , type = "coefficients", s = bestlam)
lasso.coef 

# Evaluate using LOOCV
# Select best lambda value and find MSE
set.seed (1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1, nfolds = nrow(RealEstate_Data))
bestlam <- cv.out$lambda.min
bestlam 
lasso.pred <- predict (lasso.mod, s = bestlam, newx = x[test,])
mse <- mean ((lasso.pred - y.test)^2)
mse 
out <- glmnet (x, y, alpha = 1, lambda = grid)
lasso.coef <- predict (out , type = "coefficients", s = bestlam)
lasso.coef 


# -------- Model 2 - Ridge Regression --------

set.seed (1)
x <- model.matrix (Y ~ ., RealEstate_Data)[, -1]
y <- RealEstate_Data$Y
grid <- 10 ^ seq (10, -2, length = 100)
# Perform Ridge Regression
train <- sample (1: nrow (x), nrow (x) / 1.25)
test <- (-train)
y.test <- y[test]
ridge.mod <- glmnet (x[train , ], y[train], alpha = 0, lambda = grid)
plot (ridge.mod)

# Evaluate using K-Fold Cross-Validation
# 10 fold cross validation- select best lambda value and find MSE
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 0, nfolds = 10)
bestlam <- cv.out$lambda.min
bestlam 
ridge.pred <- predict (ridge.mod, s = bestlam, newx = x[test,])
mse <- mean ((ridge.pred - y.test)^2)
mse
out <- glmnet (x, y, alpha = 0, lambda = grid)
ridge.coef <- predict (out , type = "coefficients", s = bestlam)
ridge.coef 

# Evaluate using LOOCV
# Select best lambda value and find MSE
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 0, nfolds = nrow(RealEstate_Data[train,]))
bestlam <- cv.out$lambda.min
bestlam 
ridge.pred <- predict (ridge.mod, s = bestlam, newx = x[test,])
mse <- mean ((ridge.pred - y.test)^2)
mse 
out <- glmnet (x, y, alpha = 0, lambda = grid)
ridge.coef <- predict (out , type = "coefficients", s = bestlam)
ridge.coef 

# Correlation
plot(RealEstate_Data$X1, RealEstate_Data$Y, main = "Scatter Plot with Lowess Smoother", xlab = "X1", ylab = "Y")
lines(lowess(RealEstate_Data$X1, RealEstate_Data$Y), col = "blue", lwd = 2)
cor_value <- cor(RealEstate_Data$X1, RealEstate_Data$Y)
cor_value
text(quantile(RealEstate_Data$X1, 0.9), quantile(RealEstate_Data$Y, 0.1), paste("Correlation =", round(cor_value, 2)), adj = c(0, 0))
grid()

plot(RealEstate_Data$X2, RealEstate_Data$Y, main = "Scatter Plot with Lowess Smoother", xlab = "X2", ylab = "Y")
lines(lowess(RealEstate_Data$X2, RealEstate_Data$Y), col = "blue", lwd = 2)
cor_value <- cor(RealEstate_Data$X2, RealEstate_Data$Y)
cor_value
text(quantile(RealEstate_Data$X2, 0.9), quantile(RealEstate_Data$Y, 0.1), paste("Correlation =", round(cor_value, 2)), adj = c(0, 0))
grid()
cor_value

plot(RealEstate_Data$X3, RealEstate_Data$Y, main = "Scatter Plot with Lowess Smoother", xlab = "X3", ylab = "Y")
lines(lowess(RealEstate_Data$X3, RealEstate_Data$Y), col = "blue", lwd = 2)
cor_value <- cor(RealEstate_Data$X3, RealEstate_Data$Y)
cor_value
text(quantile(RealEstate_Data$X3, 0.9), quantile(RealEstate_Data$Y, 0.1), paste("Correlation =", round(cor_value, 2)), adj = c(0, 0))
grid()

plot(RealEstate_Data$X4, RealEstate_Data$Y, main = "Scatter Plot with Lowess Smoother", xlab = "X4", ylab = "Y")
lines(lowess(RealEstate_Data$X4, RealEstate_Data$Y), col = "blue", lwd = 2)
cor_value <- cor(RealEstate_Data$X4, RealEstate_Data$Y)
cor_value
text(quantile(RealEstate_Data$X4, 0.9), quantile(RealEstate_Data$Y, 0.1), paste("Correlation =", round(cor_value, 2)), adj = c(0, 0))
grid()

plot(RealEstate_Data$X5, RealEstate_Data$Y, main = "Scatter Plot with Lowess Smoother", xlab = "X5", ylab = "Y")
lines(lowess(RealEstate_Data$X5, RealEstate_Data$Y), col = "blue", lwd = 2)
cor_value <- cor(RealEstate_Data$X5, RealEstate_Data$Y)
cor_value
text(quantile(RealEstate_Data$X5, 0.9), quantile(RealEstate_Data$Y, 0.1), paste("Correlation =", round(cor_value, 2)), adj = c(0, 0))
grid()


# -------- Model 3 - Multiple Linear Regression --------

set.seed(1)
model <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = RealEstate_Data)
summary(model) 
adjusted_r_squared <- summary(model)$adj.r.squared
adjusted_r_squared

model1 <- lm(Y ~ X2 + X3 + X4, data = RealEstate_Data)
summary(model1)
adjusted_r_squared1 <- summary(model1)$adj.r.squared
adjusted_r_squared1

model2 <- lm(Y ~ X2 + I(X6^2) + I(X4^2) + I(X5^2), data = RealEstate_Data)
summary(model2)
adjusted_r_squared2 <- summary(model2)$adj.r.squared
adjusted_r_squared2

model3 <- lm(Y ~ X2 + X3 + I(X6^2) + I(X4^2) + I(X5^2), data = RealEstate_Data)
summary(model3)
adjusted_r_squared3 <- summary(model3)$adj.r.squared
adjusted_r_squared3

# P-Value
p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
p_values 
p_values1 <- summary(model1)$coefficients[, "Pr(>|t|)"]
p_values1 
p_values2 <- summary(model2)$coefficients[, "Pr(>|t|)"]
p_values2 
p_values3 <- summary(model3)$coefficients[, "Pr(>|t|)"]
p_values3 

# Residual Sum of Squares(RSS)
rss <- sum(residuals(model)^2)
rss 
rss1 <- sum(residuals(model1)^2)
rss1 
rss2 <- sum(residuals(model2)^2)
rss2 
rss3 <- sum(residuals(model3)^2)
rss3

#Mean Squared Error(MSE)
mse <- mean(residuals(model)^2)
mse 
mse1 <- mean(residuals(model1)^2)
mse1 
mse2 <- mean(residuals(model2)^2)
mse2 
mse3 <- mean(residuals(model3)^2)
mse3

# F-value/ANOVA
anova_table <- anova(model)
f_value <- anova_table$"F value"[1]
f_value 
anova_table1 <- anova(model1)
f_value1 <- anova_table1$"F value"[1]
f_value1 
anova_table2 <- anova(model2)
f_value2 <- anova_table2$"F value"[1]
f_value2 
anova_table3 <- anova(model3)
f_value3 <- anova_table3$"F value"[1]
f_value3 

# AIC and BIC
aic <- AIC(model)
aic 
aic1 <- AIC(model1)
aic1 
aic2 <- AIC(model2)
aic2 
aic3 <- AIC(model3)
aic3 
bic <- BIC(model)
bic 
bic1 <- BIC(model1)
bic1 
bic2 <- BIC(model2)
bic2 
bic3 <- BIC(model3)
bic3 


# -------- Model 4 - Polynomial Regression --------

set.seed(3)
X <- RealEstate_Data[, -1] 
Y <- RealEstate_Data$Y
attach(RealEstate_Data)
train <- sample(1:nrow(RealEstate_Data),0.9*nrow(RealEstate_Data))
test <- (-train)
Realestate_train <- RealEstate_Data[train,]
Realestate_test <- RealEstate_Data[-train,]

degree <- 2
fit <- lm(Y ~ poly(X1, degree) + poly(X2, degree) + poly(X3, degree) + poly(X4, degree) + poly(X5, degree) + poly(X6, degree), data = Realestate_train)
summary(fit)
coef(fit)
pred =  predict(fit, newdata = Realestate_test)
mse <- mean((Y[test] - pred)^2)
mse


# -------- Model 5 - Model selection --------

set.seed(1)
# To verify that there are no missing values
sum (is.na(RealEstate_Data))
# Fit model for all the variables and check summary
regfit.full <- regsubsets(Y~., RealEstate_Data)
reg.summary <- summary(regfit.full)
# The summary suggests that a single variable model has X3 
reg.summary
num_of_coefficients <- which.max(reg.summary$adjr2)
num_of_coefficients

which.min(reg.summary$rss)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

# Display the coefficients for the best subset model
coef(regfit.full,num_of_coefficients)

# Do forward and backward selection on the model
regfit.fwd <- regsubsets(Y~., data = RealEstate_Data, method="forward")
reg.summary <- summary(regfit.fwd)
reg.summary
num_of_coefficients <- which.max(reg.summary$adjr2)
num_of_coefficients
which.min(reg.summary$rss)
which.min(reg.summary$cp)
which.min(reg.summary$bic)
coef(regfit.fwd,num_of_coefficients)

regfit.bwd <- regsubsets(Y~., data = RealEstate_Data, method = "backward")
reg.summary <- summary(regfit.bwd)
reg.summary
num_of_coefficients <- which.max(reg.summary$adjr2)
num_of_coefficients
which.min(reg.summary$rss)
which.min(reg.summary$cp)
which.min(reg.summary$bic)
coef(regfit.bwd,num_of_coefficients)


# Split into training and test set
train <- sample(1:nrow(RealEstate_Data),0.7*nrow(RealEstate_Data))
test <- (-train)
RealEstate_train <- RealEstate_Data[train,]
RealEstate_test <- RealEstate_Data[-train,]


set.seed(1)
regfit.best <- regsubsets(Y~X1+X2+X3+X4+X5,data = RealEstate_Data[train,])
test.mat<-model.matrix (Y~X1+X2+X3+X4+X5, data = RealEstate_Data[test,])
val.errors <- rep (NA, num_of_coefficients)
for (i in 1:num_of_coefficients) {
  coefi <- coef(regfit.best , id = i)
  pred <- test.mat[,names(coefi)] %*% coefi
  val.errors[i] <- mean((RealEstate_Data$Y[test] - pred)^2)
}
val.errors
num_of_predictors_mse <- which.min(val.errors)
num_of_predictors_mse

coef(regfit.best , num_of_predictors_mse)


# Predict function to predict the output Y for k fold
set.seed(1)
predict.regsubsets <- function(object,newdata,id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

# Model evaluation with 10 fold
k <- 10
n <- nrow(RealEstate_Data)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, num_of_coefficients,dimnames = list (NULL , paste (1:num_of_coefficients)))

for  (j in 1:k){
  best.fit <- regsubsets(Y~X2+X4+X5,data=RealEstate_Data[folds!=j,])
  for (i in 1:num_of_coefficients){
    pred <- predict(regfit.best,RealEstate_Data[folds==j,],id=i)
    cv.errors[j,i] <- mean((RealEstate_Data$Y[folds==j]-pred)^2)
  }
}
cv.errors
# Display the cv errors matrix
mean.cv.errors <- apply(cv.errors , 2, mean)
mean.cv.errors
min(mean.cv.errors)

# Model evaluation with 5 fold
set.seed(1)
k <- 5
n <- nrow(RealEstate_Data)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, num_of_coefficients,dimnames = list (NULL , paste (1:num_of_coefficients)))

for  (j in 1:k){
  best.fit <- regsubsets(Y~.,data=RealEstate_Data[folds!=j,])
  for (i in 1:num_of_coefficients){
    pred <- predict(regfit.best,RealEstate_Data[folds==j,],id=i)
    cv.errors[j,i] <- mean((RealEstate_Data$Y[folds==j]-pred)^2)
  }
}
cv.errors
mean.cv.errors <- apply (cv.errors , 2, mean)
mean.cv.errors
min(mean.cv.errors)


# -------- Model 6 - GAMs --------

library(readxl)
real_estate <- read_excel("RealEstate.xlsx")
summary(real_estate)

pairs(real_estate[, sapply(real_estate, is.numeric)], main = "Pairs Plot")

# Correlation matrix
library(corrplot)
cor_matrix <- cor(real_estate)
cor_matrix
corrplot(cor_matrix, method = "circle", tl.col = "black",
         tl.srt = 45, addCoef.col = "black")

#GAMs
real_estate <- as.data.frame(real_estate)
# Removing the "No" column
real_estate <- real_estate[, -1]
# Rename columns
colnames(real_estate) <- c("X1", "X2", "X3", "X4", "X5", "X6", "Y")

gam.m1 <- gam(Y ~ s(X2) + s(log(X3)) + s(X5) + s(X6), data = real_estate)
par(mfrow = c(1, 4))
plot(gam.m1, se = TRUE, col = "blue")
summary(gam.m1)
pred.m1 <- predict(gam.m1, newdata = real_estate)
rss.m1 <- sum((real_estate$Y - pred.m1)^2)
rss.m1
mse.m1 <- mean((real_estate$Y - pred.m1)^2)
mse.m1

gam.m2 <- gam(Y ~ s(X2) + s(X3) + s(sqrt(X5)) + s(X6, X2), data = real_estate)
par(mfrow = c(1, 4))
plot(gam.m2, se = TRUE, col = "blue")
summary(gam.m2)
pred.m2 <- predict(gam.m2, newdata = real_estate)
rss.m2 <- sum((real_estate$Y - pred.m2)^2)
rss.m2
mse.m2 <- mean((real_estate$Y - pred.m2)^2)
mse.m2

gam.m3 <- gam(Y ~ s(X2) + s(log(X3)) + s(sqrt(X5)) + s(X6, X2), data = real_estate)
par(mfrow = c(1, 4))
plot(gam.m3, se = TRUE, col = "blue")
summary(gam.m3)
pred.m3 <- predict(gam.m3, newdata = real_estate)
rss.m3 <- sum((real_estate$Y - pred.m3)^2)
rss.m3
mse.m3 <- mean((real_estate$Y - pred.m3)^2)
mse.m3
anova(gam.m1, gam.m2, gam.m3, test = "F")

