library(glmnet)

#####################
# ridge for Hispanics 
#####################


# Setting Seed for Reproducibility
set.seed(1)

scaled_data <- read.csv('../../data/scaled-predictors.csv')
scaled_data <- na.omit(scaled_data)

test_data = as.matrix(scaled_data[,-c(1,2,3,ncol(scaled_data) - 1)])

# Loading tests of predictor subset and obervation subset
r = ncol(test_data)

train <- sample(1:length(test_data[,1]), 400)

train_set <- test_data[train,]
test_set <- test_data[-train,]

x_train <- as.matrix(train_set[,-r])
y_train <- as.matrix(train_set[,r])

x_test <- as.matrix(test_set[,-r])
y_test <- as.matrix(test_set[,r])

lamb <- 10^seq(10, -2, length = 100)

# Preforming k = 10 Cross Variance
cv_ridge <- cv.glmnet(as.matrix(x_train), y_train, alpha = 0, intercept = FALSE,
                      standardize = FALSE, lambda = lamb)

# Minimum Lambda
ridge_lambda_min <- cv_ridge$lambda.min

# Saved ridge CV plot

plot(cv_ridge, main = "Minimum Lambda for ridge Model", xlab = "log(Lambda Value)")

# Use Cross Validated lambda value on the test sets to test MSE
y_hat <- predict(cv_ridge, as.matrix(x_test), s = ridge_lambda_min)
R_squared <- (y_test - y_hat)^2
ridge_mse <- sum(R_squared)/length(R_squared)

# Fitting on all Standardized Data Set Predictors
ridge_model <- glmnet(as.matrix(test_data[,-ncol(test_data)]), as.matrix(test_data[,ncol(test_data)]), standardize = FALSE,
                      intercept = FALSE, lambda = ridge_lambda_min, alpha = 0)

coefs = ridge_model$beta

x_vals = test_data[,-ncol(test_data)]
y_vals = test_data[,ncol(test_data)]


# Largest residuals

fitted_vals = apply(x_vals, 1, function(x) sum(x * coefs))
resid = (fitted_vals - y_vals)

ridge_hisp = cbind(scaled_data, resid)
ridge_hisp = ridge_hisp[order(ridge_hisp$resid, decreasing = TRUE),]


save(ridge_model, cv_ridge, ridge_lambda_min, ridge_mse, 
     file = "../../data/ridge-hisp-model.RData")

write.csv(ridge_hisp, "../../data/ranked-ridge-hispanic.csv")
