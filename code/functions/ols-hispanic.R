# Load Scaled Data

scaled_data = read.csv("../../data/scaled-predictors.csv")
scaled_data = na.omit(scaled_data)


################
# Hispanic OLS
################

test_data = as.matrix(scaled_data[,-c(1,2,3, ncol(scaled_data) - 1)])

################
# Testing MSE
################

set.seed(1)

# Setting Training and Testing Vectors
r = ncol(test_data)

train <- sample(1:length(test_data[,1]), 400)

train_set <- test_data[train,]
test_set <- test_data[-train,]

x_train <- as.matrix(train_set[,-r])
y_train <- as.matrix(train_set[,r])

x_test <- as.matrix(test_set[,-r])
y_test <- as.matrix(test_set[,r])


ols_test = lm(y_train ~ x_train)

preds <- predict(ols_test, as.data.frame(test_set))
ols_mse = mean((preds - y_test)^2)
ols_mse_hisp = ols_mse



# OlS MODEL
x = test_data[,-r]
y = test_data[,r]

ols_model = lm(y ~ x)
ols_hisp_model <- ols_model


## Residuals
resid = ols_model$residuals
ols_hisp = cbind(scaled_data, resid)


# In OLS High Risk Residuals Are negative
ols_hisp = ols_hisp[order(ols_hisp$resid, decreasing = FALSE),]

save(ols_hisp_model, ols_mse_hisp, file = "../../data/ols-hisp-model.RData")

write.csv(ols_hisp, "../../data/ranked-ols-hisp.csv")

