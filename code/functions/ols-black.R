# Load Scaled Data

scaled_data = read.csv("../../data/scaled-predictors.csv")
scaled_data = na.omit(scaled_data)

################
# Black OLS
################

test_data = as.matrix(scaled_data[,-c(1,2,3, ncol(scaled_data))])

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
ols_mse_black = ols_mse



# OlS MODEL
x = test_data[,-r]
y = test_data[,r]

ols_model = lm(y ~ x)
ols_black_model = ols_model

# African American Residuals
resid = ols_model$residuals
ols_black = cbind(scaled_data, resid)

ols_black = ols_black[order(ols_black$resid, decreasing = FALSE),]

save(ols_black_model, ols_mse_black, file = "../../data/ols-black-model.RData")

write.csv(ols_black, "../../data/ranked-ols-black.csv")

