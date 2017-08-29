

setwd("./GitHub/kaggleStuff/HousePrices")

library(data.table)

# Loading data
train_csv <- "./data/train.csv"
test_csv <- "./data/test.csv"
result_csv <- "./data/myResult.csv"

train_data <- read.csv(train_csv)
test_data <- read.csv(test_csv)

train_data <- data.table(train_data)
test_data <- data.table(test_data)




summary(train_data)
dim(train_data)

# LotArea (Grundstück SF) vs price
train_data_subset <- train_data[LotArea <= 50000,.(LotArea, SalePrice)]
plot(train_data_subset)

# Total SF against price
train_data_subset <- train_data[,.(TotalHouseSF = GrLivArea+X1stFlrSF+X2ndFlrSF, SalePrice)]
train_data_subset
plot(train_data_subset)

# Total SF vs LotArea
train_data_subset <- train_data[LotArea <= 50000,.(LotArea, TotalHouseSF = GrLivArea+X1stFlrSF+X2ndFlrSF)]
train_data_subset
plot(train_data_subset)


# Age of house when sold vs price
# today <- Sys.Date()
# year <- as.integer(format(today, format="%Y"))
train_data_subset <- train_data[,.(HouseAge = YrSold - YearBuilt,SalePrice)]
train_data_subset
plot(train_data_subset)


# PoolArea
train_data[PoolArea > 0,.N] # = 7 not really very differenciating


# Overall condition (rating low 1->10 high ) vs price
train_data_subset <- train_data[,.(OverallCond,SalePrice)]
train_data_subset
plot(train_data_subset)


# First try
train_data_subset <- train_data[, .(SalePrice, HouseAge = YrSold - YearBuilt, LotArea, TotalHouseSF = GrLivArea+X1stFlrSF+X2ndFlrSF, OverallCond)]
test_data_subset <- test_data[, .(Id, HouseAge = YrSold - YearBuilt, LotArea, TotalHouseSF = GrLivArea+X1stFlrSF+X2ndFlrSF, OverallCond)]

library(caret)

modelFit <- train(SalePrice~.,data=train_data_subset,method="lm")
modelFit$finalModel
predictions <- predict(modelFit,newdata=test_data_subset)
length(predictions)
result <- cbind(1461:(1460 + length(predictions)),predictions)
colnames(result) <- c("Id","SalePrice")
head(result)
write.csv(result,file=result_csv, quote=FALSE, row.names = FALSE)




# Alternative methods
X <- as.matrix(train_data[, .(bias = 1,HouseAge = YrSold - YearBuilt, LotArea, TotalHouseSF = GrLivArea+X1stFlrSF+X2ndFlrSF, OverallCond)])

y <- as.matrix(train_data[,SalePrice])

# Normal equation   (X(T) * X)^-1 * X(T)*y
solve(t(X) %*% X) %*% t(X) %*% y
# theta <- solve(t(X) %*% X) %*% t(X) %*% y

# Standard function R
lm_houseprice_1 <- lm(SalePrice~., train_data_subset)
summary(lm_houseprice_1)



scaled.X <- cbind(X[,1],scale(X[,2:5]))
scaled.X 
colMeans(scaled.X)


# Normal equation   (X(T) * X)^-1 * X(T)*y
solve(t(scaled.X) %*% scaled.X) %*% t(scaled.X) %*% y

LinReg_GradientDesc(scaled.X,y,0.001,0,100000,0.0001)




