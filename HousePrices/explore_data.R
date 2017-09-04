###

setwd("./GitHub/kaggleStuff/HousePrices")

library(data.table)
library(ggplot2)

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

# Basement SF vs Total SF
train_data_subset <- train_data[,.(TotalHouseSF = GrLivArea+X1stFlrSF+X2ndFlrSF, TotalBsmtSF)]
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
train_data_subset <- train_data[, .(SalePrice, HouseAge = YrSold - YearBuilt, LotArea, TotalHouseSF = GrLivArea+X1stFlrSF+X2ndFlrSF, OverallCond, OverallQual,TotalBsmtSF, Fireplaces)]
test_data_subset <- test_data[, .(Id, HouseAge = YrSold - YearBuilt, LotArea, TotalHouseSF = GrLivArea+X1stFlrSF+X2ndFlrSF, OverallCond, OverallQual, TotalBsmtSF,Fireplaces)]


modelFit <- lm(SalePrice~. -1, train_data_subset) 
summary(modelFit)
predictions <- predict(modelFit,newdata=test_data_subset)
length(predictions)
result <- cbind(1461:(1460 + length(predictions)),predictions)
result <- data.table(result)
colnames(result) <- c("Id","SalePrice")
head(result)
write.csv(result,file=result_csv, quote=FALSE, row.names = FALSE)




# Alternative methods
X <- as.matrix(train_data[, .(bias = 1,HouseAge = YrSold - YearBuilt, LotArea, TotalHouseSF = GrLivArea+X1stFlrSF+X2ndFlrSF, OverallCond, OverallQual,TotalBsmtSF, Fireplaces)])

y <- as.matrix(train_data[,SalePrice])

# Normal equation   (X(T) * X)^-1 * X(T)*y
solve(t(X) %*% X) %*% t(X) %*% y
# theta <- solve(t(X) %*% X) %*% t(X) %*% y

# Standard function R
lm_houseprice_1 <- lm(SalePrice~., train_data_subset)
# lm_houseprice_1 <- lm(SalePrice~. -1, train_data_subset) # without intercept
summary(lm_houseprice_1)



scaled.X <- cbind(X[,1],scale(X[,2:5]))
scaled.X 
colMeans(scaled.X)


# Normal equation   (X(T) * X)^-1 * X(T)*y
solve(t(scaled.X) %*% scaled.X) %*% t(scaled.X) %*% y

LinReg_GradientDesc(scaled.X,y,0.001,0,100000,0.0001)

# Working with a learning curve
learning_data <- Learning_curve(X,y,50)
colnames(learning_data) <- c("Samples","RMSE", "type")

learning_data <- data.table(learning_data)
head(learning_data)


learningplot <- ggplot(learning_data, aes(x=Samples,y=RMSE, group=type, colour=type)) + geom_smooth() 
learningplot
