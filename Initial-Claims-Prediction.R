library(glmnet)
library(ggplot2)

myData <- read.csv(file="/Users/Harsh/Documents/R/Yewno/Initial Claims Prediction/google correlate-Initial_Claims.csv", header=TRUE, stringsAsFactors=FALSE,
                   fileEncoding="latin1")
myData $Date <- as.Date( myData$Date, '%m/%d/%y')
ggplot( data = myData, aes( Date, Initial.Claims )) + geom_line()

## define predictor and independent variable
x <- model.matrix(Initial.Claims ~ . - Date, myData)[,-1]
y <- myData$Initial.Claims

## set the seed to make your partition reproductible
set.seed(123)

## 50% of the sample size
nRows <- nrow(myData)
smp_size <- floor(0.50 * nRows)
train <- sample(seq_len(nRows), size = smp_size)
test <- -train

## training set
xtrain <- x[train,]
ytrain <- y[train]
## test set
xtest <- x[test,]
ytest <- y[test]

## first model
myData <- subset(myData, select = -Date)
ols.model <- lm(Initial.Claims ~ ., data = myData, subset = train)
summary(ols.model)

## second try
ols.model1 <- lm(Initial.Claims ~ i.need.a.job, data = myData, subset = train)
summary(ols.model1)


## final model
ols.model2 <- lm(Initial.Claims ~ i.need.a.job + georgia.unemployment + loan.modifications + craigslist.helper, data = myData, subset = train)
summary(ols.model2)

lambda <- 10^seq(10, -2, length = ncol(x))


## Lasso: We did try it, but didn't report this as our OLS model has no signs of overfitting 
lasso.model <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
bestLamda <- cv.out$lambda.min
lasso.predict.train <- predict(lasso.model, s = bestLamda, newx = x[train,])
lasso.predict.test <- predict(lasso.model, s = bestLamda, newx = x[test,])

sqrt(mean((y[train]-ols.predict.train)^2))
sqrt(mean((y[test]-ols.predict.test)^2))z

lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlam)[1:10,]
plot(lasso.predict.test-y[test],y[test])



