library(dplyr)
library(httr)
library(caret)

history <- function(ticker, item, start_date, end_date, frequency){
  history_base <- "https://api.intrinio.com/historical_data?ticker=" 
  username <- "541af5920e9416769122ddaefc2f8ec8"
  password <- "10a220e8fb9b9ca41f8ec088b7762bdd"
  
  historical <- paste(history_base, ticker, "&item=", item, "&start_date=", start_date, "&end_date=", end_date, "&frequency=", frequency,sep="") 
  tp <- GET(historical, authenticate(username, password, type = "basic")) 
  z <- unlist(content(tp,"parsed"))
  
  n=length(z)
  if((n-5)<=0) {
    cat("n is",n,"\n")
    return("n becomes -ve")
  }
  b=as.data.frame(matrix(z[1:(n-5)],(n-5)/2, byrow = T))
  names(b)=names(z)[1:2]
  return(b)
}


## some extra features that could be used, we couldn't get this data
#"marketcap","pricetoearnings","pricetocurrentyearearnings",
#"pricetonextyearearnings","pricetorevenue","pricetocurrentyearrevenue","pricetonextyearrevenue",
#"pricetobook","dividendyield","evtocurrentyearrevenue","evtonextyearrevenue","nextyearearningsyield",
#"currentyearearningsyield","evtoebitda","evtonopat","evtoocf","evtorevenue","evtofcff")

factorList <- list(price=c("close_price","weightedavedilutedsharesos"),
growth=c("ocfqoqgrowth","revenueqoqgrowth",
"netincomeqoqgrowth","epsqoqgrowth","ebitdaqoqgrowth","ebitda",
"bookvaluepershare"),
profitability=c("nopatqoqgrowth","ebitdamargin",
"nopatmargin","operatingmargin","opextorevenue","sgaextorevenue",
"arturnover","invturnover"))

#AAPLData <- getHistoricFactors("AAPL",factorList,"2010-01-01","2016-10-01")
getHistoricFactors <- function(ticker, factors, start_date, end_date, frequency){
  result <- list()
  for (name in names(factors)) {
    cat("Getting factors for: ",name,"\n")
    for (factor in factors[[name]]){  
      historicalFactorData <- history(ticker,factor,start_date,end_date,frequency)  
      result[[factor]] <- historicalFactorData
    }
  }  
  return(result)
}

url <- "http://money.cnn.com/data/dow30/" 
doc.html = htmlTreeParse(url, useInternal = TRUE)
tables <- readHTMLTable(doc.html,stringsAsFactors=FALSE,which = 2)
tickers <- sapply(tables$Company, function(x) unlist(strsplit(x, "\\s+", fixed=F))[1])

getFactorsAllTickers <- function(ticker,factors,startDate,endDate){
  cat("Pulling historical factor data for: ",ticker,"\n")
  return(getHistoricFactors(ticker,factors,startDate,endDate))
}

dataAllTickers <- sapply(tickers,function(x) getHistoricFactors(x,factorList,"2010-01-01","2016-10-01"))

#AAPLData
mergedAAPLData <- Reduce(function(...) merge(...,by="data.date", all=T), AAPLData)

AAPLData <- getHistoricFactors("AAPL",factorList,"2010-01-01","2018-01-01","quarterly")
write.csv(AAPLData, file = "AAPLFundamentals_updated.csv")

AAPLRawData <- read.table("/Users/Harsh/Documents/R/Yewno/AAPLFundamentals_updated.csv", 
                 header = TRUE, sep = ",", stringsAsFactors=FALSE)

columnsToSubset <- c("close_price.data.date",
                     trimws(paste(unlist(factorList),"data.value",sep = "."), which = c("both")))

AAPLModifiedData <- AAPLRawData[columnsToSubset]


# sort in descending order
AAPLModifiedData <- AAPLModifiedData[with(AAPLModifiedData, order(close_price.data.date)), ]

# convert characters to numeric
colsToNumeric <- columnsToSubset[-c(1)]
AAPLModifiedData[,colsToNumeric] = apply(AAPLModifiedData[,colsToNumeric], 2, function(x) as.numeric(x))

# remove NA
#AAPLModifiedData <- AAPLModifiedData[complete.cases(AAPLModifiedData), ]

AAPLModifiedData <- cbind(AAPLModifiedData,
quarterlyReturn = 100*(lead(AAPLModifiedData$close_price.data.value,1) - AAPLModifiedData$close_price.data.value)/AAPLModifiedData$close_price.data.value,
priceToEarnings = AAPLModifiedData$close_price.data.value/(AAPLModifiedData$ebitda.data.value/AAPLModifiedData$weightedavedilutedsharesos.data.value),
priceToBook = AAPLModifiedData$close_price.data.value/AAPLModifiedData$bookvaluepershare.data.value
)

AAPLModifiedData <- AAPLModifiedData[complete.cases(AAPLModifiedData), ]

# training and test data
set.seed(123)
# 80% of the sample size
nRows <- nrow(AAPLModifiedData)
smp_size <- floor(0.8 * nRows)
train <- sample(seq_len(nRows), size = smp_size)
test <- -train


################################## Multiple Linear Regression###################################
AAPLData1 <- select(AAPLModifiedData,-close_price.data.date,-close_price.data.value,
            -weightedavedilutedsharesos.data.value,-bookvaluepershare.data.value,-ebitda.data.value)


regressionModel <- lm(quarterlyReturn ~ ., data = AAPLData1[train,])

############################## creating variable for classification problem####################
AAPLData2<- cbind(AAPLData1, 
                  isReturnPositive = sapply(AAPLData1$quarterlyReturn, function(x) 
                  {if (x>0) return(1) else return(0)}))  

AAPLData2 <- select(AAPLData2,-quarterlyReturn)

# training and test data
# 60% slipt for the classification problem
set.seed(124)
nRows <- nrow(AAPLModifiedData)
smp_size <- floor(0.6 * nRows)
train <- sample(seq_len(nRows), size = smp_size)
test <- -train

# use the same train and test spit as above
AAPLData2_train <- AAPLData2[train,] 
AAPLData2_test <- AAPLData2[test,]

###################################Logistic Regression Model################################
logisticModel <- glm(isReturnPositive ~.,family=binomial(link='logit'),data=AAPLData2_train)
summary(logisticModel)

# Analyzing training set classification accuracy

# calculate fitted probabilities
train_pred_prob <- predict(logisticModel, newdata = AAPLData2_train, type='response')

# classify returns as postive or not
train_pred <- ifelse(train_pred_prob > 0.5,1,0)

# training set accuracy
logisticModel_train_accuracy <- 100*mean(train_pred == AAPLData2_train$isReturnPositive)


# Analyzing test set classification accuracy

# calculate fitted probabilities
test_pred_prob <- predict(logisticModel, newdata = AAPLData2_test, type='response')

# classify returns as postive or not
test_pred <- ifelse(test_pred_prob > 0.5,TRUE,FALSE)

# test set accuracy
logisticModel_test_accuracy <- 100*mean(test_pred == AAPLData2_test$isReturnPositive)

##################################Support Vector Machine Model#####################################
AAPLData3<- cbind(AAPLData1, 
isReturnPositive = sapply(AAPLData1$quarterlyReturn, function(x) as.factor(x>0)))  

AAPLData3 <- select(AAPLData3,-quarterlyReturn)

AAPLData3_train <- AAPLData3[train,] 
AAPLData3_test <- AAPLData3[test,]

# setting trainControl() which species resampling method as repeated cross validation, 
# sets no of folds of cross validation as 10
# the cross validation is repeated 3 times
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# train SVM classifer
svmLinear <- train(isReturnPositive ~., data = AAPLData3_train, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

# train set accurancy
train_pred <- predict(svmLinear, newdata = AAPLData3_train)

confusionMatrix(train_pred, AAPLData3_train$isReturnPositive)

# test set accuracy
test_pred <- predict(svmLinear, newdata = AAPLData3_test)

confusionMatrix(test_pred, AAPLData3_test$isReturnPositive)

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svmLinearGrid <- train(isReturnPositive ~., data = AAPLData3_train, method = "svmLinear",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid,
                           tuneLength = 10)
test_pred_grid <- predict(svmLinearGrid, newdata = AAPLData3_test)
confusionMatrix(test_pred_grid, AAPLData3_test$isReturnPositive)


