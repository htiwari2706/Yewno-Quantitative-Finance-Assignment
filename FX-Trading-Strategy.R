Quandl.api_key("HD_sSceQtLjyiEa9LCJM")
AUDUSD <- Quandl("FRED/DEXUSAL", type="xts", start_date='2017-01-01')
NZDUSD <- Quandl("FRED/DEXUSNZ", type="xts", start_date='2017-01-01')
USDMXN <- Quandl("FRED/DEXMXUS", type="xts", start_date='2017-01-01')
USDCAD <- Quandl("FRED/DEXCAUS", type="xts", start_date='2017-01-01')
Oil <- Quandl("FRED/DCOILWTICO", type="xts", start_date='2017-01-01')
getSymbols("DXY",src="yahoo", startDate = '2017-01-01')


# log normalization of the price data
AUDUSDFactors <- na.omit((merge(AUDUSD,NZDUSD,USDMXN,USDCAD,Oil,DXY$DXY.Close)))

AUDUSDFactorModel <- lm(AUDUSD ~ .,data=log(AUDUSDFactors))

adf.test(AUDUSDFactorModel$residuals)
plot(AUDUSDFactorModel$residuals)

# testing residuals for stationarity
JOTest=ca.jo(AUDUSDFactors, type="trace", K=2, ecdet="none", spec="longrun")
summary(JOTest)

tradeSignal <- function(data, coeff, spread){
  
  N <- nrow(testData)
  sdev <-sd(spread)
  zeroprices <- rep(0,ncol(data))
  factorCoeff <- coeff[2:length(coeff)]
  prices <- list()
  exposures <- list()
  costOfTrade <- vector(mode="numeric", length=0)
  returns <- list()
  spreadPosition <- 'no'   
  countTrades <- 0
  
  for (i in 1:N) {
    
    basespreadprices <- c(-1,1,1,1,1,1)
    if(spreadPosition == 'no') {
      
      if(spread[i] > 1.5*sdev){
        # go short the spread
        cat("Going short the spread at index level", i, " and spread level ", spread[i], "\n")
        countTrades <- countTrades + 1
        prices[[countTrades]] <- data[i,] 
        exposures[[countTrades]] <- c(1,-factorCoeff) * c(1/coredata(data[i,'AUDUSD']),1/coredata(data[i,'NZDUSD']),1,1,1,1)
        costOfTrade[countTrades] <- abs(sum(c(1,-factorCoeff)*c(1,1,1,1,coredata(data[i,'Oil']),coredata(data[i,'DXY.Close']))))
        spreadPosition <- 'short'
      }
      else if(spread[i] < -1.5*sdev){
        # go long the spread
        cat("Going long the spread at index level", i, " and spread level ", spread[i], "\n")
        countTrades <- countTrades + 1
        prices[[countTrades]] <- data[i,] 
        exposures[[countTrades]] <- c(1,-factorCoeff) * c(1/coredata(data[i,'AUDUSD']),1/coredata(data[i,'NZDUSD']),1,1,1,1)
        costOfTrade[countTrades] <- abs(sum(c(1,-factorCoeff)*c(1,1,1,1,coredata(data[i,'Oil']),coredata(data[i,'DXY.Close']))))
        spreadPosition <- 'long'
      }
      
      next
    }
    
    if(spreadPosition == 'short') {
      
      if(spread[i] <= 0.1*sdev){
        # close the spreadPosition 
        cat("Closing long spreadPosition at index level", i, " and spread level ", spread[i], "\n")
        returns[[countTrades]] <- exposures[[countTrades]]*(coredata(prices[[countTrades]]) - coredata(data[i,]))*c(1,1,1/coredata(data[i,'USDMXN']),1/coredata(data[i,'USDCAD']),1,1) 
        spreadPosition <- 'no'
      }
      else if(spread[i] > 2.5*sdev){
        # stop loss
        cat("Stop Loss Trigger: closing long spreadPosition at index level", i, " and spread level ", spread[i], "\n")
        returns[[countTrades]] <- exposures[[countTrades]]*(coredata(prices[[countTrades]]) - coredata(data[i,]))*c(1,1,1/coredata(data[i,'USDMXN']),1/coredata(data[i,'USDCAD']),1,1)
        spreadPosition <- 'no'
      }
      
      next
    }
    
    
    if(spreadPosition == 'long') {
      
      if(spread[i] >= -0.1*sdev){
        # close the spreadPosition 
        cat("Closing long spreadPosition at index level", i, " and spread level ", spread[i], "\n")
        returns[[countTrades]] <- exposures[[countTrades]]*(coredata(data[i,]) - coredata(prices[[countTrades]]))*c(1,1,1/coredata(data[i,'USDMXN']),1/coredata(data[i,'USDCAD']),1,1)
        spreadPosition <- 'no'
        
      }
      else if(spread[i] < -2.5*sdev){
        # stop loss
        cat("Stop Loss Trigger: closing long spreadPosition at index level", i, " and spread level ", spread[i], "\n")
        returns[[countTrades]] <- exposures[[countTrades]]*(coredata(data[i,]) - coredata(prices[[countTrades]]))*c(1,1,1/coredata(data[i,'USDMXN']),1/coredata(data[i,'USDCAD']),1,1)
        spreadPosition <- 'no'
      }
      
      next
    }
  }
  
  # close any open spreadprices on the last day of the test period
  if(spreadPosition != 'close') {
    cat("End of Trading Period: Closing open spreadPosition at index level", i, " and spread level ", spread[i], "\n")
    if(spreadPosition == "long") returns[[countTrades]] <- exposures[[countTrades]]*(coredata(data[N,]) - coredata(prices[[countTrades]]))*c(1,1,1/coredata(data[i,'USDMXN']),1/coredata(data[i,'USDCAD']),1,1)
    else returns[[countTrades]] <- exposures[[countTrades]]*(coredata(prices[[countTrades]]) - coredata(data[N,]))*c(1,1,1/coredata(data[i,'USDMXN']),1/coredata(data[i,'USDCAD']),1,1)
  }
  
  allReturns <- do.call(rbind, returns)
  returnsTrades <- apply(allReturns,1,sum)/costOfTrade
  return(100*((prod(sapply(returnsTrades,function(x) (1+x)))) - 1))

  
  pricesMatrix <- do.call(rbind, prices)
  exposuresMatrix<- do.call(rbind, exposures)
  movementsCapture <- data * pricesMatrix
  
  markToMarketUSD <- matrix(c(1,1,1/data[N,'USDMXN'],1/data[N,'USDCAD'],1,1))
  
  pnLByFactor <- apply(markToMarketUSD, 2, sum)
  # net USD return
  return(100*sum(pnLByFactor)/abs(sum(c(1,-factorCoeff))))

  
}

train <- 60
test <- 30
noOfSamples<-nrow(AUDUSDFactors)		#NO OF DATA POINTS
noOfPeriods<-floor((noOfSamples-train)/test)

osReturns <- vector(mode="numeric", length=0)

for( i in 0:(noOfPeriods-1) ) {
  cat("Starting backtesting period ", i+1, "\n")
  trainData <- AUDUSDFactors[(1 + i*test) : (train + i*test),]
  # log scaling is necessary to interpret spread reversion as returns
  linearModel <- lm(AUDUSD ~ .,data = log(trainData)) 
  testData <- AUDUSDFactors[(train + i*test + 1) : (train + test + test*i),]
  meanRevertingspread <- apply(log(testData)*
                                 c(1,-linearModel$coefficients[2:length(linearModel$coefficients)]) - linearModel$coefficients[1],
                               1,sum)
  osReturns[i+1] <- tradeSignal(testData, linearModel$coefficients, meanRevertingspread)
}


