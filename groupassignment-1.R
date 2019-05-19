#https://solvemprobler.com/blog/2015/12/19/calculating-stocks-beta-using-r/
#https://www.datacamp.com/community/tutorials/linear-regression-R

library(dplyr)
library(ggplot2)

####### Calculating Returns ######
#SHARE1
share1<-read.csv("/Users/nirajkulkarni/Desktop/Niraj/ISB-CBA/Term-1/Business-Fundamentals/TATAMOTORS.NS.csv",stringsAsFactors = FALSE , header = TRUE)
share1<-share1[!(share1$Adj.Close=='null' | is.na(share1$Adj.Close)) ,] #removing null
maxDate1<-share1 %>% head(1) #%>% arrange(share1$Date) 
minDate1<-share1 %>% tail(1) #%>% arrange(share1$Date) 
valShare1_1<-as.numeric(as.character(maxDate1$Adj.Close))
valShare1_2<-as.numeric(as.character(minDate1$Adj.Close))

share1_ret<-((valShare1_1-valShare1_2)/valShare1_1)*100
share1_ret #58.36038
plot(x=share1$Adj.Close,ylab = 'Adjusted Close',type = 'line',col='blue',main = 'TataMotors Monthly Closing')

#SHARE2
share2<-read.csv("/Users/nirajkulkarni/Desktop/Niraj/ISB-CBA/Term-1/Business-Fundamentals/SBIN.NS.csv",stringsAsFactors = FALSE , header = TRUE)
share2<-share2[!(share2$Adj.Close=='null' | is.na(share2$Adj.Close)) ,] #removing null
maxDate2<-share2 %>% head(1) 
minDate2<-share2 %>% tail(1)
valShare2_1<-as.numeric(as.character(maxDate2$Adj.Close))
valShare2_2<-as.numeric(as.character(minDate2$Adj.Close))
share2_ret<-((valShare2_1-valShare2_2)/valShare2_1)*100

share2_ret  #-23.02963
plot(x=share2$Adj.Close,ylab = 'Adjusted Close',type = 'line',col='blue',main = 'SBI Monthly Closing')

#NIFTY
nifty<-read.csv("/Users/nirajkulkarni/Desktop/Niraj/ISB-CBA/Term-1/Business-Fundamentals/Nifty50-NSEI.csv",stringsAsFactors = FALSE , header = TRUE)
nifty<-nifty[!(nifty$Adj.Close=='null' | is.na(nifty$Adj.Close)) ,] #removing null
maxDateN<-nifty %>% arrange(nifty$Date) %>% head(1) 
minDateN<-nifty %>% arrange(nifty$Date) %>% tail(1)
valNifty_1<-as.numeric(as.character(maxDateN$Adj.Close))
valNifty_2<-as.numeric(as.character(minDateN$Adj.Close))
nifty_ret<-((valNifty_1-valNifty_2)/valNifty_1)*100

nifty_ret   #49.87026
plot(x=nifty$Adj.Close,ylab = 'Adjusted Close',type = 'line',col='blue',main = 'Nifty Monthly Closing')

####### Linear Regression ######
regression_data<-data.frame("share1_close" = as.numeric(as.character(share1$Adj.Close)) ,
                            "share2_close" = as.numeric(as.character(share2$Adj.Close)),
                            "nifty_close"  = as.numeric(as.character(nifty$Adj.Close)))
head(regression_data)

#for share1
lmret1<-lm(regression_data$share1_close~regression_data$nifty_close,data = regression_data)
res<-summary(lmret1)
res
##from the summary
intercept1 <- res$coefficients[1,1] #945.873412
nifty_reg1 <- res$coefficients[2,1] #-0.059658
nifty_mean <- mean(as.numeric(as.character(nifty$Adj.Close)))
#share=a+nifty*b
#share1=Intercept + nifty * regression_data$nifty_close
#if nifty mean is used i,e 9299.774 then our regression model for tata share share1 will be 

share1_reg<- intercept1 + (nifty_reg1 *  nifty_mean)
share1_reg   #391.0659
plot(regression_data$share1_close~regression_data$nifty_close, pch = 16, col = "blue") #Plot the results
abline(lmret1) #Add a regression line

#for share2
lmret2<-lm(regression_data$share2_close~regression_data$nifty_close,data = regression_data)
res2<-summary(lmret2)
res2
##from the summary
intercept2 <- res2$coefficients[1,1] #75.054194
nifty_reg2 <- res2$coefficients[1,1] #0.020348
nifty_mean <- mean(as.numeric(as.character(nifty$Adj.Close)))
#share=a+nifty*b
#share1=Intercept + nifty * regression_data$nifty_close
#if nifty mean is used i,e 9299.774 then our regression model for tata share share1 will be 

share2_reg<- intercept2 + (nifty_reg2 *  nifty_mean)
share2_reg
plot(regression_data$share2_close~regression_data$nifty_close,data, pch = 16, col = "blue") #Plot the results
abline(lmret2) #Add a regression line


######## Beta Calculation #########
share1Prices<-as.numeric(regression_data$share1_close)
share2Prices<-as.numeric(regression_data$share2_close)
niftyPrices<-as.numeric(regression_data$nifty_close)
length(niftyPrices)

#from the reference website --can be removed
stock1_returns <- (share1Prices[1:(length(share1Prices) - 1)] - share1Prices[2:length(share1Prices)] ) / share1Prices[2:length(share1Prices)]
stock1_returns
index_returns <- ( niftyPrices[1:(length(niftyPrices) - 1)] - niftyPrices[2:length(niftyPrices)] ) / niftyPrices[2:length(niftyPrices)]
index_returns

#according to excel sheet formula by rohit
stock1_returns<-((share1Prices[2:length(share1Prices)]  - share1Prices[1:length(share1Prices)-1] )/share1Prices[1:length(share1Prices)-1])*100
stock2_returns<-((share2Prices[2:length(share2Prices)]  - share2Prices[1:length(share2Prices)-1] )/share2Prices[1:length(share2Prices)-1])*100
index_returns<-((niftyPrices[2:length(niftyPrices)]  - niftyPrices[1:length(niftyPrices)-1] )/niftyPrices[1:length(niftyPrices)-1])*100

regOnReturns_share1 <- lm(stock1_returns ~ index_returns)
summary_share1 <- summary(regOnReturns_share1)
# summary gives us a lot of useful information, but we're mostly in beta value !!
beta_val_share1 <- summary_share1$coefficients[2,1]
print(beta_val_share1)

regOnReturns_share2 <- lm(stock2_returns ~ index_returns)
summary_share2 <- summary(regOnReturns_share2)
# summary gives us a lot of useful information, but we're mostly in beta value !!
beta_val_share2 <- summary_share2$coefficients[2,1]
print(beta_val_share2)

