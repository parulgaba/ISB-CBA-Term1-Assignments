library(dplyr)
library(ggplot2)

share1<-read.csv("/Users/nirajkulkarni/Desktop/Niraj/ISB-CBA/Term-1/Business-Fundamentals/TATAMOTORS.NS.csv",stringsAsFactors = FALSE , header = TRUE)
share1
share2<-read.csv("/Users/nirajkulkarni/Desktop/Niraj/ISB-CBA/Term-1/Business-Fundamentals/SBIN.NS.csv",stringsAsFactors = FALSE , header = TRUE)
share2
nifty<-read.csv("/Users/nirajkulkarni/Desktop/Niraj/ISB-CBA/Term-1/Business-Fundamentals/Nifty50-NSEI.csv",stringsAsFactors = FALSE , header = TRUE)
nifty

####### Calculating Returns ######
#str(share1$Adj.Close)
#SHARE1
share1<-share1[!(share1$Adj.Close=='null' | is.na(share1$Adj.Close)) ,] #removing null
share1
nrow(share1)

maxDate1<-share1 %>% arrange(share1$Date) %>% head(1) 
minDate1<-share1 %>% arrange(share1$Date) %>% tail(1)
maxDate1
minDate1
str(maxDate1$Adj.Close)
valShare1_1<-as.numeric(as.character(maxDate1$Adj.Close))
valShare1_2<-as.numeric(as.character(minDate1$Adj.Close))

share1_ret<-((valShare1_2-valShare1_1)/valShare1_2)*100
share1_ret
plot(x=share1$Adj.Close,ylab = 'Adjusted Close',type = 'line',col='blue',main = 'TataMotors Monthly Closing')

#SHARE2
share2<-share2[!(share2$Adj.Close=='null' | is.na(share2$Adj.Close)) ,] #removing null
share2
nrow(share2)

maxDate2<-share2 %>% arrange(share2$Date) %>% head(1) 
minDate2<-share2 %>% arrange(share2$Date) %>% tail(1)
maxDate2
minDate2
str(maxDate2$Adj.Close)
valShare2_1<-as.numeric(as.character(maxDate2$Adj.Close))
valShare2_2<-as.numeric(as.character(minDate2$Adj.Close))

share2_ret<-((valShare2_2-valShare2_1)/valShare2_2)*100
share2_ret
plot(x=share2$Adj.Close,ylab = 'Adjusted Close',type = 'line',col='blue',main = 'SBI Monthly Closing')

#NIFTY
nifty<-nifty[!(nifty$Adj.Close=='null' | is.na(nifty$Adj.Close)) ,] #removing null
nifty
nrow(nifty)

maxDateN<-nifty %>% arrange(nifty$Date) %>% head(1) 
minDateN<-nifty %>% arrange(nifty$Date) %>% tail(1)
maxDateN
minDateN
str(maxDateN$Adj.Close)
valNifty_1<-as.numeric(as.character(maxDateN$Adj.Close))
valNifty_2<-as.numeric(as.character(minDateN$Adj.Close))

nifty_ret<-((valNifty_2-valNifty_1)/valNifty_2)*100
nifty_ret
plot(x=nifty$Adj.Close,ylab = 'Adjusted Close',type = 'line',col='blue',main = 'Nifty Monthly Closing')

####### Linear Regression ######
regression_data<-data.frame("share1_close" = as.numeric(as.character(share1$Adj.Close)) ,
                            "share2_close" = as.numeric(as.character(share2$Adj.Close)),
                            "nifty_close"  = as.numeric(as.character(nifty$Adj.Close)))
head(regression_data)
str(regression_data$nifty_close)
#for share1
lmret1<-lm(regression_data$share1_close~regression_data$nifty_close,data = regression_data)
summary(lmret1)

##from the summary
intercept1 <- 945.873412
nifty_reg1 <- -0.059658
nifty_mean <- mean(as.numeric(as.character(nifty$Adj.Close)))
#share=a+nifty*b
#share1=Intercept + nifty * regression_data$nifty_close
#if nifty mean is used i,e 9299.774 then our regression model for tata share share1 will be 

share1_reg<- intercept1 + (nifty_reg1 *  nifty_mean)
share1_reg

#for share2
lmret2<-lm(regression_data$share2_close~regression_data$nifty_close,data = regression_data)
summary(lmret2)

##from the summary
intercept2 <- 75.054194
nifty_reg2 <- 0.020348
nifty_mean <- mean(as.numeric(as.character(nifty$Adj.Close)))
#share=a+nifty*b
#share1=Intercept + nifty * regression_data$nifty_close
#if nifty mean is used i,e 9299.774 then our regression model for tata share share1 will be 

share1_reg<- intercept2 + (nifty_reg2 *  nifty_mean)
share1_reg
