#Technical indicators (separated for ease in R markdown)



library(jsonlite)
library(ggplot2)
library(fpp2)
library(tseries)
library(tidyr)
library(tidyverse)
library(anytime)
library(quantmod)
library(techchart)
library(Rfast)
library(CandleStickPattern)
rm(list=ls())



#Firstm get the coin data (cryptocompare) and clean it up. 

get_coin_data<- function(coin_name) {
  
  
  #getting the data
  coin_daily=fromJSON(paste("https://min-api.cryptocompare.com/data/v2/histoday?fsym=",coin_name,"&tsym=USD&limit=365",sep=""))
  coin_daily_time<-coin_daily$Data$Data$time
  coin_daily_time<-anytime(coin_daily_time)
  coin_daily_close<-coin_daily$Data$Data$close
  coin_daily_open<-coin_daily$Data$Data$open
  coin_daily_high<-coin_daily$Data$Data$high
  coin_daily_low<-coin_daily$Data$Data$low
  coin_daily_volume<-(coin_daily$Data$Data$volumeto)
  
  #constructing the dataframe
  coin_all_data<-data.frame(coin_daily_time,coin_daily_open,coin_daily_high,coin_daily_low,coin_daily_close,coin_daily_volume)
  coin_all_data%>%rename(Date=coin_daily_time,Open=coin_daily_open,High=coin_daily_high,Low=coin_daily_low,Close=coin_daily_close,Volume=coin_daily_volume)->coin_all_data
  rownames(coin_all_data)<-coin_all_data$Date
  coin_all_data<-coin_all_data[c("Open","High","Low","Close","Volume")] #<<- IS TO SET A GLOBAL VARIABLE
  coin_all_data<<-as.xts(coin_all_data)
  
}


get_coin_data("BTC")  


#Set up the technical analysis tools

#1. General Candlestick patterns

#index(coin_all_data) <- round(index(coin_all_data),"day")
price<-tail(coin_all_data,10)
candleChart(price)
#doji
doji(price)
dragonfly.doji(price)
gravestone.doji(price)
#harmers
hammer(price)
inverted.hammer(price)
#engulfing
bullish.engulf(price)
bearish.engulf(price)
#harami
bullish.harami(price)
bearish.harami(price)
#median reversal
piercing.line(price)
dark.cloud.cover(price)
#two in a row
kick.up(price)
kick.down(price)
#three in a row
#requires 20 observations
price<-tail(coin_all_data,20)
three.white.soldiers(price)
three.black.crows(price)
#star
morning.star(price)
evening.star(price)
#three methods
rising.three(price)
falling.three(price)





#2. Fibonacci retracement
getFibs=function(date)
{
  #data<-getSymbols(ticker,from=as.Date(date),auto.assign=FALSE)
  data<-coin_all_data["2020-12-08/2020-12-11"]
  fibs<-find.pivots(data,type="FIB")
  fibsval<-cbind(as.data.frame(fibs$results$value),as.data.frame(fibs$results$strength))
  fibsval<-round(fibsval,2)
  
  colnames(fibsval)<-c("Fib","Strength")
  CLOSE<-round(as.numeric(last(Cl(data))),2)
  
  fibsval$Ratio<-c(0.00,0.236,0.382,0.50,0.618,1.00)
  
  #calculate the %diff between the fibonacci and the cl price  
  
  fibsval$pcdif<-round(CLOSE/fibsval$Fib-1,4)
  #add the cl price
  fibsval$CL<-CLOSE
  fibsval$FROM<-as.Date(date)
  fibsval$TO<-as.Date(Sys.Date())
  #Return the table
  fibsval
  
}

FIBS<-getFibs("2020-11-01")
FIBS
#add a chart

chartSeries(coin_all_data["2020-12-01/2020-12-15"],type="candle")
addLines(h=FIBS$Fib,col="orange")






#1. Rate of Change (ROC) - fix it

coin_all_data_roc<-ROC(coin_all_data$Close,n=1)
chartSeries(coin_all_data,theme = chartTheme("black"),name = "Rate of Change",bar.type = "candle")
addROC(n=12) #can be 12,25,200 anything

ROC_COIN<-c()

for (i in 3:nrow(coin_all_data)) {
  
  if(coin_all_data$Close[i]>coin_all_data$Close[i-1]){
    ROC_COIN[i]<-1
  }else{ROC_COIN[i]<-0}
}
ROC_COIN<-ROC_COIN[!is.na(ROC_COIN)]

#the trading signal for ROC is: when the signal goes from negative to positive. So setting up the r code to look for this change
ROC_UP<-c()

for(t in 1:(length(ROC_COIN)-1)){
  if (ROC_COIN[t+1]>ROC_COIN[t]&&
      ROC_COIN[t+1]==1) {
      ROC_UP[t]<-TRUE
  }else{ROC_UP[t]<-FALSE}
}
print(tail(ROC_UP,7))
for (i in (length(ROC_UP)-6):length(ROC_UP)) {
  if (ROC_UP[i]=="TRUE") {
  print("BUY signal as per the ROC")    
  }else{print("Sell signal as the ROC")}  
}


#2. Bollinger Band


bb_coin<-BBands(coin_all_data$Close,n=20,sd=2,maType = "SMA")
chartSeries(coin_all_data["2020-12-08/2020-12-15"],theme=chartTheme("black"),name="Bollinger Bands",bar.type = "candle")
addBBands(maType = "EMA",n=20,sd=2)

#zoomed in chart (change every week)
chartSeries(coin_all_data["2020-12-08/2020-12-22"],theme=chartTheme("black"),name="Bollinger Bands - Zoomed-in",bar.type = "candle")


#Can identify W - bottom, M-top and combination with CCI/Williams's (any support- resistance indicator)

#W-bottom and M top - have to be done graphically (not sure how to code this in?)
#Even with oscillator momentum indicators - dont know how to code it in?
#BB band + using any other indicator (support + resistance)

#BOLLINGER BAND WITH CCI
#first - will have to add CCI

CCI_coin<-CCI(c(coin_all_data$High,coin_all_data$Low,coin_all_data$Close),n=20)
chartSeries(coin_all_data,theme=chartTheme("black"),name = "CCI Index")
addCCI(n=20)
print(tail(CCI_coin,60))
#BOLLINGER BAND WITH Williams%R

WPR_coin<-WPR(c(coin_all_data$High,coin_all_data$Low,coin_all_data$Close),n=14)
chartSeries(coin_all_data,theme=chartTheme("black"))
addWPR(n=14)
print(tail(WPR_coin,30))

#Bollinger Band - check if price outside the bands 

for (i in (nrow(bb_coin)-30):nrow(bb_coin)) {
    if (Cl(coin_all_data)[i]<bb_coin[,1][i]) {
      print(paste0("Price below the lower bollinger band, check momentum oscillators on ",index(coin_all_data)[i]))  
    }else{if(Cl(coin_all_data)[i]>bb_coin[,3][i]){
    print(paste0("Price above the upper bollinger band, check momentum oscillators on ",index(coin_all_data)[i]))
  }else{print("price between the bands")}}  
}

#3. MACD

macd_coin<-MACD(coin_all_data$Close,maType = "EMA")
chartSeries(coin_all_data,theme=chartTheme("black"),name="Moving Average Convergence/Divergence",bar.type="candle")
addMACD(type="EMA")


#add macd signal code

for (i in (nrow(macd_coin)-30):nrow(macd_coin)) {
  if (macd_coin[,1][i]>=macd_coin[,2][i]) {
    print(paste0("Buy signal as per MACD as on ",index(coin_all_data)[i]))
  } else{print(paste0("Sell signal as per MACD as on ",index(coin_all_data)[i]))} 
}

#MACD crossovers

#for (i in (nrow(macd_coin)-30):nrow(macd_coin)) {
#  if(macd_coin[,1][i]>=macd_coin[,2][i] &&
#     macd_coin[,1][i+1]>=macd_coin[,2][i+1]){
#    print(paste0("buy signal crossover as at ",index(coin_all_data)[i]))
#  }else{print(paste0("sell signal crossover as at ",index(coin_all_data)[i]))}
#}

for (i in (nrow(macd_coin)-30):nrow(macd_coin)) {
  if (macd_coin[,1][i-1]>=macd_coin[,2][i-1]) {
    if(macd_coin[,1][i-1]>=macd_coin[,2][i-1] &&
       macd_coin[,1][i]>=macd_coin[,2][i]){
      print(paste0("buy signal crossover as at ",index(coin_all_data)[i]))
    }else{print(paste0("buy signal crossover end at ",index(coin_all_data)[i]))}
  }else{ 
    if (macd_coin[,1][i-1]<macd_coin[,2][i-1] &&
        macd_coin[,1][i]<macd_coin[,2][i]) {
      print(paste0("sell signal crossover as at ",index(coin_all_data)[i]))    
    }else{print(paste0("sell signal crossover end at ",index(coin_all_data)[i]))}
  }
}
  
  
#MACD Divergence - probably cannot code it in?  - maybe in future versions
#Check word doc for details on how to identify it graphically

# for (i in (nrow(macd_coin)-30):nrow(macd_coin)) {
#  if (macd_coin[,1][i-1]<macd_coin[,2][i-1]){
#    first_low<-Rfast::nth(macd_coin[,1][i],1, descending = F)
#    print(first_low)
#}else{if(macd_coin[,1][i-1]>macd_coin[,2][i-1] &&
#         macd_coin[,1][i]<macd_coin[,2][i]){
#           second_low<-Rfast::nth(macd_coin[,1][i],1,descending=F)
           #print(second_low)
#         }  
#}
#}

#Rfast::nth(macd_coin[,1],1,descending=F)
#which(macd_coin[,1]==Rfast::nth(macd_coin[,1],1,descending=F))
#which(macd_coin[,1]==Rfast::nth(macd_coin[,1],2,descending=F))
#Rfast::nth(macd_coin[,1],1,descending=F)




#MACD Rapid rise/Fall - combined with RSI

# RSI

RSI_coin<-RSI(coin_all_data$Close,maType ="EMA")
chartSeries(coin_all_data,theme=chartTheme("black"),name="Relative Strength Index",bar.type="candle")
addRSI(maType = "EMA")

#add RSI signal code

for (i in (length(RSI_coin)-30):length(RSI_coin)) {
  if (RSI_coin[i]>=70) {
    print(paste0("Overbought as per RSI as on ",index(coin_all_data)[i]))
  } else { if (RSI_coin[i]<=30) {
    print(paste0("Oversold as per RSI as on ", index(coin_all_data)[i]))  
  } 
  else{print(paste0("Neither overbought nor oversold as per RSI on ",index(coin_all_data)[i]))}}
}


#Using MACD and RSI together

for (i in (length(RSI_coin)-30):length(RSI_coin)){
  if (RSI_coin[i]>=70 &&
      macd_coin[,1][i-1]>=macd_coin[,2][i-1] &&
      macd_coin[,1][i]>=macd_coin[,2][i]){
    print(paste0("Overbought confirmed as per MACD + RSI on ",index(coin_all_data)[i]))
  } else { if (RSI_coin[i]<=30 &&
      macd_coin[,1][i-1]<macd_coin[,2][i-1] &&
      macd_coin[,1][i]<macd_coin[,2][i])  {
      print(paste0("Oversold confirmed as per MACD + RSI on ",index(coin_all_data)[i]))  
        
      }
    else {print(paste0("Neither overbought nor oversold as per MACD + RSI on ",index(coin_all_data)[i]))} 
}
}


#Money Flow Index
MFI_coin<-MFI(coin_all_data$Close,coin_all_data$Volume)
chartSeries(coin_all_data,theme=chartTheme("black"),name="Money Flow Index",bar.type = "candle")
addMFI()


#add MFI signal code

for (i in (length(MFI_coin)-30):length(MFI_coin)) {
  if (MFI_coin[i]>=80) {
    print(paste0("Overbought as per MFI as on ",rownames(coin_all_data)[i]))
  } else { if (MFI_coin[i]<=20) {
    print(paste0("Oversold as per MFI as on ", rownames(coin_all_data)[i]))  
  } 
    else{print(paste0("Neither overbought nor oversold as per MFI on ",rownames(coin_all_data)[i]))}}
}

#Using MACD and MFI together

for (i in (length(MFI_coin)-30):length(MFI_coin)){
  if (MFI_coin[i]>=80 &&
      macd_coin[,1][i-1]>=macd_coin[,2][i-1] &&
      macd_coin[,1][i]>=macd_coin[,2][i]){
    print(paste0("Overbought confirmed as per MACD + MFI on ",index(coin_all_data)[i]))
  } else { if (MFI_coin[i]<=20 &&
               macd_coin[,1][i-1]<macd_coin[,2][i-1] &&
               macd_coin[,1][i]<macd_coin[,2][i])  {
    print(paste0("Oversold confirmed as per MACD + MFI on ",index(coin_all_data)[i]))  
    
  }
    else {print(paste0("Neither overbought nor oversold as per MACD + MFI on ",index(coin_all_data)[i]))} 
  }
}


############END OF TECHNICAL INDICATORS FOR NOW (13-12-2020)####################
