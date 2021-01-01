#strategies selected(week 30-12-2020)

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
library(fpp2)
library(gridExtra)
library(PerformanceAnalytics)
library(ggthemes)
rm(list=ls())



#First get the coin data (cryptocompare) and clean it up. 

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
#doji trading signal
doji_signal<-(
  ifelse(doji(price)==TRUE,1,0)
)
dragonfly_signal<-(
  ifelse(dragonfly.doji(price)==TRUE,1,0)
)
gravestone_signal<-(
  ifelse(gravestone.doji(price)==TRUE,-1,0)
)
doji_signal_combined<-cbind(dragonfly_signal,gravestone_signal)
#doji_signal_combined<-as.data.frame(doji_signal_combined)

colnames(doji_signal_combined)<-c("Dragonfly","gravestone")
#doji_signal_combined<-as.xts(doji_signal_combined)
print(doji_signal_combined)

#harmers
hammer(price)
inverted.hammer(price)

hammer_signal<-(
  ifelse(hammer(price)==TRUE,1,0)
)
inverted_hammer_signal<-(
  ifelse(inverted.hammer(price)==TRUE,-1,0)
)
hammer_signal_combined<-cbind(hammer_signal,inverted_hammer_signal)
colnames(hammer_signal_combined)<-c("Hammer","Inverted Hammer")
print(hammer_signal_combined)

#engulfing
bullish.engulf(price)
bearish.engulf(price)

bullish_engulf_signal<-(
  ifelse(bullish.engulf(price)==TRUE,1,0)
)
bearish_engulf_signal<-(
  ifelse(bearish.engulf(price)==TRUE,-1,0)
)
engulf_signal_combined<-cbind(bullish_engulf_signal,bearish_engulf_signal)
colnames(engulf_signal_combined)<-c("Bullish Engulf","Bearish Engulf")
print(engulf_signal_combined)



#harami
bullish.harami(price)
bearish.harami(price)

bullish_harami_signal<-(
  ifelse(bullish.harami(price)==TRUE,1,0)
)
bearish_harami_signal<-(
  ifelse(bearish.harami(price)==TRUE,-1,0)
)
harami_signal_combined<-cbind(bullish_harami_signal,bearish_harami_signal)
colnames(harami_signal_combined)<-c("Bullish Harami","Bearish Harami")
print(harami_signal_combined)



#median reversal
piercing.line(price)
dark.cloud.cover(price)


piercing_line_signal<-(
  ifelse(piercing.line(price)==TRUE,1,0)
)
dark_cloud_signal<-(
  ifelse(dark.cloud.cover(price)==TRUE,-1,0)
)
median_reversal_signal_combined<-cbind(piercing_line_signal,dark_cloud_signal)
colnames(median_reversal_signal_combined)<-c("Piercing Line","Dark Cloud")
print(median_reversal_signal_combined)


#two in a row
kick.up(price)
kick.down(price)

kick_up_signal<-(
  ifelse(kick.up(price)==TRUE,1,0)
)
kick_down_signal<-(
  ifelse(kick.down(price)==TRUE,-1,0)
)
two_in_a_row_signal_combined<-cbind(kick_up_signal,kick_down_signal)
colnames(two_in_a_row_signal_combined)<-c("Kick up","Kick down")
print(two_in_a_row_signal_combined)
date<-index(tail(coin_all_data,10))

all_candle_signals_combined<-cbind(doji_signal_combined,hammer_signal_combined,engulf_signal_combined,harami_signal_combined,median_reversal_signal_combined)
all_candle_signals_combined<-as.data.frame(all_candle_signals_combined)
all_candle_signals_combined<-tail(all_candle_signals_combined,7)
#col sums
all_candlestick_sum<-colSums(all_candle_signals_combined)
all_candle_signals_combined<-rbind(all_candle_signals_combined, colSums(all_candle_signals_combined))
#adding in date
all_candle_signals_combined$Date<-rownames(all_candle_signals_combined)
all_candle_signals_combined<-all_candle_signals_combined[,c(11,1,2,3,4,5,6,7,8,9,10)]
all_candle_signals_combined$Date[8]<-all_candle_signals_combined$Date[7]
all_candle_signals_combined$Date <- strftime(all_candle_signals_combined$Date, "%d/%m/%y")
colnames(all_candle_signals_combined)<-c("Date","Dragonfly","Gravestone","Hammer","Inverted Hameer","Bullish Engulf","Bearish Engulf","Bullish Harami","Bearish Harami","Piercing Line","Dark Cloud")
all_candle_signals_combined$Date[8]<-"Total"
#all_candle_signals_combined<-as.xts(all_candle_signals_combined)
grid.table(all_candle_signals_combined,rows=NULL)

#plotting it based on the totals
all_candle_signals_combined_total<-all_candlestick_sum
all_candle_signals_combined_total<-as.data.frame(all_candle_signals_combined_total)
all_candle_signals_combined_total$Signal<-rownames(all_candle_signals_combined_total)

all_candle_signals_combined_total%>%
  ggplot(aes(x=Signal,y=all_candle_signals_combined_total,fill=Signal))+#group=Asset
  geom_bar(stat="identity")+
  ylab("Bull/Bear Signal Weekly Total")+
  xlab("Indicator")+
  ggtitle("Candlestick Indicators:Weekly Signal Sum(25-12-20/31-12-20)")+
  geom_hline(yintercept = 0,color="red")+
  theme_economist()


#Technical indicators

#1. Fibonacci Retracement

getFibs=function(date)
{
  #data<-getSymbols(ticker,from=as.Date(date),auto.assign=FALSE)
  data<-coin_all_data["2020-12-20/2020-12-30"]
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

FIBS<-getFibs("2020-12-25")
FIBS

chartSeries(coin_all_data,type="candle",name = "Fibonacci Retracement")
addLines(h=FIBS$Fib,col="orange")
zoomChart("2020-12-20/2020-12-30")
#View(FIBS)

#2. Rate of Change (ROC) 

coin_all_data_roc<-ROC(coin_all_data$Close,n=7)
chartSeries(coin_all_data,theme = chartTheme("black"),name = "Rate of Change",bar.type = "candle")
addROC(n=7) #can be 12,25,200 anything
#signal
roc_coin_signal<-Lag(
  ifelse(Lag(coin_all_data_roc)<(-0.05) & coin_all_data_roc>(-0.05),1,
         ifelse(Lag(coin_all_data_roc)<(0.05) & coin_all_data_roc>(0.05),-1,0))
)
roc_coin_signal[is.na(roc_coin_signal)]<-0
#strategy (buy =1 (if roc signal is 1). Sell=-1 (if roc signal is -1). Do nothing = 0)
roc_coin_strategy<-ifelse(roc_coin_signal>1,0,1)
for (i in 1:length(Cl(coin_all_data))) {
  roc_coin_strategy[i]<-ifelse(roc_coin_signal[i]==1,1,ifelse(roc_coin_signal[i]==-1,0,roc_coin_strategy[i-1]))  
}
roc_coin_strategy[is.na(roc_coin_strategy)]<-1
roc_coin_strategy_combined<-cbind(coin_all_data_roc,roc_coin_signal,roc_coin_strategy)

#backtesting
ret_coin<-diff(log(Cl(coin_all_data)))
benchmark_coin<-ret_coin

roc_coin_ret<-ret_coin*roc_coin_strategy
roc_coin_ret_combined<-cbind(roc_coin_ret,benchmark_coin)
colnames(roc_coin_ret_combined)<-c("ROC(7 DAYS)","COIN RETURN")
charts.PerformanceSummary(roc_coin_ret_combined)
#tends to underperform btc returns 
#ROC seems to underperform btc. Can also use it for trend identification or in combination with other strategies

#TREND IDENTIFICATION
#Long term bullish/bearish if 6 and 12 month ROC is positive/negative. Means prices are higher than they were 6/12 months ago

#Long term trend ROC
roc_coin_3_months<-ROC(coin_all_data$Close,n=90)
roc_coin_6_months<-ROC(coin_all_data$Close,n=180)

roc_coin_long_term_signal<-Lag(
  ifelse(roc_coin_3_months>0 & roc_coin_6_months>0,1,
         ifelse(roc_coin_3_months<0 & roc_coin_6_months<0,-1,0))
)
roc_coin_long_term_signal[is.na(roc_coin_long_term_signal)]<-0
ifelse(tail(roc_coin_long_term_signal,1)==1,paste("Long term trend bullish as per ROC as on ",index(tail(coin_all_data$Close,1))),
       ifelse(tail(roc_coin_long_term_signal,1)==-1,paste("Long term trend bearish as per ROC as on ",index(tail(coin_all_data$Close,1))),0)
)


#short term trend ROC
roc_coin_7d<-ROC(coin_all_data$Close,n=7)
roc_coin_30d<-ROC(coin_all_data$Close,n=30)


roc_coin_short_term_signal<-Lag(
  ifelse(roc_coin_7d>0 & roc_coin_30d>0,1,
         ifelse(roc_coin_7d<0 & roc_coin_30d<0,-1,0))
)
roc_coin_short_term_signal[is.na(roc_coin_short_term_signal)]<-0
ifelse(tail(roc_coin_short_term_signal,1)==1,paste("Short term trend bullish as per ROC as on ",index(tail(coin_all_data$Close,1))),
       ifelse(tail(roc_coin_short_term_signal,1)==-1,paste("Short term trend bearish as per ROC as on ",index(tail(coin_all_data$Close,1))),0)
)
#when was the last short term trend bearish? (over the last one year)
print(index(roc_coin_short_term_signal[tail(which(roc_coin_short_term_signal==-1),1)]))

#check overbought/oversold via ROC level - only during long term bullish/bearish, to check for short-term change in trend
chartSeries(coin_all_data,name="ROC+EMA(20)")
addEMA(n=20)
addROC(n=7)
zoomChart("2020-12-01/2020-12-29")

roc_coin_7d<-reclass(roc_coin_7d,coin_all_data)

for (i in 1:length(roc_coin_7d) ) {
  ifelse(isTRUE(roc_coin_7d$Close[i] < (-0.04))==TRUE,print(paste("ROC oversold, keep an eye on price v/s EMA(20) as on ",index(roc_coin_7d$Close[i]))),
         ifelse(isTRUE(roc_coin_7d$Close[i] > 0.2)==TRUE,print(paste("ROC overbought, keep an eye on price v/s EMA(20) as on ",index(roc_coin_7d$Close[i]))),print(paste("ROC neither overbought nor oversold as on ",index(roc_coin_7d$Close[i]))))
  )   
}

#trading signal+strategy based on identifying oversold/overbought roc

roc_over_bought_sold_signal<-c()
for (i in 1:length(roc_coin_7d)) {
  ifelse(isTRUE(roc_coin_7d$Close[i] < (-0.10))==TRUE,roc_over_bought_sold_signal[i]<- 1,
         ifelse(isTRUE(roc_coin_7d$Close[i]> 0.2)==TRUE,roc_over_bought_sold_signal[i]<- (-1),roc_over_bought_sold_signal[i]<-0)
  )
}
reclass(roc_over_bought_sold_signal,coin_all_data)
#View(roc_coin_7d)
#Trading strategy
roc_over_bought_sold_strategy<-ifelse(roc_over_bought_sold_signal>1,0,1)

for (i in 1:length(Cl(coin_all_data))) {
  roc_over_bought_sold_strategy[i]<-ifelse(roc_over_bought_sold_signal[i]==1,1,ifelse(roc_over_bought_sold_signal[i]==-1,0,roc_over_bought_sold_strategy[i-1]))  
}
roc_over_bought_sold_strategy<-Lag(roc_over_bought_sold_strategy,2)
roc_over_bought_sold_strategy[is.na(roc_over_bought_sold_strategy)]<-1
roc_over_bought_sold_strategy_combined<-cbind(roc_coin_7d,roc_over_bought_sold_signal,roc_over_bought_sold_strategy)

#Backtesting
ret_coin<-diff(log(coin_all_data$Close))
roc_coin_ret<-ret_coin*roc_over_bought_sold_strategy
roc_coin_ret_combined<-cbind(roc_coin_ret,benchmark_coin)
colnames(roc_coin_ret_combined)<-c("ROC STRATEGY","COIN RETURN")
charts.PerformanceSummary(roc_coin_ret_combined)
percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}
roc_monthly<-apply.monthly(roc_coin_ret,sum)
roc_coin_ret_monthly<-percent(roc_monthly)

#ROC alone is a bad strategy but ROC 7d might be decent

#2. #3. MACD

macd_coin<-MACD(coin_all_data$Close,maType = "EMA")
chartSeries(coin_all_data,theme=chartTheme("black"),name="Moving Average Convergence/Divergence",bar.type="candle")
addMACD(type="EMA")


#add macd signal code

for (i in (nrow(macd_coin)-30):nrow(macd_coin)) {
  if (macd_coin[,1][i]>=macd_coin[,2][i]) {
    print(paste0("Buy signal as per MACD as on ",index(coin_all_data)[i]))
  } else{print(paste0("Sell signal as per MACD as on ",index(coin_all_data)[i]))} 
}


macd_coin_signal<-Lag(
  ifelse(Lag(macd_coin[,1])>Lag(macd_coin[,2]),1,
         ifelse(Lag(macd_coin[,1])<Lag(macd_coin[,2]),-1,0))
)
macd_coin_signal[is.na(macd_coin_signal)]<-0

#strategy

macd_coin_strategy<-ifelse(macd_coin_signal>1,0,1)
for (i in 1:length(Cl(coin_all_data))) {
  macd_coin_strategy[i]<-ifelse(macd_coin_signal[i]==1,1,ifelse(macd_coin_signal[i]==-1,0,macd_coin_signal[i-1]))  
}
macd_coin_strategy[is.na(macd_coin_strategy)]<-1
macd_coin_strategy_combined<-cbind(macd_coin,macd_coin_signal,macd_coin_strategy)

#backtesting
macd_coin_ret<-ret_coin*macd_coin_strategy
macd_coin_ret_combined<-cbind(macd_coin_ret,benchmark_coin)
colnames(macd_coin_ret_combined)<-c("MACD","COIN RETURN")
charts.PerformanceSummary(macd_coin_ret_combined)
#not a bad strategy

#View(macd_coin_ret) 
macd_monthly<-apply.monthly(macd_coin_ret$Close,sum)
macd_monthly<-percent(macd_monthly)
#View(macd_monthly)



#MACD crossovers

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


#signal

macd_crossover_signal<-Lag(
  ifelse(macd_coin[,1]>=macd_coin[,2] & Lag(macd_coin[,1])>=Lag(macd_coin[,2]),1,
         ifelse(macd_coin[,1]<macd_coin[,2] & Lag(macd_coin[,1])<Lag(macd_coin[,2]),-1,0))
)
macd_crossover_signal[is.na(macd_crossover_signal)]<-0

#strategy

macd_crossover_strategy<-ifelse(macd_crossover_signal>1,0,1)
for (i in 1:length(Cl(coin_all_data))) {
  macd_crossover_strategy[i]<-ifelse(macd_crossover_signal[i]==1,1,ifelse(macd_crossover_signal[i]==-1,0,macd_crossover_signal[i-1]))  
}
macd_crossover_strategy[is.na(macd_crossover_strategy)]<-1
macd_crossover_strategy_combined<-cbind(macd_crossover_signal,macd_crossover_strategy)

#backtesting
macd_crossover_ret<-ret_coin*macd_crossover_strategy
macd_crossover_ret_combined<-cbind(macd_crossover_ret,benchmark_coin)
colnames(macd_crossover_ret_combined)<-c("MACD CROSSOVER","COIN RETURN")
charts.PerformanceSummary(macd_crossover_ret_combined)
#not a bad strategy

macd_crossover_monthly<-apply.monthly(macd_crossover_ret$Close,sum)
macd_crossover_monthly<-percent(macd_crossover_monthly)



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

#RSI SIGNAL

RSI_signal<-Lag(
  ifelse(RSI_coin>=70,1,
         ifelse(RSI_coin<=30,-1,0))
)
RSI_signal[is.na(RSI_signal)]<-0


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

#Signal

MACD_RSI_signal<-Lag(
  ifelse(RSI_coin>=70 & macd_coin[,1]>=macd_coin[,2] & Lag(macd_coin[,1])>=Lag(macd_coin[,2]),-1,
         ifelse(RSI_coin<=30 & macd_coin[,1]>=macd_coin[,2] & Lag(macd_coin[,1])>=Lag(macd_coin[,2]),1,0))
)
MACD_RSI_signal[is.na(MACD_RSI_signal)]<-0

#strategy

macd_rsi_strategy<-ifelse(MACD_RSI_signal>1,0,1)
for (i in 1:length(Cl(coin_all_data))) {
  macd_rsi_strategy[i]<-ifelse(MACD_RSI_signal[i]==1,1,ifelse(MACD_RSI_signal[i]==-1,0,MACD_RSI_signal[i-1]))  
}
macd_rsi_strategy[is.na(macd_rsi_strategy)]<-1
macd_rsi_strategy_combined<-cbind(MACD_RSI_signal,macd_rsi_strategy)

#backtesting
macd_rsi_ret<-ret_coin*macd_rsi_strategy
macd_rsi_ret_combined<-cbind(macd_rsi_ret,benchmark_coin)
colnames(macd_rsi_ret_combined)<-c("MACD+RSI","COIN RETURN")
charts.PerformanceSummary(macd_rsi_ret_combined)
#not a bad strategy

macd_rsi_monthly<-apply.monthly(macd_rsi_ret$Close,sum)
macd_rsi_monthly<-percent(macd_rsi_monthly)
#View(macd_rsi_monthly)





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

#signal

macd_mfi_signal<-Lag(
  ifelse(MFI_coin>=80 & macd_coin[,1]>=macd_coin[,2] & Lag(macd_coin[,1])>=Lag(macd_coin[,2]),-1,
         ifelse(MFI_coin<=20 & macd_coin[,1]>=macd_coin[,2] & Lag(macd_coin[,1])>=Lag(macd_coin[,2]),1,0))
)
macd_mfi_signal[is.na(macd_mfi_signal)]<-0

#strategy

macd_mfi_strategy<-ifelse(macd_mfi_signal>1,0,1)
for (i in 1:length(Cl(coin_all_data))) {
  macd_mfi_strategy[i]<-ifelse(macd_mfi_signal[i]==1,1,ifelse(macd_mfi_signal[i]==-1,0,macd_mfi_signal[i-1]))  
}
macd_mfi_strategy[is.na(macd_mfi_strategy)]<-1
#macd_rsi_strategy_combined<-cbind(MACD_RSI_signal,macd_rsi_strategy)

#backtesting
macd_mfi_ret<-ret_coin*macd_mfi_strategy
macd_mfi_ret_combined<-cbind(macd_mfi_ret,benchmark_coin)
colnames(macd_mfi_ret_combined)<-c("MACD+MFI","COIN RETURN")
charts.PerformanceSummary(macd_mfi_ret_combined)
#bad strategy

macd_mfi_monthly<-apply.monthly(macd_mfi_ret$Close,sum)
macd_mfi_monthly<-percent(macd_mfi_monthly)
#View(macd_mfi_monthly)


#4. Parabolic Stop and Reverse (SAR)
sar_coin<-SAR(cbind(Hi(coin_all_data),Lo(coin_all_data)),accel=c(0.02,0.2))
chartSeries(coin_all_data)
addSAR(accel=c(0.02,0.2),col="lightblue")

#signal
sar_coin_signal<-Lag(
  ifelse(Lag(Cl(coin_all_data))<Lag(sar_coin) & Cl(coin_all_data)>sar_coin,1,
         ifelse(Lag(Cl(coin_all_data))>Lag(sar_coin) &Cl(coin_all_data)<sar_coin,-1,0)))

sar_coin_signal[is.na(sar_coin_signal)]<-0

#strategy
sar_coin_strategy<-ifelse(sar_coin_signal>1,0,1)
for (i in 1:length(Cl(coin_all_data))) {
  sar_coin_strategy[i]<-ifelse(sar_coin_signal[i]==1,1,ifelse(sar_coin_signal[i]==-1,0,sar_coin_strategy[i-1]))  
}
sar_coin_strategy[is.na(sar_coin_strategy)]<-1
sar_coin_strategy_combined<-cbind(Cl(coin_all_data),sar_coin,sar_coin_signal,sar_coin_strategy)

#backtesting
sar_coin_ret<-ret_coin*sar_coin_strategy
sar_coin_ret_combined<-cbind(sar_coin_ret,benchmark_coin)
colnames(sar_coin_ret_combined)<-c("SAR RET","BENCHMARK COIN")
charts.PerformanceSummary(sar_coin_ret_combined,main="SAR benchmark")
sar_coin_combined_table<-table.AnnualizedReturns(sar_coin_ret_combined)
#View(sar_coin_combined_table)
#not the worst strategy tbh


#5. Stochastic Momentum Index (SMI):
smi_coin<-SMI(HLC(coin_all_data),n=13,nFast=2,nSlow=25,nSig=9)
chartSeries(coin_all_data)
addSMI(n=13,fast=2,slow=2,signal=9)

#signal
smi_coin_signal<- Lag(
  ifelse(Lag(smi_coin[,1])<Lag(smi_coin[,2]) & smi_coin[,1]>smi_coin[,2],1,
         ifelse(Lag(smi_coin[,1])>Lag(smi_coin[,2]) & smi_coin[,1]<smi_coin[,2],-1,0)))

smi_coin_signal[is.na(smi_coin_signal)]<-0

#strategy
smi_coin_strategy<-ifelse(smi_coin_signal>1,0,1)
for (i in 1:length(Cl(coin_all_data))) {
  smi_coin_strategy[i]<-ifelse(smi_coin_signal[i]==1,1,ifelse(smi_coin_signal[i]==-1,0,smi_coin_strategy[i-1]))  
}
smi_coin_strategy[is.na(smi_coin_strategy)]<-1
smi_coin_strategy_combined<-cbind(smi_coin[,1],smi_coin[,2],smi_coin_signal,smi_coin_strategy)

#backtesting
smi_coin_ret<-ret_coin*smi_coin_strategy
smi_coin_ret_combined<-cbind(smi_coin_ret,benchmark_coin)
colnames(smi_coin_ret_combined)<-c("SMI RET","BENCHMARK COIN")
charts.PerformanceSummary(smi_coin_ret_combined,main="SMI benchmark")
smi_coin_combined_table<-table.AnnualizedReturns(smi_coin_ret_combined)
#another good strategy


#Signal summation:weekly
all_technical_signals_combined<-cbind(roc_over_bought_sold_signal,macd_coin_signal,macd_crossover_signal,MACD_RSI_signal,sar_coin_signal,smi_coin_signal)
colnames(all_technical_signals_combined)<-c("ROC","MACD","MACD Crossover","MACD+RSI","Parabolic Stop and Reverse(SAR)","Stochastic Momentum Index(SMI)")
all_technical_signals_combined<-tail(all_technical_signals_combined,7)
all_technical_signals_combined_total<-colSums(all_technical_signals_combined)
all_technical_signals_combined_total<-as.data.frame(all_technical_signals_combined_total)
all_technical_signals_combined_total$Signal<-rownames(all_technical_signals_combined_total)

all_technical_signals_combined_total%>%
  ggplot(aes(x=Signal,y=all_technical_signals_combined_total,fill=Signal))+#group=Asset
  geom_bar(stat="identity")+
  ylab("Bull/Bear Signal Weekly Total")+
  xlab("Indicator")+
  ggtitle("Technical Indicators:Weekly Signal Sum(25-12-20/31-12-20)")+
  geom_hline(yintercept = 0,color="red")+
  theme_economist()+
  ylim(-10,10)

