library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(reshape2)

ibm = read.csv("data/IBM.csv", header=TRUE)[,c('Date','Close','Adj.Close')]
msft = read.csv("data/MSFT.csv", header=TRUE)[,c('Close','Adj.Close')]
goog = read.csv("data/GOOG.csv", header=TRUE)[,c('Close','Adj.Close')]
amzn = read.csv("data/AMZN.csv", header=TRUE)[,c('Close','Adj.Close')]
meta = read.csv("data/META.csv", header=TRUE)[,c('Close','Adj.Close')]
nflx = read.csv("data/NFLX.csv", header=TRUE)[,c('Close','Adj.Close')]
tsla = read.csv("data/TSLA.csv", header=TRUE)[,c('Close','Adj.Close')]
orcl = read.csv("data/ORCL.csv", header=TRUE)[,c('Close','Adj.Close')]
sap = read.csv("data/SAP.csv", header=TRUE)[,c('Close','Adj.Close')]
aapl = read.csv("data/AAPL.csv", header=TRUE)[,c('Close','Adj.Close')]

IBM_dividend = mutate(ibm, IBM_AdjClose_Diff = (Adj.Close/lag(Adj.Close) - 1) * 100, IBM_Dividend = Close * round(abs(lag(Close)/Close - lag(Adj.Close)/Adj.Close),digits=5))
ibmData = rename(IBM_dividend,IBM_Close=Close,IBM_AdjClose=Adj.Close)
head(ibmData)

MSFT_dividend = mutate(msft, MSFT_AdjClose_Diff = (Adj.Close/lag(Adj.Close) - 1) * 100, MSFT_Dividend = Close * round(abs(lag(Close)/Close - lag(Adj.Close)/Adj.Close),digits=5))
msftData = rename(MSFT_dividend,MSFT_Close=Close, MSFT_AdjClose=Adj.Close)
head(msftData)

GOOG_dividend = mutate(goog, GOOG_AdjClose_Diff = (Adj.Close/lag(Adj.Close) - 1) * 100, GOOG_Dividend = Close * round(abs(lag(Close)/Close - lag(Adj.Close)/Adj.Close),digits=5))
googData = rename(GOOG_dividend, GOOG_Close=Close, GOOG_AdjClose=Adj.Close)
head(googData)

AMZN_dividend = mutate(amzn, AMZN_AdjClose_Diff = (Adj.Close/lag(Adj.Close) - 1) * 100, AMZN_Dividend = Close * round(abs(lag(Close)/Close - lag(Adj.Close)/Adj.Close),digits=5))
amznData = rename(AMZN_dividend, AMZN_Close=Close, AMZN_AdjClose=Adj.Close)
head(amznData)

META_dividend = mutate(meta, META_AdjClose_Diff = (Adj.Close/lag(Adj.Close) - 1) * 100, META_Dividend = Close * round(abs(lag(Close)/Close - lag(Adj.Close)/Adj.Close),digits=5))
metaData = rename(META_dividend,META_Close=Close, META_AdjClose=Adj.Close)
head(metaData)

NFLX_dividend = mutate(nflx, NFLX_AdjClose_Diff = (Adj.Close/lag(Adj.Close) - 1) * 100, NFLX_Dividend = Close * round(abs(lag(Close)/Close - lag(Adj.Close)/Adj.Close),digits=5))
nflxData = rename(NFLX_dividend,NFLX_Close=Close, NFLX_AdjClose=Adj.Close)
head(nflxData)

TSLA_dividend = mutate(tsla, TSLA_AdjClose_Diff = (Adj.Close/lag(Adj.Close) - 1) * 100, TSLA_Dividend = Close * round(abs(lag(Close)/Close - lag(Adj.Close)/Adj.Close),digits=5))
tslaData = rename(TSLA_dividend,TSLA_Close=Close, TSLA_AdjClose=Adj.Close)
head(tslaData)

ORCL_dividend = mutate(orcl, ORCL_AdjClose_Diff = (Adj.Close/lag(Adj.Close) - 1) * 100, ORCL_Dividend = Close * round(abs(lag(Close)/Close - lag(Adj.Close)/Adj.Close),digits=5))
orclData = rename(ORCL_dividend,ORCL_Close=Close, ORCL_AdjClose=Adj.Close)
head(orclData)

SAP_dividend = mutate(sap, SAP_AdjClose_Diff = (Adj.Close/lag(Adj.Close) - 1) * 100, SAP_Dividend = Close * round(abs(lag(Close)/Close - lag(Adj.Close)/Adj.Close),digits=5))
sapData = rename(SAP_dividend,SAP_Close=Close, SAP_AdjClose=Adj.Close)
head(sapData)

AAPL_dividend = mutate(aapl, AAPL_AdjClose_Diff = (Adj.Close/lag(Adj.Close) - 1) * 100, AAPL_Dividend = Close * round(abs(lag(Close)/Close - lag(Adj.Close)/Adj.Close),digits=5))
aaplData = rename(AAPL_dividend,AAPL_Close=Close, AAPL_AdjClose=Adj.Close)
head(aaplData)

universe = cbind(ibmData,msftData,googData,amznData,metaData,nflxData,tslaData,orclData,sapData,aaplData)
universe[is.na(universe)] <- 0
head(universe)

INITIALIZE=function(){
    cashLeft <- 0

    ibmClose <- universe[1,'IBM_Close']
    ibmShares <- floor(1000000/ibmClose)
    ibmCash <- 1000000 %% ibmClose
    cashLeft <- cashLeft + ibmCash
    
    msftClose <- universe[1,'MSFT_Close']
    msftShares <- floor(1000000/msftClose)
    msftCash <- 1000000 %% msftClose
    cashLeft <- cashLeft + msftCash
    

    googClose <- universe[1,'GOOG_Close']
    googShares <- floor(1000000/googClose)
    googCash <- 1000000 %% googClose
    cashLeft <- cashLeft + googCash

    
    aaplClose <- universe[1,'AAPL_Close']
    aaplShares <- floor(1000000/aaplClose)
    aaplCash <- 1000000 %% aaplClose
    cashLeft <- cashLeft + aaplCash
    
    amznClose <- universe[1,'AMZN_Close']
    amznShares <- floor(1000000/amznClose)
    amznCash <- 1000000 %% amznClose
    cashLeft <- cashLeft + amznCash
    portfolio = data.frame("Date"=universe['Date'], 
                              "MTM"=0,
                              "Cash"=0,
                       "IBM"=0,
                       "MSFT"=0,
                        "GOOG"=0,
                       "AMZN"=0,
                      "AAPL"=0,
                        "META"=0,
                       "NFLX"=0,
                        "TSLA"=0,
                       "ORCL"=0,
                      "SAP"=0)
   

    portfolio[1,'IBM'] = ibmShares
    portfolio[1,'MSFT'] = msftShares
    portfolio[1,'GOOG'] = googShares
    portfolio[1,'AAPL'] = aaplShares
    portfolio[1,'AMZN'] = amznShares
    portfolio[1,'MTM'] = 5000000
    portfolio[1,'Cash'] = cashLeft
    portfolio
    
    }


REBALANCE = function(Optimal_dayInterval,portfolio,Flag){
    for (i in 2:nrow(universe)) {
    
    stock_name=c()
    if((i-1)%%Optimal_dayInterval!=0) {
        portfolio[i,'Cash'] = portfolio[i-1,'Cash']
        portfolio[i,'IBM'] = portfolio[i-1,'IBM']
        portfolio[i,'MSFT'] = portfolio[i-1,'MSFT']
        portfolio[i,'AMZN'] = portfolio[i-1,'AMZN']
        portfolio[i,'GOOG'] = portfolio[i-1,'GOOG']
        portfolio[i,'AAPL'] = portfolio[i-1,'AAPL']
        portfolio[i,'META'] = portfolio[i-1,'META']
        portfolio[i,'NFLX'] = portfolio[i-1,'NFLX']
        portfolio[i,'TSLA'] = portfolio[i-1,'TSLA']
        portfolio[i,'ORCL'] = portfolio[i-1,'ORCL']
        portfolio[i,'SAP'] = portfolio[i-1,'SAP']
        mtm=0
        mtm = (portfolio[i,'IBM'] * universe[i, 'IBM_Close'])+(portfolio[i,'MSFT'] * universe[i, 'MSFT_Close']) + (portfolio[i,'GOOG'] * universe[i, 'GOOG_Close'])+ (portfolio[i,'AMZN'] * universe[i, 'AMZN_Close']) + (portfolio[i,'AAPL'] * universe[i, 'AAPL_Close'])+ (portfolio[i,'META'] * universe[i, 'META_Close'])+ (portfolio[i,'NFLX'] * universe[i, 'NFLX_Close'])+ (portfolio[i,'TSLA'] * universe[i, 'TSLA_Close'])+ (portfolio[i,'ORCL'] * universe[i, 'ORCL_Close'])+ (portfolio[i,'SAP'] * universe[i, 'SAP_Close'])
        portfolio[i,'MTM'] = mtm
        
        dividend = 0
        dividend =  (portfolio[i,'IBM'] * universe[i, 'IBM_Dividend'])+(portfolio[i,'MSFT'] * universe[i, 'MSFT_Dividend']) + (portfolio[i,'GOOG'] * universe[i, 'GOOG_Dividend'])+ (portfolio[i,'AMZN'] * universe[i, 'AMZN_Dividend']) + (portfolio[i,'AAPL'] * universe[i, 'AAPL_Dividend'])+ (portfolio[i,'META'] * universe[i, 'META_Dividend'])+ (portfolio[i,'NFLX'] * universe[i, 'NFLX_Dividend'])+ (portfolio[i,'TSLA'] * universe[i, 'TSLA_Dividend'])+ (portfolio[i,'ORCL'] * universe[i, 'ORCL_Dividend'])+ (portfolio[i,'SAP'] * universe[i, 'SAP_Dividend'])
        portfolio[i,'Cash'] = dividend + portfolio[i,'Cash']
        
    } else {
        cashLeft = 0;
        mtm = 0;
        new_amount = 0;
        
        dividend = 0
        dividend = (portfolio[i-1,'IBM'] * universe[i, 'IBM_Dividend'])+(portfolio[i-1,'MSFT'] * universe[i, 'MSFT_Dividend']) + (portfolio[i-1,'GOOG'] * universe[i, 'GOOG_Dividend'])+ (portfolio[i-1,'AMZN'] * universe[i, 'AMZN_Dividend']) + (portfolio[i-1,'AAPL'] * universe[i, 'AAPL_Dividend'])+ (portfolio[i-1,'META'] * universe[i, 'META_Dividend'])+ (portfolio[i-1,'NFLX'] * universe[i, 'NFLX_Dividend'])+ (portfolio[i-1,'TSLA'] * universe[i, 'TSLA_Dividend'])+ (portfolio[i-1,'ORCL'] * universe[i, 'ORCL_Dividend'])+ (portfolio[i-1,'SAP'] * universe[i, 'SAP_Dividend'])
        
        mtm = (portfolio[i-1,'IBM'] * universe[i, 'IBM_Close'])+(portfolio[i-1,'MSFT'] * universe[i, 'MSFT_Close']) + (portfolio[i-1,'GOOG'] * universe[i, 'GOOG_Close'])+ (portfolio[i-1,'AMZN'] * universe[i, 'AMZN_Close']) + (portfolio[i-1,'AAPL'] * universe[i, 'AAPL_Close'])+ (portfolio[i-1,'META'] * universe[i, 'META_Close'])+ (portfolio[i-1,'NFLX'] * universe[i, 'NFLX_Close'])+ (portfolio[i-1,'TSLA'] * universe[i, 'TSLA_Close'])+ (portfolio[i-1,'ORCL'] * universe[i, 'ORCL_Close'])+ (portfolio[i-1,'SAP'] * universe[i, 'SAP_Close'])
        mtm = mtm + dividend + portfolio[i-1,'Cash']
        new_amount = mtm/5;
    
        AdjClose = select(universe[i,],ends_with("_AdjClose_Diff"))
        x = 0;
        repeat {
            x = x+1
            if (Flag){
                a<-colnames(AdjClose)[apply(AdjClose,1,which.min)]
            }else{
                a<-colnames(AdjClose)[apply(AdjClose,1,which.max)]
            }
            stock_name <- append(stock_name,a)
            AdjClose <- select(AdjClose, -c(a))
            if(x==5) {
                break
            }
        }
        
        
        stock_name = str_replace(stock_name,"_AdjClose_diff","")
        portfolio[i,'MTM'] = 0
        portfolio[i,'Cash'] = 0
        portfolio[i,'IBM'] = 0
        portfolio[i,'MSFT'] = 0
        portfolio[i,'GOOG'] = 0
        portfolio[i,'AMZN'] = 0
        portfolio[i,'TSLA'] = 0
        portfolio[i,'NFLX'] = 0
        portfolio[i,'SAP'] = 0
        portfolio[i,'ORCL'] = 0
        portfolio[i,'META'] = 0
        portfolio[i,'AAPL'] = 0
        
        for (s in stock_name){
            share = str_extract(s,"[^_]+")
            shareName = paste(share,"_Close",sep="")
            shareClose = universe[i, shareName]
            shares = floor(new_amount/shareClose)
            shareCash = new_amount %% shareClose
            cashLeft = cashLeft + shareCash
            portfolio[i,share] = shares
        }
        mtm = 0;
        mtm = mtm + (portfolio[i,'IBM'] * universe[i, 'IBM_Close'])+(portfolio[i,'MSFT'] * universe[i, 'MSFT_Close']) + (portfolio[i,'GOOG'] * universe[i, 'GOOG_Close'])+ (portfolio[i,'AMZN'] * universe[i, 'AMZN_Close']) + (portfolio[i,'AAPL'] * universe[i, 'AAPL_Close'])+ (portfolio[i,'META'] * universe[i, 'META_Close'])+ (portfolio[i,'NFLX'] * universe[i, 'NFLX_Close'])+ (portfolio[i,'TSLA'] * universe[i, 'TSLA_Close'])+ (portfolio[i,'ORCL'] * universe[i, 'ORCL_Close'])+ (portfolio[i,'SAP'] * universe[i, 'SAP_Close'])
        portfolio[i,'MTM'] = mtm
        portfolio[i, 'Cash'] = cashLeft 
    }
}
   portfolio
}

initial_portfolio=INITIALIZE()
portfolio=REBALANCE(5,initial_portfolio,TRUE)
closevalues=select(universe,ends_with("_Close"))
#calculating HTI 
hti = mutate(portfolio, HTI = apply(closevalues, 1, mean))
portfolio_low = mutate(hti, HTI_Percent_Change =round( ( (HTI - HTI[[1]]) / HTI[[1]] ) * 100, 3),MTM_Percent_Change = round( ( (MTM - MTM[[1]]) / MTM[[1]] ) * 100, 3))
head(portfolio_low)



#MTM vs HTI graph                       
mtm_series_change=portfolio_low$MTM_Percent_Change
high_tech_index_change=portfolio_low$HTI_Percent_Change
change_dates = as.factor(as.vector(universe[,'Date']))
plot(change_dates,mtm_series_change,type="l", col="blue", lty=1, main="MTM Series vs High Tech Index", 
     xlab="Dates", ylab="% Change in Value")
lines(change_dates,mtm_series_change,type="l", col="blue", lty=1)
points(change_dates,high_tech_index_change, col="red",lty=1,pch="*")
lines(change_dates,high_tech_index_change, col="red",lty=1)
legend(-5, 0, legend=c("MTM", "HTI"),text.col=c("blue","red"))


#MTM USD vs MTM JPY
usd_jpy=read.csv("USDJPY_historical_data.csv")
matching_dates = intersect(universe['Date'],usd_jpy['Date'])
usd_jpy_Close = merge(matching_dates, usd_jpy, by = intersect(names(matching_dates),names(usd_jpy)))
usd_jpy_Close = usd_jpy_Close[,'Close']
mtm_jpy = usd_jpy_Close * portfolio_low$MTM
mtm_jpy_change = round( ( (mtm_jpy - mtm_jpy[[1]]) / mtm_jpy[[1]] ) * 100, 3)
usd_jpy_change_dataframe = data.frame(date=(1:length(portfolio_low$MTM_Percent_Change)),USD=portfolio_low$MTM_Percent_Change,JPY=mtm_jpy_change)
usd_jpy_change_dataframe %>%
    gather(key,value, USD, JPY) %>%
    ggplot(aes(x=date, y=value, colour=key)) +
    geom_line() + geom_smooth() + 
    labs( title= "MTM USD VS MTM JPY (Curve)", y="% Change", x = "# Business Day of 2018")

LastMaximumValue = portfolio_low[nrow(portfolio_low),'MTM']
optimalInterval = 0
#print(length(universe[,1]))
for(m in 1:nrow(universe)){
    initial_portfolio=INITIALIZE()
    portfolio=REBALANCE(m,initial_portfolio,TRUE)
    #cat("last",portfolio[nrow(portfolio),'MTM'])    
    
    if(portfolio[nrow(portfolio),'MTM'] > LastMaximumValue){
        
        LastMaximumValue = portfolio[nrow(portfolio),'MTM']
        optimalInterval = m
    }
}
cat("The optimal interval is:",optimalInterval)
cat("\nThe maximum amount we obtain for the above optimal interval is:",LastMaximumValue)



initial_portfolio=INITIALIZE()
portfolio=REBALANCE(5,initial_portfolio,FALSE)
closevalues=select(universe,ends_with("_Close"))
#calculating HTI 
hti = mutate(portfolio, HTI = apply(closevalues, 1, mean))
portfolio_high = mutate(hti, HTI_Percent_Change =round( ( (HTI - HTI[[1]]) / HTI[[1]] ) * 100, 3),MTM_Percent_Change = round( ( (MTM - MTM[[1]]) / MTM[[1]] ) * 100, 3))
head(portfolio_high)

#MTM vs HTI graph                       
mtm_series_change=portfolio_high$MTM_Percent_Change
high_tech_index_change=portfolio_high$HTI_Percent_Change
change_dates = as.factor(as.vector(universe[,'Date']))
plot(change_dates,mtm_series_change,type="l", col="blue", lty=1, main="MTM Series vs High Tech Index", 
     xlab="Dates", ylab="% Change in Value")
lines(change_dates,mtm_series_change,type="l", col="blue", lty=1)
points(change_dates,high_tech_index_change, col="red",lty=1,pch="*")
lines(change_dates,high_tech_index_change, col="red",lty=1)
legend(-5, 0, legend=c("MTM", "HTI"),text.col=c("blue","red"))

#MTM USD vs MTM JPY
usd_jpy=read.csv("USDJPY_historical_data.csv")
matching_dates = intersect(universe['Date'],usd_jpy['Date'])
usd_jpy_Close = merge(matching_dates, usd_jpy, by = intersect(names(matching_dates),names(usd_jpy)))
usd_jpy_Close = usd_jpy_Close[,'Close']
mtm_jpy = usd_jpy_Close * portfolio_high$MTM
mtm_jpy_change = round( ( (mtm_jpy - mtm_jpy[[1]]) / mtm_jpy[[1]] ) * 100, 3)
usd_jpy_change_dataframe = data.frame(date=(1:length(portfolio_high$MTM_Percent_Change)),USD=portfolio_high$MTM_Percent_Change,JPY=mtm_jpy_change)
usd_jpy_change_dataframe %>%
    gather(key,value, USD, JPY) %>%
    ggplot(aes(x=date, y=value, colour=key)) +
    geom_line() + geom_smooth() + 
    labs( title= "MTM USD VS MTM JPY (Curve)", y="% Change", x = "# Business Day of 2018")

LastMaximumValue = portfolio_high[nrow(portfolio_high),'MTM']
optimalInterval = 0
#print(length(universe[,1]))
for(m in 1:nrow(universe)){
    initial_portfolio=INITIALIZE()
    portfolio=REBALANCE(m,initial_portfolio,FALSE)
    
    if(portfolio[nrow(portfolio),'MTM'] > LastMaximumValue){
        
        LastMaximumValue = portfolio[nrow(portfolio),'MTM']
        optimalInterval = m
    }
}
cat("The optimal interval is:",optimalInterval)
cat("\nThe maximum amount we obtain for the above optimal interval is:",LastMaximumValue)


