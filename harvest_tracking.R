# 
rm(list=ls())
#CUstom LIB.
#
{library("beepr")
library("sound")
library("audio")
library("magrittr")
library("e1071")
library("TTR")
library("quantmod")
library("xts")}
#
setwd("C:/Temp/")
extra.lib.path <-"C:/Users/linus/Documents/Project/1.R/"
#### 仔入額外函式 #### 
# source("C:/Users/linus/Documents/Project/8.Research.Material/NEW_GENERATION/PT.Tools.R")
#Analysis.of.trading.strategies
source(paste0(extra.lib.path, "Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/R/m_misc.R"))
source(paste0(extra.lib.path, "Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/R/m_env.R"))
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_custom.R")
# source("C:/Users/linus/Documents/Project/6.APITols/FutureTools_DataMGR.R")
source("C:/Users/linus/Documents/Project/6.APITols/configure.R")
##

portfolio.count<-function(buyin, sellout, amount=1, discount=0.4)
{
  
  unit=1000
  rate.service <-0.1425/100
  rate.trade   <-0.3/100
  
  pay.service <-(buyin*unit*amount*rate.service)
                  +(sellout*unit*amount*rate.service)
  pay.trade <-(sellout*unit*amount*rate.trade) 
  
  stock.buyin <-buyin*unit*amount*discount
  stock.sellout <-sellout*unit*amount*discount
  result <-c(stock.buyin, stock.sellout, (stock.sellout -stock.buyin), pay.trade*-1, pay.service*-1)
  names(result) <-c("buyin", "sellout", "diff", "pay.trade", "pay.service")
  return(result)
}



cross.status<- function(.op, .hi, .lo, .cl, obj.PRICE) {
 
  if(.hi >obj.PRICE 
     & .lo <obj.PRICE
     & .cl >obj.PRICE
     & .cl >.op){
    return(1)
  }
  
  if(.hi >obj.PRICE 
     & .lo <obj.PRICE
     & .cl <obj.PRICE
     & .cl <.op){
    return(-1)
  }
  
  return(0)
   
}


analyze.stock.rate <-function(file.list, date)
{
  
  all.data<-data.frame()
  for(file.id in 1:length(file.list))
  {
    
    file.data<- read.csv(file=file.list[file.id], header=FALSE, sep = ",")
    stock.code <-m_gsub(c(.patterm, ".log"), "", basename(file.list[file.id]))
    names(file.data) <-title.name
    a <- file.data[file.data$date ==m.date,][,-1]
    
    if(nrow(a) >10)
    {
      a.sub <- a[,c("time", "close")]
      a.sub$roc <-ROC(a.sub$close)
      a.sub$roc[1] <-0
      a.sub$cumsum <- cumsum(a.sub$roc)
      a.sub <- a.sub[, c("time", "cumsum")]
      names(a.sub) <-c("time", stock.code)
      if(file.id ==1)
      {
        all.data <-a.sub
        
      }else{
        all.data <- merge(all.data, a.sub, by="time")
        
        
      }      
    }

  }
  
  return(all.data)
  
}

##主程式
.pattern <- "盤中即時紀錄_TSE"
# Control.group <-c("2337", "2408", "2449", "4967", "8150")
file.list <- list.files(path=source.path, pattern =.pattern, full.names = TRUE )
# file.list <- list.files(path=paste0(getwd(), "/", "Group.main"), pattern =.pattern, full.names = TRUE )
# file.list.Control.group <- list.files(path=paste0(getwd(), "/", "Group.control"), pattern =.pattern, full.names = TRUE )

m.date <-"20211112"
##

all.data<-data.frame()

# for(file.id in 1:length(file.list))
# {
#   
#   file.data<- read.csv(file=file.list[file.id], header=FALSE, sep = ",")
#   stock.code <-m_gsub(c(.patterm, ".log"), "", basename(file.list[file.id]))
#   names(file.data) <-title.name
#   a <- file.data[file.data$date ==m.date,][,-1]
#   
#   if(nrow(a) >10)
#   {
#     a.sub <- a[,c("time", "close")]
#     a.sub$roc <-ROC(a.sub$close)
#     a.sub$roc[1] <-0
#     a.sub$cumsum <- cumsum(a.sub$roc)
#     a.sub <- a.sub[, c("time", "cumsum")]
#     names(a.sub) <-c("time", stock.code)
#     if(file.id ==1)
#     {
#       all.data <-a.sub
# 
#     }else{
#       all.data <- merge(all.data, a.sub, by="time")
# 
# 
#     }      
#   }
#   
# 
# }


##
all.data <-analyze.stock.rate(file.list, m.date)
# Control.group.data <- -analyze.stock.rate(file.list.Control.group, "20211111")

# for(file.id in 1:nrow(all.data))
# {
#   all.data$rate[file.id] <-sum(all.data[file.id,-1])
#   Control.group.data$rate[file.id] <-sum(Control.group.data[file.id,-1])
# }

sec.all.data <-all.data[, -1]
all.name <-colnames(all.data)
sec.name <-all.name[-1]
sec.time <-as.character(all.data[, 1])
leng <-nrow(sec.all.data)

monitor.data <-function(mod.test=FALSE)
{
  
  for(miu in 2:leng)
  {
    
    sec.data <-sec.all.data[miu,]
    sec.order <-order(sec.data, decreasing = TRUE)
    sec.order.data <-sec.data[sec.order]
    sec.order.name <-sec.name[sec.order]
    sec.order.time <-sec.time[miu]
    # names(sec.order.data) <-sec.order.name
    sec.order.num <-paste0(round(as.numeric(sec.order.data), digits = 4)*100, "%")

    sec.order.bind <-(paste0(sec.order.name, " :", sec.order.num))
    sec.info <-c(sec.order.time, sec.order.bind[1:4])
    print(sec.info) 
    Sys.sleep(1)
  }  
  
}

monitor.data()

# .col <-c("black", "red", "orange", "yellow", "green", "blue", "purple", "blue", "blue")
plot(all.data$TSE22.TW*100, type="l", col=.col)
# lines(all.data$TSE21.TW, col="red")
# lines(all.data$TSE22.TW, col="orange")
# lines(all.data$TSE28.TW, col="yellow")
# lines(all.data$TSE26.TW, col="green")
# lines(all.data$TSE12.TW, col="blue")
# lines(all.data$TSE15.TW, col="purple")
# lines(all.data$TSE25.TW, col="blue")
# lines(all.data$TSE16.TW, col="blue")
# #
# plot(Control.group.data$rate, type="l", col="red")
# lines(Control.group.data$rate, col="blue")
# 
# #
# append.to.file(data=all.data, path="test1.csv", m.col.names = TRUE, m.append = FALSE)
