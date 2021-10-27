# Order mudule base

#### 設定下單程式位置 ####
ExecPath <- "C:/Users/linus/Documents/Project/6.APITols/YuantaSmart API/"
Product <- ""
#### 市價委託單 ####
OrderMKT<-function(Product, .BorS, .Qty, .Daytrade){
    
  # 下單後回傳委託書號 Order.exe TXFA8 B 0 3 MKT IOC 1
  OrderNo <- system2(paste0(ExecPath,'Order.exe'),args=paste(Product,.BorS,'0',.Qty,'MKT',"IOC", .Daytrade),stdout = TRUE)
  # 回傳委託序號
  return(OrderNo)
}

#### 限價委託單 ####
OrderLMT<-function(Product, .BorS, .Price, .Qty, .Daytrade){
  
  # 下單後回傳委託書號 Order.exe TXFA8 B 10800 3 LMT ROD 1
  OrderNo<-system2(paste0(ExecPath,'Order.exe'),args=paste(Product,.BorS,.Price,.Qty,'LMT',"ROD",.Daytrade),stdout = TRUE)
  # 回傳委託序號
  return(OrderNo)
}

#### 單筆委託查詢 ####
QueryOrder<-function(OrderNo){
  Match<-system2(paste0(ExecPath,'GetAccount.exe'),args=paste(OrderNo),stdout = TRUE)
  return(Match)
}

#### 總委託查詢 ####
QueryAllOrder<-function(){
  Match<-system2(paste0(ExecPath,'GetAccount.exe'),args=paste("ALL"),stdout = TRUE)
  Match<-Match[-c(length(Match)-1,length(Match))]
  return(Match)
}

#### 未平倉查詢 ####
QueryOnOpen<-function(){
  onopeninfo<-system2(paste0(ExecPath,'OnOpenInterest.exe'),stdout = TRUE)
  return(onopeninfo)
}

#### 權益數查詢 ####
QueryRight<-function(){
  rightinfo<-system2(paste0(ExecPath,'FutureRights.exe'),stdout = TRUE)
  nn = as.numeric(strsplit(rightinfo,split=',')[[1]][c(7,1,3,2,20,17)])
  return = list("今日權益總值"=nn[1],"權益數"=nn[2],"可動用保證金"=nn[3],
                "原始保證金"=nn[4],"維持保證金"=nn[5],"未沖銷期貨浮動損益"=nn[6])
  return(return)
}

#### 取消委託單 ####
CancelOrder<-function(OrderNo){
  system2(paste0(ExecPath,'Order.exe') ,args=paste('Delete',OrderNo),stdout = TRUE)
}

#### 查詢是否成交 ####
QueryMatch<-function(OrderNo){
  Match<-strsplit(system2(paste0(ExecPath,'GetAccount.exe'),args=paste(OrderNo),stdout = TRUE),",")[[1]]
  if( Match[2]=="全部成交" | Match[2]=="部分成交" ){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#### 限價單到期轉市價單 ####
LMT2MKT <- function(Product,.BorS,.Price,.Qty, .Daytrade, Sec){
  OrderNo<-system2(paste0(ExecPath,'Order.exe') ,args=paste(Product,.BorS,.Price,.Qty,'LMT','ROD',.Daytrade),stdout = TRUE)
  to <- Sys.time()
  while(as.numeric(difftime(Sys.time(), to, u = 'secs')) < Sec){
    if(isTRUE(QueryMatch(OrderNo))){
      return(QueryOrder(OrderNo))
    }
  }
  CancelOrder(OrderNo)
  Match<-OrderMKT(Product,.BorS,.Qty)
  return(Match)
}

#### 限價單到期轉刪單 ####
LMT2DEL <- function(Product, .BorS, .Price, .Qty, .Daytrade, Sec){
  OrderNo<-system2(paste0(ExecPath,'Order.exe') ,args=paste(Product,.BorS,.Price,.Qty,'LMT','ROD',.Daytrade),stdout = TRUE)
  to <- Sys.time()
  while(as.numeric(difftime(Sys.time(), to, u = 'secs')) < Sec){
    if(isTRUE(QueryMatch(OrderNo))){
      return(QueryOrder(OrderNo))
    }
  }
  CancelOrder(OrderNo)
  return(FALSE)
}

#### 查看所有未取消委託 ####
QueryAllUnfinished<-function(){
  system2(paste0(ExecPath,'GetUnfinished.exe'),stdout = TRUE)
}

#### 取消所有委託 ####
CancelAll<-function(){
  system2(paste0(ExecPath,'CancelALL.exe'),stdout = TRUE)
}

#### 緊急平倉 ####
#ClosePositionAll<-function(){
#  system2(paste0(ExecPath,'MayDay.exe'),stdout = TRUE)
#}

#### 變更連線中商品 ####
ChangeProd<-function(){
  system2(paste0(ExecPath,'ChangeProdid.exe'), args = Product,stdout = TRUE)
}

#### 查詢帳戶資訊 ####
QueryFutureRights<-function(){
  system2(paste0(ExecPath,'FutureRights.exe'), stdout = TRUE)
}

#### 取得所有未成交之委託單 ####
QueryUnfinished<-function(){
  system2(paste0(ExecPath,'GetUnfinished.exe'), stdout = TRUE)
}

#### 行情 取用 ####
QueryOHCL<-function(data.path){
  #system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", n, " ",data.path), stdout = TRUE)
  .Price.file <- read.csv(data.path, header = FALSE)
  .Price.tail <- tail(.Price.file, 1)
  single.Price <- as.numeric(.Price.tail[3])
  return(single.Price)
}

#### 緊急平倉 ####
ClosePositionAll<-function()
{
  # if (!simu.mode){
    # 下單後回傳委託書號 Order.exe TXFA8 B 10800 3 LMT ROD 1
    OrderNo<-system2(paste0(ExecPath,'MayDay.exe'),stdout = TRUE)
    # 回傳委託序號
    return(OrderNo)

}

#### 限價委託單 ####
Place.OrderLMT<-function(.BorS, .Price, .Qty, .Daytrade)
{
  
  order.cmd <- ""
  
  # if (!simu.mode){
    # 下單後回傳委託書號 Order.exe TXFA8 B 10800 3 LMT ROD 1
    order.cmd <-paste(Product, .BorS, .Price, .Qty, 'LMT', "ROD", .Daytrade)
    OrderNo<-system2(paste0(ExecPath,'Order.exe'),args=order.cmd,stdout = TRUE)
    # 回傳委託序號
    return(OrderNo)
  # }else{
  #   return("SIMU")
  # }
}

#### 市價委託單 ####
Place.OrderMKT<-function(.BorS, .Qty, .Daytrade)
{
  
  order.cmd <- ""
  
  # if (!simu.mode){
    # 下單後回傳委託書號 Order.exe TXFA8 B 0 3 MKT IOC 1
    order.cmd <- paste(Product, .BorS, '0', .Qty, 'MKT', "IOC", .Daytrade)
    OrderNo <- system2(paste0(ExecPath,'Order.exe'), args=order.cmd, stdout = TRUE)
    # 回傳委託序號
    return(OrderNo)
  # }else{
  #   return("SIMU")
  # }
}
