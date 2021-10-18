##
#### 緊急平倉 ####
ClosePositionAll<-function(){
  #system2(paste0(ExecPath,'MayDay.exe'),stdout = TRUE)
  if (!simu){
    # 下單後回傳委託書號 Order.exe TXFA8 B 10800 3 LMT ROD 1
    OrderNo<-system2(paste0(ExecPath,'MayDay.exe'),stdout = TRUE)
    print(paste("[下單觸發] 快速平倉"))
    # 回傳委託序號
    return(OrderNo)
  }else{
    print(paste("[模擬下單觸發] 快速平倉"))
  }  
}

#### 最新報價 ####
Price.current<-function(pr="CL")
{

  if(!file.exists(data.path))
  {
    return(0)
  }else{
    x <-3
    if(pr=="CL"){x=3}
    if(pr=="HI"){x=6}
    if(pr=="LO"){x=7}
    
    result <- QueryOHCL(data.path, 1)
    result <- strsplit(result, ",") 
    result <- result[[1]][x]   
  }

}

#### 權益數解讀 ####
Right.current<-function(x=6)
{
  
  result <- QueryRight()
  return(result[[x]])
  
}

#### 限價委託單 ####
Place.OrderLMT<-function()
{
  
  #Qty <-1  
  order.cmd <- ""
  
  if (!simu){
    # 下單後回傳委託書號 Order.exe TXFA8 B 10800 3 LMT ROD 1
    order.cmd <-paste(Product,BorS,Price,Qty,'LMT',"ROD",Daytrade)
    # OrderNo<-system2(paste0(ExecPath,'Order.exe'),args=paste(Product,BorS,Price,Qty,'LMT',"ROD",Daytrade),stdout = TRUE)
    OrderNo<-system2(paste0(ExecPath,'Order.exe'),args=order.cmd,stdout = TRUE)
    print(paste("[下單觸發] 限價委託單 :", OrderNo, " | ",order.cmd))
    # 回傳委託序號
    return(OrderNo)
  }else{
    print(paste("[模擬下單觸發] 限價委託單 :",order.cmd))
  }
}

#### 市價委託單 ####
Place.OrderMKT<-function()
{
  
  #Qty <-1  
  order.cmd <- ""
  
  if (!simu){
    # 下單後回傳委託書號 Order.exe TXFA8 B 0 3 MKT IOC 1
    order.cmd <- paste(Product,BorS,'0',Qty,'MKT',"IOC", Daytrade)
    OrderNo <- system2(paste0(ExecPath,'Order.exe'), args=order.cmd, stdout = TRUE)
    print(paste("[下單觸發] 市價委託單 :", OrderNo, "|", order.cmd))
    # 回傳委託序號
    return(OrderNo)
  }else{
    print(paste("[模擬下單觸發] 市價委託單 :",order.cmd))
  }
}

#執行平倉方式
if.safeClose <-function(bs=NULL)
{
  if (!safe.Close)
  {
    #預設限價單
    if(bs =="B" || bs =="S")
    {
      BorS <- bs
      Price <- Price.current()
      result <- Place.OrderLMT()      
    }
    
    #市價單 
    if(bs =="MB" || bs =="MS")
    {
      BorS <- gsub("M", "", bs)
      # Price <- Price.current()
      # result <- Place.OrderLMT() 
      # BorS <- "S"
      Price <- Price.current()
      result <- Place.OrderMKT()
    }
    
  }else{
    result <- ClosePositionAll()             
  }  
}
# 

filename.gen <-function(x=NULL, name)
{
  
  if(x =="product" || is.null(x)){return(paste0(name, "_Match.txt"))}
  if(x ==MXFSIMU.Name){return(paste0(MXFSIMU.Name, "_Match.txt"))}
  if(x =="log"){return(paste0(MXFSIMU.Name, "_MatchLOG.txt"))}
}

finacial.dataparg.gen <- function(data.path, date.format, Product, Product.file)
{
  data.path <- paste0(data.path
                      , date.format, "/"
                      , Product, "/"
                      , Product.file)
  return(data.path)
}

data.source.switch <-function(x)
{
  .soource <- ifelse(x, SECURTIES.data.path, MXFSIMU.forSERVER.filename)
  return(.soource)
}

TF.Switch <-function(logic.val)
{
  if(logic.val){val <-FALSE}
  else{val <-TRUE}
  
  return(val)
}

extra.data <-function(name="CL", p.mode="num")
{
  #設定訊息檔案路徑
  close.ALL.path <- paste0(price.path, "close_allPOSITION", ".csv")
  price.open.path <- paste0(price.path, "open", ".csv")
  price.high.path <- paste0(price.path, "high", ".csv")
  price.close.path <- paste0(price.path, "close", ".csv")
  price.low.path <- paste0(price.path, "low", ".csv")
  price.ma5.path <- paste0(price.path, "_ma5", ".csv")
  price.ma10.path <- paste0(price.path, "_ma10", ".csv")
  price.ma20.path <- paste0(price.path, "_ma20", ".csv")  
  price.Rate_sma5.path <- paste0(price.path, "_Rate_sma5", ".csv")
  price.Rate_sma10.path <- paste0(price.path, "_Rate_sma10", ".csv")
  price.Rate_sma20.path <- paste0(price.path, "_Rate_sma20", ".csv") 
  #
  ploar_star.path <- paste0(price.path, "_ploar_star", ".csv")
  ploar_star_price.path <- paste0(price.path, "_ploar_star_price", ".csv")
  ploar_star_StopLoss.path <- paste0(price.path, "_ploar_star_stopLoss", ".csv")
  polar_star_switch.path <- paste0(price.path, "_polar_star_switch", ".csv")
  #
  b_upper.path <- paste0(price.path, "_b_upper", ".csv")
  b_lower.path <- paste0(price.path, "_b_lower", ".csv")
  #
  rsi.path <- paste0(price.path, "RSI", ".csv")
  rsi_pre.path <- paste0(price.path, "RSI_pre", ".csv")
  rsi_ma5.path <- paste0(price.path, "RSI_MA5", ".csv")
  #
  bsrate.path <- paste0(price.path, "_BSRate", ".csv")
  bsrateDiff.path <- paste0(price.path, "_BSRateDiff", ".csv")
  #
  op_ma5.path <- paste0(price.path, "_op_ma5", ".csv")
  op_ma10.path <- paste0(price.path, "_op_ma10", ".csv")
  op_ma20.path <- paste0(price.path, "_op_ma20", ".csv")
  op_ma60.path <- paste0(price.path, "_op_ma60", ".csv")
  op_ma.path <- paste0(price.path, "_op_ma", ".csv")
  #
  Research_Line_Upper.path <- paste0(price.path, "_Research_Line_Upper", ".csv")
  Research_Line_Mid.path <-   paste0(price.path, "_Research_Line_Mid", ".csv")
  Research_Line_lower.path <- paste0(price.path, "_Research_Line_lower", ".csv")
  extremes_Line_Upper.path <- paste0(price.path, "_extremes_Line_Upper", ".csv")
  extremes_Line_Mid.path <-   paste0(price.path, "_extremes_Line_Mid", ".csv")
  extremes_Line_lower.path <- paste0(price.path, "_extremes_Line_lower", ".csv")
  #
  enable.STABLE.Stop.PORT.path  <- paste0(price.path, "enable.stable.Stop.PORT", ".csv")
  enable.onlyMDD.path  <- paste0(price.path, "enable.onlyMDD", ".csv")
  enable.RSI.TrendADDED.path  <- paste0(price.path, "enable.RSI.TrendADDED", ".csv")
  enable.Bolling.path  <- paste0(price.path, "enable.BollingPATH.ADDED", ".csv")
  #
  DISABLE_MXFSIMU.SERVERE.path <- paste0(price.path, "DISABLE_MXFSIMU.SERVERE", ".csv")
  #
  
  operator <- gsub(" ", "", name)
  
  if(p.mode =="num")
  {
    switch(operator,
           
           OP =
             {
               price.open <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.open.path), stdout = TRUE))
               return(price.open)
             },
           HI =
             {
               price.high <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.high.path), stdout = TRUE))
               return(price.high)
             },         
           CL =
             {
               price.close <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.close.path), stdout = TRUE))
               return(price.close)
             },
           LO =
             {
               price.low <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.low.path), stdout = TRUE))
               return(price.low)
             }, 
           MA5 =
             {
               price.ma5 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.ma5.path), stdout = TRUE))
               return(price.ma5)
             },
           MA10 =
             {
               price.ma10 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.ma10.path), stdout = TRUE))
               return(price.ma10)
             },
           MA20 =
             {
               price.ma20 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.ma20.path), stdout = TRUE))
               return(price.ma20)
             },
           Rate_sma5 =
             {
               price.Rate_sma5 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.Rate_sma5.path), stdout = TRUE))
               return(price.Rate_sma5)
             },
           Rate_sma10 =
             {
               price.Rate_sma10 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.Rate_sma10.path), stdout = TRUE))
               return(price.Rate_sma10)
             },
           Rate_sma20 =
             {
               price.Rate_sma20 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.Rate_sma20.path), stdout = TRUE))
               return(price.Rate_sma20)
             },
           Research_Line_Upper =
             {
               price.Research_Line_Upper <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", Research_Line_Upper.path), stdout = TRUE))
               return(price.Research_Line_Upper)
             },         
           Research_Line_Mid =
             {
               price.Research_Line_Mid <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", Research_Line_Mid.path), stdout = TRUE))
               return(price.Research_Line_Mid)
             },   
           Research_Line_lower =
             {
               price.Research_Line_lower <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", Research_Line_lower.path), stdout = TRUE))
               return(price.Research_Line_lower)
             }, 
           extremes_Line_Upper =
             {
               price.extremes_Line_Upper <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", extremes_Line_Upper.path), stdout = TRUE))
               return(price.extremes_Line_Upper)
             }, 
           extremes_Line_Mid =
             {
               price.extremes_Line_Mid <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", extremes_Line_Mid.path), stdout = TRUE))
               return(price.extremes_Line_Mid)
             }, 
           extremes_Line_lower =
             {
               price.extremes_Line_lower <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", extremes_Line_lower.path ), stdout = TRUE))
               return(price.extremes_Line_lower)
             }, 
           ploar_star =
             {
               code.ploar_star <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", ploar_star.path), stdout = TRUE))
               return(code.ploar_star)
             },
           ploar_star_price =
             {
               code.ploar_star_price <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", ploar_star_price.path), stdout = TRUE))
               return(code.ploar_star_price)
             },
           ploar_star_stopLoss =
             {
               code.ploar_star_stopLoss <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", ploar_star_StopLoss.path), stdout = TRUE))
               return(code.ploar_star_stopLoss)
             },
           polar_star_switch =
             {
               code.polar_star_switch.path <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", polar_star_switch.path), stdout = TRUE))
               return(code.polar_star_switch.path)
             },           
           
           B_UP =
             {
               price.b_upper <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", b_upper.path), stdout = TRUE))
               return(price.b_upper)
             },
           B_LO =
             {
               price.b_lower <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", b_lower.path), stdout = TRUE))
               return(price.b_lower)
             },
           RSI =
             {
               price.rsi <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", rsi.path), stdout = TRUE))
               return(price.rsi)
             },
           RSI_PRE =
             {
               price.rsi.pre <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", rsi_pre.path), stdout = TRUE))
               return(price.rsi.pre)
             },
           RSI_MA5 =
             {
               price.rsi_ma5 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", rsi_ma5.path), stdout = TRUE))
               return(price.rsi_ma5)
             },
           BSRate =
             {
               price.bsrate <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", bsrate.path), stdout = TRUE))
               return(price.bsrate)
             },
           BSRateDiff =
             {
               price.bsrateDiff <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", bsrateDiff.path), stdout = TRUE))
               return(price.bsrateDiff)
             },
           op_ma5 =
             {
               price.op_ma5 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", op_ma5.path), stdout = TRUE))
               return(price.op_ma5)
             },
           op_ma10 =
             {
               price.op_ma10 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", op_ma10.path), stdout = TRUE))
               return(price.op_ma10)
             },
           op_ma20 =
             {
               price.op_ma20 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", op_ma20.path), stdout = TRUE))
               return(price.op_ma20)
             },
           op_ma60 =
             {
               price.op_ma60 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", op_ma60.path), stdout = TRUE))
               return(price.op_ma60)
             },
           op_ma =
             {
               price.op_ma <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", op_ma.path), stdout = TRUE))
               return(price.op_ma)
             }
    )    
  }
  
  if(p.mode =="path")
  {
    switch(operator,
           
           OP =
             {
               return(price.open.path)
             },
           HI =
             {
               return(price.high.path)
             },
           CL =
             {
               return(price.close.path)
             },
           LO =
             {
               return(price.low.path)
             },
           MA5 =
             {
               return(price.ma5.path)
             },
           MA10 =
             {
               return(price.ma10.path)
             },
           MA20 =
             {
               return(price.ma20.path)
             },
           Rate_sma5 =
             {
               return(price.Rate_sma5.path)
             },
           Rate_sma10 =
             {
               return(price.Rate_sma10.path)
             },
           Rate_sma20 =
             {
               return(price.Rate_sma20.path)
             },
           Research_Line_Upper =
             {
               return(Research_Line_Upper.path)
             },
           Research_Line_Mid =
             {
               return(Research_Line_Mid.path)
             },
           Research_Line_lower =
             {
               return(Research_Line_lower.path)
             },
           extremes_Line_Upper =
             {
               return(extremes_Line_Upper.path)
             },
           extremes_Line_Mid =
             {
               return(extremes_Line_Mid.path)
             },
           extremes_Line_lower =
             {
               return(extremes_Line_lower.path)
             },
           ploar_star =
             {
               return(ploar_star.path)
             },
           ploar_star_price =
             {
               return(ploar_star_price.path)
             },
           ploar_star_stopLoss =
             {
               return(ploar_star_StopLoss.path)
             },
           polar_star_switch =
             {
               return(polar_star_switch.path)
             }, 
           B_UP =
             {
               return(b_upper.path)
             },
           B_LO =
             {
               return(b_lower.path)
             },
           RSI =
             {
               return(rsi.path)
             },
           RSI_PRE =
             {
               return(rsi_pre.path)
             },
           RSI_MA5 =
             {
               return(rsi_ma5.path)
             },
           BSRate =
             {
               return(bsrate.path)
             },
           BSRateDiff =
             {
               return(bsrateDiff.path)
             },
           op_ma5 =
             {
               return(op_ma5.path)
             },
           op_ma10 =
             {
               return(op_ma10.path)
             },
           op_ma20 =
             {
               return(op_ma20.path)
             },
           op_ma60 =
             {
               return(op_ma60.path)
             },
           op_ma =
             {
               return(op_ma.path)
             },
           
           enable.STABLE.Stop.PORT =
             {
               return(enable.STABLE.Stop.PORT.path)
             },
           enable.onlyMDD =
             {
               return(enable.onlyMDD.path)
             },
           enable.RSI.TrendADDED =
             {
               return(enable.RSI.TrendADDED.path)
             },
           enable.BollingPATH.ADDED =
             {
               return(enable.Bolling.path)
             },
           DMSS ={
               return(DISABLE_MXFSIMU.SERVERE.path)
             },
           close.ALLPOSITION =
             {
               return(close.ALL.path)
             }
    )
  }
  
  
}


