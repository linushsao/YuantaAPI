##
#### 緊急平倉 ####
ClosePositionAll<-function(){
  #system2(paste0(ExecPath,'MayDay.exe'),stdout = TRUE)
  if (!simu){
    # 下單後回傳委託書號 Order.exe TXFA8 B 10800 3 LMT ROD 1
    OrderNo<-system2(paste0(ExecPath,'MayDay.exe'),stdout = TRUE)
    # print(paste("[下單觸發] 快速平倉"))
    # 回傳委託序號
    return(OrderNo)
  }else{
    # print(paste("[模擬下單觸發] 快速平倉"))
    return("SIMU")
    
  }  
}

#### 最新報價 ####
Price.current<-function(pr="CL")
{

  # if(!file.exists(data.path))
  # {
  #   return(0)
  # }else{
  #   x <-3
  #   if(pr=="CL"){x=3}
  #   if(pr=="HI"){x=6}
  #   if(pr=="LO"){x=7}
  #   
  #   result <- QueryOHCL(data.path, 1)
  #   result <- strsplit(result, ",") 
  #   result <- result[[1]][x]   
  # }
  result <- QueryOHCL(data.path = data.path)

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
    # 回傳委託序號
    return(OrderNo)
  }else{
    return("SIMU")
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
    # 回傳委託序號
    return(OrderNo)
  }else{
    return("SIMU")
  }
}

#執行平倉方式
if.safeClose <-function(bs=NULL)
{
  if(!simu)
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
        Price <- Price.current()
        result <- Place.OrderMKT()
      }
      
    }else{
      result <- ClosePositionAll()             
    }  
  }else{
    
    result <-"SIMU"
  }
  
  return(result)
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

append.to.file <- function(data, path)
{
  write.table(data, file=path, sep=","
              , row.names=F, na = "NA", 
              append=TRUE,col.names=FALSE)
}

push.pull <- function(x, p.mode)
{

.leng <- length(x)

if(p.mode) #左推出最舊一筆資料(PUSH最常用)
{
  for(value1 in 2:.leng)
  {
    x[value1 -1] = x[value1];
  }
  x[.leng] = 0;
}else ##右推出最新一筆資料(PULL)
  {
    for(value1 in 2:.leng)
    {
      x[value1] = x[value1 -1];
    }	
  x[1] = 0;
  }

  return(x)
}

p_n.sig <-function(x)
{
  if(x>0){return(1)}
  else if(x<0){return(-1)}
  else{reutrn(x)}
  
}

check.if.deal <-function(force=FALSE, decoded.info=NULL)
{
  if(switch.check.if.deal || force)
  {
    .check <-account.info(p.mode="by.name", info=decoded.info)
    if( .check =="全部成交" ){return(TRUE)}
    else{return(FALSE)}    
  }
  else{return(TRUE)}

}

account.info <- function(code=NULL, p.mode="analyze", info=NULL, name="status", simu=FALSE)
{
  
  if(p.mode =="analyze")
  {
    if(code !="SIMU")
    {
      .info <-  QueryOrder(code)
      .info <- strsplit(.info, ",")
      
      result <-c(
        .info[[1]][2]
        , .info[[1]][3]
        , .info[[1]][4]
        , .info[[1]][5]
        , .info[[1]][6]
        , .info[[1]][7]
        
      )      
    }else
          {
            result <- c(rep(NULL, 6))
            result[1] <-"全部成交"
            result[4] <-Price.current()
      
            result <-c(result, simu)
          }
      
    return(result)    
  }
  
  if(p.mode =="by.name")
  {
    if(!is.null(info))
    {
      switch(name,
             status ={x<-1},
             product={x<-2},
             ls     ={x<-3},
             price  ={x<-4},
             amount ={x<-5},
             period ={x<-6},
             x=0
             )
      if(x ==0){return(0)}
      else{return(info[x])}
    }else{return(0)}
   
  }

}
# 
# transaction.MGR <-function(pdt =NULL, bors=NULL, pr=NULL, Qty=1, type="LMT")
# {
#   if(is.null(pdt))
#   {
#     Price <- Product
#   }else{
#     Price <- pdt
#     
#   }
#   
#   BorS <- bors
#   if(is.null(pr))
#   {
#     Price <- Price.current()
#   }else{
#     Price <- pr
#     
#   }
#   
#   if(type =="LMT"){result<- Place.OrderLMT()} #下單
#   if(type =="MKT"){result<- Place.OrderMKT()} #下單
#   
#   return(result)
# 
# }

extra.data <-function(name="CL", p.mode="num")
{
  #設定訊息檔案路徑
  create.positionLONG.path <- paste0(price.path, "create.POSITIONLong", ".csv")
  create.positionSHORT.path <- paste0(price.path, "create.POSITIONShort", ".csv")
  
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
  price.Buyin.path <- paste0(price.path, "Price.buyin", ".csv") 
  price.PCL.path <- paste0(price.path, "Price.pcl", ".csv")
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
  switch_to.ma.path <- paste0(price.path, "switch_to.ma", ".csv")
  switch_to.rsi.path <- paste0(price.path, "switch_to.rsi", ".csv")
  
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
  DISABLE_AGENT.SERVERE.path <- paste0(price.path, "DISABLE_AGENT.SERVERE", ".csv")
  RESET_AGENT.SERVERE.path <- paste0(price.path, "RESET_AGENT.SERVERE", ".csv")
  
  #
  MA5.CREATE.LONG.path <- paste0(price.path, "MA5.CREATE.LONG", ".csv")
  MA10.CREATE.LONG.path <- paste0(price.path, "MA10.CREATE.LONG", ".csv")
  MA20.CREATE.LONG.path <- paste0(price.path, "MA20.CREATE.LONG", ".csv")
  MA5.CREATE.SHORT.path <- paste0(price.path, "MA5.CREATE.SHORT", ".csv")
  MA10.CREATE.SHORT.path <- paste0(price.path, "MA10.CREATE.SHORT", ".csv")
  MA20.CREATE.SHORT.path <- paste0(price.path, "MA20.CREATE.SHORT", ".csv")  
  #
  CUSTOM.CREATE.LONG.path <- paste0(price.path, "CUSTOM.CREATE.LONG", ".csv")
  CUSTOM.CREATE.SHORT.path <- paste0(price.path, "CUSTOM.CREATE.SHORT", ".csv")
  
  #
  currentBar.path <- paste0(price.path, "currentBar", ".csv")
  
  #
  m.tail <-function(path)
  {
    price.file <- read.csv(path, header = FALSE)
    price.tail <- tail(price.file, 1)
    return(price.tail)
  }
  operator <- gsub(" ", "", name)
  
  if(p.mode =="num")
  {
    switch(operator,
           
           OP =
             {
               if(file.exists(price.open.path))
               {
                 # price.open <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.open.path), stdout = TRUE))
                 price.open <- as.character(m.tail(price.open.path))
                 
                 return(price.open)
                }else{return(0)}
             },
           HI =
             {
               if(file.exists(price.high.path))
               {
                 # price.high <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.high.path), stdout = TRUE))
                 price.high <- as.character(m.tail(price.high.path))
                 
                 return(price.high)
               }else{return(0)}
               # 
               # price.high <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.high.path), stdout = TRUE))
               # return(price.high)
             },         
           CL =
             {
               if(file.exists(price.close.path))
               {
                 # price.close <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.close.path), stdout = TRUE))
                 price.close <- as.character(m.tail(price.close.path))
                 
                 return(price.close)
               }else{return(0)}
               # price.close <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.close.path), stdout = TRUE))
               # return(price.close)
             },
           LO =
             {
               if(file.exists(price.low.path))
               {
                 # price.low <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.low.path), stdout = TRUE))
                 price.low <- as.character(m.tail(price.low.path))
                 
                 return(price.low)
               }else{return(0)}
               # price.low <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.low.path), stdout = TRUE))
               # return(price.low)
             }, 
           MA5 =
             {
               if(file.exists(price.ma5.path))
               {
                 # price.ma5 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.ma5.path), stdout = TRUE))
                 price.ma5 <- as.character(m.tail(price.ma5.path))
                 
                 return(price.ma5)
               }else{return(0)}
               # price.ma5 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.ma5.path), stdout = TRUE))
               # return(price.ma5)
             },
           MA10 =
             {
               if(file.exists(price.ma10.path))
               {
                 # price.ma10 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.ma10.path), stdout = TRUE))
                 price.ma10 <- as.character(m.tail(price.ma10.path))
                 
                 return(price.ma10)
               }else{return(0)}
               # price.ma10 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.ma10.path), stdout = TRUE))
               # return(price.ma10)
             },
           MA20 =
             {
               if(file.exists(price.ma20.path))
               {
                 # price.ma20 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.ma20.path), stdout = TRUE))
                 price.ma20 <- as.character(m.tail(price.ma20.path))
                 
                 return(price.ma20)
               }else{return(0)}
               # price.ma20 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.ma20.path), stdout = TRUE))
               # return(price.ma20)
             },
           Rate_sma5 =
             {
               if(file.exists(price.Rate_sma5.path))
               {
                 # price.Rate_sma5 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.Rate_sma5.path), stdout = TRUE))
                 price.Rate_sma5 <- as.character(m.tail(price.Rate_sma5.path))
                 
                 return(price.Rate_sma5)
               }else{return(0)}
               # price.Rate_sma5 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.Rate_sma5.path), stdout = TRUE))
               # return(price.Rate_sma5)
             },
           Rate_sma10 =
             {
               if(file.exists(price.Rate_sma10.path))
               {
                 # price.Rate_sma10 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.Rate_sma10.path), stdout = TRUE))
                 price.Rate_sma10 <- as.character(m.tail(price.Rate_sma10.path))
                 
                 return(price.Rate_sma10)
               }else{return(0)}
               # price.Rate_sma10 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.Rate_sma10.path), stdout = TRUE))
               # return(price.Rate_sma10)
             },
           Rate_sma20 =
             {
               if(file.exists(price.Rate_sma20.path))
               {
                 # price.Rate_sma20 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.Rate_sma20.path), stdout = TRUE))
                 price.Rate_sma20 <- as.character(m.tail(price.Rate_sma20.path))
                 
                 return(price.Rate_sma20)
               }else{return(0)}
               # price.Rate_sma20 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.Rate_sma20.path), stdout = TRUE))
               # return(price.Rate_sma20)
             },
           Research_Line_Upper =
             {
               if(file.exists(Research_Line_Upper.path))
               {
                 # price.Research_Line_Upper <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", Research_Line_Upper.path), stdout = TRUE))
                 price.Research_Line_Upper <- as.character(m.tail(Research_Line_Upper.path))
                 
                 return(price.Research_Line_Upper)
               }else{return(0)}
               # price.Research_Line_Upper <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", Research_Line_Upper.path), stdout = TRUE))
               # return(price.Research_Line_Upper)
             },         
           Research_Line_Mid =
             {
               if(file.exists(Research_Line_Mid.path))
               {
                 # price.Research_Line_Mid <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", Research_Line_Mid.path), stdout = TRUE))
                 price.Research_Line_Mid <- as.character(m.tail(Research_Line_Mid.path))
                 
                 return(price.Research_Line_Mid)
               }else{return(0)}
               # price.Research_Line_Mid <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", Research_Line_Mid.path), stdout = TRUE))
               # return(price.Research_Line_Mid)
             },   
           Research_Line_lower =
             {
               if(file.exists(Research_Line_lower.path))
               {
                 # price.Research_Line_lower <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", Research_Line_lower.path), stdout = TRUE))
                 price.Research_Line_lower <- as.character(m.tail(Research_Line_lower.path))
                 
                 return(price.Research_Line_lower)
               }else{return(0)}
               # price.Research_Line_lower <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", Research_Line_lower.path), stdout = TRUE))
               # return(price.Research_Line_lower)
             }, 
           extremes_Line_Upper =
             {
               if(file.exists(extremes_Line_Upper.path))
               {
                 # price.extremes_Line_Upper <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", extremes_Line_Upper.path), stdout = TRUE))
                 price.extremes_Line_Upper <- as.character(m.tail(extremes_Line_Upper.path))
                 
                 return(price.extremes_Line_Upper)
               }else{return(0)}
               # price.extremes_Line_Upper <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", extremes_Line_Upper.path), stdout = TRUE))
               # return(price.extremes_Line_Upper)
             }, 
           extremes_Line_Mid =
             {
               if(file.exists(extremes_Line_Mid.path))
               {
                 # price.extremes_Line_Mid <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", extremes_Line_Mid.path), stdout = TRUE))
                 price.extremes_Line_Mid <- as.character(m.tail(extremes_Line_Mid.path))
                 
                 return(price.extremes_Line_Mid)
               }else{return(0)}
               # price.extremes_Line_Mid <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", extremes_Line_Mid.path), stdout = TRUE))
               # return(price.extremes_Line_Mid)
             }, 
           extremes_Line_lower =
             {
               if(file.exists(extremes_Line_lower.path))
               {
                 # price.extremes_Line_lower <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", extremes_Line_lower.path ), stdout = TRUE))
                 price.extremes_Line_lower <- as.character(m.tail(extremes_Line_lower.path))
                 
                 return(price.extremes_Line_lower)
               }else{return(0)}
               # price.extremes_Line_lower <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", extremes_Line_lower.path ), stdout = TRUE))
               # return(price.extremes_Line_lower)
             }, 
           ploar_star =
             {
               if(file.exists(ploar_star.path))
               {
                 # code.ploar_star <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", ploar_star.path), stdout = TRUE))
                 code.ploar_star <- as.character(m.tail(ploar_star.path))
                 
                 return(code.ploar_star)
               }else{return(0)}
               # code.ploar_star <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", ploar_star.path), stdout = TRUE))
               # return(code.ploar_star)
             },
           ploar_star_price =
             {
               if(file.exists(ploar_star_price.path))
               {
                 # code.ploar_star_price <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", ploar_star_price.path), stdout = TRUE))
                 code.ploar_star_price <- as.character(m.tail(ploar_star_price.path))
                 
                 return(code.ploar_star_price)
               }else{return(0)}
               # code.ploar_star_price <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", ploar_star_price.path), stdout = TRUE))
               # return(code.ploar_star_price)
             },
           ploar_star_stopLoss =
             {
               if(file.exists(ploar_star_StopLoss.path))
               {
                 # code.ploar_star_stopLoss <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", ploar_star_StopLoss.path), stdout = TRUE))
                 code.ploar_star_stopLoss <- as.character(m.tail(ploar_star_StopLoss.path))
                 
                 return(code.ploar_star_stopLoss)
               }else{return(0)}
               # code.ploar_star_stopLoss <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", ploar_star_StopLoss.path), stdout = TRUE))
               # return(code.ploar_star_stopLoss)
             },
           polar_star_switch =
             {
               if(file.exists(polar_star_switch.path))
               {
                 # code.polar_star_switch.path <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", polar_star_switch.path), stdout = TRUE))
                 code.polar_star_switch.path <- as.character(m.tail(polar_star_switch.path))
                 
                 return(code.polar_star_switch.path)
               }else{return(0)}
               # code.polar_star_switch.path <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", polar_star_switch.path), stdout = TRUE))
               # return(code.polar_star_switch.path)
             },           
           
           B_UP =
             {
               # price.b_upper <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", b_upper.path), stdout = TRUE))
               price.b_upper <- as.character(m.tail(b_upper.path))
               
               return(price.b_upper)
             },
           B_LO =
             {
               # price.b_lower <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", b_lower.path), stdout = TRUE))
               price.b_lower <- as.character(m.tail(b_lower.path))
               
               return(price.b_lower)
             },
           RSI =
             {
               # price.rsi <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", rsi.path), stdout = TRUE))
               price.rsi <- as.character(m.tail(rsi.path))
               
               return(price.rsi)
             },
           RSI_PRE =
             {
               # price.rsi.pre <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", rsi_pre.path), stdout = TRUE))
               price.rsi.pre <- as.character(m.tail(rsi_pre.path))
               
               return(price.rsi.pre)
             },
           RSI_MA5 =
             {
               # price.rsi_ma5 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", rsi_ma5.path), stdout = TRUE))
               price.rsi_ma5 <- as.character(m.tail(rsi_ma5.path))
               
               return(price.rsi_ma5)
             },
           BSRate =
             {
               # price.bsrate <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", bsrate.path), stdout = TRUE))
               price.bsrate <- as.character(m.tail(bsrate.path))
               
               return(price.bsrate)
             },
           BSRateDiff =
             {
               # price.bsrateDiff <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", bsrateDiff.path), stdout = TRUE))
               price.bsrateDiff <- as.character(m.tail(bsrateDiff.path))
               
               return(price.bsrateDiff)
             },
           op_ma5 =
             {
               # price.op_ma5 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", op_ma5.path), stdout = TRUE))
               price.op_ma5 <- as.character(m.tail(op_ma5.path))
               
               return(price.op_ma5)
             },
           op_ma10 =
             {
               # price.op_ma10 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", op_ma10.path), stdout = TRUE))
               price.op_ma10 <- as.character(m.tail(op_ma10.path))
               
               return(price.op_ma10)
             },
           op_ma20 =
             {
               # price.op_ma20 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", op_ma20.path), stdout = TRUE))
               price.op_ma20 <- as.character(m.tail(op_ma20.path))
               
               return(price.op_ma20)
             },
           op_ma60 =
             {
               # price.op_ma60 <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", op_ma60.path), stdout = TRUE))
               price.op_ma60 <- as.character(m.tail(op_ma60.path))
               
               return(price.op_ma60)
             },
           price.Buyin ={
             if(file.exists(price.Buyin.path))
             {
               # price.Buyin <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.Buyin.path), stdout = TRUE))
               price.Buyin <- as.character(m.tail(price.Buyin.path))
               
               return(price.Buyin)
             }else{return(0)}
                # price.Buyin <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.Buyin.path), stdout = TRUE))
                # return(price.Buyin)
             },
           price.PCL ={
             if(file.exists(price.PCL.path))
             {
               # price.PCL <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", price.PCL.path), stdout = TRUE))
               price.PCL <- as.character(m.tail(price.PCL.path))
               
               return(price.PCL)
             }else{return(0)}
           },           
           # MA5.CREATE.LONG ={
           #   return(MA5.CREATE.LONG.path)
           # },
           MA5.CREATE.LONG ={
             if(file.exists(MA5.CREATE.LONG.path))
             {
               # price.MA5.CREATE.LONG <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", MA5.CREATE.LONG.path), stdout = TRUE))
               price.MA5.CREATE.LONG <- as.character(m.tail(MA5.CREATE.LONG.path))
               
               return(price.MA5.CREATE.LONG)
             }else{return(0)}
             },
           MA10.CREATE.LONG ={
             if(file.exists(MA10.CREATE.LONG.path))
             {
               # price.MA10.CREATE.LONG <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", MA10.CREATE.LONG.path), stdout = TRUE))
               price.MA10.CREATE.LONG <- as.character(m.tail(MA10.CREATE.LONG.path))
               
               return(price.MA10.CREATE.LONG)
             }else{return(0)}
             },
           MA20.CREATE.LONG ={
             if(file.exists(MA20.CREATE.LONG.path))
             {
               # price.MA20.CREATE.LONG <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", MA20.CREATE.LONG.path), stdout = TRUE))
               price.MA20.CREATE.LONG <- as.character(m.tail(MA20.CREATE.LONG.path))
               
               return(price.MA20.CREATE.LONG)
             }else{return(0)}
           },
           MA5.CREATE.SHORT ={
             if(file.exists(MA5.CREATE.SHORT.path))
             {
               # price.MA5.CREATE.SHORT <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", MA5.CREATE.SHORT.path), stdout = TRUE))
               price.MA5.CREATE.SHORT <- as.character(m.tail(MA5.CREATE.SHORT.path))
               
               return(price.MA5.CREATE.SHORT)
             }else{return(0)}
           },
           MA10.CREATE.SHORT ={
             if(file.exists(MA10.CREATE.SHORT.path))
             {
               
               # price.MA10.CREATE.SHORT <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", MA10.CREATE.SHORT.path), stdout = TRUE))
               price.MA10.CREATE.SHORT <- as.character(m.tail(MA10.CREATE.SHORT.path))
               
               return(price.MA10.CREATE.SHORT)
             }else{return(0)}
           },
           MA20.CREATE.SHORT ={
             if(file.exists(MA20.CREATE.SHORT.path))
             {
               # price.MA20.CREATE.SHORT <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", MA20.CREATE.SHORT.path), stdout = TRUE))
               price.MA20.CREATE.SHORT <- as.character(m.tail(MA20.CREATE.SHORT.path))
               
               return(price.MA20.CREATE.SHORT)
             }else{return(0)}
           },
           switch_to.ma ={
             if(file.exists(switch_to.ma.path))
             {
               # price.switch_to.ma <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", switch_to.ma.path), stdout = TRUE))
               price.switch_to.ma <- as.character(m.tail(switch_to.ma.path))
               
               return(price.switch_to.ma)
             }else{return(0)}
           },
           switch_to.rsi ={
             if(file.exists(switch_to.rsi.path))
             {
               # price.switch_to.ma <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", switch_to.ma.path), stdout = TRUE))
               price.switch_to.rsi <- as.character(m.tail(switch_to.rsi.path))
               
               return(price.switch_to.rsi)
             }else{return(0)}
           },          
           currentBar ={
             if(file.exists(currentBar.path))
             {
               # price.currentBar <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", currentBar.path), stdout = TRUE))
               price.currentBar <- as.character(m.tail(currentBar.path))
               
               return(price.currentBar)
             }else{return(0)}
           },
           CUSTOM.CREATE.LONG ={
             if(file.exists(CUSTOM.CREATE.LONG.path))
             {
               # price.currentBar <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", currentBar.path), stdout = TRUE))
               price.CUSTOM.CREATE.LONG <- as.character(m.tail(CUSTOM.CREATE.LONG.path))
               
               return(price.CUSTOM.CREATE.LONG)
             }else{return(0)}
           },
           CUSTOM.CREATE.SHORT ={
             if(file.exists(CUSTOM.CREATE.SHORT.path))
             {
               # price.currentBar <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", currentBar.path), stdout = TRUE))
               price.CUSTOM.CREATE.SHORT <- as.character(m.tail(CUSTOM.CREATE.SHORT.path))
               
               return(price.CUSTOM.CREATE.SHORT)
             }else{return(0)}
           },
           op_ma =
             {
               # price.op_ma <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", op_ma.path), stdout = TRUE))
               price.op_ma <- as.character(m.tail(op_ma.path))
               
               return(price.op_ma)
             }
    )    
  }

  if(p.mode =="path")
  {
    switch(operator,

            create.positionLONG =
             {
               return(create.positionLONG.path)
             },
           create.positionSHORT =
             {
               return(create.positionSHORT.path)
             },
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
           DAGS ={
             return(DISABLE_AGENT.SERVERE.path)
           },           
           price.Buyin ={
              return(price.Buyin.path)
             },
           price.PCL ={
             return(price.PCL.path)
           },
           MA5.CREATE.LONG ={
             return(MA5.CREATE.LONG.path)
           },
           MA10.CREATE.LONG ={
             return(MA10.CREATE.LONG.path)
           },
           MA20.CREATE.LONG ={
             return(MA20.CREATE.LONG.path)
           },
           MA5.CREATE.SHORT ={
             return(MA5.CREATE.SHORT.path)
           },
           MA10.CREATE.SHORT ={
             return(MA10.CREATE.SHORT.path)
           },
           MA20.CREATE.SHORT ={
             return(MA20.CREATE.SHORT.path)
           },
           RESET_AGENT.SERVERE ={
             return(RESET_AGENT.SERVERE.path)
           },  
           switch_to.ma ={
             return(switch_to.ma.path)
           },  
           switch_to.rsi ={
             return(switch_to.rsi.path)
           },
           currentBar ={
             return(currentBar.path)
           }, 
           CUSTOM.CREATE.LONG ={
             return(CUSTOM.CREATE.LONG.path)
           }, 
           CUSTOM.CREATE.SHORT ={
             return(CUSTOM.CREATE.SHORT.path)
           },
           close.ALLPOSITION =
             {
               return(close.ALL.path)
             }
    )
  }
  
  
}


