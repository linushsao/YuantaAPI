


#### 最新報價 ####
Price.current<-function(data.path=NULL)
{

  if(is.null(data.path))
  {
    m.data.path <-SECURTIES.data.path
  }else{m.data.path <-data.path}
  
  result <- QueryOHCL(data.path = m.data.path)

}

#### 權益數解讀 ####
Right.current<-function(x=6)
{
  result <- QueryRight()
  return(result[[x]])
}

#執行平倉方式
if.safeClose <-function(.BorS, .Price, .Qty, .Daytrade, simu.mode)
{
  if(!simu.mode)
  {
    # if (!safe.Close)
    # {
    #   #預設限價單
    #     BorS <- .BorS
    #     Price <- Price.current()
    #     Qty <- .Qty
    #     Daytrade <-.Daytrade 
    #     result <- Place.OrderLMT(BorS, Price, Qty, Daytrade, simu.mode = simu.mode)
    # 
    #   #市價單
    #     BorS <- gsub("M", "", .BorS)
    #     Price <- Price.current()
    #     result <- Place.OrderMKT(BorS, Qty, Daytrade, simu.mode=simu.mode)
    # 
    # }else{
      result <- ClosePositionAll(simu.mode = simu.mode)
    # }
  }else{

    result <-"SIMU"
  }

  return(result)
}


filename.gen <-function(x=NULL, name)
{
  
  if(x =="product" || is.null(x)){return(paste0(name, "_Match.txt"))}
  if(x ==MXFSIMU.Name){return(paste0(MXFSIMU.Name, "_Match.txt"))}
  if(x =="log"){return(paste0(MXFSIMU.Name, "_MatchLOG.txt"))}
}

finacial.dataparg.gen <- function(data.path, date.format, Product, Product.file)
{
  result <- paste0(data.path
                      , date.format, "/"
                      , Product, "/"
                      , Product.file)
  return(result)
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

append.to.file <- function(data, path, m.append=TRUE, m.col.names=FALSE)
{
  write.table(data, file=path, sep=","
              , row.names=F, na = "NA", 
              append=m.append, col.names=ifelse(m.append, FALSE, m.col.names))
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


#[1] "2021102500139494T0EO,全部成交,MXFK1,限賣,16900,1,114317,226,3414338,,7D930,,,,,1,0 "

account.info <- function(code=NULL, by.name=NULL, info)
{
  if(!is.null(code))
  {
      leng <-LENGTH.COLLECT.ANSWER
      #錯誤檢查，尚未進行連接
      ##要求參數有誤 "Delete KeyNo"
      if(code[6] ==CONNECTED.ANSWER.BorS.WrongPARAM)
      {
        result <- c(rep(NULL, leng))
        result[1] <-"CONNECTED.ANSWER.BorS.WrongPARAM"
        result[2] <-CONNECTED.ANSWER.BorS.WrongPARAM
        result[5] <-Price.current()
        result[8] <-FALSE
        
        return(result)
      }

      if(code !=CODE.SIMU)
      {
        .info <-  QueryOrder(code)
        
        #錯誤檢查
        ##CONNECTED.ANSWER.RightPARAM.NoDATA "Nodata"
        if(.info[1] ==CONNECTED.ANSWER.RightPARAM.NoDATA)
        {
          result <- c(rep(NULL, leng))
          result[1] <-"CONNECTED.ANSWER.RightPARAM.NoDATA"
          result[2] <-CONNECTED.ANSWER.RightPARAM.NoDATA
          result[5] <-Price.current()
          result[8] <-FALSE
          
          return(result)
        }
        ##UNCONNECTED.ANSWER.RightPARAM "請開啟Smart API"
        if(.info[1] ==UNCONNECTED.ANSWER.RightPARAM)
        {
          result <- c(rep(NULL, leng))
          result[1] <-"UNCONNECTED.ANSWER.RightPARAM"
          result[2] <-UNCONNECTED.ANSWER.RightPARAM
          result[5] <-Price.current()
          result[8] <-FALSE
          
          return(result)
        } 
        ##COMMON.ANSWER.EmptyPARAM "KeyNo or ALL"
        if(.info[1] ==COMMON.ANSWER.EmptyPARAM)
        {
          result <- c(rep(NULL, leng))
          result[1] <-"COMMON.ANSWER.EmptyPARAM"
          result[2] <-COMMON.ANSWER.EmptyPARAM
          result[5] <-Price.current()
          result[8] <-FALSE
          
          return(result)
        } 
        
        #下單連接成功
        result <- strsplit(.info, ",")[[1]]
        result[8] <-TRUE
        
        return(result)
        
      }else{ #模擬交易回應
        result <- c(rep(NULL, leng))
        result[1] <-code
        result[2] <-ANSWER.ALLDeal
        result[5] <-Price.current()
        result[8] <-FALSE
        
        return(result)
      }
  }
  
  #取出DECODED INFO之特定資料
  if(!is.null(by.name))
  {
  
  switch (by.name,
    order   = {x=1},
    status  = {x=2},
    product = {x=3},
    bors    = {x=4},
    price   = {x=5},
    amount  = {x=6},
    time    = {x=7},
    complete = {x=8}, #TRUE表示下單成功
    x=0
    )
  result <-ifelse(x!=0, info[x], FALSE)
  
  return(result)  
    
  }

}

#卷商主機連線測試
connect.test <-function(x=NULL)
{
  #設定最多檢查次數
  if(is.null(x)){x <-6}
  if.connected <-FALSE
  #測試連線結果
  for(miu in 1:x)
  {
    result <- !is.na(QueryRight()[[1]]) #TRUE表示正常
    if(result)
    {
      if.connected <-TRUE
      break
    }
  }
  return(if.connected)
}
#
trans.lang <-function(mode, param)
{
  switch (mode,
    SIMU ={
      if(param)
      {
        return("模擬")
        }else{return("真實")}
    }
  )
}
# 
m.tail <-function(path)
{
  price.file <- read.csv(path, header = FALSE, fileEncoding = "big5")
  price.tail <- tail(price.file, 1)
  return(price.tail)
}

path.MGR <-function(x)
{
  
  switch (x,
    #設定訊息檔案路徑
    create.positionLONG.path   = result<- paste0(price.path, "create.POSITIONLong", ".csv"),
    create.positionSHORT.path  = result<- paste0(price.path, "create.POSITIONShort", ".csv"),
    
    close.ALL.path  = result<- paste0(price.path, "close_allPOSITION", ".csv"),
    price.open.path  = result<- paste0(price.path, "open", ".csv"),
    price.high.path  = result<- paste0(price.path, "high", ".csv"),
    price.close.path  = result<- paste0(price.path, "close", ".csv"),
    price.low.path  = result<- paste0(price.path, "low", ".csv"),
    price.ma5.path  = result<- paste0(price.path, "_ma5", ".csv"),
    price.ma10.path  = result<- paste0(price.path, "_ma10", ".csv"),
    price.ma20.path  = result<- paste0(price.path, "_ma20", ".csv"),  
    price.Rate_sma5.path  = result<- paste0(price.path, "_Rate_sma5", ".csv"),
    price.Rate_sma10.path  = result<- paste0(price.path, "_Rate_sma10", ".csv"),
    price.Rate_sma20.path  = result<- paste0(price.path, "_Rate_sma20", ".csv"), 
    price.Buyin.path  = result<- paste0(price.path, "Price.buyin", ".csv"), 
    price.PCL.path  = result<- paste0(price.path, "Price.pcl", ".csv"),
    #
    ploar_star.path  = result<- paste0(price.path, "_ploar_star", ".csv"),
    ploar_star_price.path  = result<- paste0(price.path, "_ploar_star_price", ".csv"),
    ploar_star_StopLoss.path  = result<- paste0(price.path, "_ploar_star_stopLoss", ".csv"),
    polar_star_switch.path  = result<- paste0(price.path, "_polar_star_switch", ".csv"),
    #
    b_upper.path  = result<- paste0(price.path, "_b_upper", ".csv"),
    b_lower.path  = result<- paste0(price.path, "_b_lower", ".csv"),
    #
    rsi.path  = result<- paste0(price.path, "RSI", ".csv"),
    rsi_pre.path  = result<- paste0(price.path, "RSI_pre", ".csv"),
    rsi_ma5.path  = result<- paste0(price.path, "RSI_MA5", ".csv"),
    #
    bsrate.path  = result<- paste0(price.path, "_BSRate", ".csv"),
    bsrateDiff.path <- paste0(price.path, "_BSRateDiff", ".csv"),
    #
    op_ma5.path  = result<- paste0(price.path, "_op_ma5", ".csv"),
    op_ma10.path  = result<- paste0(price.path, "_op_ma10", ".csv"),
    op_ma20.path  = result<- paste0(price.path, "_op_ma20", ".csv"),
    op_ma60.path  = result<- paste0(price.path, "_op_ma60", ".csv"),
    op_ma.path  = result<- paste0(price.path, "_op_ma", ".csv"),
    #
    switch_to.ma.path  = result<- paste0(price.path, "switch_to.ma", ".csv"),
    switch_to.rsi.path  = result<- paste0(price.path, "switch_to.rsi", ".csv"),
    
    #
    Research_Line_Upper.path  = result<- paste0(price.path, "_Research_Line_Upper", ".csv"),
    Research_Line_Mid.path  = result<-   paste0(price.path, "_Research_Line_Mid", ".csv"),
    Research_Line_lower.path  = result<- paste0(price.path, "_Research_Line_lower", ".csv"),
    extremes_Line_Upper.path  = result<- paste0(price.path, "_extremes_Line_Upper", ".csv"),
    extremes_Line_Mid.path  = result<-   paste0(price.path, "_extremes_Line_Mid", ".csv"),
    extremes_Line_lower.path  = result<- paste0(price.path, "_extremes_Line_lower", ".csv"),
    #
    enable.STABLE.Stop.PORT.path   = result<- paste0(price.path, "enable.stable.Stop.PORT", ".csv"),
    enable.onlyMDD.path   = result<- paste0(price.path, "enable.onlyMDD", ".csv"),
    enable.RSI.TrendADDED.path   = result<- paste0(price.path, "enable.RSI.TrendADDED", ".csv"),
    enable.Bolling.path   = result<- paste0(price.path, "enable.BollingPATH.ADDED", ".csv"),
    #
    DISABLE_MXFSIMU.SERVERE.path  = result<- paste0(price.path, "DISABLE_MXFSIMU.SERVERE", ".csv"),
    DISABLE_AGENT.SERVERE.path  = result<- paste0(price.path, "DISABLE_AGENT.SERVERE", ".csv"),
    RESET_AGENT.SERVERE.path  = result<- paste0(price.path, "RESET_AGENT.SERVERE", ".csv"),
    
    #
    MA5.CREATE.LONG.path  = result<- paste0(price.path, "MA5.CREATE.LONG", ".csv"),
    MA10.CREATE.LONG.path  = result<- paste0(price.path, "MA10.CREATE.LONG", ".csv"),
    MA20.CREATE.LONG.path  = result<- paste0(price.path, "MA20.CREATE.LONG", ".csv"),
    MA5.CREATE.SHORT.path  = result<- paste0(price.path, "MA5.CREATE.SHORT", ".csv"),
    MA10.CREATE.SHORT.path  = result<- paste0(price.path, "MA10.CREATE.SHORT", ".csv"),
    MA20.CREATE.SHORT.path  = result<- paste0(price.path, "MA20.CREATE.SHORT", ".csv"),  
    #
    CUSTOM.CREATE.LONG.path  = result<- paste0(price.path, "CUSTOM.CREATE.LONG", ".csv"),
    CUSTOM.CREATE.SHORT.path  = result<- paste0(price.path, "CUSTOM.CREATE.SHORT", ".csv"),
    
    #
    currentBar.path  = result<- paste0(price.path, "currentBar", ".csv"),
    msg.lite.path  = result<- paste0(price.path, "msg.lite", ".csv"),
    #
    CLOSEPositionByMA.path  = result<- paste0(price.path, "CLOSEPositionByMA", ".csv"),
    #
    create.positionLONG.path  = result<- paste0(price.path, "create.positionLONG", ".csv"),
    create.positionSHORT.path  = result<- paste0(price.path, "create.positionSHORT", ".csv"),
    enable.STABLE.Stop.PORT.path  = result<- paste0(price.path, "enable.STABLE.Stop.PORT", ".csv"),
    enable.onlyMDD.path  = result<- paste0(price.path, "enable.onlyMDD", ".csv"),
    enable.RSI.TrendADDED.path  = result<- paste0(price.path, "enable.RSI.TrendADDED", ".csv"),
    enable.Bolling.path  = result<- paste0(price.path, "enable.BollingPATH.ADDED", ".csv"),
    DISABLE_MXFSIMU.SERVERE.path  = result<- paste0(price.path, "DMSS", ".csv"),
    DISABLE_AGENT.SERVERE.path  = result<- paste0(price.path, "DAGS", ".csv"),
    RESET_AGENT.SERVERE.path  = result<- paste0(price.path, "RESET_AGENT.SERVERE", ".csv"),
    close.ALL.path  = result<- paste0(price.path, "close.ALLPOSITION", ".csv"),
    REMOTE_SWITCH_SIMULATION.path  = result<- paste0(price.path, "REMOTE_SWITCH_SIMULATION", ".csv")
    
    
  )
  
    return(result)
  
}

extra.data <-function(name="CL", p.mode="num")
{
  
  #
  operator <- gsub(" ", "", name)
  
  # if(p.mode =="num")
  # {
    switch(operator,
           
           OP =
             {
               m.path <-path.MGR("price.open.path")
               
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.open <- as.character(m.tail(m.path))
                   
                   return(price.open)
                 }else{return(0)}                 
               }
              },
           HI =
             {
               m.path <-path.MGR("price.high.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.high <- as.character(m.tail(m.path))
                   
                   return(price.high)
                 }else{return(0)}
               }
             },         
           CL =
             {
               m.path <-path.MGR("price.close.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.close <- as.character(m.tail(m.path))
                   
                   return(price.close)
                 }else{return(0)}                
               }
   
             },
           LO =
             {
               m.path <-path.MGR("price.low.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.low <- as.character(m.tail(m.path))
                   
                   return(price.low)
                 }else{return(0)}                
               }
          
             }, 
           MA5 =
             {
               m.path <-path.MGR("price.ma5.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.ma5 <- as.character(m.tail(m.path))
                   
                   return(price.ma5)
                 }else{return(0)}                
               }
    
             },
           MA10 =
             {
               m.path <-path.MGR("price.ma10.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.ma10 <- as.character(m.tail(m.path))
                   
                   return(price.ma10)
                 }else{return(0)}               
               }
        
             },
           MA20 =
             {
               m.path <-path.MGR("price.ma20.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.ma20 <- as.character(m.tail(m.path))
                   
                   return(price.ma20)
                 }else{return(0)}                
               }
    
             },
           Rate_sma5 =
             {
               m.path <-path.MGR("price.Rate_sma5.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.Rate_sma5 <- as.character(m.tail(m.path))
                   
                   return(price.Rate_sma5)
                 }else{return(0)}                
               }
    
             },
           Rate_sma10 =
             {
               m.path <-path.MGR("price.Rate_sma10.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.Rate_sma10 <- as.character(m.tail(m.path))
                   
                   return(price.Rate_sma10)
                 }else{return(0)}              
               }
       
             },
           Rate_sma20 =
             {
               m.path <-path.MGR("price.Rate_sma20.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.Rate_sma20 <- as.character(m.tail(m.path))
                   
                   return(price.Rate_sma20)
                 }else{return(0)}               
               }
           
             },
           Research_Line_Upper =
             {
               m.path <-path.MGR("Research_Line_Upper.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.Research_Line_Upper <- as.character(m.tail(m.path))
                   
                   return(price.Research_Line_Upper)
                 }else{return(0)} 
               }
          
             },         
           Research_Line_Mid =
             {
               m.path <-path.MGR("Research_Line_Mid.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.Research_Line_Mid <- as.character(m.tail(m.path))
                   
                   return(price.Research_Line_Mid)
                 }else{return(0)}                
               }
          
             },   
           Research_Line_lower =
             {
               m.path <-path.MGR("Research_Line_lower.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.Research_Line_lower <- as.character(m.tail(m.path))
                   
                   return(price.Research_Line_lower)
                 }else{return(0)}                
               }
           
             }, 
           extremes_Line_Upper =
             {
               m.path <-path.MGR("extremes_Line_Upper.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.extremes_Line_Upper <- as.character(m.tail(m.path))
                   
                   return(price.extremes_Line_Upper)
                 }else{return(0)}                
               }
       
             }, 
           extremes_Line_Mid =
             {
               m.path <-path.MGR("extremes_Line_Mid.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.extremes_Line_Mid <- as.character(m.tail(m.path))
                   
                   return(price.extremes_Line_Mid)
                 }else{return(0)}                
               }
            
             }, 
           extremes_Line_lower =
             {
               m.path <-path.MGR("extremes_Line_lower.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.extremes_Line_lower <- as.character(m.tail(m.path))
                   
                   return(price.extremes_Line_lower)
                 }else{return(0)}                
               }
      
             }, 
           ploar_star =
             {
               m.path <-path.MGR("ploar_star.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   code.ploar_star <- as.character(m.tail(m.path))
                   return(code.ploar_star)
                 }else{return(0)}                 
               }
        
             },
           ploar_star_price =
             {
               m.path <-path.MGR("ploar_star_price.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   code.ploar_star_price <- as.character(m.tail(m.path))
                   
                   return(code.ploar_star_price)
                 }else{return(0)}                
               }
      
             },
           ploar_star_stopLoss =
             {
               m.path <-path.MGR("ploar_star_StopLoss.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   code.ploar_star_stopLoss <- as.character(m.tail(m.path))
                   
                   return(code.ploar_star_stopLoss)
                 }else{return(0)}                
               }
      
             },
           polar_star_switch =
             {
               m.path <-path.MGR("polar_star_switch.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   code.polar_star_switch.path <- as.character(m.tail(m.path))
                   
                   return(code.polar_star_switch.path)
                 }               
               }
           
             },           
           
           B_UP =
             {
               m.path <-path.MGR("b_upper.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 price.b_upper <- as.character(m.tail(m.path))
                 return(price.b_upper)                
               }
     
             },
           B_LO =
             {
               m.path <-path.MGR("b_lower.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 price.b_lower <- as.character(m.tail(m.path))
                 return(price.b_lower)         
               }
             },
           RSI =
             {
               m.path <-path.MGR("rsi.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 price.rsi <- as.character(m.tail(m.path))
                 return(price.rsi)    
               }
             },
           RSI_PRE =
             {
               m.path <-path.MGR("rsi_pre.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 price.rsi.pre <- as.character(m.tail(m.path))
                 return(price.rsi.pre)        
               }
             },
           RSI_MA5 =
             {
               m.path <-path.MGR("rsi_ma5.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 price.rsi_ma5 <- as.character(m.tail(path.MGR("rsi_ma5.path")))
                 return(price.rsi_ma5)          
               }
             },
           BSRate =
             {
               m.path <-path.MGR("bsrate.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 price.bsrate <- as.character(m.tail(m.path))
                 return(price.bsrate)       
               }
             },
           BSRateDiff =
             {
               m.path <-path.MGR("bsrateDiff.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 price.bsrateDiff <- as.character(m.tail(m.path))
                 return(price.bsrateDiff)      
               }
             },
           op_ma5 =
             {
               m.path <-path.MGR("op_ma5.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 price.op_ma5 <- as.character(m.tail(m.path))
                 return(price.op_ma5)     
               }
             },
           op_ma10 =
             {
               m.path <-path.MGR("op_ma10.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 price.op_ma10 <- as.character(m.tail(m.path))
                 return(price.op_ma10)      
               }
             },
           op_ma20 =
             {
               m.path <-path.MGR("op_ma20.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 price.op_ma20 <- as.character(m.tail(m.path))
                 return(price.op_ma20)    
               }
             },
           op_ma60 =
             {
               m.path <-path.MGR("op_ma60.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 price.op_ma60 <- as.character(m.tail(m.path))
                 return(price.op_ma60)    
               }
             },
           price.Buyin =
             {
               m.path <-path.MGR("price.Buyin.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.Buyin <- as.character(m.tail(m.path))
                   
                   return(price.Buyin)
                 }else{return(0)}    
               }
       
             },
           price.PCL =
            {
               m.path <-path.MGR("price.PCL.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.PCL <- as.character(m.tail(m.path))
                   
                   return(price.PCL)
                 }else{return(0)}    
               }
            },           
           MA5.CREATE.LONG =
             {
               m.path <-path.MGR("MA5.CREATE.LONG.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.MA5.CREATE.LONG <- as.character(m.tail(m.path))
                   
                   return(price.MA5.CREATE.LONG)
                 }else{return(0)}      
               }
       
             },
           MA10.CREATE.LONG =
             {
               m.path <-path.MGR("MA10.CREATE.LONG.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 if(file.exists(m.path))
                 {
                   price.MA10.CREATE.LONG <- as.character(m.tail(m.path))
                   
                   return(price.MA10.CREATE.LONG)
                 }else{return(0)}   
               }
           
             },
           MA20.CREATE.LONG ={
             m.path <-path.MGR("MA20.CREATE.LONG.path")
             if(p.mode =="path"){return(m.path)}
             if(p.mode =="num")
             {
               if(file.exists(m.path))
               {
                 price.MA20.CREATE.LONG <- as.character(m.tail(m.path))
                 
                 return(price.MA20.CREATE.LONG)
               }else{return(0)}    
             }
     
           },
           MA5.CREATE.SHORT ={
             m.path <-path.MGR("MA5.CREATE.SHORT.path")
             if(p.mode =="path"){return(m.path)}
             if(p.mode =="num")
             {
               if(file.exists(m.path))
               {
                 price.MA5.CREATE.SHORT <- as.character(m.tail(m.path))
                 
                 return(price.MA5.CREATE.SHORT)
               }else{return(0)}    
             }
         
           },
           MA10.CREATE.SHORT ={
             m.path <-path.MGR("MA10.CREATE.SHORT.path")
             if(p.mode =="path"){return(m.path)}
             if(p.mode =="num")
             {
               if(file.exists(m.path))
               {
                 
                 price.MA10.CREATE.SHORT <- as.character(m.tail(m.path))
                 
                 return(price.MA10.CREATE.SHORT)
               }else{return(0)}    
             }
           },
           MA20.CREATE.SHORT ={
             m.path <-path.MGR("MA20.CREATE.SHORT.path")
             if(p.mode =="path"){return(m.path)}
             if(p.mode =="num")
             {
               if(file.exists(m.path))
               {
                 price.MA20.CREATE.SHORT <- as.character(m.tail(m.path))
                 
                 return(price.MA20.CREATE.SHORT)
               }else{return(0)}    
             }
           },
           switch_to.ma ={
             m.path <-path.MGR("switch_to.ma.path")
             if(p.mode =="path"){return(m.path)}
             if(p.mode =="num")
             {
               if(file.exists(m.path))
               {
                 price.switch_to.ma <- as.character(m.tail(m.path))
                 
                 return(price.switch_to.ma)
               }else{return(0)}  
             }
           },
           switch_to.rsi ={
             m.path <-path.MGR("switch_to.rsi.path")
             if(p.mode =="path"){return(m.path)}
             if(p.mode =="num")
             {
               if(file.exists(m.path))
               {
                 price.switch_to.rsi <- as.character(m.tail(m.path))
                 
                 return(price.switch_to.rsi)
               }else{return(0)}    
             }
           },          
           currentBar ={
             m.path <-path.MGR("currentBar.path")
             if(p.mode =="path"){return(m.path)}
             if(p.mode =="num")
             {
               if(file.exists(m.path))
               {
                 price.currentBar <- as.character(m.tail(m.path))
                 
                 return(price.currentBar)
               }else{return(0)}  
             }
           },
           CUSTOM.CREATE.LONG ={
             m.path <-path.MGR("CUSTOM.CREATE.LONG.path")
             if(p.mode =="path"){return(m.path)}
             if(p.mode =="num")
             {
               if(file.exists(m.path))
               {
                 price.CUSTOM.CREATE.LONG <- as.character(m.tail(m.path))
                 
                 return(price.CUSTOM.CREATE.LONG)
               }else{return(0)}    
             }
           },
           CUSTOM.CREATE.SHORT ={
             m.path <-path.MGR("CUSTOM.CREATE.SHORT.path")
             if(p.mode =="path"){return(m.path)}
             if(p.mode =="num")
             {
               if(file.exists(m.path))
               {
                 price.CUSTOM.CREATE.SHORT <- as.character(m.tail(m.path))
                 
                 return(price.CUSTOM.CREATE.SHORT)
               }else{return(0)}  
             }
           },
           msg.lite ={
             m.path <-path.MGR("msg.lite.path")
             if(p.mode =="path"){return(m.path)}
             if(p.mode =="num")
             {
               if(file.exists(m.path))
               {
                 price.msg.lite <- as.character(m.tail(m.path))
                 
                 return(price.msg.lite)
               }else{return(0)}   
             }
           },
           CLOSEPositionByMA ={
             m.path <-path.MGR("CLOSEPositionByMA.path")
             if(p.mode =="path"){return(m.path)}
             if(p.mode =="num")
             {
               if(file.exists(m.path))
               {
                 price.CLOSEPositionByMA <- as.character(m.tail(m.path))
                 
                 return(price.CLOSEPositionByMA)
               }else{return(0)}   
             }
           },
           op_ma =
             {
               m.path <-path.MGR("op_ma.path")
               if(p.mode =="path"){return(m.path)}
               if(p.mode =="num")
               {
                 price.op_ma <- as.character(m.tail(m.path))
                 
                 return(price.op_ma)           
               }
       
             },
           ##
           create.positionLONG =
             {
               m.path <-path.MGR("create.positionLONG.path")
               if(p.mode =="path"){return(m.path)}
             },
           create.positionSHORT =
             {
               m.path <-path.MGR("create.positionSHORT.path")
               if(p.mode =="path"){return(m.path)}
             },
           
           
           enable.STABLE.Stop.PORT =
             {
               m.path <-path.MGR("enable.STABLE.Stop.PORT.path")
               if(p.mode =="path"){return(m.path)}
             },
           enable.onlyMDD =
             {
               m.path <-path.MGR("enable.onlyMDD.path")
               if(p.mode =="path"){return(m.path)}
             },
           enable.RSI.TrendADDED =
             {
               m.path <-path.MGR("enable.RSI.TrendADDED.path")
               if(p.mode =="path"){return(m.path)}
             },
           enable.BollingPATH.ADDED =
             {
               m.path <-path.MGR("enable.Bolling.path")
               if(p.mode =="path"){return(m.path)}
             },
           DMSS ={
             return(path.MGR("DISABLE_MXFSIMU.SERVERE.path"))
           },
           DAGS ={
             m.path <-path.MGR("DISABLE_AGENT.SERVERE.path")
             if(p.mode =="path"){return(m.path)}
           },           
           RESET_AGENT.SERVERE ={
             m.path <-path.MGR("RESET_AGENT.SERVERE.path")
             if(p.mode =="path"){return(m.path)}
           },  
           REMOTE_SWITCH_SIMULATION ={
             m.path <-path.MGR("REMOTE_SWITCH_SIMULATION.path")
             if(p.mode =="path"){return(m.path)}
           },             
           close.ALLPOSITION =
             {
               m.path <-path.MGR("close.ALL.path")
               if(p.mode =="path"){return(m.path)}
             }
    )    
  }


  
  



