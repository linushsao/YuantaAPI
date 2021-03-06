#
#
#
PT.data.reset <-function(mode="reg")
{
  if(mode =="reg")
  {
    #
    rm.conf(name = "close.ALLPOSITION", dataset = dataset.name)
    rm.conf(name = "RESET_AGENT.SERVERE", dataset = dataset.name)
    
    rm.conf(name = "price.Buyin", dataset = dataset.name)
    rm.conf(name = "price.PCL", dataset = dataset.name)
    rm.conf(name = "OrderNO", dataset = dataset.name)
    rm.conf(name = "create.positionLONG", dataset = dataset.name)
    rm.conf(name = "create.positionSHORT", dataset = dataset.name)
    #
    rm.conf(name = "switch.create.positionLONG", dataset = dataset.name)
    rm.conf(name = "switch.create.positionSHORT", dataset = dataset.name) 
    
    return(TRUE)
  }else{
    return(FALSE)
  }

}


multi.source.load <-function(.path, .pattern =".R")
{
  
  if(dir.exists(.path))
  {
    list.source <-list.files(path = .path, pattern = .pattern)
    len <-length(list.source)
    
    if(len !=0)
    {
      for(.index in 1:len)
      {
        file.name <- paste0(.path, "/", list.source[.index])
        file.name <- gsub("//", "/", file.name)
        source(file.name)
      }
      
      return(TRUE)
        
    }
    
    return(FALSE)
    
  }
  
}

multi.file.remove <-function(.path, .pattern=NULL, .tail_keep=0)
{
  
  if(is.null(.pattern))
  {
    list.msg.file <-list.files(path = .path)
  }else{
    list.msg.file <-list.files(path = .path, pattern = .pattern)
  } 
  
  for(miu in 1:length(list.msg.file))
  {
    .file.path <-paste0(.path, "/", list.msg.file[miu])
    
    if(.tail_keep >0)
    {
      .tail.msg <- m.tail(path = .file.path)
      append.to.file(data = .tail.msg, path = .file.path, m.append = FALSE)
      
    }else{
      unlink(.file.path)
      
    }
    
  }
}


#
BorS2PCL <- function(x, .mode="en")
{
  if(.mode =="en")
  {
    switch (x,
            B = return(1),
            b = return(1),
            S = return(-1),
            s = return(-1),
            return(0)
    )    
  }
  
  if(.mode =="zh")
  {
    
    if(length(grep("買",x)) !=0){return(1)}
    if(length(grep("賣",x)) !=0){return(-1)}
  }
  
}

info2PCL <- function(x)
{
  .buy <-num2TF(length(grep("買", x)))
  .sell<-num2TF(length(grep("賣", x)))
  
  .res <-0
  
  if(.buy) {.res <- 1}
  if(.sell){.res <--1}
  
  return(.res)
}

#是否為有效檔案
if.Valid.file <-function(path)
{
  file.size <-file.info(path)$size
  
  if( file.exists(path) & 
      file.size !=0 &
      !is.na(file.size) )
  {
    return(TRUE)
  }else{return(FALSE)}
}

#### 最新報價 ####
Price.current<-function(data.path=NULL)
{
  m.data.path <-c()
  #檢查路徑
  if(is.null(data.path))
  {
    m.data.path <-data.source.switch(get.conf(name="switch.DATA.Source"
                                              , dataset = dataset.name))
  }else{m.data.path <-data.path}

  # if(if.Valid.file(m.data.path))
  while(TRUE)
  {
    if(file.access(m.data.path, mode=2) ==0) #for write permission success
    {
      result <- QueryOHCL(data.path = m.data.path)
      break
    }
  }
  
  return(result)
  
}

#### 權益數解讀 ####
Right.current<-function(x=6)
{
  result <- QueryRight()
  return(result[[x]])
}

#執行平倉方式
if.safeClose <-function(.BorS, .Price, .Qty, .Daytrade, .simu)
{
  if(!.simu)
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
    result <- ClosePositionAll()
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

num2TF <-function(num)
{
  if(num ==0){val <-FALSE}
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

# QueryRight<-function(){
#   rightinfo<-system2(paste0(ExecPath,'FutureRights.exe'),stdout = TRUE)
#   nn = as.numeric(strsplit(rightinfo,split=',')[[1]][c(7,1,3,2,20,17)])
#   return = list("今日權益總值"=nn[1],"權益數"=nn[2],"可動用保證金"=nn[3],
#                 "原始保證金"=nn[4],"維持保證金"=nn[5],"未沖銷期貨浮動損益"=nn[6])
#   return(return)
# }

# [[1]]
# [1] "MXFK1"  "限買"   "17082"  "1MXFK1" "市賣"   "17057"  "1MXFK1" "限買"   "17040"  "1MXFK1" "市賣"   "17032" 
# [13] "1MXFK1" "限買"   "17030"  "1MXFK1" "多"     "17030"  "1" 
portfolio.monitor <-function()
{
  #檢查是否有未平倉
  .OnOpen <-QueryOnOpen()
  .check.NO.OnOpen <-length(.OnOpen)

  #查詢權益數
  .get.QueryRIGHT <-QueryRight()  
  
  #產生結果向量
  result <-c(rep(0,1,6))
  #c(.bors, .price, .amount, .pcl, .portfolio, .portfolio.right)
  
  #尚有未平倉部位
  if(.check.NO.OnOpen !=0)
  {

    #取出<未沖銷期貨浮動損益>數字
    .decode <- strsplit(.OnOpen, ",")[[1]]
    .leng <- as.numeric(length(.decode))
    
    result[1] <-.decode[.leng-2]
    result[2] <-.decode[.leng-1]
    result[3] <-.decode[.leng]
    
    .bors <-result[1]
    if(.bors =="多")
    {result[4] =1
    }else if(.bors =="空")
    {result[4] =-1
    }else{
      result[4]=0}
  }
  
    result[5] <-as.numeric(.get.QueryRIGHT[[6]][1]) #未沖銷期貨浮動損益
    result[6] <-as.numeric(.get.QueryRIGHT[[3]][1]) #可動用保證金
  
  return(result)
}

#執行交易並回傳交易序號
PTrading.MGR <-function(.BorS, .Price, .Qty, .Daytrade, .simu)
{
  
  BorS <- .BorS
  Price <- .Price
  Qty <- .Qty
  Daytrade <- .Daytrade
  
  transaction <- c()
  
  if(!switch.DATA.Source)
  {
    OrderNO <- CODE.MXFSIMU
  }else if(!.simu)
  {
    OrderNO <- Place.OrderLMT(BorS, Price, Qty, Daytrade)
  }else{
    OrderNO <- CODE.SIMU
  }
  
  #匯出交易序號
  set.conf(name="OrderNO", value =OrderNO, dataset =dataset.name)
  # append.to.file(data = OrderNO, path = extra.data(name = "OrderNO", p.mode = "path"), m.append = FALSE)             
  #匯出交易PCL
  set.conf(name="price.PCL", value =BorS2PCL(BorS), dataset =dataset.name)
  # append.to.file(data = BorS2PCL(BorS), path = extra.data(name = "price.PCL", p.mode = "path"), m.append = FALSE) 
  
  return(OrderNO)
  
}


#依下單回傳序號解碼成文字向量，並確認交易結果
PTrading.confirm <-function(.OrderNO=NULL, .times=NULL)
{
  
  if(length(.OrderNO) ==0){return(FALSE)}
  
  .checkTIMES  <-0
  .checkRESULT <-FALSE
  
  if(!is.null(.times))
  {
    .t.checkTIMES <-.times
  }else{.t.checkTIMES <-transaction.checkTIMES}
  
  #檢查交易是否成交
  while(TRUE)
  {
    
    .checkTIMES <- .checkTIMES+1
    if(.checkTIMES <=.t.checkTIMES)
    {
      transaction  <-account.info(code=.OrderNO) #依下單回傳訊息解碼成文字向量
      .checkRESULT <-account.info(by.name ="complete", info =transaction) #取出結果
      if(.checkRESULT)
      {#交易成功
        return(list(.checkRESULT, transaction))
      } 
    }else{
      return(FALSE)
    }
    
  }
  
}


# #交易成功後執行後續設定
# PTConf.export <-function(transaction)
# {
#  
#   ## return(c(.bors, .price, .amount, .pcl, .portfolio))
#   # return(c(0, 0, 0)) 
#   
#   # result <- portfolio.monitor()
#   # 
#   # if(result[1] !=0)
#   # {
#     # Price.buyin <- as.numeric(account.info(by.name ="price", info = transaction ))
#     # PCL <- info2PCL( x =account.info(by.name ="bors", info = transaction) )
#     
#     .price.path <- extra.data(name="price.Buyin", p.mode = "path")
#     .PCL.path <- extra.data(name="price.PCL", p.mode = "path")
#     # order.name <- ifelse(.PCL ==1, "create.positionLONG", "create.positionSHORT")
#     # .msg.path <- extra.data(name=order.name, p.mode = "path")
#     #Price.buyin
#     append.to.file(data=result[2]
#                    , path=.price.path, m.append = FALSE)
#     #PCL
#     append.to.file(data=result[4]
#                    , path=.PCL.path, m.append = FALSE)
#     # result <-file.create(.msg.path)
#     
#     return(result)    
#   }else{
#     return(FALSE)
#   }
#   
# 
# }



#[1] "2021102500139494T0EO,全部成交,MXFK1,限賣,16900,1,114317,226,3414338,,7D930,,,,,1,0 "

account.info <- function(code=NULL, by.name=NULL, info)
{
  
  leng <-LENGTH.COLLECT.ANSWER
  
  if(length(code) !=0)
  {
    #錯誤檢查，尚未進行連接
    ##要求參數有誤 "Delete KeyNo"
    if(length(code) >1)
    {
      if(code[6] ==CONNECTED.ANSWER.BorS.WrongPARAM)
      {
        result <- c(rep(NULL, leng))
        result[1] <-"CONNECTED.ANSWER.BorS.WrongPARAM"
        result[2] <-CONNECTED.ANSWER.BorS.WrongPARAM
        result[5] <-Price.current()
        result[8] <-FALSE
        names(result) <- transaction.name
        
        return(result)
      }        
    }
    
    if(code !=CODE.SIMU &
       code !=CODE.MXFSIMU)
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
        names(result) <- transaction.name
        
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
        names(result) <- transaction.name
        
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
        names(result) <- transaction.name
        
        return(result)
      } 
      
      #下單連接成功
      result <- strsplit(.info, ",")[[1]]
      result[8] <-TRUE
      names(result) <- transaction.name
      
      return(result)
      
    }else{ #模擬交易回應
      result <- c(rep(NULL, leng))
      result[1] <-code
      result[2] <-CONNECTED.ANSWER.BorS.ALLDeal
      result[5] <-Price.current()
      result[8] <-TRUE
      names(result) <- transaction.name
      
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
  
  return(FALSE)
  
}

#卷商主機連線測試
connect.test <-function(x=6)
{
  #設定最多檢查次數
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
          },
          SECURTIES ={
            if(param)
            {
              return("卷商")
            }else{return("模擬卷商")}
          },
          stop.Portfolio.MODE ={
            if(param =="1"){return("RSI熱區平倉")}
            if(param =="2"){return("MA回檔平倉")}
          },
          AUTO.STOPLOSS ={
            if(param) {return("主動停損開啟")}
            if(!param){return("主動停損關閉")}
          }
          
  )
}

m.tail <-function(path, .tail.num=1, .header = FALSE)
{
  price.file <- read.csv(path, header = .header, fileEncoding = "big5")
  price.tail <- tail(price.file, .tail.num)
  return(price.tail)
}

menu_0 <-function()
{
  print(paste0("[1] RSI      stopPORTFOLIO"))
  print(paste0("[2] MA Cross stopPORTFOLIO"))
  print(paste0("[S] SIMULATION SWITCH      :",simu))
  
  print("")
}

