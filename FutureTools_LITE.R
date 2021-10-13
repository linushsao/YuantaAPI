# Order mudule base
rm(list=ls())
library("beepr")
setwd("C:/Temp/")

#### 設定額外函式位置 ####
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_base.R")
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_custom.R")

Product <-"MXFJ1"
Price <-0
BorS <- "" #買(B)或賣(S)
Daytrade <-"1" #設定當沖(否1是0)

DateFolder <- ""
result <- "  "
Qty <-1
gear <-0
Stop_portfolio <- 10 #停利價差
default.enable_stopPORTFOLIO <- 15
# Stop_loss <- 0 #停損
PCL <- 0 #多空代號 1 -1
Max.DDM <- 5
Price.buyin <- 0
simu <-TRUE
Auto.positionCLOSE <-FALSE

safe.Close <- TRUE #TRUE表使用緊急平倉來平倉
Stop_portfolio.type <-c("(1)MDD", "(2)RsiOVER_SB", "(3)Bolling")
Stop_portfolio.code <-1
# names(Stop_portfolio.type) <- c("7", "77")
Stop_loss.type <-c("(1)RsiREVERSAL", "(2)ResearchLINE", "(3)ExtremeLINE", "(4)Bolling", "(5)PolarSTAR")
Stop_loss.code <-1
next.step <- ""

get.hour <- as.numeric(format(Sys.time(), "%H"))
get.sysDate <-  Sys.Date()
if (get.hour <8){get.sysDate = get.sysDate -1 } 
date.format <- gsub("-", "", get.sysDate)
#date.format <- gsub("-", "", Sys.Date())


Product.file <- paste0(date.format, "_Match.txt")
data.path <- paste0("C:/Users/linus/Documents/Project/9.Shared.Data/8.forSmartAPI/"
                    , date.format, "/"
                    , Product, "/"
                    , Product.file)
msg.path <- paste0("C:/Temp/")
price.path <- paste0("C:/Temp/msg/")

enable.onlyMDD.path  <- extra.data(name="enable.onlyMDD", p.mode = "path") #MDD停利
enable.RSI.TrendADDED.path  <- extra.data(name="enable.RSI.TrendADDED", p.mode = "path") #RSI超買超賣停利
enable.Bolling.path  <- extra.data(name="enable.BollingPATH.ADDED", p.mode = "path") #布林通道停利

#MAIN#
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
  
  x <-3
  if(pr=="CL"){x=3}
  if(pr=="HI"){x=6}
  if(pr=="LO"){x=7}
  
  result <- QueryOHCL(data.path, 1)
  result <- strsplit(result, ",") 
  result <- result[[1]][x]
  
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

#### (建倉)多重策略 ####
Position.multi.create<-function()
{
  
  for.LONG <- "B"
  for.SHORT <- "S"
  msg.North.start <- -1
  msg.South.start <- 1
  local.msg <-""
  action <- as.numeric(readline("建倉條件[RSI.Switch(1)極星(2)] :"))
  cond.enable <-FALSE
  
  while(TRUE)
  {
    
    #讀取現價  
    # Price <- extra.data(name="CL")  
    Price <- as.numeric(Price.current())
    #讀取建倉條件
    #RSI法
    if(action ==1 && !cond.enable)
    {
      polar_star_switch <-extra.data(name="polar_star_switch")
      enable.north.star <-(polar_star_switch <0)
      enable.south.star <-(polar_star_switch >0)
      # price.OP <-extra.data(name="OP")
      # price.op.ma <-extra.data(name="op_ma")
      # 
      # price.RSI <-extra.data(name="RSI")
      # price.RSI_PRE <-extra.data(name="RSI_PRE")
      # price.RSI_MA5 <-extra.data(name="RSI_MA5")
      # price.BSRate <-extra.data(name="BSRate")
      # price.BSRateDiff <-extra.data(name="BSRateDiff")
      # 
      # enable.north.star <-  price.RSI <0 &&
      #                       price.RSI_PRE >0 &&
      #                       price.RSI_MA5 >0 &&
      #                       price.op.ma >0 &&
      #                       Price -price.OP <0
      # enable.south.star <-  price.RSI >0 &&
      #                       price.RSI_PRE <0 &&
      #                       price.RSI_MA5 <0 &&
      #                       price.op.ma <0 &&
      #                       Price -price.OP >0
      cond.enable <-(enable.north.star || enable.south.star)
      local.msg <- paste("<RSI.Switch建倉法>", price.RSI)
    }
    
    #極星法
    if(action ==2 && !cond.enable)
    {
      local.msg <- "<極星建倉法>"
       #極星類型
      code.polar_star <- extra.data(name="ploar_star")
      #極星建倉點
      price.polar_star <- extra.data(name="ploar_star_price")
      
      enable.north.star <- (code.polar_star ==msg.North.start &&
                              Price >price.polar_star)
      enable.south.star <- (code.polar_star ==msg.South.start &&
                              Price <price.polar_star)
      cond.enable <-(enable.north.star || enable.south.star)
    }
    
    #判斷是否啟動建倉
    if (cond.enable)
    {    
 
      # 設定建倉
        if (enable.north.star){BorS =for.SHORT} 
        if (enable.south.star){BorS =for.LONG} 
      
        #執行建倉
        # BorS <- "B"
        Price <- Price.current()
        result <- Place.OrderLMT()
        beep(sound = 2)
        Price.buyin <- as.numeric(Price)
        PCL <- ifelse(BorS ==for.LONG, msg.South.start, msg.North.start)
        print(paste("[動作] 執行建倉價位 :", Price, BorS))
        
        if(Auto.positionCLOSE)
        {
          Sys.sleep(1)
          beep(sound = 2)
          Position.stop()
          Price.buyin <-0
          PCL <-0
        }  
        break

    }
    
    #
    print(paste("[待命中] :", Price, local.msg))
    Sys.sleep(0.20) 
    
  }  
  
}

#### (平倉)動態停損停利 ####
Position.stop<-function()
{

  enable.ddm <- FALSE
  for.LONG <- 1
  for.SHORT <- -1
  Price.diff <-0 #漲跌幅
  Price.ddm <-0  #動態停利價
  Stop_loss.price <-0
  ddm.Ratio <-0

  Price.in <-Price.buyin 
  # p.mode.switch <- p.mode

  REGISTER.ClosePOSITION <- c(TRUE, rep(FALSE, 8))
  CHECK.ClosePOSITION <- c(rep(FALSE, 9))
  
  while(TRUE)
  {
    
    # #查詢是否有部位
    # get.onOpen <- length(QueryOnOpen())
    # 
    # if (get.onOpen ==0)
    # {
    #   print("[回報] 倉位數量應大於零，請確認")
    #   break
    # }
    
    #目前價位
    Price.curr <- as.numeric(Price.current())
    # Price.curr <- extra.data(name="CL")
    Price.open <- extra.data(name="OP")
    
    #計算價格變動
    Price.diff <- Price.curr -Price.in

    #檢查是否達到動態停利條件
    #已設定動態停利且還沒啟動即執行
    if(Max.DDM !=0 && !enable.ddm)
    {
      if((PCL ==for.LONG && Price.diff >=Stop_portfolio) ||
         (PCL ==for.LONG && Price.curr -Price.open >=default.enable_stopPORTFOLIO) ||
         (PCL ==for.SHORT && Price.diff <=Stop_portfolio*-1) ||
         (PCL ==for.SHORT && Price.curr -Price.open <=default.enable_stopPORTFOLIO*-1))
      {
        enable.ddm <- TRUE #啟動動態停損
        Price.ddm <- Price.curr #設定動態停利計算起始點
        print(paste("[設定] 啟動動態停利，價位 :", Price.curr))
        beep(sound = 5)
      }
    }
    
    #檢查是否啟動停損
    if ( Stop_loss.code ==1)
    {
      
      #RSI停損
      # Stop_loss.price <-0
      operator <- as.character(Stop_loss.code)
      
      switch(operator,
             
             # RSI_RESVERSAL
             "1" ={
                     Stop_loss.price.RSI <-extra.data(name="RSI")
                     Stop_loss.price.RSI_MA5 <-extra.data(name="RSI_MA5")
                     
                     if(
                       PCL ==for.LONG && 
                       (Stop_loss.price.RSI <0 &&
                        Price.curr -Price.in <=default.enable_stopPORTFOLIO*-1 &&
                        # Stop_loss.price.RSI_MA5 <0 &&
                        Stop_loss.price.RSI <Stop_loss.price.RSI_MA5*0.5)
                     )
                       {
                         if.safeClose(bs="MS") 
                         beep(sound = 7)
                         print(paste("[動作] 執行多頭停損價位 :", Price.curr))
                         break #回到主MENU
                       }
                     if(
                       PCL ==for.SHORT && 
                       (Stop_loss.price.RSI >0 &&
                        Price.curr -Price.in >=default.enable_stopPORTFOLIO &&
                         # Stop_loss.price.RSI_MA5 >0 &&
                        Stop_loss.price.RSI >Stop_loss.price.RSI_MA5*0.5)
                     )
                     {
                         if.safeClose(bs="MB")
                         beep(sound = 7)
                         print(paste("[動作] 執行空頭停損價位 :", Price.curr))
                         break #回到主MENU
                     }
                 }            
             )
      

      
    }
    
    if ( Stop_loss.code >=2 && Stop_loss.code <=5)
    {
      
      #停損價位
      # Stop_loss.price <-0
      operator <- as.character(Stop_loss.code)
      
      switch(operator,

             # Research_Line
             "2" ={
                    if(PCL ==for.LONG ){Stop_loss.price <-extra.data(name="Research_Line_lower")} 
                    if(PCL ==for.SHORT){Stop_loss.price <-extra.data(name="Research_Line_Upper")} 
                  },
             # extremes_Line
             "3" ={
                     if(PCL ==for.LONG ){Stop_loss.price <-extra.data(name="extremes_Line_lower")} 
                     if(PCL ==for.SHORT){Stop_loss.price <-extra.data(name="extremes_Line_Upper")} 
                   },
             # Bolling
             "4" ={
                     if(PCL ==for.LONG ){Stop_loss.price <-extra.data(name="B_LO")} 
                     if(PCL ==for.SHORT){Stop_loss.price <-extra.data(name="B_UP")} 
                   },
             # PolarSTAR
             "5" ={
                    Stop_loss.price <-extra.data(name="ploar_star_stopLoss ")
                   }
             )
      
      if(PCL ==for.LONG && Price.curr <=Stop_loss.price)
      {
        if.safeClose(bs="MS") 
        beep(sound = 7)
        print(paste("[動作] 執行多頭停損價位 :", Price.curr))
        break #回到主MENU
      }
      if(PCL ==for.SHORT && Price.curr >=Stop_loss.price)
      {
        if.safeClose(bs="MB")
        beep(sound = 7)
        print(paste("[動作] 執行空頭停損價位 :", Price.curr))
        break #回到主MENU
      }

    }

    #檢查是否停利平倉及種類
    #編號1為預設之MDD因此為TRUE
    Stop_PORTFOLIO.price.RSI <-extra.data(name="RSI")
    RSI.OverBOUGHT <- 20
    RSI.OverSOLD <- -20
    BollingPATH.UPPER <-extra.data(name="B_UP")
    BollingPATH.LOWER <-extra.data(name="B_LO")

    if(file.exists(enable.onlyMDD.path))
    {
      unlink(enable.onlyMDD.path)
      # p.mode.switch =1
      REGISTER.ClosePOSITION <- c(TRUE, rep(FALSE, 8))
      CHECK.ClosePOSITION <- c(rep(FALSE, 9))
      print(paste("[設定] 切換為預設onlyMDD停利，價位 :", Price.curr))
      beep(sound = 2)
    }
    if(file.exists(enable.RSI.TrendADDED.path))
    {
        unlink(enable.RSI.TrendADDED.path)
        # p.mode.switch <-2
        REGISTER.ClosePOSITION[2] <-TRUE
        print(paste("[設定] 附加RSI.TrendADDED停利，價位 :", Price.curr))
        beep(sound = 2)
    }
    if(file.exists(enable.Bolling.path))
    {
        unlink(enable.Bolling.path)
        # p.mode.switch <-3
        REGISTER.ClosePOSITION[3] <-TRUE
        print(paste("[設定] 附加BollingPATH.ADDED停利，價位 :", Price.curr))
        beep(sound = 2)
    }
    
    #預設值，MDD停利
    # if(p.mode.switch ==1)
    # {
    #   MDD.ClosePOSITION <-TRUE #預設判斷MDD
    # } 
    # 附加，RSI超買超賣停利
    if(REGISTER.ClosePOSITION[2]) #考慮RSI
    {
      if(
        (PCL ==for.LONG && (Stop_PORTFOLIO.price.RSI >RSI.OverBOUGHT))
        ||
        (PCL ==for.SHORT && (Stop_PORTFOLIO.price.RSI <RSI.OverSOLD))
      )
      {
        CHECK.ClosePOSITION[2] <-TRUE
      }
    }
    #附加，布林通道停利
    if(REGISTER.ClosePOSITION[3]) #考慮布林通道
    {
      if(
        (PCL ==for.LONG && (Price.curr >BollingPATH.UPPER))
        ||
        (PCL ==for.SHORT && (Price.curr <BollingPATH.LOWER))
      )
      {
        CHECK.ClosePOSITION[3] <-TRUE
      }
    }
    
    #附加條件總檢查
    EXTRA.ClosePOSITION <- TRUE
    check_series <- (REGISTER.ClosePOSITION ==CHECK.ClosePOSITION)
    check_leng <- length(check_series)
    mark_REGISTER <- 0
    mark_CHECK <- 0
    for(miu in 1:check_leng)
    {
      if(check_series[miu] ==FALSE){EXTRA.ClosePOSITION <-FALSE}
      if(REGISTER.ClosePOSITION[miu] ==TRUE){mark_REGISTER <-mark_REGISTER+1}
      if(CHECK.ClosePOSITION[miu]    ==TRUE){mark_CHECK <-mark_CHECK+1}
    }

    ##無開啟回檔檢查(無動態停利)
    if(Max.DDM ==0) 
    {
        if(PCL ==for.LONG && Price.diff >=Stop_portfolio) #做多平倉
        {
          
          if.safeClose(bs="S")
          beep(sound = 8)
          print(paste("[動作] 執行多頭停利價位 :", Price.curr))
          break
        }
        if(PCL ==for.SHORT && Price.diff <=Stop_portfolio*-1) #做空平倉
        {
          
          if.safeClose(bs="B")
          beep(sound = 8)
          print(paste("[動作] 執行空頭停利價位 :", Price.curr))
          break
        }         
    }

    ##已開啟動態停利
    if(enable.ddm)
    {
      #回檔加成係數
      ddm.Ratio <-0
      #創新高
      if((PCL ==for.LONG && Price.curr >Price.ddm) ||
         (PCL ==for.SHORT && Price.curr <Price.ddm))
      {
        
        CHECK.ClosePOSITION[1] <-TRUE
        
        Price.ddm <-Price.curr
        beep(sound = 2)
        print(paste("[設定] 更新停利點價位 :", Price.ddm))
        
        Stop_portfolio_RatioPRICE_DIFF <-(Stop_portfolio +Max.DDM)
        #以回檔點數換算是否更新DDM動態加成點數
        if(PCL ==for.LONG && Price.ddm -Price.in >=Stop_portfolio_RatioPRICE_DIFF)
        {
          ddm.Ratio = round(abs((Price.curr 
                                 -(Price.in +Stop_portfolio))/Max.DDM)) *-1
          beep(sound = 2)
          print(paste("[設定] 更新多倉停利點加成價位及點數 :", Price.curr, Price.ddm, ddm.Ratio))
        }
        if(PCL ==for.SHORT && Price.ddm -Price.in <=Stop_portfolio_RatioPRICE_DIFF*-1 )
        {
          ddm.Ratio = round(abs((Price.curr 
                                 -(Price.in -Stop_portfolio))/Max.DDM)) *-1
          beep(sound = 2)
          print(paste("[設定] 更新空倉停利點加成價位及點數 :", Price.curr, Price.ddm, ddm.Ratio))
        }
        
      }
      #回檔
      else{ 
            #檢查停利條件
            if(EXTRA.ClosePOSITION)
            {
              if((PCL ==for.LONG  && Price.curr -Price.ddm <= (Max.DDM+ddm.Ratio)*-1))
              {
                if.safeClose(bs="S")
                beep(sound = 8)
                print(paste("[動作] 執行多頭停利價位 :", Price.curr, Price.diff))
                break
              }
              if(PCL ==for.SHORT && Price.curr -Price.ddm >= (Max.DDM+ddm.Ratio))
              {
                if.safeClose(bs="B")
                beep(sound = 8)
                print(paste("[動作] 執行空頭停利價位 :", Price.curr, Price.diff))
                break
              }             
            }
          }
    }

    #價差/目前/買進價位/停利/停損/回檔/回檔調整係數/ 
    print(paste("[待命中] :"
                , Price.diff, "<", Price.curr, Price.in,">", Stop_portfolio, Stop_loss.price
                , Max.DDM, ddm.Ratio, PCL, Stop_PORTFOLIO.price.RSI
                , EXTRA.ClosePOSITION, mark_REGISTER, mark_CHECK))

    Sys.sleep(0.20)      
  }

}

### 接收XQ訊號執行交易 ###
remoted.ByMsg <-function()
{

  #紀錄ACTION
  Create_LongPosition  <-  11 #多建倉代號
  Close_LongPosition   <-  10 #多平倉代號
  Create_ShortPosition <- -11 #空建倉代號
  Close_ShortPosition  <- -10 #空平倉代號
  TO_StandBy <- 99 #返回選單
  array_msg <- "_remote_action.csv"
  array_path <- paste0(msg.path, array_msg)
  unlink(array_path)
  
  while(TRUE)
  {
      math.trading <- 0
      get.onOpen <- QueryOnOpen()
      Price <- as.numeric(Price.current())
    
      get.file <-file.exists(array_path)
      print(paste("get.file :", get.file))
      
      if (get.file)
      {
          math.trading = as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", array_path), stdout = TRUE))
          print(paste("math.trading :", math.trading))
          unlink(array_path)
          
          if (math.trading !=0) 
          { #產生交易訊號
            
              if (math.trading ==Create_LongPosition )
              { #下多單_
                BorS <- "B"
                
                #做多下限價單
                result <- Place.OrderLMT()
                beep(sound = 2)
                #break
              }
              
              if (math.trading ==Create_ShortPosition )
              { #下空單_
                BorS <- "S"
                
                #做空下限價單
                result <- Place.OrderLMT()
                beep(sound = 2)
                #break
              }
              
              if ((math.trading ==Close_LongPosition 
                   || math.trading ==Close_ShortPosition))
              { #有倉位_平倉
                result <- ClosePositionAll()
                beep(sound = 2)
                #break
              }
              
              if (math.trading ==TO_StandBy )
              { 
                break
              }
            
          }          
          
      }

      print(paste("[待命中] 目前價位 :", Price))
      Sys.sleep(0.2)

    }

}

##主程式
#ChangeProd()

repeat
{

  Price <- Price.current()
  
  print(paste0("DataTIME      : ", date.format)) 
  print(paste0("Product       : ", Product)) 
  print(paste0("Price         : ", Price)) 
  print(paste0("Quantity      : ", Qty)) 
  print(paste0("BorS          : ", BorS)) 
  print(paste0("StopPORTFOLIO : ", Stop_portfolio)) 
  print(paste0("default.S.P   : ", default.enable_stopPORTFOLIO))
  print(paste0("Auto.pos.CLOSE: ", Auto.positionCLOSE))
  print(paste0("AUTO.StopPORT : ", Stop_portfolio.type[Stop_portfolio.code]))
  print(paste0("AUTO.StopLOSS : ", Stop_loss.type[Stop_loss.code])) 
  print(paste0("Max.DDM       : ", Max.DDM))
  print(paste0("DayTRADE      : ", Daytrade))
  print(paste0("Price.buyin   : ", Price.buyin))
  print(paste0("PCL           : ", PCL))
  print(paste0("Simulation    : ", simu))  
  print(" ")

  # print("(CL)CloseAllPOSITION")
  # print("(CA)CancelAllOrder")
  print("(QR)QueryRight")
  print("(CP)ChangePRodid")
  print("(QA)QueryAllOrder")
  print("(QO)QueryOnOpen")
  print("(QU)QueryUnfinished")
  print("(OL)Place.OrderLMT")
  print("(OM)Place.OrderMKT")
  print("(PR)oduct bundle")
  print("(P)rice bundle")
  print("(Q)uantity bundle")
  print("(BS)Buy|Sell bundle")
  print("(RBM)remoted.ByMsg")
  print("(SPT)StopPORT.TYPE")
  print("(SLT)StopLOSS.TYPE")
  print("(DT)_switch_DayTRADE")  
  print("(PRB)Price.buyin")
  print("(PCL)PCL")
  print("(EOMD)enable.onlyMDD")
  print("(ERTA)enable.RsiTREND.ADDED")
  print("(EBPA)enable.BollingPATH.ADDED")
  print("(APC)_switch_Auto.pos.CLOSE")
  print("(SS)_switch_Simulation") 
   
  if(next.step =="")
  {
    action <- readline("[COMMAND] :")
  }else{
    action <-next.step
    next.step <-""
  }
 
  if (action != "")
  {
      switch(action,
    
            CL ={result <- ClosePositionAll()},
            "0" ={result <- ClosePositionAll()},
            CP ={result <- ChangeProd()},
            CA ={result <- CancelAll()},
            "5"  ={result <- CancelAll()},
            QA ={result <- QueryAllOrder()},
            QO ={result <- QueryOnOpen()},
            QF ={result <- QueryFutureRights()},
            QU ={result <- QueryUnfinished()},
            QR ={result <- QueryRight()
                  print(result)
                },
            OL ={result <- Place.OrderLMT()},
            "4"  ={
                    #Qty <-1 
                    BorS <- "B"
                    Price <- Price.current()
                    result <- Place.OrderLMT()
                    Price.buyin <- as.numeric(Price)
                    PCL <- 1
                    if(Auto.positionCLOSE)
                    {
                      next.step <- "7"
                    }
                  },
            "2"  ={
                    #Qty <-1 
                    BorS <- "B"
                    Price <- Price.current()
                    result <- Place.OrderMKT()
                    Price.buyin <- as.numeric(Price)
                    PCL <- 1
                  },
            "6"  ={
                    #Qty <-1
                    BorS <- "S"
                    Price <- Price.current()
                    result <- Place.OrderLMT()
                    Price.buyin <- as.numeric(Price)
                    PCL <- -1
                    if(Auto.positionCLOSE)
                    {
                      next.step <- "7"
                    }
                 },
            "8"  ={
                    #Qty <-1
                    BorS <- "S"
                    Price <- Price.current()
                    result <- Place.OrderMKT()
                    Price.buyin <- as.numeric(Price)
                    PCL <- -1
                },
            # # 多重建倉法
            "1" ={
                    Position.multi.create()
                    if(Auto.positionCLOSE)
                    {
                      next.step <- "7"
                    }
            },
            # # 極星法建平倉
            # "3" ={
            #       Position.polar_star()
            #       Price.buyin <-0
            #       PCL <-0
            #       },
            # 停利停損>RSI超買超賣法
            # "9"  ={
            #         Position.stop(p.mode = 2)
            #         Price.buyin <-0
            #         PCL <-0
            #       },
            # 停利停損>回檔法
            "7" ={
                  Position.stop()
                  Price.buyin <-0
                  PCL <-0
                  }, 
            # "77" ={
            #       Position.stop(p.mode = 77)
            #       Price.buyin <-0
            #       PCL <-0
            #      }, 
            # "++"  = if(as.numeric(Price) != 0)
            #   { gear = gear +1
            #   Price = as.character(as.numeric(Price)+gear)},
            # "--"  = if(as.numeric(Price) != 0)
            #   { gear = gear -1
            #   Price = as.character(as.numeric(Price)+gear)},
            # "+-"  = if(as.numeric(Price) != 0)
            #   { gear = 0
            #   Price = as.character(as.numeric(Price))},
            
            OM ={result <- Place.OrderMKT()},
            P  ={Price <- readline("Price bundle :")
                  if (Price ==""){Price =Price.current()}
                },  
            Q ={Qty <- readline("Quantity bundle :")},
            BS ={BorS <- readline("(B)uy/(S)ell :")},  
            PB ={Product <- readline("Product bundle :")}, 
            SP ={Stop_portfolio <- readline("StopPORTFOLIO :")},
            SL ={Stop_loss <- readline("StopLOSS :")},
            RBM ={remoted.ByMsg()},
            PPS ={Position.polar_star()},
            PMC ={Position.multi.create()},
            
            DESP ={default.enable_stopPORTFOLIO <- as.numeric(readline("default.enable_stopPORTFOLIO bundle :"))},
            
            PRB ={Price.buyin <- as.numeric(readline("Price.buyin bundle :"))},
            PCL ={PCL <- as.numeric(readline("PCL bundle(1/-1):"))},
            APC ={
                  if(Auto.positionCLOSE){Auto.positionCLOSE <-FALSE}
                  else{Auto.positionCLOSE <-TRUE}
                  },
            SS ={
                if(simu){simu <-FALSE}
                else{simu <-TRUE}
                 },
            DT ={
                if(Daytrade =="0"){Daytrade <-"1"}
                else{Daytrade <-"0"}
                 },
            SLT  ={
                    Stop_loss.code <- as.numeric(readline("Stop_loss.type[(1)RsiREVER, (2)ResearchLINE, (3)ExtremeLINE, (4)Bolling, (5)PolarSTAR] :"))
                    if (is.na(Stop_loss.code) ||
                        Stop_loss.code <1 ||
                        Stop_loss.code >5)
                      {Stop_loss.code <-1}
                  },
            SPT  ={
                    Stop_portfolio.code <- as.numeric(readline("Stop_loss.type[(1)MDD, (2)RsiOVER_SB] :"))
                          if (is.na(Stop_portfolio.code) ||
                              Stop_portfolio.code <1 ||
                              Stop_portfolio.code >2)
                             {Stop_portfolio.code <-1}
                  },
            EOMD ={
                      file.create(enable.onlyMDD.path)
                  },
            ERTA ={
                      file.create(enable.RSI.TrendADDED.path)
                  },
            EBPA ={
                      file.create(enable.Bolling.path)
                  },
            QQ ={break},
            
            print(paste0("Command is not correct. [", action, "]"))
            
      )
    }
  
  result <- ""
}

