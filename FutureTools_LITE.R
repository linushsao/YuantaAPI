# Order mudule base
rm(list=ls())
library("beepr")
setwd("C:/Temp/")

#### 設定下單程式位置 ####
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_base.R")

Product <-"MXFJ1"
Price <-0
BorS <- ""
DateFolder <- ""
result <- "  "
Qty <-1
gear <-0
Stop_portfolio <- 10 #停利
Stop_loss <- 0 #停損
PCL <- 0 #多空代號 1 -1
Max.DDM <- 5
Price.buyin <- 0
simu <-TRUE
safe.Close <- TRUE
auto.stopPortfolio <- FALSE

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
  
  if (!simu){
  # 下單後回傳委託書號 Order.exe TXFA8 B 10800 3 LMT ROD 1
  OrderNo<-system2(paste0(ExecPath,'Order.exe'),args=paste(Product,BorS,Price,Qty,'LMT',"ROD",'1'),stdout = TRUE)
  print(paste("[下單觸發] 限價委託單 :", Product,BorS,Price,Qty,'LMT',"ROD",'1'))
  # 回傳委託序號
  return(OrderNo)
  }else{
    print(paste("[模擬下單觸發] 限價委託單 :", Product,BorS,Price,Qty,'LMT',"ROD",'1'))
  }
}

#### 市價委託單 ####
Place.OrderMKT<-function()
{

  #Qty <-1  
  
  if (!simu){
  # 下單後回傳委託書號 Order.exe TXFA8 B 0 3 MKT IOC 1
  OrderNo <- system2(paste0(ExecPath,'Order.exe'),args=paste(Product,BorS,'0',Qty,'MKT',"IOC",'1'),stdout = TRUE)
  print(paste("[下單觸發] 市價委託單 :", Product,BorS,Price,Qty,'LMT',"ROD",'1'))
  # 回傳委託序號
  return(OrderNo)
  }else{
    print(paste("[模擬下單觸發] 市價委託單 :", Product,BorS,Price,Qty,'LMT',"ROD",'1'))
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

###
#### (建倉)平均法 ####
Position.create<-function(bs=NULL)
{
  
  BorS <-bs
  for.LONG <- "B"
  for.SHORT <- "S"
  #設定訊息檔案路徑
  avg_price_msg <- "_avg_price.csv"
  avg_price_path <- paste0(msg.path, avg_price_msg)
  # #歸零
  # unlink(avg_price_path)
  # sleep(0.25)

  while(TRUE)
  {
    
    get.file <-file.exists(avg_price_path)
    print(paste("get.file :", get.file))   
    if.price.enable <-FALSE
    if.exist <-FALSE

    if (get.file)
    {
      
      avg.Price <- as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", avg_price_path), stdout = TRUE))
      Price <- as.numeric(Price.current())
      
      #檢查是否開始待命建倉
      if(BorS ==for.LONG && Price <avg.Price)
      {
        beep(sound = 2)
        if.price.enable <-TRUE
        print(paste("[設定] 啟動多頭建倉監控價位 :", Price))  
      }
      
      if(BorS ==for.SHORT && Price >avg.Price)
      {
        beep(sound = 2)
        if.price.enable <-TRUE
        print(paste("[設定] 啟動空頭建倉監控價位 :", Price))  
      }
      
      #待命建倉
      while(if.price.enable)
      {
        Price <- as.numeric(Price.current())
        local.msg <-"待命中"
        
        if((BorS ==for.LONG && Price >avg.Price) ||
           (BorS ==for.SHORT && Price <avg.Price) )
        {
            BorS <- ifelse(BorS ==for.LONG, "B", "S")
            Price <- Price.current()
            result <- Place.OrderLMT()
            beep(sound = 8)
            Price.buyin <- as.numeric(Price)
            PCL <- ifelse(BorS ==for.LONG, 1, -1)
            if.price.enable <-FALSE
            if.exist <-TRUE
            local.msg <- "執行多頭建倉"
        } 
        
        #
        print(paste("[待命中] :" 
                    , Price.curr, "<",avg.Price,">", local.msg))
        Sys.sleep(0.20) 
      }
    }
    
    if(if.exist){break}
  }  
  
  #歸零
  unlink(avg_price_path)

}

#### (平倉)動態停損停利 ####
Position.stop<-function(pr=NULL, pcl=NULL)
{
  print(paste("pr :", pr))
  print(paste("pcl :", pcl))
  
  if (!is.null(pr) && !is.null(pcl) &&
      (pr !=0 && pcl !=0))
  {
    
    enable.ddm <- FALSE
    for.LONG <- 1
    for.SHORT <- -1
    Price.in <- pr #建倉價
    PCL <- pcl     #多空編號
    Price.diff <-0 #漲跌幅
    Price.ddm <-0  #動態停利價
    ddm.Ratio <-0
    
    while(TRUE)
    {
      
      #查詢是否有部位
      get.onOpen <- length(QueryOnOpen())
      if (get.onOpen ==0)
      {
        print("[回報] 部位數量變動為零，請確認")
        break
      }
      
      #目前價位
      Price.curr <- as.numeric(Price.current())
      #計算價格變動
      Price.diff <- Price.curr -Price.in

      #檢查是否達到動態停利條件
      #已設定動態停利且還沒啟動即執行
      if(Max.DDM !=0 && !enable.ddm)
      {
        if((PCL ==for.LONG && Price.diff >=Stop_portfolio) ||
           (PCL ==for.SHORT && Price.diff <=Stop_portfolio*-1))
        {
          enable.ddm <- TRUE #啟動動態停損
          Price.ddm <- Price.curr
          print(paste("[設定] 啟動動態停利價位 :", Price.curr))
          beep(sound = 2)
        }
      }
      
      #檢查是否停損
      if ( Stop_loss !=0)
      {
        if(PCL ==for.LONG && Price.diff <=Stop_loss*-1)
        {
          if.safeClose(bs="MS") 
          beep(sound = 8)
          print(paste("[動作] 執行多頭停損價位 :", Price.curr))
          break #回到主MENU
        }
        if(PCL ==for.SHORT && Price.diff >=Stop_loss)
        {
          if.safeClose(bs="MB")
          beep(sound = 8)
          print(paste("[動作] 執行空頭停損價位 :", Price.curr))
          break #回到主MENU
        }
            # {
            # result <- ClosePositionAll()
            # print(paste("[動作] 執行停損價位 :", Price.curr))
            # break #回到主MENU
            # } 
      }
      #else{
      #檢查是否停利平倉
      ##無開啟回檔檢查(無動態停利)
      if(Max.DDM ==0) 
      {
        if(PCL ==for.LONG && Price.diff >=Stop_portfolio) #做多平倉
        {
          # if (!safe.Close)
          # {
          #   BorS <- "S"
          #   Price <- Price.current()
          #   result <- Place.OrderLMT()              
          # }else{
          #   result <- ClosePositionAll()             
          # }
          if.safeClose(bs="S")
          beep(sound = 8)
          print(paste("[動作] 執行多頭停利價位 :", Price.curr))
          break
        }
        if(PCL ==for.SHORT && Price.diff <=Stop_portfolio*-1) #做空平倉
        {
          # BorS <- "B"
          # Price <- Price.current()
          # result <- Place.OrderLMT()
          #result <- ClosePositionAll()
          if.safeClose(bs="B")
          beep(sound = 8)
          print(paste("[動作] 執行空頭停利價位 :", Price.curr))
          break
        }          
      }
        
      ##已開啟動態停利
      if(enable.ddm)
      {
        #創新高
        if((PCL ==for.LONG && Price.curr >Price.ddm) ||
           (PCL ==for.SHORT && Price.curr <Price.ddm))
        {
          Price.ddm <-Price.curr
          beep(sound = 2)
          print(paste("[設定] 更新停利點價位 :", Price.ddm))
          
          # ddm.Ratio <-0
          Stop_portfolio_RatioPRICE_DIFF <-(Stop_portfolio +Max.DDM)
          #以回跌點數換算是否更新DDM動態加成點數
          if(PCL ==for.LONG && Price.ddm -Price.in >=Stop_portfolio_RatioPRICE_DIFF)
          {
            ddm.Ratio = floor(abs((Price.curr 
                                   -(Price.in +Stop_portfolio))/Max.DDM)) *-1
            beep(sound = 2)
            print(paste("[設定] 更新停利點加成價位及點數 :", Price.curr, Price.ddm, ddm.Ratio))
          }
          if(PCL ==for.SHORT && Price.ddm -Price.in <=Stop_portfolio_RatioPRICE_DIFF*-1 )
          {
            ddm.Ratio = floor(abs((Price.curr 
                                   -(Price.in -Stop_portfolio))/Max.DDM)) *-1
            beep(sound = 2)
            print(paste("[設定] 更新停利點加成價位及點數 :", Price.curr, Price.ddm, ddm.Ratio))
          }
          
        }
        else{ #自新高回跌
              # ddm.Ratio <-0
              # Stop_portfolio_RatioPRICE_DIFF <-(Stop_portfolio +Max.DDM)
              # #以回跌點數換算是否更新DDM動態加成點數
              # if(PCL ==for.LONG && Price.ddm -Price.in >=Stop_portfolio_RatioPRICE_DIFF)
              # {
              #   ddm.Ratio = floor(abs((Price.curr 
              #                           -(Price.in +Stop_portfolio))/Max.DDM))
              #   beep(sound = 2)
              #   print(paste("[設定] 更新停利點加成價位及點數 :", Price.curr, Price.ddm, ddm.Ratio))
              # }
              # if(PCL ==for.SHORT && Price.ddm -Price.in <=Stop_portfolio_RatioPRICE_DIFF*-1 )
              # {
              #   ddm.Ratio = floor(abs((Price.curr 
              #                           -(Price.in -Stop_portfolio))/Max.DDM))
              #   beep(sound = 2)
              #   print(paste("[設定] 更新停利點加成價位及點數 :", Price.curr, Price.ddm, ddm.Ratio))
              # }
              
              #檢查停利條件
              if(PCL ==for.LONG  && Price.curr -Price.ddm <= (Max.DDM+ddm.Ratio)*-1)
              {
                # BorS <- "S"
                # Price <- Price.current()
                # result <- Place.OrderLMT()
                #result <- ClosePositionAll()
                if.safeClose(bs="S")
                beep(sound = 8)
                print(paste("[動作] 執行多頭停利價位 :", Price.curr))
                break
              }
              if(PCL ==for.SHORT && Price.curr -Price.ddm >= (Max.DDM+ddm.Ratio))
              {
                # BorS <- "B"
                # Price <- Price.current()
                # result <- Place.OrderLMT()
                #result <- ClosePositionAll()
                if.safeClose(bs="B")
                beep(sound = 8)
                print(paste("[動作] 執行空頭停利價位 :", Price.curr))
                break
              }
        
          #result <- ClosePositionAll()
          # break #回到主MENU                
        }
      }
        
      #}
      #目前/買進價位/停利/停損/價差 
      print(paste("[待命中] :"
                  , Price.curr, "<",Price.in,">", Stop_portfolio, Stop_loss, Price.diff))
      Sys.sleep(0.20)      
    }
    
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
  
  # output <- paste0("[", format(Sys.time(), "%X"), "] 報價 ", Price.current())
  # print(output)
  Price <- Price.current()
  
  print(paste0("DataTIME      : ", date.format)) 
  print(paste0("Product       : ", Product)) 
  print(paste0("Price         : ", Price)) 
  print(paste0("Quantity      : ", Qty)) 
  print(paste0("Gear          : ", gear)) 
  print(paste0("BorS          : ", BorS)) 
  print(paste0("StopPORTFOLIO : ", Stop_portfolio)) 
  print(paste0("StopLOSS      : ", Stop_loss)) 
  print(paste0("Max.DDM       : ", Max.DDM)) 
  print(paste0("Simulation    : ", simu))  
  print(" ")
  
  print("(CL)CloseAllPOSITION")
  print("(CA)CancelAllOrder")
  print("(QR)QueryRight")
  print("(CP)ChangePRodid")
  print("(CL)CloseAllPOSITION")
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
  print("(SPL)STOP portfolio/loss")
  print("(SS)_switch_Simulation") 
  
  action <- readline("[COMMAND] :")
  
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
                  },
            "1"  ={Position.create(bs="B")},
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
                 },
            "3"  ={Position.create(bs="S")},
            "8"  ={
              #Qty <-1
              BorS <- "S"
              Price <- Price.current()
              result <- Place.OrderMKT()
              Price.buyin <- as.numeric(Price)
              PCL <- -1
                 },
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
            "9" ={remoted.ByMsg()},
            SPL ={
                    Position.stop(pr=Price.buyin, pcl=PCL)
                    Price.buyin <-0
                    PCL <-0
                  }, #停利停損
            "7" ={
                    Position.stop(pr=Price.buyin, pcl=PCL)
                    Price.buyin <-0
                    PCL <-0
                  }, #停利停損
            
            SS ={
                if(simu){simu <-FALSE}
                else{simu <-TRUE}
                 },
            PC ={result <- Price.current()}, 
            "++"  = if(as.numeric(Price) != 0)
                      { gear = gear +1
                        Price = as.character(as.numeric(Price)+gear)},
            "--"  = if(as.numeric(Price) != 0)
                      { gear = gear -1
                        Price = as.character(as.numeric(Price)+gear)},
            "+-"  = if(as.numeric(Price) != 0)
                      { gear = 0
                        Price = as.character(as.numeric(Price))},
            QQ ={break},
            
            print(paste0("Command is not correct. [", action, "]"))
            
      )
    }
  
  result <- ""
}



