# Order mudule base
rm(list=ls())
library("beepr")
setwd("C:/Temp/")

#### 設定額外函式位置 ####
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_base.R")
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_custom.R")
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_POSITION.R")

Product <-"MXFJ1"
Price <-0
BorS <- "" #買(B)或賣(S)
Daytrade <-"1" #設定當沖(否1是0)

DateFolder <- ""
result <- "  "
Qty <-1
gear <-0
BASE_portfolio <- 3  #無虧損停利價差
Stop_portfolio <- 10 #動態停利價差
default.enable_stopPORTFOLIO <- 15 #固定停利價差
Max.DDM <- 0
default.PORTFOLIO.buffer <-5
# Stop_loss <- 0 #停損
PCL <- 0 #多空代號 1 -1
Price.buyin <- 0
simu <-TRUE
Auto.positionCLOSE <-FALSE
enable.STABLE.Stop.PORT <-TRUE #停利功能>>預設非動態停利
enable.defaultPORT.check <-TRUE #開啟停利功能

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

enable.STABLE.Stop.PORT.path  <- extra.data(name="enable.STABLE.Stop.PORT", p.mode = "path") #default固定停利
enable.onlyMDD.path  <- extra.data(name="enable.onlyMDD", p.mode = "path") #MDD停利
enable.RSI.TrendADDED.path  <- extra.data(name="enable.RSI.TrendADDED", p.mode = "path") #RSI超買超賣停利
enable.Bolling.path  <- extra.data(name="enable.BollingPATH.ADDED", p.mode = "path") #布林通道停利

#MAIN#


##主程式
#ChangeProd()

repeat
{

  Price <- Price.current()
  
  print(paste0("DataTIME           : ", date.format)) 
  print(paste0("Product            : ", Product)) 
  print(paste0("Price              : ", Price)) 
  print(paste0("Quantity           : ", Qty)) 
  print(paste0("BorS               : ", BorS))

  print(paste0("STABLE S.P.        : ", default.enable_stopPORTFOLIO)) 
  print(paste0("AUTO.S.P.          : ", Stop_portfolio)) 
  print(paste0("ENABLE default P.C : ", enable.defaultPORT.check)) 
  print(paste0("ENABLE STABLE S.P. : ", enable.STABLE.Stop.PORT))
  print(paste0("Auto.pos.CLOSE     : ", Auto.positionCLOSE))
  print(paste0("AUTO.StopPORT      : ", Stop_portfolio.type[Stop_portfolio.code]))
  print(paste0("AUTO.StopLOSS      : ", Stop_loss.type[Stop_loss.code])) 
  print(paste0("Max.DDM            : ", Max.DDM))
  print(paste0("DayTRADE           : ", Daytrade))
  print(paste0("Price.buyin        : ", Price.buyin))
  print(paste0("PCL                : ", PCL))
  print(paste0("Simulation         : ", simu))  
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
 
  print("(EDPC)enable.default.P.CHECK") 
  print("(ESSP)enable.stable.S.P.")
  print("(EOMD)enable.onlyMDD")
  print("(ERTA)enable.RsiTREND.ADDED")
  print("(EBPA)enable.BollingPATH.ADDED")
  print("(APC)_switch_Auto.pos.CLOSE")
  print("(SDP)_switch_defaultPORT") 
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
            # "0" ={result <- ClosePositionAll()},
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
            "0" ={
                    msg.file  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
                    file.create(msg.file)
                    result <- ClosePositionAll()
                  },
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
            "9" ={
                    Position.multi.create()
                    if(Auto.positionCLOSE)
                    {
                      next.step <- "7"
                    }
            },
            # # 切換保本模式
            # "3" ={
            #       price.diff <-abs(as.numeric(readline("KEEP.CASH MOD(adjust buffer) :")))
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
            EDPC ={
                if(enable.defaultPORT.check){enable.defaultPORT.check <-FALSE}
                else{enable.defaultPORT.check <-TRUE}
                },
            SDP ={
                if(enable.STABLE.Stop.PORT){enable.STABLE.Stop.PORT <-FALSE}
                else{enable.STABLE.Stop.PORT <-TRUE}
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
            ESSP ={
                    file.create(enable.STABLE.Stop.PORT.path)
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

