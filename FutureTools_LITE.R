#
rm(list=ls())
#CUstom LIB.
LIBRS <- c('roxygen2')
sapply(LIBRS,library,character.only=TRUE)
setwd("C:/Users/linus/Documents/Project/1.R/Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/")
library('roxygen2')
roxygenize()
library("stock.Analyze")

#
library("beepr")
setwd("C:/Temp/")

##
msg.path <- "C:/Temp/"
price.path <- "C:/Temp/msg/"
realdata.path <- "C:/Users/linus/Documents/Project/9.Shared.Data/8.forSmartAPI/"

##
#### 設定額外函式位置 #### 
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_base.R")
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_custom.R")
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_POSITION.R")
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_SIMUServer.R")
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_AGENTServer.R")
source("C:/Users/linus/Documents/Project/6.APITols/FutureTools_config.R")
##

#測試連線結果
print(paste("[動作] 測試卷商連線品質..."))

check.result <-c()
if(connect.test(x=6))
{
  check.result <-(paste("[訊息] 聯線正常."))
  
}else{
  check.result <- (paste("[錯誤] 無法建立聯線."))
}
print(check.result)

action <- readline("請按ENTER繼續...")

##主程式
#ChangeProd()

repeat
{
  
  Price <- Price.current()
  print(paste0("--------Fear Of Market Out--------"))
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
  print(paste0("Max.DDM            : ", Max.DDM))
  print(paste0("DayTRADE           : ", Daytrade))
  print(paste0("Price.buyin        : ", Price.buyin))
  print(paste0("PCL                : ", PCL)) 
  print(paste0("switch.stopPORT.MA : ", switch.stopPORT))
  print(paste0("switch.stopPORT.RSI: ", switch.stopPORT_RSI))
  
  print(paste0("S&P Unbreaked time : ", Price.reachLIMITED.times.Limited))
  print(paste0("Simulation         : ", simu))  
  print(paste0("REAL.XFSource      : ", switch.DATA.Source))  
  
  print(" ")
  print("(SCK) ShortCUT KET MAP")
  print("(CNT) CONNECTION TEST")
  
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
    
            # CL ={result <- ClosePositionAll()},
            CP ={result <- ChangeProd()},
            "5"  ={
                    result <- CancelAll()
                    print(paste("回傳結果(取消下單) :", result))
                    
            },
            QA ={result <- QueryAllOrder()},
            QO ={result <- QueryOnOpen()},
            QF ={result <- QueryFutureRights()},
            QU ={result <- QueryUnfinished()},
            QR ={result <- QueryRight()
                  print(result)
                },
            OL ={result <- Place.OrderLMT()},
            SCK =shortcut.key(),
            CNT ={
              print(paste("[動作] 測試卷商連線品質..."))
              
              check.result <-c()
              if(connect.test(x=6))
              {
                check.result <-(paste("[訊息] 聯線正常."))
                
              }else{
                check.result <- (paste("[錯誤] 無法建立聯線."))
              }
              print(check.result)
              
              action <- readline("請按ENTER繼續...")
            },
            
            "0" ={
                    msg.file  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
                    file.create(msg.file)
                    result <- ClosePositionAll()
                    print(paste("交易序號回傳 :", result))
                    
                    print(paste("回傳結果(全平倉) :", result))
                    
                  },
            "4"  ={
                    BorS <- "B"
                    Price <- Price.current()
                    #執行交易並回傳交易序號
                    OrderNO <-PTrading.MGR(.BorS=BorS
                                            , .Price=Price
                                            , .Qty=Qty
                                            , .Daytrade=Daytrade
                                            , .simu=simu)
                    
                    #依下單回傳序號解碼成文字向量，並確認交易結果
                    list.RESULT <-PTrading.confirm(OrderNO)
                    .checkRESULT <-list.RESULT[[1]]
                    transaction  <-list.RESULT[[2]]
                    
                    #交易成功則執行後續設定
                    if(.checkRESULT){PTConf.export(transaction)}

                    #匯出交易紀錄
                    append.to.file(data = as.data.frame(transaction), path = paste0(OrderNO, ".csv"))
                    m.act <-readline(paste0("交易序號回傳 :", transaction[1]
                                            , "，交易結果 :", transaction[2], " <Press Any Key pls.>"))
                    
                    # 
                    if(Auto.positionCLOSE)
                    {
                      next.step <- "7"
                    }  
       
                  },
            "2"  ={
                    BorS <- "B"
                    Price <- Price.current()
                    #執行交易並回傳交易序號
                    OrderNO <-PTrading.MGR(.BorS=BorS
                                           , .Price=Price
                                           , .Qty=Qty
                                           , .Daytrade=Daytrade
                                           , .simu=simu)
                    
                    #依下單回傳序號解碼成文字向量，並確認交易結果
                    list.RESULT <-PTrading.confirm(OrderNO)
                    .checkRESULT <-list.RESULT[[1]]
                    transaction  <-list.RESULT[[2]]
                    
                    #交易成功則執行後續設定
                    if(.checkRESULT){PTConf.export(transaction)}
                    
                    #匯出交易紀錄
                    append.to.file(data = as.data.frame(transaction), path = paste0(OrderNO, ".csv"))
                    m.act <-readline(paste0("交易序號回傳 :", transaction[1]
                                            , "，交易結果 :", transaction[2], " <Press Any Key pls.>"))
                    
                    # 
                    if(Auto.positionCLOSE)
                    {
                      next.step <- "7"
                    }                      
                  },
            "6"  ={
                    BorS <- "S"
                    Price <- Price.current()
                    #執行交易並回傳交易序號
                    OrderNO <-PTrading.MGR(.BorS=BorS
                                           , .Price=Price
                                           , .Qty=Qty
                                           , .Daytrade=Daytrade
                                           , .simu=simu)
                    
                    #依下單回傳序號解碼成文字向量，並確認交易結果
                    list.RESULT <-PTrading.confirm(OrderNO)
                    .checkRESULT <-list.RESULT[[1]]
                    transaction  <-list.RESULT[[2]]
                    
                    #交易成功則執行後續設定
                    if(.checkRESULT){PTConf.export(transaction)}
                    
                    #匯出交易紀錄
                    append.to.file(data = as.data.frame(transaction), path = paste0(OrderNO, ".csv"))
                    m.act <-readline(paste0("交易序號回傳 :", transaction[1]
                                            , "，交易結果 :", transaction[2], " <Press Any Key pls.>"))
                    
                    if(Auto.positionCLOSE)
                    {
                      next.step <- "7"
                    }                     
                 },
            "8"  ={
                    BorS <- "S"
                    Price <- Price.current()
                    #執行交易並回傳交易序號
                    OrderNO <-PTrading.MGR(.BorS=BorS
                                           , .Price=Price
                                           , .Qty=Qty
                                           , .Daytrade=Daytrade
                                           , .simu=simu)
                    
                    #依下單回傳序號解碼成文字向量，並確認交易結果
                    list.RESULT <-PTrading.confirm(OrderNO)
                    .checkRESULT <-list.RESULT[[1]]
                    transaction  <-list.RESULT[[2]]
                    
                    #交易成功則執行後續設定
                    if(.checkRESULT){PTConf.export(transaction)}
                    
                    #匯出交易紀錄
                    append.to.file(data = as.data.frame(transaction), path = paste0(OrderNO, ".csv"))
                    m.act <-readline(paste0("交易序號回傳 :", transaction[1]
                                            , "，交易結果 :", transaction[2], " <Press Any Key pls.>"))
                    
                    if(Auto.positionCLOSE)
                    {
                      next.step <- "7"
                    }                     
                },
            # # # 多重建倉法
            # "9" ={
            #         Position.multi.create()
            #         if(Auto.positionCLOSE)
            #         {
            #           next.step <- "7"
            #         }
            # },
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
            EPM ={
              
                  .count<-0
                  
                  while(TRUE)
                  {
                    
                    .count <-.count+1
                    if(.count >50)
                    {
                      if(!connect.test(x=6)){print(paste("[錯誤] 無法建立聯線."))}
                    }
                    
                    .portfolio.current <-portfolio.monitor()
                    
                    if(.portfolio.current ==-1)
                    {
                      print("[訊息] 待命中，目前無倉位.")
                    }else{
                      print(paste0("[訊息] <未沖銷期貨浮動損益> :", .portfolio.current))
                    }
                    
                    #關閉顯示
                    if(file.exists(EPM.path)) 
                    {
                      unlink(EPM.path)
                      beep(sound = 2)
                      m.action <- readline(paste0("[動作] 跳出訊息視窗，press any key pls..."))
                      break
                    } 
                    
                    Sys.sleep(1)
                    
                  }
              

            },
            RSS ={file.create(RSS.path)},
            MSGL ={
                    msg.lite <- TF.Switch(msg.lite)
                    .path <-extra.data(name="msg.lite", p.mode = "path")
                    print(paste("[NEW VALUE] msg.lite :", msg.lite))
                    .price <- as.character(msg.lite)
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
                    result <- readline("PLS. PRESS ANY KEY to continue...")
                  
            },
            # BorS <- "B"
            # Price <- Price.current()
            EPPT ={
                    if(Price !=0)
                    {
                      m.action <-readline(paste0("Price :", Price, " & PCL:", PCL," ready for export,(Y/N)"))
                      if(m.action =="Y")
                      {
                        PTConf.export(.Price =Price ,.PCL = PCL)              
                        
                      }
                    }
            },
            EMSS ={
                    SIMU.DATA.Server()
                  },
            EAS ={
                    Position.AGENT()
                  },
            SPUT ={
                    Price.reachLIMITED.times.Limited <-as.numeric(readline("Quantity bundle :"))
                   },
            SMS ={
                  switch.DATA.Source <-TF.Switch(switch.DATA.Source)
                  data.path.tmp <- data.source.switch(switch.DATA.Source)
                  if(file.exists(data.path.tmp))
                  {
                    data.path <- data.path.tmp
                    print(paste("[訊息] 資料源切換模式 :", switch.DATA.Source))
                  }else{
                    switch.DATA.Source <- TF.Switch(switch.DATA.Source)
                    print(paste("[錯誤] 資料源不存在，目前模式 :", switch.DATA.Source))
                  }
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
            CCL ={
                    while(TRUE)
                    {
                      .price <- as.numeric(readline("CUSTOM.CreateLONG.price :"))
                      if(.price >0 || is.na(.price)){break}
                    }
                    .path <-extra.data(name="CUSTOM.CREATE.LONG", p.mode = "path")
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
                  },
            CCS ={
                    while(TRUE)
                    {
                      .price <- as.numeric(readline("CUSTOM.CreateSHORT.price :"))
                      if(.price >0 || is.na(.price)){break}
                    }
                    .path <-extra.data(name="CUSTOM.CREATE.SHORT", p.mode = "path")
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
                  },
   
            BMA5 ={
                    .path <-extra.data(name="MA5.CREATE.LONG", p.mode = "path")
                    .price <- extra.data(name="MA5")
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
                  },
            BMA5_ ={
                    .path <-extra.data(name="MA5.CREATE.LONG", p.mode = "path")
                    .price <- 5
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
                  },
            BMA10 ={
                    .path <-extra.data(name="MA10.CREATE.LONG", p.mode = "path")
                    .price <- extra.data(name="MA10")
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
                  },
            BMA10_ ={
                    .path <-extra.data(name="MA10.CREATE.LONG", p.mode = "path")
                    .price <- 10
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
            },
            BMA20 ={
                    .path <-extra.data(name="MA20.CREATE.LONG", p.mode = "path")
                    .price <- extra.data(name="MA20")
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
            },
   
            BMA20_ ={
                    .path <-extra.data(name="MA20.CREATE.LONG", p.mode = "path")
                    .price <- 20
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
            },
            SMA5 ={
                    .path <-extra.data(name="MA5.CREATE.SHORT", p.mode = "path")
                    .price <- extra.data(name="MA5")
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
            },
            SMA5_ ={
                    .path <-extra.data(name="MA5.CREATE.SHORT", p.mode = "path")
                    .price <- 5
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
            },
            SMA10 ={
                    .path <-extra.data(name="MA10.CREATE.SHORT", p.mode = "path")
                    .price <- extra.data(name="MA10")
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
            },
            SMA10_ ={
                    .path <-extra.data(name="MA10.CREATE.SHORT", p.mode = "path")
                    .price <- 10
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
            },
            SMA20 ={
                    .path <-extra.data(name="MA20.CREATE.SHORT", p.mode = "path")
                    .price <- extra.data(name="MA20")
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
            },
       
            SMA20_ ={
                    .path <-extra.data(name="MA20.CREATE.SHORT", p.mode = "path")
                    .price <- 20
                    unlink(.path)
                    append.to.file(data=.price
                                   , path=.path)
            },
            DMSU ={
                    file.create(DMSS.path)
            },
            DAGS ={
                    file.create(DAGS.path)
                    beep(sound = 2)
              
                  },
            RAGS ={
                    file.create(RAGS.path)
                    beep(sound = 2)
              
                  },
            SSPM ={
                    while(TRUE)
                    {
                      result <- as.numeric(readline("Switch STOP.PORT MA(0/5/10):"))
                      if(result ==0 ||result ==5 || result ==10)
                      {
                        switch.stopPORT <-result
                        .path <-extra.data(name="switch_to.ma", p.mode = "path")
                        append.to.file(data=switch.stopPORT
                                       , path=.path)
                        break
                      }else{print("Wrong Param.MA(0/5/10)")}                     
                    }

                    
                  },
      
            SSPR ={
              while(TRUE)
              {
                result <- as.numeric(readline("Switch STOP.PORT RSI(20<X<=45):"))
                if(result >25 && result <45)
                {
                  switch.stopPORT.RSI <-result
                  .path <-extra.data(name="switch_to.rsi", p.mode = "path")
                  append.to.file(data=switch.stopPORT.RSI
                                 , path=.path)
                  break
                }else{print("Wrong Param.(20<X<=45)")}                     
              }
              
              
            },            
            QQ ={break},
            
            result <- readline(paste0("Command is not correct. [", action, "]"))
            
            
      )
    }
  
  result <- ""
}

