#source("C:/Users/linus/Documents/Project/6.APITols/FutureTools_LITE.R")
#
action <- readline("remove all obj&value? (y/N)")
if(action != "") {rm(list=ls())}


#
library("beepr")
library("sound")
library("audio")
library("magrittr")
setwd("C:/Temp/")

##
msg.path <- "C:/Temp/"
price.path <- "C:/Temp/msg/"
sound.path <-"C:/Temp/wav/"
realdata.path <- "C:/Users/linus/Documents/Project/9.Shared.Data/8.forSmartAPI/"
extra.lib.path <-"C:/Users/linus/Documents/Project/1.R/"
dataset.name <-"Futures.Tools"
##
##Analysis.of.trading.strategies
source(paste0(extra.lib.path, "Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/R/m_misc.R"))
source(paste0(extra.lib.path, "Analysis.of.trading.strategies/ExtraPackages/linus/stock.Analyze/R/m_env.R"))
#### 設定額外函式位置 #### 
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_base.R")
source("C:/Users/linus/Documents/Project/6.APITols/Order_module_custom.R")
source("C:/Users/linus/Documents/Project/6.APITols/FutureTools_DataMGR.R")
source("C:/Users/linus/Documents/Project/6.APITols/FutureTools_config.R")

##
##

action <- readline("交易資料重設? (y/N)")
if(action != "")
{
  m_msg("[設定] 交易資料回復系統預設值...")
  PT.data.reset()
}


shortcut.key <-function()
{
  print("")
  print("")
  
  Price <- Price.current()
  
  print(paste0("-----assign( c(O, I, C, R), MA )-----"))
  
  print(paste0("DataTIME           : ", date.format)) 
  print(paste0("Product            : ", ifelse(switch.DATA.Source, Product, MXFSIMU.Name) )) 
  print(paste0("Price              : ", Price)) 
  print(paste0("Quantity           : ", Qty)) 
  print(paste0("BorS               : ", BorS))
  print(paste0("MODE.XFSource      : ", trans.lang(mode="SECURTIES", switch.DATA.Source)))  
  print(paste0("PATH.XFSource      : ", data.source.switch(get.conf(name="switch.DATA.Source"
                                                                    , dataset = dataset.name))))  
  
  print("")
  
  print("-----COMMON FUNCTION-----")
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
  print("(SPT)StopPORT.TYPE")
  print("(SLT)StopLOSS.TYPE")
  print("(DT)_switch_DayTRADE")  
  print("(PRB)Price.buyin")
  print("(PCL)PCL")
  print("(SPUT)S&P Unbreaked times")
  print("(APC)switch_Auto.pos.CLOSE")
  print("(SDP)switch_defaultPORT") 
  print("(SDP)switch_defaultPORT")
  print("(SS)switch_Simulation") 
  print("(RSS)REMOTE switch_Simulation")
  print("(EPPT)EXPORT PTConf")
  print("(SSPM)SWITCH StopPORT.MA")
  print("(SSPR)SWITCH StopPORT.RSI")
  print("")
  
  print("-----AGENT.SERVER FUNCTION-----")
  print("(EAS)ENABLE_AGENT.SERVER") 
  print("(DAGS)DISABLE_AGENT.SERVER") 
  print("(RAGS)RESET_AGENT.SERVER") 
  print("(RSS)REMOTE.SWITCH.SIMU_AGENT.SERVER") 
  print("")
  
  print("-----PORTFOLIO.MINITOR FUNCTION-----")
  print("(EPM)ENABLE_PORTFOLIO.MINITOR") 
  print("")
  
  print("-----MXFSIMU.SERVER FUNCTION-----")
  print("(EMSS)ENABLE_MXFSIMU.SERVER") 
  print("(DMSS)DISABLE_MXFSIMU.SERVER")
  print("(SMS)SWITCH MFXSource")
  print("")
  
  action <- readline("[COMMAND] :")
  return(action)
}

#測試連線結果
m.action <-readline(paste("[是否測試卷商連線品質...[y/N]"))
if(m.action =="Y" | m.action =="y")
{
  if(connect.test())
  {
    check.result <-(paste("[訊息] 聯線正常."))
  }else{
    check.result <- (paste("[錯誤] 無法建立聯線."))
  }
  
  action <- readline(COMMON.ANYKEY.TO.EXIST) 
}


##主程式
#ChangeProd()

repeat
{
  
  Price <- Price.current()
  print(" ")
  print(" ")
  
  print(paste0("-----assign( c(O, I, C, R), MA )-----"))
  print(paste0("DataTIME           : ", date.format)) 
  print(paste0("Product            : ", ifelse(switch.DATA.Source, Product, MXFSIMU.Name) )) 
  print(paste0("Price              : ", Price)) 
  print(paste0("Quantity           : ", Qty)) 
  print(paste0("BorS               : ", BorS))
  
  print(paste0("STABLE S.P.        : ", default.enable_stopPORTFOLIO)) 
  print(paste0("AUTO.S.P.          : ", Stop_portfolio)) 
  print(paste0("Auto.pos.CLOSE     : ", Auto.positionCLOSE))
  print(paste0("Max.DDM            : ", Max.DDM))
  print(paste0("DayTRADE           : ", Daytrade))
  print(paste0("Price.buyin        : ", Price.buyin))
  print(paste0("PCL                : ", PCL)) 
  print(paste0("switch.stopPORT.MA : ", switch.stopPORT))
  print(paste0("switch.stopPORT.RSI: ", switch.stopPORT_RSI))
  
  print(paste0("S&P Unbreaked time : ", Price.reachLIMITED.times.Limited))
  print(paste0("MODE.XFSource      : ", trans.lang(mode="SECURTIES", switch.DATA.Source))) 
  print(paste0("PATH.XFSource      : ", data.source.switch(get.conf(name="switch.DATA.Source"
                                                                    , dataset = dataset.name))))  
  print(paste0("Simulation         : ", simu))  
  
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
           SCK ={next.step <-shortcut.key()},
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
             msg.file  <- set.conf(name="close.ALLPOSITION", value = "", dataset = dataset.name)
             # file.create(msg.file)
             OrderNO <- ClosePositionAll()
             print(paste("交易序號回傳 :", OrderNO))
             
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
             
             # #匯出交易序號
             # m_env(name="OrderNO", value =OrderNO, dataset =dataset.name)
             # # append.to.file(data = OrderNO, path = extra.data(name = "OrderNO", p.mode = "path"), m.append = FALSE)             
             # #匯出交易PCL
             # m_env(name="price.PCL", value =BorS2PCL(BorS), dataset =dataset.name)
             # # append.to.file(data = BorS2PCL(BorS), path = extra.data(name = "price.PCL", p.mode = "path"), m.append = FALSE) 
             # 
             m.act <-readline(paste0("交易序號回傳 :", OrderNO, " <Press Any Key pls.>"))
             
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
             
             # #匯出交易序號
             # m_env(name="OrderNO", value =OrderNO, dataset =dataset.name)
             # # append.to.file(data = OrderNO, path = extra.data(name = "OrderNO", p.mode = "path"), m.append = FALSE)             
             # #匯出交易PCL
             # append.to.file(data = BorS2PCL(BorS), path = extra.data(name = "price.PCL", p.mode = "path"), m.append = FALSE) 
             # 
             m.act <-readline(paste0("交易序號回傳 :", OrderNO, " <Press Any Key pls.>"))
             
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
             
             # #匯出交易序號
             # append.to.file(data = OrderNO, path = extra.data(name = "OrderNO", p.mode = "path"), m.append = FALSE)             
             # #匯出交易PCL
             # append.to.file(data = BorS2PCL(BorS), path = extra.data(name = "price.PCL", p.mode = "path"), m.append = FALSE) 
             # 
             m.act <-readline(paste0("交易序號回傳 :", OrderNO, " <Press Any Key pls.>"))
             
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
             
             # #匯出交易序號
             # append.to.file(data = OrderNO, path = extra.data(name = "OrderNO", p.mode = "path"), m.append = FALSE)             
             # #匯出交易PCL
             # append.to.file(data = BorS2PCL(BorS), path = extra.data(name = "price.PCL", p.mode = "path"), m.append = FALSE) 
             # 
             m.act <-readline(paste0("交易序號回傳 :", OrderNO, " <Press Any Key pls.>"))
             
             if(Auto.positionCLOSE)
             {
               next.step <- "7"
             } 
             
           },
           
           "7" ={
             # {
             source("C:/Users/linus/Documents/Project/6.APITols/Order_module_POSITION.R")
             # }
             
             Position.stop()
             rm(Position.stop)
             Price.buyin <-0
             PCL <-0
           },
           
           "9" ={
             # {
             source("C:/Users/linus/Documents/Project/6.APITols/FutureTools_StopPORTFOLIO.R")
             # }
             
             stop.Portfolio.lite()
             rm(stop.Portfolio.lite)
   
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
           PPS ={Position.polar_star()},
           PMC ={Position.multi.create()},
           
           DESP ={default.enable_stopPORTFOLIO <- as.numeric(readline("default.enable_stopPORTFOLIO bundle :"))},
           
           PRB ={Price.buyin <- as.numeric(readline("Price.buyin bundle :"))},
           PCL ={PCL <- as.numeric(readline("PCL bundle(1/-1):"))},
           APC ={Auto.positionCLOSE <-TF.Switch(Auto.positionCLOSE)
           # if(Auto.positionCLOSE){Auto.positionCLOSE <-FALSE}
           # else{Auto.positionCLOSE <-TRUE}
           },
           SS ={simu <-TF.Switch(simu)
           # if(simu){simu <-FALSE}
           # else{simu <-TRUE}
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
               #讀取未平倉資料
               .portfolio.current <-portfolio.monitor()
               .title <-paste0("[訊息 ", .portfolio.current[6],"]") 
               
               if(.portfolio.current[2] ==0)
               {
                 print(paste0(.title, " 待命中，目前無倉位."))
               }else{
                 .bors <-.portfolio.current[1]
                 .price.Buyin <- as.numeric(.portfolio.current[2])
                 .price.current <- as.numeric(Price.current())
                 .price.portfolio <- as.numeric(.portfolio.current[5])
                 
                 m_msg(paste0(.title, " 浮動損益 : ", .price.portfolio
                              , " ",as.numeric(.price.current -.price.Buyin), " "
                              ,.price.Buyin, " >>",.price.current))
               }
               
               #關閉顯示
               # if(file.exists(EPM.path)) 
               if(!is.null(get.conf(name="REMOTE_SWITCH_PORTFOLIO.MONITOR", dataset = dataset.name)))
               {
                 # unlink(EPM.path)
                 rm.conf(name="REMOTE_SWITCH_PORTFOLIO.MONITOR", dataset = dataset.name)
                 beep(sound = 2)
                 m.action <- readline(paste0("[動作] 跳出訊息視窗，press any key pls..."))
                 break
               } 
               
               
             }
             
             
           },
           
           RSS ={
             
             REMOTE.SWITCH.SIMULATION <- TF.Switch(REMOTE.SWITCH.SIMULATION)
             
             set.conf(name="REMOTE.SWITCH.SIMULATION"
                      , value =as.character(REMOTE.SWITCH.SIMULATION)
                      , dataset =dataset.name)
             
           },
           # m.REMOTE.SWITCH.SIMULATION <-get.conf(name = "REMOTE.SWITCH.SP.SIMULATION", dataset = dataset.name)
           RSSS ={
             m.value <-set.conf(name = "REMOTE.SWITCH.SP.SIMULATION"
                                , value = "FOO.BAR" 
                                , dataset = dataset.name)
           },
           MSGL ={
             msg.lite <- TF.Switch(msg.lite)
             # .path <-extra.data(name="msg.lite", p.mode = "path")
             print(paste("[NEW VALUE] msg.lite :", msg.lite))
             # .price <- as.character(msg.lite)
             # unlink(.path)
             
             set.conf(name="msg.lite", value =as.character(msg.lite), dataset =dataset.name)
             # append.to.file(data=.price
             #                , path=.path)
             result <- readline(paste0("[NEW VALUE] msg.lite :", msg.lite, " ，PLS. PRESS ANY KEY to continue..."))
             
           },
           
           EPPT ={
             
             m.price <-readline("Price :")
             m.pcl   <-readline("PCL   :")
             m.action <-readline(paste0("Price :", m.price, " & PCL:", m.pcl," ready for export,(y/N)"))
             
             if(m.action =="Y" || m.action =="y")
             {
               set.conf(name="price.Buyin", value =m.price, dataset =dataset.name)
               set.conf(name="price.PCL", value =m.pcl, dataset =dataset.name)
               if(as.numeric(m.pcl) ==1) 
               {
                 set.conf(name="switch.create.positionLONG", value ="TRUE", dataset =dataset.name)
                 set.conf(name="switch.create.positionSHORT", value ="FALSE", dataset =dataset.name)
               }
               if(as.numeric(m.pcl) ==-1)
               {
                 set.conf(name="switch.create.positionLONG", value ="FALSE", dataset =dataset.name)
                 set.conf(name="switch.create.positionSHORT", value ="TRUE", dataset =dataset.name)
               }
               
             }
           },
           EMSS ={
             source("C:/Users/linus/Documents/Project/6.APITols/Order_module_SIMUServer.R")
             SIMU.DATA.Server()
             rm(SIMU.DATA.Server)
           },
           DMSS ={
             set.conf(name="DMSS", value ="", dataset =dataset.name)
           },
           DSPL ={
             set.conf(name="DSPL", value ="", dataset =dataset.name)
           },
           EAS ={
             
             source("C:/Users/linus/Documents/Project/6.APITols/Order_module_AGENTServer.R")
             Position.AGENT()
             rm(Position.AGENT)
           },
           DCL ={
             while(TRUE)
             {
               .price <- readline("Del Count Limited :")
               if(.price >0 || is.na(.price)){break}
             }
             set.conf(name="del.count.limited", value =.price, dataset =dataset.name)
             
             # .path <-extra.data(name="del.count.limited", p.mode = "path")
             # append.to.file(data=.price
             #                , path=.path
             #                , m.append = FALSE)
           },
           SPUT ={
             Price.reachLIMITED.times.Limited <-as.numeric(readline("S&P Unbreaked time :"))
           },
           SMS ={
             
             #切換至卷商即時盤中價位資料路徑或虛擬資料伺服器
             switch.DATA.Source <-TF.Switch(switch.DATA.Source)
             data.path.tmp <- data.source.switch(switch.DATA.Source)
             
             if(if.Valid.file(data.path.tmp))
             {
               data.path <- data.path.tmp
               print(paste("[訊息] 資料源切換模式 :", switch.DATA.Source, data.path))
             }else{
               
               print(paste(MXFSIMU.SOURCE.UNAVILABLE, switch.DATA.Source, data.path))
               
               switch.DATA.Source <-FALSE
               data.path <- data.source.switch(switch.DATA.Source)
               print(paste(MXFSIMU.SOURCE.AUTO.SWITCH, switch.DATA.Source, data.path))
               
             } 
             
             set.conf(name="switch.DATA.Source"
                      , value = switch.DATA.Source
                      , dataset = dataset.name)
             
             m.action <- readline(COMMON.ANYKEY.TO.EXIST)
             
             
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
           
           CCL ={
             while(TRUE)
             {
               .price <- as.numeric(readline("CUSTOM.CreateLONG.price :"))
               if(.price >0 || is.na(.price)){break}
             }
             set.conf(name="CUSTOM.CREATE.LONG", value =.price, dataset =dataset.name)
             # .path <-extra.data(name="CUSTOM.CREATE.LONG", p.mode = "path")
             # unlink(.path)
             # append.to.file(data=.price
             #                , path=.path)
           },
           CCS ={
             while(TRUE)
             {
               .price <- as.numeric(readline("CUSTOM.CreateSHORT.price :"))
               if(.price >0 || is.na(.price)){break}
             }
             set.conf(name="CUSTOM.CREATE.SHORT", value =.price, dataset =dataset.name)
             # .path <-extra.data(name="CUSTOM.CREATE.SHORT", p.mode = "path")
             # unlink(.path)
             # append.to.file(data=.price
             #                , path=.path)
           },
           
           BMA5 ={
             .price <- extra.data(name="MA5")
             set.conf(name="MA5.CREATE.LONG", value =.price, dataset =dataset.name)
             # .path <-extra.data(name="MA5.CREATE.LONG", p.mode = "path")
             # .price <- extra.data(name="MA5")
             # unlink(.path)
             # append.to.file(data=.price
             #                , path=.path)
           },
           BMA5_ ={
             .price <- 5
             set.conf(name="MA5.CREATE.LONG", value =.price, dataset =dataset.name)
             # .path <-extra.data(name="MA5.CREATE.LONG", p.mode = "path")
             # .price <- 5
             # unlink(.path)
             # append.to.file(data=.price
             #                , path=.path)
           },
           BMA10 ={
             .price <- extra.data(name="MA10")
             set.conf(name="MA10.CREATE.LONG", value =.price, dataset =dataset.name)
             # .path <-extra.data(name="MA10.CREATE.LONG", p.mode = "path")
             # .price <- extra.data(name="MA10")
             # unlink(.path)
             # append.to.file(data=.price
             #                , path=.path)
           },
           BMA10_ ={
             .price <- 10
             set.conf(name="MA10.CREATE.LONG", value =.price, dataset =dataset.name)
             # .path <-extra.data(name="MA10.CREATE.LONG", p.mode = "path")
             # .price <- 10
             # unlink(.path)
             # append.to.file(data=.price
             #                , path=.path)
           },
           BMA20 ={
             # .path <-extra.data(name="MA20.CREATE.LONG", p.mode = "path")
             .price <- extra.data(name="MA20")
             set.conf(name="MA20.CREATE.LONG", value =.price, dataset =dataset.name)
             
             # unlink(.path)
             # append.to.file(data=.price
             #                , path=.path)
           },
           
           BMA20_ ={
             # .path <-extra.data(name="MA20.CREATE.LONG", p.mode = "path")
             .price <- 20
             # unlink(.path)
             # append.to.file(data=.price
             #                , path=.path)
             set.conf(name="MA20.CREATE.LONG", value =.price, dataset =dataset.name)
             
           },
           SMA5 ={
             # .path <-extra.data(name="MA5.CREATE.SHORT", p.mode = "path")
             .price <- extra.data(name="MA5")
             # unlink(.path)
             # append.to.file(data=.price
             #                , path=.path)
             set.conf(name="MA5.CREATE.SHORT", value =.price, dataset =dataset.name)
             
           },
           SMA5_ ={
             # .path <-extra.data(name="MA5.CREATE.SHORT", p.mode = "path")
             .price <- 5
             # unlink(.path)
             # append.to.file(data=.price
             #                , path=.path)
             set.conf(name="MA5.CREATE.SHORT", value =.price, dataset =dataset.name)
             
           },
           SMA10 ={
             # .path <-extra.data(name="MA10.CREATE.SHORT", p.mode = "path")
             .price <- extra.data(name="MA10")
             # unlink(.path)
             # append.to.file(data=.price
             #                , path=.path)
             set.conf(name="MA10.CREATE.SHORT", value =.price, dataset =dataset.name)
             
           },
           SMA10_ ={
             # .path <-extra.data(name="MA10.CREATE.SHORT", p.mode = "path")
             .price <- 10
             # unlink(.path)
             # append.to.file(data=.price
             #                , path=.path)
             set.conf(name="MA10.CREATE.SHORT", value =.price, dataset =dataset.name)
             
           },
           SMA20 ={
             # .path <-extra.data(name="MA20.CREATE.SHORT", p.mode = "path")
             .price <- extra.data(name="MA20")
             # unlink(.path)
             # append.to.file(data=.price
             #                , path=.path)
             set.conf(name="MA20.CREATE.SHORT", value =.price, dataset =dataset.name)
             
           },
           
           SMA20_ ={
             # .path <-extra.data(name="MA20.CREATE.SHORT", p.mode = "path")
             .price <- 20
             # unlink(.path)
             # append.to.file(data=.price
             #                , path=.path)
             set.conf(name="MA20.CREATE.SHORT", value =.price, dataset =dataset.name)
             
           },
           
           DAGS ={
             set.conf(name="DAGS", value ="", dataset =dataset.name)
             
             # file.create(DAGS.path)
             beep(sound = 2)
             
           },
           RAGS ={
             set.conf(name="RESET_AGENT.SERVERE", value ="", dataset =dataset.name)
             
             # file.create(RAGS.path)
             beep(sound = 2)
             
           },
           SSPM ={
             while(TRUE)
             {
               result <- as.numeric(readline("Switch STOP.PORT MA(0/5/10):"))
               if(result ==0 ||result ==5 || result ==10)
               {
                 switch.stopPORT <-result
                 # .path <-extra.data(name="switch_to.ma", p.mode = "path")
                 # append.to.file(data=switch.stopPORT
                 #                , path=.path)
                 set.conf(name="switch_to.ma", value =switch.stopPORT, dataset =dataset.name)
                 
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
                 # .path <-extra.data(name="switch_to.rsi", p.mode = "path")
                 # append.to.file(data=switch.stopPORT.RSI
                 #                , path=.path)
                 set.conf(name="switch_to.rsi", value =switch.stopPORT.RSI, dataset =dataset.name)
                 
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

