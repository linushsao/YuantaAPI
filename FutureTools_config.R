#全域變數
##
Product <-"MXFK1"
Price <-0
BorS <- "" #買(B)或賣(S)
Qty <-1
Daytrade <-"1" #設定當沖(否1是0)
#
transaction.checkTIMES <-5 #檢查交易結果次數
simu <-TRUE
CODE.SIMU <- "SIMU"

transaction <-NULL #交易結果訊息向量
transaction.name <-c("OrderNO", "Status", "Product", "BorS", "Price"
                     , "Amount", "Time", "Company", "Account", "Exchange"
                     , "OrderSeries", "BeforeChanged", "AfterChanged", "DealNO", "SubAccount"
                     , "Salesperson", "Commission")

ANSWER.POSITION.AFTER.CONNECT <-8
LENGTH.COLLECT.ANSWER <-17

CONNECTED.ANSWER.BorS.WrongPARAM <- "Delete KeyNo"
CONNECTED.ANSWER.RightPARAM.NoDATA <-"Nodata"

CONNECTED.ANSWER.BorS.ALLDeal <-"全部成交"
CONNECTED.ANSWER.BorS.SOMEDeal <-"部分成交"
CONNECTED.ANSWER.BorS.ALLCancel <-"全部取消"

UNCONNECTED.ANSWER.RightPARAM <- "請開啟Smart API"

COMMON.ANSWER.EmptyPARAM <- "KeyNo or ALL"

MXFSIMU.Name <- "MXFSIMU"
MXFSIMU.file <- filename.gen(name=MXFSIMU.Name)
MXFSIMU.data.path <- paste0(msg.path, "/", MXFSIMU.Name, "/_Match.txt")
MXFSIMU.forSERVER.filename <- paste0(msg.path, filename.gen(x="log"))


switch.stopPORT <-5 #MA5出場
switch.stopPORT_RSI <-40 #RSI30出場
.path <-extra.data(name="switch_to.ma", p.mode = "path")
append.to.file(data=switch.stopPORT
               , path=.path)

DateFolder <- ""
result <- "  "
gear <-0
BASE_portfolio <- 10  #基本獲利
Stop_portfolio <- 15 #動態停利價差
default.enable_stopPORTFOLIO <- 15 #固定停利價差
default.enable_stopHIGH.PORTFOLIO <- 25 #固定停利價差

Max.DDM <- 0
default.PORTFOLIO.buffer <-5
Keep.NOLOSS.ratio <-2
PCL <- 0 #多空代號 1 -1
Price.buyin <- 0
Auto.positionCLOSE <-FALSE
enable.STABLE.Stop.PORT <-TRUE #停利功能>>預設非動態停利
enable.defaultPORT.check <-TRUE #開啟停利功能
msg.lite <-TRUE

preWORK.name <-c("CURRENTBAR.ADDED")
preWORK.check <-c(rep(FALSE, length(preWORK.name)))

safe.Close <- TRUE #TRUE表使用緊急平倉來平倉
Stop_portfolio.type <-c("(1)MDD", "(2)RsiOVER_SB", "(3)Bolling")
Stop_portfolio.code <-1
Stop_loss.type <-c("(1)RsiREVERSAL", "(2)ResearchLINE", "(3)ExtremeLINE", "(4)Bolling", "(5)PolarSTAR")
Stop_loss.code <-1
next.step <- ""
Price.reachLIMITED.times.Limited <-2

get.hour <- as.numeric(format(Sys.time(), "%H"))
get.sysDate <-  Sys.Date()
if (get.hour <8){get.sysDate = get.sysDate -1 } 
date.format <- gsub("-", "", get.sysDate)

Product.file <- filename.gen(name=date.format)
SECURTIES.data.path <-finacial.dataparg.gen(realdata.path, date.format, Product, Product.file)

#設定預設資料源<證卷商>
switch.DATA.Source <-TRUE #T表示證卷商
# data.path <- data.source.switch(switch.DATA.Source)
data.path <- SECURTIES.data.path

enable.STABLE.Stop.PORT.path  <- extra.data(name="enable.STABLE.Stop.PORT", p.mode = "path") #default固定停利
enable.onlyMDD.path  <- extra.data(name="enable.onlyMDD", p.mode = "path") #MDD停利
enable.RSI.TrendADDED.path  <- extra.data(name="enable.RSI.TrendADDED", p.mode = "path") #RSI超買超賣停利
enable.Bolling.path  <- extra.data(name="enable.BollingPATH.ADDED", p.mode = "path") #布林通道停利
DMSS.path  <- extra.data(name="DMSS", p.mode = "path") #停止虛擬資料伺服器
DAGS.path  <- extra.data(name="DAGS", p.mode = "path") #停止代理人伺服器
RAGS.path  <- extra.data(name="RESET_AGENT.SERVERE", p.mode = "path") #重設代理人伺服器
RSS.path   <- extra.data(name="REMOTE_SWITCH_SIMULATION", p.mode = "path") #遙控切換模擬/真實
EPM.path   <- extra.data(name="REMOTE_SWITCH_PORTFOLIO.MONITOR", p.mode = "path") #遙控切換<未沖銷期貨浮動損益>訊息視窗

shortcut.key <-function()
{
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
  print("")
  
  # print("(EDPC)enable.default.P.CHECK") 
  # print("(ESSP)enable.stable.S.P.")
  print("")
  
  print("(EAS)ENABLE_AGENT.SERVERE") 
  print("(DAGS)DISABLE_AGENT.SERVERE") 
  print("(RAGS)RESET_AGENT.SERVERE") 
  print("(RSS)REMOTE.SWITCH.SIMU_AGENT.SERVERE") 
  print("")

  print("(EPM)ENABLE_PORTFOLIO.MINITOR") 
  print("")
  
  print("(EMSS)ENABLE_MXFSIMU.SERVERE") 
  print("(DMSU)DISABLE_MXFSIMU.SERVERE") 
  print("")
  
  print("(SSPM)SWITCH StopPORT.MA")
  print("(SSPR)SWITCH StopPORT.RSI")
  print("")
  
  print("(SMS)SWITCH MFXSource")
  print("(SPUT)S&P Unbreaked times")
  print("(APC)switch_Auto.pos.CLOSE")
  print("(SDP)switch_defaultPORT") 
  print("(SDP)switch_defaultPORT")
  print("(SS)switch_Simulation") 
  print("(RSS)REMOTE switch_Simulation")
  print("(EPPT)EXPORT PTConf")
  print("")
  
  
  
  action <- readline("PRESS ANY KEY PLS... :")
  
}
