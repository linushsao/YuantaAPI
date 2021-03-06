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
CODE.MXFSIMU <- "MXFSIMU"

transaction <-NULL #交易結果訊息向量
transaction.name <-c("OrderNO", "Status", "Product", "BorS", "Price"
                     , "Amount", "Time", "Company", "Account", "Exchange"
                     , "OrderSeries", "BeforeChanged", "AfterChanged", "DealNO", "SubAccount"
                     , "Salesperson", "Commission")

ANSWER.POSITION.AFTER.CONNECT <-8
LENGTH.COLLECT.ANSWER <-17

#COMMON INFO
COMMON.ANYKEY.TO.EXIST <- "press any key pls..."

#ANSWER INFO
CONNECTED.ANSWER.BorS.WrongPARAM <- "Delete KeyNo"
CONNECTED.ANSWER.RightPARAM.NoDATA <-"Nodata"

CONNECTED.ANSWER.BorS.ALLDeal <-"全部成交"
CONNECTED.ANSWER.BorS.SOMEDeal <-"部分成交"
CONNECTED.ANSWER.BorS.ALLCancel <-"全部取消"

UNCONNECTED.ANSWER.RightPARAM <- "請開啟Smart API"

COMMON.ANSWER.EmptyPARAM <- "KeyNo or ALL"

#MXFSIMU
MXFSIMU.Name <- "MXFSIMU"
MXFSIMU.file <- filename.gen(name=MXFSIMU.Name)
MXFSIMU.data.path <- paste0(msg.path, "/", MXFSIMU.Name, "/_Match.txt")
MXFSIMU.forSERVER.filename <- paste0(msg.path, filename.gen(x="log"))

##MXFSIMU ERROR INFO
MXFSIMU.SOURCE.UNAVILABLE<-paste0("[錯誤] 虛擬資料伺服器 :", "原始卷商資料源路徑錯誤或檔案空白")
MXFSIMU.SOURCE.AUTO.SWITCH  <-paste0("[設定] 虛擬資料伺服器 :", "自動切換至虛擬卷商")
MXFSIMU.SOURCE.REDIFINE  <-paste0("[錯誤] 虛擬資料伺服器 :", "從最近日期回頭尋找可替代之卷商資料源")

#
switch.stopPORT <-ifelse(
            is.null(get.conf(name="switch_to.ma", dataset = dataset.name))
            , 5
            ,get.conf(name="switch_to.ma", dataset = dataset.name))#MA5出場
  
switch.stopPORT_RSI <-ifelse(
            is.null(get.conf(name="switch_to.rsi", dataset = dataset.name))
            , 40
            ,get.conf(name="switch_to.rsi", dataset = dataset.name))#RSI出場

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

#設定預設資料源<證卷商>T表示證卷商
switch.DATA.Source <-ifelse(
                    is.null(get.conf(name="switch.DATA.Source", dataset = dataset.name))
                    , TRUE
                    , as.logical(get.conf(name="switch.DATA.Source", dataset = dataset.name))) 
data.path <- data.source.switch(switch.DATA.Source)

REMOTE.SWITCH.SIMULATION <-simu

meta.record.name <-c(
  "date"
  , "time"
  , "barInterval"
  , "open"
  , "high"
  , "low"
  , "close"
  , "currentBar"
  
  , "ma5"
  , "ma10"
  , "ma20"
  
  , "RL.Upper"
  , "RL.Mid"
  , "RL.lower"
  , "EL.Upper"
  , "EL.Mid"
  , "EL.lower"
  , "b.upper"
  , "b.lower"
  , "RSI"
  , "BSRate"
  
  , "north.star"
  , "south.star"
  , "NS.price"
  , "NS.stopLoss"
  , "SS.price"
  , "SS.stopLoss"
  , "OMA.L20.10"
  , "OMA.S20.10"
  , "OMA.L20.5"
  , "OMA.S20.5"
)

meta.leng <-length(meta.record.name)
meta.No <-c(1:meta.leng)
names(meta.No) <-meta.record.name
