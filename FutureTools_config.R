# # Order mudule base
# rm(list=ls())
# library("beepr")
# setwd("C:/Temp/")
# 
# #### 設定額外函式位置 #### 
# 
# source("C:/Users/linus/Documents/Project/6.APITols/Order_module_base.R")
# source("C:/Users/linus/Documents/Project/6.APITols/Order_module_custom.R")
# source("C:/Users/linus/Documents/Project/6.APITols/Order_module_POSITION.R")
# source("C:/Users/linus/Documents/Project/6.APITols/Order_module_SIMUServer.R")
# source("C:/Users/linus/Documents/Project/6.APITols/Order_module_AGENTServer.R")
# source("C:/Users/linus/Documents/Project/6.APITols/m_libs.R")
# 
# ##
msg.path <- "C:/Temp/"
price.path <- "C:/Temp/msg/"
realdata.path <- "C:/Users/linus/Documents/Project/9.Shared.Data/8.forSmartAPI/"

##
switch.check.if.deal <-FALSE
transaction <-NULL #交易結果訊息向量
MXFSIMU.Name <- "MXFSIMU"
MXFSIMU.file <- filename.gen(name=MXFSIMU.Name)
MXFSIMU.data.path <- paste0(msg.path, "/", MXFSIMU.Name, "/_Match.txt")
MXFSIMU.forSERVER.filename <- paste0(msg.path, filename.gen(x="log"))

##
Product <-"MXFJ1"
Price <-0
BorS <- "" #買(B)或賣(S)
Daytrade <-"1" #設定當沖(否1是0)
switch.stopPORT <-5
.path <-extra.data(name="switch_to.ma", p.mode = "path")
append.to.file(data=switch.stopPORT
               , path=.path)

DateFolder <- ""
result <- "  "
Qty <-1
gear <-0
BASE_portfolio <- 3  #無虧損停利價差
Stop_portfolio <- 10 #動態停利價差
default.enable_stopPORTFOLIO <- 15 #固定停利價差
Max.DDM <- 0
default.PORTFOLIO.buffer <-5
Keep.NOLOSS.ratio <-2
PCL <- 0 #多空代號 1 -1
Price.buyin <- 0
simu <-TRUE
Auto.positionCLOSE <-FALSE
enable.STABLE.Stop.PORT <-TRUE #停利功能>>預設非動態停利
enable.defaultPORT.check <-TRUE #開啟停利功能

TRENDMark.LONG <-FALSE
TRENDMark.SHORT <-FALSE

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
data.path <- data.source.switch(switch.DATA.Source)

enable.STABLE.Stop.PORT.path  <- extra.data(name="enable.STABLE.Stop.PORT", p.mode = "path") #default固定停利
enable.onlyMDD.path  <- extra.data(name="enable.onlyMDD", p.mode = "path") #MDD停利
enable.RSI.TrendADDED.path  <- extra.data(name="enable.RSI.TrendADDED", p.mode = "path") #RSI超買超賣停利
enable.Bolling.path  <- extra.data(name="enable.BollingPATH.ADDED", p.mode = "path") #布林通道停利
DMSS.path  <- extra.data(name="DMSS", p.mode = "path") #停止虛擬資料伺服器
DAGS.path  <- extra.data(name="DAGS", p.mode = "path") #停止代理人伺服器
RAGS.path  <- extra.data(name="RESET_AGENT.SERVERE", p.mode = "path") #重設代理人伺服器
