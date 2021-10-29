
#<<無連線導致交易錯誤訊息>>
#限價或市價買進
# [1] "Period(TXFL7)  B/S(Buy/Sell) Price Qty LMT/MKT ROD/IOC/FOK Daytrade:0/NoDaytrade:1"
# [2] "Ex:"                                                                               
# [3] "## Order.exe TXFL7 B 11000 1 LMT ROD 1"                                            
# [4] "## Order.exe TXFL7 B 0 1 MKT IOC 1"                                                
# [5] "## Order.exe Delete KeyNo"                                                         
# [6] "Delete KeyNo"                                                                      
# attr(,"status")

# > QueryOrder(" ")
# [1] "KeyNo or ALL"
# attr(,"status")
# [1] 1
# > QueryOrder("")
# [1] "KeyNo or ALL"
# attr(,"status")
# [1] 1
# > QueryOrder("AA")
# [1] "請開啟Smart API"

#<<有連線錯誤訊息>>
# > QueryOrder("")
# [1] "KeyNo or ALL"
# attr(,"status")
# [1] 1
# > QueryOrder(" ")
# [1] "KeyNo or ALL"
# attr(,"status")
# [1] 1
# > QueryOrder("AA")
# [1] "Nodata"

# [1] "[動作] 切換模擬/真實設定"
# [1] "[  0 ] 0 0 >> 16992  + 17056.99 16928 - + 16942.71 17052.99 -  真實 813"
# Error in if (extremes_Line_Upper < Bolling_Line_upper) { :
#     需要 TRUE/FALSE 值的地方有缺值
#   Warning messages:
#     1: package 'roxygen2' was built under R version 4.0.5
#   2: package 'beepr' was built under R version 4.0.5
#   3: In scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :
#                輸入連結 'C:/Temp/msg/_b_upper.csv' 中的輸入不正確
#              4: In Position.AGENT() : 強制變更過程中產生了 NA

# [1] "[ PMA.1.5PL.17038.4 0 ] 0 0 >> 17043  + 17092.32 17006 - + 17009.78 17090.16 -  真實 810"
# [1] "[ PMA.1.5PL.17042.2 0 ] 0 0 >> 17044  + 17092.32 17006 - + 17009.78 17090.16 -  真實 810"
# [1] "[設定] 待命多頭均線服從建倉，價位 : 17043"
# [1] "[ PMA.1.5PL.17042.2 0 ] 0 0 >> 17043  + 17092.32 17006 - + 17009.78 17090.16 -  真實 810"
# Error in if (abs(create.price) == ma.all[miu] || abs(create.price.dynamic) ==  :
#              需要 TRUE/FALSE 值的地方有缺值
#              Warning messages:
#              1: In scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :
#                         輸入連結 'C:/Temp/msg/_ma5.csv' 中的輸入不正確
#                         2: In Position.AGENT() : 強制變更過程中產生了 NA

# [COMMAND] :4
# Error in if (!is.null(code) & !is.na(code)) { : 引 數長度為零

# [COMMAND] :6
# Error in if (!is.null(code) & !is.na(code)) { : 引 數長度為零

# [COMMAND] :0
# [1] "交易序號回傳 : MaydaySuccess"
# [1] "回傳結果(全平倉) : MaydaySuccess"

# > QueryOnOpen()
# [1] "MXFK1,限買,17082,1MXFK1,市賣,17057,1MXFK1,限買,17040,1MXFK1,市賣,17032,1MXFK1,限買,17030,1MXFK1,多,17030,1"
# > QueryOnOpen()
# [1] "MXFK1,限買,17082,1MXFK1,市賣,17057,1MXFK1,限買,17040,1MXFK1,市賣,17032,1MXFK1,限買,17030,1MXFK1,多,17030,1"

# > a <-QueryOnOpen()
# > a
# [1] "MXFK1,限賣,17004,1MXFK1,市賣,17003,1MXFK1,限買,17030,1MXFK1,市賣,17032,1MXFK1,限買,17082
# ,1MXFK1,市賣,17057,1MXFK1,限買,17040,1MXFK1,市買,17033,1MXFK1,空,0,0"

# m_msg(paste("[", alarm.msg, Price.PCL, "]", ifelse(Price.buyin.PRE ==0, 0, Price.diff)
            
# [1] "[ 下午 12:16:56 ] [  0 ] 0 0 >> 16943  + 16970 16933.47 - + 16934.23 16960.43 -  模擬 871"
# [1] "[ 下午 12:16:57 ] [  0 ] 0 0 >> 16944  + 16970 16933.58 - + 16934.29 16960.42 -  模擬 871"
# [1] "[ 下午 12:16:58 ] [  0 ] 0 0 >> 16944  + 16970 16933.58 - + 16934.29 16960.42 -  模擬 871"
# [1] "[ 下午 12:16:59 ] [訊息] 已執行建倉但未確認結果"
# [1] "[ 下午 12:17:00 ] [訊息] 交易成功，執行後續設定"
# [1] "[ 下午 12:17:00 ] 交易序號回傳 :2021102900170479S6EO，交易結果 :全部成交"
# [1] "[ 下午 12:17:00 ] [  -1 ] 0 0 >> 16944  + 16970 16933.69 - + 16934.34 16960.41 -  模擬 871"
# [1] "[ 下午 12:17:02 ] [  NA ] NA NA >> 16945  + 16970 16933.69 - + 16934.34 16960.41 -  模擬 871"
# [1] "[ 下午 12:17:02 ] [  NA ] NA NA >> 16944  + 16970 16933.69 - + 16934.34 16960.41 -  模擬 871"

# [1] "(SCK) ShortCUT KET MAP"
# [1] "(CNT) CONNECTION TEST"
# [COMMAND] :7
# Error in if (PCL == for.LONG && ((Price.curr - Price.in <= default.enable_stopPORTFOLIO *  :
#                                   需要 TRUE/FALSE 值的地方有缺值
#                                   Warning messages:
#                                   1: In Position.stop() : 強制變更過程中產生了 NA
#                                   2: In Position.stop() : 強制變更過程中產生了 NA