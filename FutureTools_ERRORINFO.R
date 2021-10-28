
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


