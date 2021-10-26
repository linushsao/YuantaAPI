
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

