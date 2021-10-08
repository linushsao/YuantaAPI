執行檔使用方式

取帳務 : GetAccount.exe  委託序號 or ALL(全部帳務)
下單   : Order.exe  ProdNo('TXFL7','option,TXO07700L7')  B/S  price  Qty LMT/MKT  ROD/IOC/FOK   daytrade : 1/0(當沖為1，非當沖為0)
刪單   : Order.exe  Delete + 委託序號
權益數 : FutureRights.exe
未平倉 : OnOpenInterest.exe
緊急平倉 : MayDay.exe
取得所有未成交單 : GetUnfinished.exe
取消所有未成交單 : CancelALL.exe
GetOrderPrice.exe  與 Order.exe  若不加參數直接執行，則會顯示使用說明
FutureRights.exe、 MayDay.exe、GetUnfinished.exe、CancelALL.exe 與 OnOpenInterest.exe 直接執行即可
只要下單機主機開啟後執行即可