
#### (平倉)動態停損停利 ####
Position.stop<-function()
{
  
  enable.ddm <- FALSE
  for.LONG <- 1
  for.SHORT <- -1
  Price.diff <-0 #漲跌幅
  Price.ddm <-0  #動態停利價
  ddm.Ratio <-0
  ddm.times <-0
  ddm.times.lmited <-3
  ddm.times.keepNOLoss <-0
  ddm.times.keepNOLoss.price <-0
  ddm.times.keepNOLoss.PREprice <-0
  stable.ddm.price <-0
  Price.in <-Price.buyin 
  Price.reachLIMITED.times.Long <-0
  Price.reachLIMITED.times.Short <-0
  Price.RECeiling.PRE <- 0
  Price.REFloor.PRE   <- 0
  Price.curr.PRE <-0
  alarm_NoLOSS <- "  "
  alarm_DDM <- "  "
  alarm_SP <- "  "
  
  REGISTER.ClosePOSITION <- c(TRUE, rep(FALSE, 8))
  CHECK.ClosePOSITION <- c(rep(FALSE, 9))
  
  while(TRUE)
  {
  
    #目前價位
    Price.curr <- as.numeric(Price.current())
    # Price.curr <- extra.data(name="CL")
    # Price.open <- extra.data(name="OP")
    # Price.high <- extra.data(name="HI")
    # Price.low <- extra.data(name="LO")

    #計算價格變動
    Price.diff <- Price.curr -Price.in
 
    ##尋找天花板/地板支撐/壓力
    ##第一次更新極值參數
    if(Price.RECeiling.PRE ==0 ||
       Price.REFloor.PRE ==0)
    {
      Price.RECeiling.PRE <-Price.in
      Price.REFloor.PRE <-Price.in
      
    }else{
          #檢查極值
          Price.RECeiling.diff <- Price.curr -Price.RECeiling.PRE
          Price.REFloor.diff   <- Price.curr -Price.REFloor.PRE
          
          #檢查天花板
          #產生新的天花板
          if(Price.curr >Price.RECeiling.PRE) 
          {
            Price.RECeiling.PRE <- Price.curr #記錄新的天花板
            Price.reachLIMITED.times.Long <-0 #歸零<無法突破天花板之次數>
            alarm_SP <- "^^"
          } 
          #再次回到同樣天花板
          else if(Price.RECeiling.diff ==0 && 
                  Price.curr.PRE <Price.curr) #非重複
          {
            #更新<無法突破天花板>之次數
            Price.reachLIMITED.times.Long <- Price.reachLIMITED.times.Long +1
            alarm_SP <- paste0("^", Price.reachLIMITED.times.Long)
          }
          
          #檢查地板
          #產生新的地板
          if(Price.curr <Price.REFloor.PRE) 
          {
            Price.REFloor.PRE <- Price.curr #記錄新的地板
            Price.reachLIMITED.times.Short <-0 #歸零<無法突破地板之次數>
            alarm_SP <- "VV"
          } 
          #再次回到同樣地板
          else if(Price.curr ==Price.REFloor.PRE && 
                  Price.curr.PRE >Price.curr) #非重複
          {
            #更新<無法突破地板>之次數
            Price.reachLIMITED.times.Short <- Price.reachLIMITED.times.Short +1
            alarm_SP <- paste0("v", Price.reachLIMITED.times.Short)
          }
          
        # }
          
    }
    
    #平倉
    ##檢查停損

     if(PCL ==for.LONG && 
        (
          (Price.curr -Price.in <=default.enable_stopPORTFOLIO*-1)
         ||
         (Price.curr -Price.in <=Stop_portfolio*-1 && 
          Price.RECeiling.PRE <Price.in +BASE_portfolio) #未達基本獲利點
        )
     )
     {
       result <- ClosePositionAll(simu.mode = simu)
       beep(sound = 7)
       print(paste("[動作] 執行多頭停損價位 :", Price.curr, "<", Price.in))
       break #回到主MENU
     }
     if(
       PCL ==for.SHORT && 
       (
          (Price.curr -Price.in >=default.enable_stopPORTFOLIO)
          ||
          (Price.curr -Price.in >=Stop_portfolio && 
           Price.REFloor.PRE >Price.in -BASE_portfolio)
       )
     )
     {
       if.safeClose(bs="MB", simu.mode = simu)
       beep(sound = 7)
       print(paste("[動作] 執行空頭停損價位 :", Price.curr, "<",Price.in))
       break #回到主MENU
     }
 
    #是否開啟系統停利功能
    if(enable.defaultPORT.check)
    {
      
      Stop_PORTFOLIO.price.RSI <-as.numeric(extra.data(name="RSI"))

      #手動平倉
      msg.close.ALLPOSITION  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
      if(file.exists(msg.close.ALLPOSITION))
      {
        unlink(msg.close.ALLPOSITION)
        
        beep(sound = 2)
        print(paste("[動作] 執行手動平倉價位 :", Price.curr, Price.diff))
        break
        
      }
      
      ##固定停利
      # 7~15 保本法
      # 15~  回檔法
      # -10<price<3 特別停損
      # 其他停損 <-15
      if(enable.STABLE.Stop.PORT)
      {

        #平倉啟動條件檢查
        
         ##保本檢查
        if(ddm.times.keepNOLoss.price ==0)
        {
          if(PCL ==for.LONG && 
             Price.diff >=BASE_portfolio) #做多達保本價位
          {
            ddm.times.keepNOLoss.price <- Price.curr
            print(paste("[設定] 啟動多頭保本停利點，價位 :", ddm.times.keepNOLoss.price))
            alarm_NoLOSS <- "NL"
          }
          if(PCL ==for.SHORT && 
             Price.diff <=BASE_portfolio *-1) #做空達保本價位
          {
            ddm.times.keepNOLoss.price <- Price.curr
            print(paste("[設定] 啟動空頭保本停利點，價位 :", ddm.times.keepNOLoss.price))
            alarm_NoLOSS <- "nl"
          } 
        }
        
        #標記固定停利起始點，設定兩次回檔現制
        if(ddm.times ==0)
        {
          if(PCL ==for.LONG && 
             Price.diff >=default.enable_stopPORTFOLIO) #做多平倉
          {
            stable.ddm.price <- Price.curr
            ddm.times =ddm.times.lmited
            alarm_DDM <- "MD"
          }
          if(PCL ==for.SHORT && 
             Price.diff <=default.enable_stopPORTFOLIO *-1) #做空平倉
          {
            stable.ddm.price <- Price.curr
            ddm.times =ddm.times.lmited
            alarm_DDM <- "md"
          }          
        }
        
        #平倉檢查
        ##檢查保本平倉
        if(ddm.times.keepNOLoss.price !=0)
        {
          #創新高
          if( (PCL ==for.LONG && 
               Price.curr >ddm.times.keepNOLoss.price &&
               Price.curr <(Price.in +Stop_portfolio)) #多倉
              ||
              (PCL ==for.SHORT && 
               Price.curr <ddm.times.keepNOLoss.price &&
               Price.curr >(Price.in -Stop_portfolio)) ) #空倉
          {
            ddm.times.keepNOLoss.price <- Price.curr #更新價位
            ddm.times.keepNOLoss <-0 #歸零計數器
            print(paste("[設定] 更新保本停利點，價位 :", ddm.times.keepNOLoss.price))
          }else{
          
                  #回檔
                  if( (PCL ==for.LONG && Price.curr <ddm.times.keepNOLoss.price) #
                      ||
                      (PCL ==for.SHORT && Price.curr >ddm.times.keepNOLoss.price) ) #
                  {
                    #首次紀錄回檔價
                    if(ddm.times.keepNOLoss.PREprice ==0)
                    {
                      ddm.times.keepNOLoss.PREprice =Price.curr
                    }
                    #更新回檔價
                    else if(
                            (PCL ==for.LONG && Price.curr <ddm.times.keepNOLoss.PREprice)
                            ||
                            (PCL ==for.SHORT && Price.curr >ddm.times.keepNOLoss.PREprice)
                            )
                          {
                            ddm.times.keepNOLoss.PREprice =Price.curr
                            ddm.times.keepNOLoss =ddm.times.keepNOLoss +1
                          }
                    
                    .ratio <- 1/abs(ddm.times.keepNOLoss.price -Price.curr)
                    ALL.ddm.times.keepNOLoss <-BASE_portfolio *(1+ .ratio)*Keep.NOLOSS.ratio
                    Price.diff.ddm <- Price.curr -ddm.times.keepNOLoss.price
                    
                    if(( (ddm.times.keepNOLoss >=ALL.ddm.times.keepNOLoss)
                       ||
                       (PCL ==for.LONG && Price.diff.ddm <=BASE_portfolio*-1) ||
                       (PCL ==for.SHORT && Price.diff.ddm >=BASE_portfolio) )
                       &&
                       Price.curr != Price.curr.PRE
                       )
                    {
                      if(PCL ==for.LONG)
                      {
                        if.safeClose(BorS="S", Price, Qty, Daytrade, simu.mode=simu)
                        beep(sound = 8)
                        print(paste("[動作] 執行多頭保本停利價位 :", Price.curr, Price.diff, "<",ddm.times.keepNOLoss, "/",ALL.ddm.times.keepNOLoss,">"))
                        break
                      }
                      if(PCL ==for.SHORT)
                      {
                        if.safeClose(BorS="B", Price, Qty, Daytrade, simu.mode=simu)
                        beep(sound = 8)
                        print(paste("[動作] 執行空頭保本停利價位 :", Price.curr, Price.diff, "<",ddm.times.keepNOLoss, "/",ALL.ddm.times.keepNOLoss,">"))
                        break
                      }
                    }else
                    {
                      print(paste("[設定] 更新保本回檔係數 :", "<",ddm.times.keepNOLoss, "/",ALL.ddm.times.keepNOLoss,">", Price.curr))
                      
                    }
                  }
               } 
        }
        
        ##檢查回檔平倉
        if(ddm.times !=0)
        {
          #創新高
          if( (PCL ==for.LONG && Price.curr >stable.ddm.price) #多倉
              ||
              (PCL ==for.SHORT && Price.curr <stable.ddm.price) ) #空倉
          {
            stable.ddm.price <- Price.curr
            ddm.times =ddm.times.lmited
            print(paste("[設定] 更新固定停利點，重設回檔係數 :", stable.ddm.price, ddm.times))
          
          #回檔  
          }else if( ( (PCL ==for.LONG && Price.curr <stable.ddm.price) #
                  ||
                  (PCL ==for.SHORT && Price.curr >stable.ddm.price) )
                  &&
                  Price.curr != Price.curr.PRE ) #
                  {
            
                    Price.diff.ddm <- Price.curr -stable.ddm.price
                    ddm.times =ddm.times-1
                    if( (ddm.times <=0) ||
                       (PCL ==for.LONG && Price.diff.ddm <=BASE_portfolio*-1) ||
                       (PCL ==for.SHORT && Price.diff.ddm >=BASE_portfolio) )
                    {
                      if(PCL ==for.LONG)
                      {
                        if.safeClose(BorS="S", Price, Qty, Daytrade, simu.mode=simu)
                        beep(sound = 8)
                        print(paste("[動作] 執行多頭回檔停利價位 :", Price.curr, ddm.times))
                        break
                      }
                      if(PCL ==for.SHORT)
                      {
                        if.safeClose(BorS="B", Price, Qty, Daytrade, simu.mode=simu)
                        beep(sound = 8)
                        print(paste("[動作] 執行空頭回檔停利價位 :", Price.curr, ddm.times))
                        break
                      }
                    }else
                    {
                      print(paste("[設定] 更新回檔係數 :", ddm.times, Price.curr))
                      
                    }
                  }
        }
        
        #壓力支撐區形成平倉
          if( (PCL ==for.LONG && 
               Price.diff >=default.enable_stopPORTFOLIO &&
               Price.reachLIMITED.times.Long >Price.reachLIMITED.times.Limited) #多倉
              ||
              (PCL ==for.SHORT && 
               Price.diff <=default.enable_stopPORTFOLIO *-1 &&
               Price.reachLIMITED.times.Short >Price.reachLIMITED.times.Limited) ) #空倉
          {
              if(PCL ==for.LONG)
              {
                if.safeClose(BorS="S", Price, Qty, Daytrade, simu.mode=simu)
                beep(sound = 8)
                print(paste("[動作] 執行多頭撐壓停利價位 :", Price.curr, Price.diff, ddm.times))
                break
              }
              if(PCL ==for.SHORT)
              {
                if.safeClose(BorS="B", Price, Qty, Daytrade, simu.mode=simu)
                beep(sound = 8)
                print(paste("[動作] 執行空頭撐壓停利價位 :", Price.curr, Price.diff, ddm.times))
                break
              }
            }
          }

      } 
      

    # }
    Price.curr.PRE =Price.curr
    
    print(paste("[",alarm_SP, "/", Price.reachLIMITED.times.Limited 
                ,alarm_NoLOSS, alarm_DDM, simu, PCL,"]"
                , Price.diff, ":",Price.in, ">>", Price.curr
                ,"+",Price.RECeiling.PRE, Price.REFloor.PRE, "-", Stop_portfolio
                , ddm.Ratio, Max.DDM, round(Stop_PORTFOLIO.price.RSI, digits = 3)
                , "<", enable.defaultPORT.check, ">",enable.STABLE.Stop.PORT, Stop_loss.code))
    
    # Sys.sleep(0.10)      
  }
  
}

### 接收XQ訊號執行交易 ###
remoted.ByMsg <-function()
{
  
  #紀錄ACTION
  Create_LongPosition  <-  11 #多建倉代號
  Close_LongPosition   <-  10 #多平倉代號
  Create_ShortPosition <- -11 #空建倉代號
  Close_ShortPosition  <- -10 #空平倉代號
  TO_StandBy <- 99 #返回選單
  array_msg <- "_remote_action.csv"
  array_path <- paste0(msg.path, array_msg)
  unlink(array_path)
  
  while(TRUE)
  {
    math.trading <- 0
    get.onOpen <- QueryOnOpen()
    Price <- as.numeric(Price.current())
    
    get.file <-file.exists(array_path)
    print(paste("get.file :", get.file))
    
    if (get.file)
    {
      math.trading = as.numeric(system2(paste0(ExecPath,'tail.exe'),  args = paste0(" -n", 1, " ", array_path), stdout = TRUE))
      print(paste("math.trading :", math.trading))
      unlink(array_path)
      
      if (math.trading !=0) 
      { #產生交易訊號
        
        if (math.trading ==Create_LongPosition )
        { #下多單_
          BorS <- "B"
          
          #做多下限價單
          result <- Place.OrderLMT()
          beep(sound = 2)
          #break
        }
        
        if (math.trading ==Create_ShortPosition )
        { #下空單_
          BorS <- "S"
          
          #做空下限價單
          result <- Place.OrderLMT()
          beep(sound = 2)
          #break
        }
        
        if ((math.trading ==Close_LongPosition 
             || math.trading ==Close_ShortPosition))
        { #有倉位_平倉
          result <- ClosePositionAll()
          beep(sound = 2)
          #break
        }
        
        if (math.trading ==TO_StandBy )
        { 
          break
        }
        
      }          
      
    }
    
    print(paste("[待命中] 目前價位 :", Price))
    Sys.sleep(0.2)
    
  }
  
}
