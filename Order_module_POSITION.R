
#### (建倉)多重策略 ####
Position.multi.create<-function()
{
  
  for.LONG <- "B"
  for.SHORT <- "S"
  msg.North.start <- -1
  msg.South.start <- 1
  local.msg <-""
  action <- as.numeric(readline("建倉條件[RSI.Switch(1)極星(2)] :"))
  cond.enable <-FALSE
  
  while(TRUE)
  {
    
    #讀取現價  
    # Price <- extra.data(name="CL")  
    Price <- as.numeric(Price.current())
    #讀取建倉條件
    #RSI法
    if(action ==1 && !cond.enable)
    {
      polar_star_switch <-extra.data(name="polar_star_switch")
      enable.north.star <-(polar_star_switch <0)
      enable.south.star <-(polar_star_switch >0)
      
      cond.enable <-(enable.north.star || enable.south.star)
      local.msg <- paste("<RSI.Switch建倉法>", price.RSI)
    }
    
    #極星法
    if(action ==2 && !cond.enable)
    {
      local.msg <- "<極星建倉法>"
      #極星類型
      code.polar_star <- extra.data(name="ploar_star")
      #極星建倉點
      price.polar_star <- extra.data(name="ploar_star_price")
      
      enable.north.star <- (code.polar_star ==msg.North.start &&
                              Price >price.polar_star)
      enable.south.star <- (code.polar_star ==msg.South.start &&
                              Price <price.polar_star)
      cond.enable <-(enable.north.star || enable.south.star)
    }
    
    #判斷是否啟動建倉
    if (cond.enable)
    {    
      
      # 設定建倉
      if (enable.north.star){BorS =for.SHORT} 
      if (enable.south.star){BorS =for.LONG} 
      
      #執行建倉
      # BorS <- "B"
      Price <- Price.current()
      result <- Place.OrderLMT()
      beep(sound = 5)
      Price.buyin <- as.numeric(Price)
      PCL <- ifelse(BorS ==for.LONG, msg.South.start, msg.North.start)
      print(paste("[動作] 執行建倉價位 :", Price, BorS))
      
      if(Auto.positionCLOSE)
      {
        Sys.sleep(1)
        beep(sound = 2)
        Position.stop()
        Price.buyin <-0
        PCL <-0
      }  
      break
      
    }
    
    #
    print(paste("[待命中] :", Price, local.msg))
    Sys.sleep(0.20) 
    
  }  
  
}

#### (平倉)動態停損停利 ####
Position.stop<-function()
{
  
  enable.ddm <- FALSE
  for.LONG <- 1
  for.SHORT <- -1
  Price.diff <-0 #漲跌幅
  Price.ddm <-0  #動態停利價
  Stop_loss.price <-0
  ddm.Ratio <-0
  ddm.times <-0
  stable.ddm.price <-0
  Price.in <-Price.buyin 
  # p.mode.switch <- p.mode
  
  REGISTER.ClosePOSITION <- c(TRUE, rep(FALSE, 8))
  CHECK.ClosePOSITION <- c(rep(FALSE, 9))
  
  while(TRUE)
  {
    
    # #查詢是否有部位
    # get.onOpen <- length(QueryOnOpen())
    # 
    # if (get.onOpen ==0)
    # {
    #   print("[回報] 倉位數量應大於零，請確認")
    #   break
    # }
    
    #目前價位
    Price.curr <- as.numeric(Price.current())
    # Price.curr <- extra.data(name="CL")
    Price.open <- extra.data(name="OP")
    
    #計算價格變動
    Price.diff <- Price.curr -Price.in
    
    #檢查是否達到動態停利條件 
    #已設定動態停利且還沒啟動即執行
    # if(Max.DDM !=0 && !enable.ddm)
    if(!enable.STABLE.Stop.PORT && !enable.ddm)
    {
      if((PCL ==for.LONG && Price.diff >=Stop_portfolio) ||
         (PCL ==for.LONG && Price.curr -Price.open >=default.enable_stopPORTFOLIO) ||
         (PCL ==for.SHORT && Price.diff <=Stop_portfolio*-1) ||
         (PCL ==for.SHORT && Price.curr -Price.open <=default.enable_stopPORTFOLIO*-1))
      {
        enable.ddm <- TRUE #啟動動態停損
        Price.ddm <- Price.curr #設定動態停利計算起始點
        print(paste("[設定] 進入機槍射程範圍，價位 :", Price.curr))
        beep(sound = 5)
      }
    }
    
    #檢查是否啟動停損
    if ( Stop_loss.code ==1)
    {
      
      #RSI停損
      # Stop_loss.price <-0
      operator <- as.character(Stop_loss.code)
      
      switch(operator,
             
             # RSI_RESVERSAL+STABLE.StopPORT
             "1" ={
               Stop_loss.price.RSI <-extra.data(name="RSI")
               Stop_loss.price.RSI_MA5 <-extra.data(name="RSI_MA5")
               
               if(
                 PCL ==for.LONG && 
                 (
                   ( Stop_loss.price.RSI <0 &&
                     Stop_loss.price.RSI_MA5 >0 &&
                     abs(Stop_loss.price.RSI) >abs(Stop_loss.price.RSI_MA5) &&
                     Price.curr -Price.in <=Stop_portfolio*-1 )
                   ||
                   (Price.curr -Price.in <=default.enable_stopPORTFOLIO*-1)
                 )
               )
               {
                 if.safeClose(bs="MS") 
                 beep(sound = 7)
                 print(paste("[動作] 執行多頭停損價位 :", Price.curr, "<", Price.in, operator))
                 break #回到主MENU
               }
               if(
                 PCL ==for.SHORT && 
                 (
                   ( Stop_loss.price.RSI >0 &&
                     Stop_loss.price.RSI_MA5 <0 &&
                     abs(Stop_loss.price.RSI) >abs(Stop_loss.price.RSI_MA5) &&
                     Price.curr -Price.in >=Stop_portfolio )
                   ||
                   (Price.curr -Price.in >=default.enable_stopPORTFOLIO)
                 )
               )
               {
                 if.safeClose(bs="MB")
                 beep(sound = 7)
                 print(paste("[動作] 執行空頭停損價位 :", Price.curr, "<",Price.in, operator))
                 break #回到主MENU
               }
             }            
      )
      
      
      
    }
    
    if ( Stop_loss.code >=2 && Stop_loss.code <=5)
    {
      
      #停損價位
      # Stop_loss.price <-0
      operator <- as.character(Stop_loss.code)
      
      switch(operator,
             
             # Research_Line
             "2" ={
               if(PCL ==for.LONG ){Stop_loss.price <-extra.data(name="Research_Line_lower")} 
               if(PCL ==for.SHORT){Stop_loss.price <-extra.data(name="Research_Line_Upper")} 
             },
             # extremes_Line
             "3" ={
               if(PCL ==for.LONG ){Stop_loss.price <-extra.data(name="extremes_Line_lower")} 
               if(PCL ==for.SHORT){Stop_loss.price <-extra.data(name="extremes_Line_Upper")} 
             },
             # Bolling
             "4" ={
               if(PCL ==for.LONG ){Stop_loss.price <-extra.data(name="B_LO")} 
               if(PCL ==for.SHORT){Stop_loss.price <-extra.data(name="B_UP")} 
             },
             # PolarSTAR
             "5" ={
               Stop_loss.price <-extra.data(name="ploar_star_stopLoss ")
             }
      )
      
      if(PCL ==for.LONG && Price.curr <=Stop_loss.price)
      {
        if.safeClose(bs="MS") 
        beep(sound = 7)
        print(paste("[動作] 執行多頭停損價位 :", Price.curr))
        break #回到主MENU
      }
      if(PCL ==for.SHORT && Price.curr >=Stop_loss.price)
      {
        if.safeClose(bs="MB")
        beep(sound = 7)
        print(paste("[動作] 執行空頭停損價位 :", Price.curr))
        break #回到主MENU
      }
      
    }
    
    #是否開啟系統停利功能
    if(enable.defaultPORT.check)
    {
      
      #檢查是否停利平倉及種類
      #編號1為預設之MDD因此為TRUE
      Stop_PORTFOLIO.price.RSI <-extra.data(name="RSI")
      RSI.OverBOUGHT <- 20
      RSI.OverSOLD <- -20
      BollingPATH.UPPER <-extra.data(name="B_UP")
      BollingPATH.LOWER <-extra.data(name="B_LO")
      
      if(file.exists(enable.STABLE.Stop.PORT.path))
      {
        unlink(enable.STABLE.Stop.PORT.path)
        # p.mode.switch =1
        Max.DDM <- 0
        enable.STABLE.Stop.PORT <-TRUE
        REGISTER.ClosePOSITION <- c(rep(FALSE, 9))
        CHECK.ClosePOSITION <- c(rep(FALSE, 9))
        print(paste("[設定] 切換為預設固定停利，價位 :", Price.curr))
        beep(sound = 2)
      }
      
      if(file.exists(enable.onlyMDD.path))
      {
        unlink(enable.onlyMDD.path)
        # p.mode.switch =1
        Max.DDM <- 5
        enable.STABLE.Stop.PORT <-FALSE
        REGISTER.ClosePOSITION <- c(TRUE, rep(FALSE, 8))
        CHECK.ClosePOSITION <- c(rep(FALSE, 9))
        print(paste("[設定] 切換為動態(預設)onlyMDD停利，價位 :", Price.curr))
        beep(sound = 2)
      }
      
      if(file.exists(enable.RSI.TrendADDED.path))
      {
        unlink(enable.RSI.TrendADDED.path)
        
        if(enable.STABLE.Stop.PORT)
        {
          print(paste("[錯誤] 請先開啟<動態(預設)onlyMDD停利>"))
        }else
        {
          # p.mode.switch <-2
          REGISTER.ClosePOSITION[2] <-TRUE
          print(paste("[設定] 附加RSI.TrendADDED停利，價位 :", Price.curr))
          beep(sound = 2)        
        }
      }
      
      if(file.exists(enable.Bolling.path))
      {
        unlink(enable.Bolling.path)
        if(enable.STABLE.Stop.PORT)
        {
          print(paste("[錯誤] 請先開啟<動態(預設)onlyMDD停利>"))
        }else
        {
        # p.mode.switch <-3
        REGISTER.ClosePOSITION[3] <-TRUE
        print(paste("[設定] 附加BollingPATH.ADDED停利，價位 :", Price.curr))
        beep(sound = 2)
        }
      }
      
      #預設值，MDD停利
      # if(p.mode.switch ==1)
      # {
      #   MDD.ClosePOSITION <-TRUE #預設判斷MDD
      # } 
      # 附加，RSI超買超賣停利
      if(REGISTER.ClosePOSITION[2]) #考慮RSI
      {
        if(
          (PCL ==for.LONG && (Stop_PORTFOLIO.price.RSI >RSI.OverBOUGHT))
          ||
          (PCL ==for.SHORT && (Stop_PORTFOLIO.price.RSI <RSI.OverSOLD))
        )
        {
          CHECK.ClosePOSITION[2] <-TRUE
        }
      }
      #附加，布林通道停利
      if(REGISTER.ClosePOSITION[3]) #考慮布林通道
      {
        if(
          (PCL ==for.LONG && (Price.curr >BollingPATH.UPPER))
          ||
          (PCL ==for.SHORT && (Price.curr <BollingPATH.LOWER))
        )
        {
          CHECK.ClosePOSITION[3] <-TRUE
        }
      }
      
      #附加條件總檢查
      EXTRA.ClosePOSITION <- TRUE
      check_series <- (REGISTER.ClosePOSITION ==CHECK.ClosePOSITION)
      check_leng <- length(check_series)
      mark_REGISTER <- 0
      mark_CHECK <- 0
      for(miu in 1:check_leng)
      {
        #檢查有不符合已啟動條件
        if(check_series[miu] ==FALSE){EXTRA.ClosePOSITION <-FALSE}
        
        if(REGISTER.ClosePOSITION[miu] ==TRUE){mark_REGISTER <-mark_REGISTER+1}
        if(CHECK.ClosePOSITION[miu]    ==TRUE){mark_CHECK <-mark_CHECK+1}
      }
      
      #手動停利
      msg.close.ALLPOSITION  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
      if(file.exists(msg.close.ALLPOSITION))
      {
        unlink(msg.close.ALLPOSITION)
        
        beep(sound = 2)
        print(paste("[動作] 執行手動平倉價位 :", Price.curr, Price.diff))
        break
        
      }
      
      ##無開啟回檔檢查(無動態停利，採固定停利)
      # if(Max.DDM ==0) 
      if(enable.STABLE.Stop.PORT)
      {
        # if Price.diff >Stop_portfolio_RatioPRICE_DIFF
        #標記固定停利起始點，設定兩次回檔現制
        if(ddm.times ==0)
        {
          if(PCL ==for.LONG && 
             Price.diff >=default.enable_stopPORTFOLIO) #做多平倉
          {
            stable.ddm.price <- Price.curr
            ddm.times =2
          }
          if(PCL ==for.SHORT && 
             Price.diff <=default.enable_stopPORTFOLIO *-1) #做空平倉
          {
            stable.ddm.price <- Price.curr
            ddm.times =2
          }          
        }
        
        #檢查平倉
        if(ddm.times !=0)
        {
          #創新高
          if( (PCL ==for.LONG && Price.curr >stable.ddm.price) #多倉
              ||
              (PCL ==for.SHORT && Price.curr <stable.ddm.price) ) #空倉
          {
            stable.ddm.price <- Price.curr
          }
          
          #回檔
          if( (PCL ==for.LONG && Price.curr <stable.ddm.price) #
              ||
              (PCL ==for.SHORT && Price.curr >stable.ddm.price) ) #
          {
            ddm.times =ddm.times-1
            if(ddm.times <=0)
            {
              if(PCL ==for.LONG)
              {
                if.safeClose(bs="S")
                beep(sound = 8)
                print(paste("[動作] 執行多頭停利價位 :", Price.curr))
                break
              }
              if(PCL ==for.SHORT)
              {
                if.safeClose(bs="B")
                beep(sound = 8)
                print(paste("[動作] 執行空頭停利價位 :", Price.curr))
                break
              }
            }
          }
        }
        
        
      }
      
      ##已觸發動態停利
      if(enable.ddm)
      {
        #回檔加成係數
        ddm.Ratio <-0
        #創新高
        if((PCL ==for.LONG && Price.curr >Price.ddm) ||
           (PCL ==for.SHORT && Price.curr <Price.ddm))
        {
          
          CHECK.ClosePOSITION[1] <-TRUE
          
          Price.ddm <-Price.curr
          beep(sound = 2)
          print(paste("[設定] 更新停利點價位 :", Price.ddm))
          
          Stop_portfolio_RatioPRICE_DIFF <-(Stop_portfolio +Max.DDM)
          #以回檔點數換算是否更新DDM動態加成點數
          if(PCL ==for.LONG && Price.ddm -Price.in >=Stop_portfolio_RatioPRICE_DIFF)
          {
            ddm.Ratio = round(abs((Price.curr 
                                   -(Price.in +Stop_portfolio))/Max.DDM)) *-1
            beep(sound = 2)
            print(paste("[設定] 更新多倉停利點加成價位及點數 :", Price.curr, Price.ddm, ddm.Ratio))
          }
          if(PCL ==for.SHORT && Price.ddm -Price.in <=Stop_portfolio_RatioPRICE_DIFF*-1 )
          {
            ddm.Ratio = round(abs((Price.curr 
                                   -(Price.in -Stop_portfolio))/Max.DDM)) *-1
            beep(sound = 2)
            print(paste("[設定] 更新空倉停利點加成價位及點數 :", Price.curr, Price.ddm, ddm.Ratio))
          }
          
        }
        #回檔
        else{ 
          #檢查停利條件
          
          if(EXTRA.ClosePOSITION)
          {
            if((PCL ==for.LONG  && Price.curr -Price.ddm <= (Max.DDM+ddm.Ratio)*-1))
            {
              if.safeClose(bs="S")
              beep(sound = 8)
              print(paste("[動作] 執行多頭停利價位 :", Price.curr, Price.diff))
              break
            }
            if(PCL ==for.SHORT && Price.curr -Price.ddm >= (Max.DDM+ddm.Ratio))
            {
              if.safeClose(bs="B")
              beep(sound = 8)
              print(paste("[動作] 執行空頭停利價位 :", Price.curr, Price.diff))
              break
            }             
          }
        }
      }
    }
    
    #價差/目前/買進價位/停利/停損/回檔/回檔調整係數/ 
    print(paste("[待命中 simu:", simu,PCL,"]"
                , Price.diff, ":",Price.in, ">>", Price.curr, Stop_portfolio, Stop_loss.price
                , ddm.Ratio, Max.DDM, round(Stop_PORTFOLIO.price.RSI, digits = 3)
                , EXTRA.ClosePOSITION, mark_REGISTER, mark_CHECK, "<", enable.defaultPORT.check, ">",enable.STABLE.Stop.PORT, Stop_loss.code))
    
    Sys.sleep(0.10)      
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
