for.backup <-function()
{
  #### (建平倉)極星法 ####
  Position.polar_star<-function()
  {
    
    if.exist <-FALSE
    if.createPOSITION <-FALSE
    BorS <-""
    for.LONG <- "B"
    for.SHORT <- "S"
    msg.North.start <- -1
    msg.South.start <- 1
    condition.Long.CreatePOSITION <- FALSE
    condition.Short.CreatePOSITION <- FALSE
    code.ploar_star <-0
    code.ploar_star_stopLoss <-0
    # Price.buyin <-0
    
    while(TRUE)
    {
      
      #查詢是否有部位
      get.onOpen <- length(QueryOnOpen())
      #確認目前無部位，才執行建倉檢查
      if (get.onOpen ==0)
      {
        
        price.open <- extra.data(name="OP")
        price.high <- extra.data(name="HI")
        price.close <- extra.data(name="CL")
        price.low <- extra.data(name="LO")
        price.ma5 <- extra.data(name="MA5")
        price.ma10 <- extra.data(name="MA10")
        price.ma20 <- extra.data(name="MA20")
        
        # price.Rate_sma5 <- extra.data(name="Rate_sma5")
        # price.Rate_sma10 <- extra.data(name="Rate_sma10")
        # price.Rate_sma20 <- extra.data(name="Rate_sma20")
        
        code.ploar_star <- extra.data(name="ploar_star")
        code.ploar_star_price <- extra.data(name="ploar_star_price")
        
        #南十字星多建倉訊號
        condition.Long.CreatePOSITION <- code.ploar_star == msg.South.start &&
          (
            (
              price.low <price.ma5 &&
                (price.low +price.close)*0.5 >price.ma5 &&
                price.close >price.open
            ) 
            ||
              #close超過天階_low<ma5
              (
                price.close >code.ploar_star_price &&
                  price.low <price.ma5
                # price.Rate_sma5 *price.Rate_sma10 <0
              )
          )
        #北方之星空建倉訊號
        condition.Short.CreatePOSITION <- code.ploar_star == msg.North.start &&
          (
            ( 
              price.high >price.ma5 &&
                (price.high +price.close)*0.5 <price.ma5 &&
                price.close <price.open
            ) 
            ||
              #close低於天階_high>ma5
              (
                price.close <code.ploar_star_price &&
                  price.high <price.ma5
                # price.Rate_sma5 *price.Rate_sma10 <0  
              )
          )
        
        # #檢查是否建倉
        local.msg <-""
        
        if(condition.Long.CreatePOSITION ||
           condition.Short.CreatePOSITION )
        {
          if (condition.Long.CreatePOSITION){BorS =for.LONG} 
          if (condition.Short.CreatePOSITION){BorS =for.SHORT}  
          Price <- Price.current()
          result <- Place.OrderLMT()
          beep(sound = 2)
          Price.buyin <- as.numeric(Price)
          PCL <- ifelse(BorS ==for.LONG, msg.South.start, msg.North.start)
          # if.createPOSITION <-TRUE
          local.msg <- ifelse(BorS ==for.LONG, "執行多頭建倉", "執行空頭建倉")
        }
        
        # }
        
        #
        print(paste("[待命中] :"
                    , Price, code.ploar_star, code.ploar_star_price, local.msg))
        Sys.sleep(0.20) 
        
      }
      
      #執行建倉後，偵測極星平倉
      if(get.onOpen !=0)
      {
        cond.stopPORTFOLIO <-FALSE
        cond.stopLoss <-FALSE
        createPOSITION.code.ploar_star <- code.ploar_star #紀錄建倉極星編號
        code.ploar_star_stopLoss <- extra.data(name="ploar_star_stopLoss") #紀錄建倉極星停損點
        
        while(TRUE)
        {
          Price <- extra.data(name="CL")
          code.ploar_star <-  extra.data(name="ploar_star")
          
          #停利條件<反向極星出現>
          cond.stopPORTFOLIO  <- (code.ploar_star != createPOSITION.code.ploar_star)
          #停損條件<極星條損點>
          cond.stopLoss       <- ((BorS ==for.LONG || Price <=code.ploar_star_stopLoss) ||
                                    (BorS ==for.SHORT || Price >=code.ploar_star_stopLoss))
          #執行停利停損
          if(cond.stopPORTFOLIO || cond.stopLoss)
          {
            #設定反向平倉
            BorS <- ifelse(createPOSITION.code.ploar_star ==msg.North.start, "B", "S")
            Price <- Price.current()
            if.safeClose(bs=BorS)
            beep(sound = 8)
            print(paste("[動作] 執行平倉價位 :", Price, BorS))
            if.exist <-TRUE
            break 
            
          }else{
            
            print(paste("[待命中] :"
                        , Price, code.ploar_star_stopLoss, "已建倉，偵測平倉條件"))
            Sys.sleep(0.20) 
          }
          
        }       
      }    
      
      if(if.exist){break}
      
    }  
    
  }
  
  
  #### (建倉)多重策略 ####
  Position.multi.create<-function()
  {
    
    if.exist <-FALSE
    if.createPOSITION <-FALSE
    BorS <-""
    for.LONG <- "B"
    for.SHORT <- "S"
    msg.North.start <- -1
    msg.South.start <- 1
    condition.Long.CreatePOSITION <- FALSE
    condition.Short.CreatePOSITION <- FALSE
    code.ploar_star <-0
    code.ploar_star_stopLoss <-0
    # Price.buyin <-0
    
    action <- as.numeric(readline("建倉條件[極星(1)/布林通道()] :"))
    cond.enable <-FALSE
    
    while(TRUE)
    {
      
      #查詢是否有部位
      get.onOpen <- length(QueryOnOpen())
      
      if (get.onOpen !=0)
      {
        print("[回報] 目前已持有部位，請確認")
        break
      }
      
      #讀取現價  
      Price <- extra.data(name="CL")  
      
      #讀取建倉條件
      #極星法
      if(action ==1 && !cond.enable)
      {
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
      
      #判斷是否啟動待建倉模式
      if (cond.enable)
      {    
        
        #南十字星多建倉訊號
        condition.Long.CreatePOSITION <- (code.polar_star ==msg.South.start &&
                                            Price >price.polar_star)       
        #北方之星空建倉訊號
        condition.Short.CreatePOSITION <- (code.polar_star ==msg.North.start &&
                                             Price <price.polar_star)
        
        local.msg <-""
        
        if(condition.Long.CreatePOSITION ||
           condition.Short.CreatePOSITION )
        { #設定建倉
          if (code.ploar_star ==msg.North.start){BorS =for.SHORT} 
          if (code.ploar_star ==msg.South.start){BorS =for.LONG} 
          
          Price <- Price.current()
          if.safeClose(bs=BorS)
          beep(sound = 8)
          Price.buyin <- Price
          PCL <- ifelse(BorS ==for.LONG, msg.South.start, msg.North.start)
          print(paste("[動作] 執行建倉價位 :", Price, BorS))
          break 
        } 
        
      }
      
      #
      print(paste("[待命中] :", Price, local.msg))
      Sys.sleep(0.20) 
      
    }  
    
  }
  
  
  #### (平倉)多重動態停利停損 ####
  Position.multi.stop<-function()
  {
    
    # enable.ddm <- FALSE
    for.LONG <- 1
    for.SHORT <- -1
    msg.North.start <- -1
    msg.South.start <- 1
    # Price.in <- pr #建倉價
    # PCL <- pcl     #多空編號
    Price.diff <-0 #漲跌幅
    
    action <- readline("停利條件[極星(1)/布林通道(2)] :")
    
    while(TRUE)
    {
      
      #查詢是否有部位
      get.onOpen <- length(QueryOnOpen())
      if (get.onOpen ==0)
      {
        print("[回報] 部位數量變為零，請確認")
        break
      }
      
      #目前價位
      Price.curr <- as.numeric(Price.current())
      #計算價格變動
      Price.diff <- Price.curr -Price.in
      
      #極星條件平倉法
      if(action ==1)
      {
        
        code.ploar_star <- extra.data(name="ploar_star")
        
        cond.CloseLong <- (PCL ==for.LONG && code.ploar_star ==msg.North.start)
        cond.CloseShort <- (PCL ==for.SHORT && code.ploar_star ==msg.South.start)
        
        #
        # if(cond.CloseLong || cond.CloseShort)
        # {
        if(cond.CloseLong)
        {
          if.safeClose(bs=for.SHORT)
          beep(sound = 8)
          print(paste("[動作] 執行多頭停利價位 :", Price.curr))
          break            
        }
        if(cond.CloseShort)
        {
          if.safeClose(bs=for.LONG)
          beep(sound = 8)
          print(paste("[動作] 執行空頭停利價位 :", Price.curr))
          break            
        }
        
        # }
      }
      
      #布林平倉法
      if(action ==2) 
      {
        
        price.b_upper <- extra.data(name="B_UP")
        price.b_lower <- extra.data(name="B_LO")
        price.close <- extra.data(name="CL")
        
        cond.CloseLong <- (PCL ==for.LONG && price.close >=price.b_upper)
        cond.CloseShort <- (PCL ==for.SHORT && price.close <=price.b_lower)
        
        #
        # if(cond.CloseLong || cond.CloseShort)
        # {
        if(cond.CloseLong)
        {
          if.safeClose(bs=for.SHORT)
          beep(sound = 8)
          print(paste("[動作] 執行多頭停利價位 :", Price.curr))
          break            
        }
        if(cond.CloseShort)
        {
          if.safeClose(bs=for.LONG)
          beep(sound = 8)
          print(paste("[動作] 執行空頭停利價位 :", Price.curr))
          break            
        }
        
        # }
      }   
      
      print(paste("[待命中] :", Price.curr, action, Price.diff))
      Sys.sleep(0.20)   
      
    }
    # }
    
  }
  
}