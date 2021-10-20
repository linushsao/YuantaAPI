#### AGENT ####
Position.AGENT<-function()
{
  create.price <-0
  MatchBUFFER <-0
  for.LONG <- 1
  for.SHORT <- -1 
  Buyin <- "B"
  Sellout <- "S"
  msg.North.start <- -1
  msg.South.start <- 1
  .stopLOSS.price.LONG <- 0
  .stopLOSS.price.SHORT <-0
  .stopPORT.price.LONG <- 0
  .stopPORT.price.SHORT <-0
  alarm.msg <-NULL
  Price.curr.PRE <-0
  Price.culster.limited <- c(rep(0, 15))
  switch.create.positionLONG  <-FALSE
  switch.create.positionSHORT <-FALSE
  ma.all <- c(5,10,20)
  ENABLE.ByMA <-FALSE
  CROSS.Stop.PORT.LINE <-0
  stop.PORT.MAPrice <-0
  
  Research_Line_Upper <- 0
  Research_Line_lower <- 0
  extremes_Line_Upper <- 0
  extremes_Line_lower <- 0
  Bolling_Line_upper <- 0
  Bolling_Line_lower <- 0
  
  path.create.positionLONG  <- extra.data(name="create.positionLONG", p.mode = "path")
  path.create.positionSHORT <- extra.data(name="create.positionSHORT", p.mode = "path")
  path.closeALLPosition     <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
  path.price.Buyin     <- extra.data(name="price.Buyin", p.mode = "path") 
  path.price.PCL     <- extra.data(name="price.PCL", p.mode = "path") 
 
  path.MA5.CREATE.LONG <- extra.data(name="MA5.CREATE.LONG", p.mode = "path") 
  path.MA10.CREATE.LONG <- extra.data(name="MA10.CREATE.LONG", p.mode = "path") 
  path.MA20.CREATE.LONG <- extra.data(name="MA20.CREATE.LONG", p.mode = "path") 
  path.MA5.CREATE.SHORT <- extra.data(name="MA5.CREATE.SHORT", p.mode = "path") 
  path.MA10.CREATE.SHORT <- extra.data(name="MA10.CREATE.SHORT", p.mode = "path") 
  path.MA20.CREATE.SHORT <- extra.data(name="MA20.CREATE.SHORT", p.mode = "path") 
    
  # DAGS.path <- extra.data(name="DAGS", p.mode = "path") 
  
  unlink(path.closeALLPosition)
  unlink(path.price.Buyin)
  unlink(path.price.PCL)
  unlink(path.create.positionLONG)
  unlink(path.create.positionSHORT)
  unlink(RAGS.path)
  
  repeat
  {
    action <-NULL
    while(TRUE)
    {
      #產生MENU
      print(paste0("MatchBUFFER        : ", MatchBUFFER))  
      print(paste0("Simulation         : ", simu))       
      # 
      print(" ")
      
      print("(E)ENABLE.AGENT.Server")
      print("(SMB)SWITCH.MAtchBUFFER")
      # print("(PF)Price Freq. ")
      # print("(VS)Verbose SWITCH ") 
      # print("(DS)DATA Generator ")   
      # print("(FT)gen FromTIME(XX:XX:XX) ")
      # print("(RE)RESET ") 
      
      #
      action <- readline("[COMMAND] :")
      if(action =="E" || action =="QQ"){break}
      
      switch (action,
        SMB = { MatchBUFFER <-as.numeric(readline("NEW MatchBUFFER :"))}
      )
    }
 
  if(action =="QQ"){break}#回到上層
    
  while(action == "E")
  {

    #計算目前狀況
    if(file.exists(DAGS.path))
    {
      unlink(DAGS.path)
      break
    }
    
    #目前價位
    Price.curr <- as.numeric(Price.current())
    # Price.curr <- extra.data(name="CL")
    # Price.open <- extra.data(name="OP")
    # Price.high <- extra.data(name="HI")
    # Price.low <- extra.data(name="LO")
    
    # if(switch.DATA.Source)
    # {
      Research_Line_Upper <- extra.data(name="Research_Line_Upper")
      Research_Line_lower <- extra.data(name="Research_Line_lower")
      extremes_Line_Upper <- extra.data(name="extremes_Line_Upper")
      extremes_Line_lower <- extra.data(name="extremes_Line_lower")
      Bolling_Line_upper <- extra.data(name="B_UP")
      Bolling_Line_lower <- extra.data(name="B_LO")
      price.ma5 <- extra.data(name="MA5") 
      price.ma10 <- extra.data(name="MA10") 
      price.ma20 <- extra.data(name="MA20") 
      currentbar.num <- extra.data(name="currentBar") 
      switch.stopPORT_ <-extra.data(name="switch_to.ma")
      
      
    # }else{
    #   #儘量不用
    #   Research_Line_Upper <- max(c(ifelse(Research_Line_Upper ==0, Price.curr, Research_Line_Upper), Price.curr.PRE))
    #   Research_Line_lower <- min(c(ifelse(Research_Line_lower ==0, Price.curr, Research_Line_lower), Price.curr.PRE))
    #   
    #   Price.culster.limited <- push.pull(Price.culster.limited, p.mode=FALSE)
    #   Price.culster.limited[1] <- Price.curr
    #   extremes_Line_Upper <- max(Price.culster.limited)
    #   extremes_Line_lower <- min(Price.culster.limited)    
    # }
    
    .stopLOSS.price.LONG <- max(c(extremes_Line_lower, Bolling_Line_lower))
    .stopLOSS.price.SHORT <- min(c(extremes_Line_Upper, Bolling_Line_upper)) 
    .stopPORT.price.LONG <- max(c(extremes_Line_Upper, Bolling_Line_upper))
    .stopPORT.price.SHORT <- min(c(extremes_Line_lower, Bolling_Line_lower))  
    
    #計算價格變動
    Price.buyin.PRE <- extra.data(name="price.Buyin")
    Price.PCL <- extra.data(name="price.PCL")
    
    Price.diff <- Price.curr -Price.buyin.PRE
    
    #艦橋指令讀取
    ##已全平倉 
    if(file.exists(path.closeALLPosition) || #緊急平倉
       file.exists(RAGS.path))
    {
      unlink(path.closeALLPosition)
      unlink(path.price.Buyin)
      unlink(path.price.PCL)
      unlink(path.create.positionLONG)
      unlink(path.create.positionSHORT)
      unlink(RAGS.path)
      alarm.msg <-NULL
      switch.create.positionLONG =FALSE
      switch.create.positionSHORT =FALSE
      ENABLE.ByMA <-FALSE
      create.price <-0

    }    
    ##已開多倉
    if(file.exists(path.create.positionLONG))
    {
      unlink(path.create.positionLONG)
      switch.create.positionLONG =TRUE
      alarm.msg <- "CR.PL"
      beep(sound = 2)

    }
    ##已開空倉
    if(file.exists(path.create.positionSHORT))
    {
      unlink(path.create.positionSHORT)
      switch.create.positionSHORT =TRUE
      alarm.msg <- "CR.PS"
      beep(sound = 2)

    } 
    
    ##均線服從建倉
    ###[create.price均線價格]
    if(file.exists(path.MA5.CREATE.LONG)) 
    {
      create.price <- extra.data(name="MA5.CREATE.LONG")
      unlink(path.MA5.CREATE.LONG)
  
      alarm.msg <- paste0("PMAs.5PL.", create.price)
      # print(create.price)
    }
    if(file.exists(path.MA10.CREATE.LONG)) 
    {
      create.price <- extra.data(name="MA10.CREATE.LONG")
      unlink(path.MA10.CREATE.LONG)
      
      alarm.msg <- paste0("PMAs.10PL.", create.price)

    }
    if(file.exists(path.MA20.CREATE.LONG)) 
    {
      create.price <- extra.data(name="MA20.CREATE.LONG")
      unlink(path.MA20.CREATE.LONG)
      
      alarm.msg <- paste0("PMAs.20PL.", create.price)
    }
    if(file.exists(path.MA5.CREATE.SHORT)) 
    {
      create.price <- extra.data(name="MA5.CREATE.SHORT") *-1
      unlink(path.MA5.CREATE.SHORT)
      
      alarm.msg <- paste0("PMAs.-5PL.", create.price)
    }
    if(file.exists(path.MA10.CREATE.SHORT)) 
    {
      create.price <- extra.data(name="MA10.CREATE.SHORT") *-1
      unlink(path.MA10.CREATE.SHORT)
      
      alarm.msg <- paste0("PMAs.-10PL.", create.price)
    }
    if(file.exists(path.MA20.CREATE.SHORT)) 
    {
      create.price <- extra.data(name="MA20.CREATE.SHORT") *-1
      unlink(path.MA20.CREATE.SHORT)
      
      alarm.msg <- paste0("PMAs.-20PL.", create.price)
    }

    ##檢查create.price是否標記為動態均線服從
    if(create.price !=0)
    {
      for(miu in 1:length(ma.all))
      {
        if(abs(create.price) ==ma.all[miu])
        {
          if(ma.all[miu] ==5)
          {
            .sig <-p_n.sig(create.price)
            create.price <-price.ma5 *p_n.sig(create.price)
            alarm.msg <- paste0("PMA.",.sig, ".5PL.", create.price)
            
            
          }
          if(ma.all[miu] ==10)
          {
            .sig <-p_n.sig(create.price)
            create.price <-price.ma10 *p_n.sig(create.price)
            alarm.msg <- paste0("PMA.", .sig, ".10PL.", create.price)
            
          }
          if(ma.all[miu] ==20)
          {
            .sig <-p_n.sig(create.price)
            create.price <-price.ma20 *p_n.sig(create.price)
            alarm.msg <- paste0("PMA.", .sig, ".20PL.", create.price)
            
          }
          
        }
      }
      
      #AGENT FUNCTION
      ##
      #檢查建倉條件
      if(create.price >0 && !switch.create.positionLONG)
      {
        if(Price.curr <=create.price+MatchBUFFER && !ENABLE.ByMA)
        {
          ENABLE.ByMA <-TRUE
          beep(sound = 2)
          print(paste("[設定] 待命多頭均線服從建倉，價位 :", Price.curr))
        }
        else if(Price.curr >create.price+MatchBUFFER && ENABLE.ByMA)
        {
          #Qty <-1 
          BorS <- "B"
          Price <- Price.current()
          result <- Place.OrderMKT()
          transaction <-account.info(code=result)
          print(paste("回傳結果 :", result, ">>", transaction))
          
          # Price.buyin <- as.numeric(Price)
          Price.buyin <- as.numeric(account.info(info =transaction
                                                 , name = "price" ))
          PCL <- -1
          print(paste("[動作] 多頭均線服從建倉，價位 :", Price.curr))
          
          beep(sound = 2)
          
          # if(check.if.deal())
          # {  
            .path <- extra.data(name="price.Buyin", p.mode = "path")
            .PCL.path <- extra.data(name="price.PCL", p.mode = "path")
            .msg.path <- extra.data(name="create.positionLONG", p.mode = "path")
            
            unlink(.path)
            append.to.file(data=Price.buyin
                           , path=.path)
            append.to.file(data=PCL
                           , path=.PCL.path)
            file.create(.msg.path)
            
            create.price <-0
            ENABLE.ByMA <-FALSE
            switch.create.positionLONG <-TRUE
          }
        # }
      }
      
      if(create.price <0 && !switch.create.positionSHORT)
      {
        if(Price.curr >=abs(create.price)-MatchBUFFER && !ENABLE.ByMA)
        {
          ENABLE.ByMA <-TRUE
          beep(sound = 2)
          print(paste("[設定] 待命空頭均線服從建倉，價位 :", Price.curr))
          
        }
        else if(Price.curr <abs(create.price)-MatchBUFFER && ENABLE.ByMA)
        {
          #Qty <-1
          BorS <- "S"
          Price <- Price.current()
          result <- Place.OrderMKT()
          transaction <-account.info(code=result)
          print(paste("回傳結果 :", result, ">>", transaction))
          
          # Price.buyin <- as.numeric(Price)
          Price.buyin <- as.numeric(account.info(info =transaction
                                                 , name = "price" ))
          PCL <- -1
          print(paste("[動作] 空頭均線服從建倉，價位 :", Price.curr))
          
          beep(sound = 2)
          
          # if(check.if.deal())
          # {
            .path <- extra.data(name="price.Buyin", p.mode = "path")
            .PCL.path <- extra.data(name="price.PCL", p.mode = "path")
            .msg.path <- extra.data(name="create.positionSHORT", p.mode = "path")
            
            unlink(.path)
            append.to.file(data=Price.buyin
                           , path=.path)
            append.to.file(data=PCL
                           , path=.PCL.path)
            file.create(.msg.path)
            
            create.price <-0
            ENABLE.ByMA <-FALSE  
            switch.create.positionSHORT <-TRUE
          }

        # }
      }
    }
    
    ##已建倉，檢查平倉條件
    ###出場均線設定
    if(switch.stopPORT_ ==0){stop.PORT.MAPrice =0} #不考慮均線
    if(switch.stopPORT_ ==5){stop.PORT.MAPrice =price.ma5}
    if(switch.stopPORT_ ==10){stop.PORT.MAPrice =price.ma10}
    
    if(switch.create.positionLONG && 
       Price.PCL ==for.LONG )
    {
      #檢查是否停損
      if(Price.curr <.stopLOSS.price.LONG)
      {
        msg.file  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
        file.create(msg.file)
        result <- ClosePositionAll()
        print(paste("回傳結果 :", result, ">>", transaction))
        
        print(paste("[動作] 執行多頭停損價位 :", Price.buyin.PRE, ">", Price.curr
                    , switch.create.positionLONG
                    , Price.PCL
                    , Price.diff
                    , .stopLOSS.price.LONG))
        beep(sound = 7)
        CROSS.Stop.PORT.LINE <-0
        stop.PORT.MAPrice <-0
                
      }
      
      #檢查是否停利
      if(Price.curr >=.stopPORT.price.LONG && CROSS.Stop.PORT.LINE ==0)
      {
        CROSS.Stop.PORT.LINE <-currentbar.num
      }
      if(
        CROSS.Stop.PORT.LINE !=0 
        &&
        ( (stop.PORT.MAPrice !=0 &&
          Price.curr <=stop.PORT.MAPrice &&
          CROSS.Stop.PORT.LINE !=currentbar.num) ||(stop.PORT.MAPrice ==0) )
        )
      {
        msg.file  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
        file.create(msg.file)
        result <- ClosePositionAll()
        print(paste("回傳結果 :", result, ">>", transaction))
        
        print(paste("[動作] 執行多頭停利價位 :", Price.buyin.PRE, ">", Price.curr
                    , switch.create.positionLONG
                    , Price.PCL
                    , Price.diff
                    , .stopPORT.price.LONG))
        beep(sound = 8)
        CROSS.Stop.PORT.LINE <-0
        stop.PORT.MAPrice <-0     
      }

      
    }
    
    if(switch.create.positionSHORT && 
       Price.PCL ==for.SHORT )
    {
      #檢查是否停損
      if(Price.curr >.stopLOSS.price.SHORT)
      {
        msg.file  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
        file.create(msg.file)
        result <- ClosePositionAll()
        print(paste("回傳結果 :", result, ">>", transaction))
        
        print(paste("[動作] 執行空頭停損價位 :", Price.curr, "<", Price.buyin.PRE
                    , switch.create.positionSHORT
                    , Price.PCL
                    , Price.diff
                    , .stopLOSS.price.SHORT))
        beep(sound = 7)
        CROSS.Stop.PORT.LINE <-0
        stop.PORT.MAPrice <-0
                
      }
      #檢查是否停利
      if(Price.curr <=.stopPORT.price.SHORT && CROSS.Stop.PORT.LINE ==0)
      {
        CROSS.Stop.PORT.LINE <-currentbar.num
      }
      if(
        CROSS.Stop.PORT.LINE !=0 
        &&
        ((stop.PORT.MAPrice !=0 &&
          Price.curr >=stop.PORT.MAPrice &&
          CROSS.Stop.PORT.LINE !=currentbar.num) ||(stop.PORT.MAPrice ==0))
      )
      {
        msg.file  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
        file.create(msg.file)
        result <- ClosePositionAll()
        print(paste("回傳結果 :", result, ">>", transaction))
        
        print(paste("[動作] 執行空頭停利價位 :", Price.curr, "<", Price.buyin.PRE
                    , switch.create.positionSHORT
                    , Price.PCL
                    , Price.diff
                    , .stopPORT.price.LONG))
        beep(sound = 8)
        CROSS.Stop.PORT.LINE <-0
        stop.PORT.MAPrice <-0
                
      }
    }
    
   
    ##

    if(Price.curr.PRE ==0){Price.curr.PRE <-Price.curr}
    
    print(paste("[", alarm.msg, Price.PCL, "]", ifelse(Price.buyin.PRE ==0, 0, Price.diff)
                , Price.buyin.PRE, ">>", Price.curr
                , switch.create.positionLONG, switch.create.positionSHORT
                , "+",.stopLOSS.price.LONG
                , round(ifelse(.stopLOSS.price.LONG ==0,0,.stopLOSS.price.LONG -Price.buyin.PRE), digits = 2)
                , round(ifelse(.stopLOSS.price.SHORT ==0,0,.stopLOSS.price.SHORT -Price.buyin.PRE), digits = 2)
                , .stopLOSS.price.SHORT,"-"
                , "+", .stopPORT.price.LONG, .stopPORT.price.SHORT, "-"
                , "<", Research_Line_Upper, Research_Line_lower
                , extremes_Line_Upper, extremes_Line_lower
                , round(Bolling_Line_upper, digits = 2), round(Bolling_Line_lower, digits = 2), ">"
                , simu, currentbar.num)
                )    
     
    }
  
  }
  
}

