#### AGENT ####
Position.AGENT<-function()
{
  create.price <-0
  create.price.dynamic <- 0
  MatchBUFFER <-1
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
  # Price.curr.PRE <-0
  Price.culster.limited <- c(rep(0, 15))
  switch.create.positionLONG  <-FALSE
  switch.create.positionSHORT <-FALSE
  Finished.create.positionLONG <-FALSE
  Finished.create.positionSHORT <-FALSE
  msg.lite_ <-TRUE
  
  ma.all <- c(5,10,20)
  ENABLE.ByMA <-FALSE
  CROSS.Stop.PORT.LINE <-0
  stop.PORT.MAPrice <-0
  stop.PORT.RSIPrice <-0
  switch.stopPORT.pre <-0
  switch.stopPORT_RSI.pre <-0
  
  Research_Line_Upper <- 0
  Research_Line_lower <- 0
  extremes_Line_Upper <- 0
  extremes_Line_lower <- 0
  Bolling_Line_upper <- 0
  Bolling_Line_lower <- 0
  price.rsi <-0
  
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
  path.switch_to.ma <- extra.data(name="switch_to.ma", p.mode = "path") 
  path.switch_to.rsi <- extra.data(name="switch_to.rsi", p.mode = "path") 
  path.msg.lite <- extra.data(name="msg.lite", p.mode = "path") 
  path.CLOSEPositionByMA <- extra.data(name="CLOSEPositionByMA", p.mode = "path") 
  
  unlink(path.closeALLPosition)
  unlink(path.price.Buyin)
  unlink(path.price.PCL)
  unlink(path.create.positionLONG)
  unlink(path.create.positionSHORT)
  unlink(RAGS.path)
  
  INIT.ENABLE <-FALSE
  
  repeat
  {
    action <-NULL
    while(TRUE)
    {
      #產生MENU
      print(paste0("MatchBUFFER        : ", MatchBUFFER)) 
      print(paste0("switch.stopPORT    : ", switch.stopPORT))  
      print(paste0("switch.stopPORT_RSI: ", switch.stopPORT_RSI))  
      
      print(paste0("Simulation         : ", simu))       
      # 
      print(" ")
      
      print("(E)ENABLE.AGENT.Server")
      print("(SMB)SWITCH.MAtchBUFFER")
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

    #檢查停止
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
    price.ma5 <- as.numeric(extra.data(name="MA5")) 
    price.ma10 <- as.numeric(extra.data(name="MA10")) 
    price.ma20 <- as.numeric(extra.data(name="MA20")) 
    price.rsi <- as.numeric(extra.data(name="RSI"))
    
    Research_Line_Upper <- as.numeric(extra.data(name="Research_Line_Upper"))
    Research_Line_lower <- as.numeric(extra.data(name="Research_Line_lower"))
    extremes_Line_Upper <- as.numeric(extra.data(name="extremes_Line_Upper"))
    extremes_Line_lower <- as.numeric(extra.data(name="extremes_Line_lower"))
    Bolling_Line_upper <- as.numeric(extra.data(name="B_UP"))
    Bolling_Line_lower <- as.numeric(extra.data(name="B_LO"))

    currentbar.num <- as.numeric(extra.data(name="currentBar")) 
      
      
    #   #儘量不用
    #   Research_Line_Upper <- max(c(ifelse(Research_Line_Upper ==0, Price.curr, Research_Line_Upper), Price.curr.PRE))
    #   Research_Line_lower <- min(c(ifelse(Research_Line_lower ==0, Price.curr, Research_Line_lower), Price.curr.PRE))
    #   
    #   Price.culster.limited <- push.pull(Price.culster.limited, p.mode=FALSE)
    #   Price.culster.limited[1] <- Price.curr
    #   extremes_Line_Upper <- max(Price.culster.limited)
    #   extremes_Line_lower <- min(Price.culster.limited)    
    
    if(extremes_Line_lower >Bolling_Line_lower)
    {
      .stopLOSS.price.LONG <- (extremes_Line_lower +Bolling_Line_lower)*0.5
    }else{.stopLOSS.price.LONG <- max(c(extremes_Line_lower, Bolling_Line_lower))
    }
    if(extremes_Line_Upper <Bolling_Line_upper)
    {
      .stopLOSS.price.SHORT <- (extremes_Line_Upper +Bolling_Line_upper)*0.5
    }else{.stopLOSS.price.SHORT <- min(c(extremes_Line_Upper, Bolling_Line_upper)) 
    }
    
    .stopPORT.price.LONG <- max(c(extremes_Line_Upper, Bolling_Line_upper))
    .stopPORT.price.SHORT <- min(c(extremes_Line_lower, Bolling_Line_lower))  
    
    #計算價格變動
    Price.buyin.PRE <- as.numeric(extra.data(name="price.Buyin"))
    Price.PCL <- as.numeric(extra.data(name="price.PCL"))
    
    Price.diff <- Price.curr -Price.buyin.PRE
    
    #艦橋指令讀取
    ##已全平倉 
    if(file.exists(path.closeALLPosition) || #緊急平倉
       file.exists(RAGS.path) || 
       !INIT.ENABLE)
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
      create.price.dynamic <-0
      INIT.ENABLE <-TRUE

    }
 
    ##均線強制停利點
    # if(file.exists(path.CLOSEPositionByMA))
    # {
    #   closePositionBY.MA <-as.numeric(extra.data(name="CLOSEPositionByMA"))
    #   unlink(path.CLOSEPositionByMA)
    # 
    #   beep(sound = 2)
    #   print(paste("[設定] 均線強制停利點 :", closePositionBY.MA))
    #   
    # }
    
    ##均線停利點
    if(file.exists(path.switch_to.ma) && 
       switch.stopPORT != switch.stopPORT.pre)
    {
      switch.stopPORT <-as.numeric(extra.data(name="switch_to.ma"))
      switch.stopPORT.pre <-switch.stopPORT
      
      beep(sound = 2)
      print(paste("[設定] 均線停利點重設 :", switch.stopPORT))

    } 
    ##RSI停利點
    if(file.exists(path.switch_to.rsi) &&
       switch.stopPORT_RSI != switch.stopPORT_RSI.pre)
    {
      switch.stopPORT_RSI <-as.numeric(extra.data(name="switch_to.rsi"))
      switch.stopPORT_RSI.pre <-switch.stopPORT_RSI
      
      beep(sound = 2)
      print(paste("[設定] RSI停利點重設 :", switch.stopPORT_RSI))
      
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
    
    ##msg.lite switch
    if(file.exists(path.msg.lite))
    {
      msg.lite_ =as.logical(extra.data(name="msg.lite"))
      unlink(path.msg.lite)
      beep(sound = 2)
      
    } 

    ##自訂價位建倉
    ###[create.price價格]
    CUSTOM.CREATE.LONG.path <-extra.data(name="CUSTOM.CREATE.LONG", p.mode = "path")
    if(file.exists(CUSTOM.CREATE.LONG.path)) 
    {
      create.price <- as.numeric(extra.data(name="CUSTOM.CREATE.LONG"))
      if(create.price >0)
      {
        alarm.msg <- paste0("PC.PL.", create.price)
        print(paste("[設定] 自訂價位建多倉 :", create.price))
        beep(sound = 2)        
      }else{
        print(paste("[錯誤] 自訂價位有誤 :", create.price))
        create.price <-0
      }
      unlink(CUSTOM.CREATE.LONG.path)
      
    }
    
    CUSTOM.CREATE.SHORT.path <-extra.data(name="CUSTOM.CREATE.SHORT", p.mode = "path")
    if(file.exists(CUSTOM.CREATE.SHORT.path)) 
    {
      create.price <- as.numeric(extra.data(name="CUSTOM.CREATE.SHORT"))
      if(create.price >0)
      {
        alarm.msg <- paste0("PC.PS.", create.price)
        print(paste("[設定] 自訂價位建空倉 :", create.price))
        beep(sound = 2)
        create.price <-create.price*-1
      }else{
        print(paste("[錯誤] 自訂價位有誤 :", create.price))
        create.price <-0
      }
      unlink(CUSTOM.CREATE.SHORT.path)
      
    }
    
    ##均線服從建倉
    ###[create.price均線價格]
    if(file.exists(path.MA5.CREATE.LONG)) 
    {
      create.price <- as.numeric(extra.data(name="MA5.CREATE.LONG"))
      unlink(path.MA5.CREATE.LONG)
      beep(sound = 2)
      
      alarm.msg <- paste0("PMAs.5PL.", create.price)
    }
    if(file.exists(path.MA10.CREATE.LONG)) 
    {
      create.price <- as.numeric(extra.data(name="MA10.CREATE.LONG"))
      unlink(path.MA10.CREATE.LONG)
      beep(sound = 2)
      
      alarm.msg <- paste0("PMAs.10PL.", create.price)

    }
    if(file.exists(path.MA20.CREATE.LONG)) 
    {
      create.price <- as.numeric(extra.data(name="MA20.CREATE.LONG"))
      unlink(path.MA20.CREATE.LONG)
      beep(sound = 2)
      
      alarm.msg <- paste0("PMAs.20PL.", create.price)
    }
    if(file.exists(path.MA5.CREATE.SHORT)) 
    {
      create.price <- as.numeric(extra.data(name="MA5.CREATE.SHORT")) *-1
      unlink(path.MA5.CREATE.SHORT)
      beep(sound = 2)
      
      alarm.msg <- paste0("PMAs.-5PL.", create.price)
    }
    if(file.exists(path.MA10.CREATE.SHORT)) 
    {
      create.price <- as.numeric(extra.data(name="MA10.CREATE.SHORT")) *-1
      unlink(path.MA10.CREATE.SHORT)
      beep(sound = 2)
      
      alarm.msg <- paste0("PMAs.-10PL.", create.price)
    }
    if(file.exists(path.MA20.CREATE.SHORT)) 
    {
      create.price <- as.numeric(extra.data(name="MA20.CREATE.SHORT")) *-1
      unlink(path.MA20.CREATE.SHORT)
      beep(sound = 2)
      
      alarm.msg <- paste0("PMAs.-20PL.", create.price)
    }
    
    #切換模擬/真實設定
    if(file.exists(RSS.path)) 
    {
      unlink(RSS.path)
      simu <-TF.Switch(simu)
      print(paste("[動作] 切換模擬/真實設定"))
      beep(sound = 2)
      
    }    
    
    ###出場均線設定
    if(switch.stopPORT ==0) {stop.PORT.MAPrice =0} #不考慮均線
    if(switch.stopPORT ==5) {stop.PORT.MAPrice =price.ma5}
    if(switch.stopPORT ==10){stop.PORT.MAPrice =price.ma10}
    ###出場RSI設定
    if(switch.stopPORT_RSI ==0){stop.PORT.RSIPrice =0} #不考慮RSI
    if(switch.stopPORT_RSI >0) {stop.PORT.RSIPrice =switch.stopPORT_RSI}

    ##待命建倉
    if(create.price !=0)
    {
      ##檢查create.price是否標記為動態均線服從
      for(miu in 1:length(ma.all))
      {

          if( abs(create.price) ==ma.all[miu] ||
             abs(create.price.dynamic) ==ma.all[miu] )
          {
            if(ma.all[miu] ==5)
            {
              .sig <-p_n.sig(create.price)
              create.price <-price.ma5 *.sig
              alarm.msg <- paste0("PMA.",.sig, ".5PL.", create.price)
              create.price.dynamic <-ma.all[miu] *.sig
              
            }
            if(ma.all[miu] ==10)
            {
              .sig <-p_n.sig(create.price)
              create.price <-price.ma10 *.sig
              alarm.msg <- paste0("PMA.", .sig, ".10PL.", create.price)
              create.price.dynamic <-ma.all[miu] *.sig
              
            }
            if(ma.all[miu] ==20)
            {
              .sig <-p_n.sig(create.price)
              create.price <-price.ma20 *.sig
              alarm.msg <- paste0("PMA.", .sig, ".20PL.", create.price)
              create.price.dynamic <-ma.all[miu] *.sig
              
            }
            
          }          
    
      }
      
      #檢查建倉條件
      if(create.price >0 && 
         !switch.create.positionLONG && #標示已手動完成建倉
         !Finished.create.positionLONG) #標示已AGENT function完成建倉
      {
        if(Price.curr <=create.price+MatchBUFFER && !ENABLE.ByMA)
        {
          ENABLE.ByMA <-TRUE
          beep(sound = 2)
          print(paste("[設定] 待命多頭均線服從建倉，價位 :", Price.curr))
        }
        else if(Price.curr >create.price+MatchBUFFER && 
                            ENABLE.ByMA
                && !Finished.create.positionLONG)
        {
          print(paste("[動作] 執行多頭均線服從建倉，價位 :", Price.curr))
          
          BorS <- "B"
          Price <- Price.current()
          #執行交易並回傳交易序號
          OrderNO <-PTrading.MGR(.BorS=BorS
                                 , .Price=Price
                                 , .Qty=Qty
                                 , .Daytrade=Daytrade
                                 , .simu=simu)
          
          #依下單回傳序號解碼成文字向量，並確認交易結果
          list.RESULT <-PTrading.confirm(OrderNO)
          .checkRESULT <-list.RESULT[[1]]
          transaction  <-list.RESULT[[2]]
          
          #交易成功則執行後續設定
          if(.checkRESULT){PTConf.export(transaction)}
          
          #匯出交易紀錄
          append.to.file(data = as.data.frame(transaction), path = paste0(OrderNO, ".csv"))
          m.act <-readline(paste0("交易序號回傳 :", transaction[1]
                                  , "，交易結果 :", transaction[2], " <Press Any Key pls.>"))
          
          Finished.create.positionLONG <-TRUE
        }
        }###
      
    if(create.price <0 && 
       !switch.create.positionSHORT &&
       !Finished.create.positionSHORT)
    {
      if(Price.curr >=abs(create.price)-MatchBUFFER && !ENABLE.ByMA)
      {
        ENABLE.ByMA <-TRUE
        beep(sound = 2)
        print(paste("[設定] 待命空頭均線服從建倉，價位 :", Price.curr))
        
      }
      else if(Price.curr <abs(create.price)-MatchBUFFER && 
                          ENABLE.ByMA &&
                          !Finished.create.positionSHORT)
      {
        #建倉
        BorS <- "S"
        Price <- Price.current()
        #執行交易並回傳交易序號
        OrderNO <-PTrading.MGR(.BorS=BorS
                               , .Price=Price
                               , .Qty=Qty
                               , .Daytrade=Daytrade
                               , .simu=simu)
        
        #依下單回傳序號解碼成文字向量，並確認交易結果
        list.RESULT <-PTrading.confirm(OrderNO)
        .checkRESULT <-list.RESULT[[1]]
        transaction  <-list.RESULT[[2]]
        
        #交易成功則執行後續設定
        if(.checkRESULT){PTConf.export(transaction)}
        
        #匯出交易紀錄
        append.to.file(data = as.data.frame(transaction), path = paste0(OrderNO, ".csv"))
        m.act <-readline(paste0("交易序號回傳 :", transaction[1]
                                , "，交易結果 :", transaction[2], " <Press Any Key pls.>"))
          
        Finished.create.positionSHORT <-TRUE
        }
    }
  }
    
    ##已建倉，檢查平倉條件
    if(switch.create.positionLONG && 
       Price.PCL ==for.LONG )
    {
      #檢查是否停損
      if(Price.curr <.stopLOSS.price.LONG)
      {
        msg.file  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
        file.create(msg.file)
        OrderNO <- ClosePositionAll()
        
        print(paste("[動作] 執行多頭停損價位 :", Price.buyin.PRE, ">", Price.curr
                    , switch.create.positionLONG
                    , Price.PCL
                    , Price.diff
                    , .stopLOSS.price.LONG))
        beep(sound = 7)
        CROSS.Stop.PORT.LINE <-0
        stop.PORT.MAPrice <-0
        stop.PORT.RSIPrice <-0
        create.price.dynamic <-0
        
        #
        ENABLE.ByMA <-FALSE
        Finished.create.positionLONG <-FALSE
                
      }
      
      #檢查是否停利
      if(Price.curr >=.stopPORT.price.LONG 
         && CROSS.Stop.PORT.LINE ==0)
      {
        CROSS.Stop.PORT.LINE <-currentbar.num
      }
      if(
          (
            CROSS.Stop.PORT.LINE !=0 
            &&
              ( ((stop.PORT.MAPrice !=0 &&
                Price.curr <=stop.PORT.MAPrice &&
                CROSS.Stop.PORT.LINE !=currentbar.num) || (stop.PORT.MAPrice ==0)) 
                ||
                ((stop.PORT.RSIPrice !=0 &&
                 price.rsi >=stop.PORT.RSIPrice &&
                 CROSS.Stop.PORT.LINE !=currentbar.num) || (stop.PORT.RSIPrice ==0))
                 )
          )
        )
      {
        msg.file  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
        file.create(msg.file)
        OrderNO <- ClosePositionAll()
        
        print(paste("[動作] 執行多頭停利價位 :", Price.buyin.PRE, ">", Price.curr
                    , switch.create.positionLONG
                    , Price.PCL
                    , Price.diff
                    , .stopPORT.price.LONG))
        beep(sound = 8)
        CROSS.Stop.PORT.LINE <-0
        stop.PORT.MAPrice <-0 
        stop.PORT.RSIPrice <-0
        create.price.dynamic <-0
        #
        ENABLE.ByMA <-FALSE
        Finished.create.positionLONG <-FALSE
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
        OrderNO <- ClosePositionAll()
        
        print(paste("[動作] 執行空頭停損價位 :", Price.curr, "<", Price.buyin.PRE
                    , switch.create.positionSHORT
                    , Price.PCL
                    , Price.diff
                    , .stopLOSS.price.SHORT))
        beep(sound = 7)
        CROSS.Stop.PORT.LINE <-0
        stop.PORT.MAPrice <-0
        stop.PORT.RSIPrice <-0
        create.price.dynamic <-0
        
        #
        ENABLE.ByMA <-FALSE
        Finished.create.positionSHORT <-FALSE
                
      }
      #檢查是否停利
      if(Price.curr <=.stopPORT.price.SHORT && CROSS.Stop.PORT.LINE ==0)
      {
        CROSS.Stop.PORT.LINE <-currentbar.num
      }
      if(
          (
            CROSS.Stop.PORT.LINE !=0 
            &&
              ( ((stop.PORT.MAPrice !=0 &&
                  Price.curr >=stop.PORT.MAPrice &&
                  CROSS.Stop.PORT.LINE !=currentbar.num) || (stop.PORT.MAPrice ==0)) 
                ||
                ((stop.PORT.RSIPrice !=0 &&
                  price.rsi <=stop.PORT.RSIPrice*-1 &&
                  CROSS.Stop.PORT.LINE !=currentbar.num) || (stop.PORT.RSIPrice ==0))
              )
          )
        )
      {
        msg.file  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
        file.create(msg.file)
        OrderNO <- ClosePositionAll()
        
        print(paste("[動作] 執行空頭停利價位 :", Price.curr, "<", Price.buyin.PRE
                    , switch.create.positionSHORT
                    , Price.PCL
                    , Price.diff
                    , .stopPORT.price.LONG))
        beep(sound = 8)
        CROSS.Stop.PORT.LINE <-0
        stop.PORT.MAPrice <-0
        stop.PORT.RSIPrice <-0
        create.price.dynamic <-0
        #
        ENABLE.ByMA <-FALSE
        Finished.create.positionSHORT <-FALSE
                
        }
      }

    #訊息顯示方式切換
    extra.msg1 <- c()
    extra.msg2 <- c()
    
    if(msg.lite_)
    {
      extra.msg1 <-""
      extra.msg2 <-""
      }else{
        extra.msg1 <- paste(
          switch.create.positionLONG, switch.create.positionSHORT
          , ENABLE.ByMA, Finished.create.positionLONG, Finished.create.positionSHORT
          )   
        extra.msg2 <- paste(
          "<", round(Research_Line_Upper, digits = 2), round(Research_Line_lower, digits = 2)
          , round(extremes_Line_Upper, digits = 2), round(extremes_Line_lower, digits = 2)
          , round(Bolling_Line_upper, digits = 2), round(Bolling_Line_lower, digits = 2) 
          , switch.stopPORT, stop.PORT.MAPrice, switch.stopPORT_RSI, stop.PORT.RSIPrice, ">"
        ) 
    }
    
    print(paste("[", alarm.msg, Price.PCL, "]", ifelse(Price.buyin.PRE ==0, 0, Price.diff)
                , Price.buyin.PRE, ">>", Price.curr
                , extra.msg1
                , "+", round(.stopPORT.price.LONG, digits = 2), round(.stopPORT.price.SHORT, digits = 2), "-"
                , "+", round(.stopLOSS.price.LONG, digits = 2), round(.stopLOSS.price.SHORT, digits = 2), "-"
                , extra.msg2
                , trans.lang(mode=CODE.SIMU, param=simu), currentbar.num)
                ) 
    
    Sys.sleep(0.1)
     

  
  }#by action="E"
  
  }#by repeat loop
}

