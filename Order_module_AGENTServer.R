#### AGENT ####
Position.AGENT<-function()
{
  
  Price.curr.PRE <-0
  create.price <-0
  create.price.dyn <-0
  Price.buyin.PRE <-0
  Price.PCL <-0
  MatchBUFFER <-15
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
  
  extremes_Line_Upper <- 0
  extremes_Line_lower <- 0
  Bolling_Line_upper <- 0
  Bolling_Line_lower <- 0
  price.rsi <-0
  
  rm.conf(name = "close.ALLPOSITION")
  
  rm.conf(name = "price.Buyin")
  rm.conf(name = "price.PCL")
  rm.conf(name = "OrderNO")
  
  rm.conf(name = "create.positionLONG")
  rm.conf(name = "create.positionSHORT")
  rm.conf(name = "RESET_AGENT.SERVERE")
  
  meta.path <- paste0(price.path, "meta_record.csv")
  
  INIT.ENABLE <-FALSE
  
  ##
  
  ##  
  repeat
  {
    action <-NULL
    #TRUE真實卷商 FALSE模擬卷商
    if.MXFSIMU <- get.conf(name="switch.DATA.Source", dataset = dataset.name)
    
    while(TRUE)
    {
      #產生MENU
      print(paste0("MatchBUFFER        : ", MatchBUFFER)) 
      print(paste0("switch.stopPORT    : ", switch.stopPORT))  
      print(paste0("switch.stopPORT_RSI: ", switch.stopPORT_RSI))  
      print(paste0("MODE.XFSource      : ", trans.lang(mode="SECURTIES"
                                                       , if.MXFSIMU)))  
      print(paste0("PATH.XFSource      : ", data.source.switch(if.MXFSIMU))) 
      print(paste0("Simulation         : ", simu))       
      # 
      print(" ")
      
      print("(E)ENABLE.AGENT.Server")
      #
      action <- readline("[COMMAND] :")
      if(action =="E" || action =="QQ"){break}
      
      switch (action,
              SMB = { MatchBUFFER <-as.numeric(readline("NEW MatchBUFFER :"))}
      )
    }
    
    if(action =="QQ"){break}#回到上層
    
    while(TRUE)
    {
      #檢查停止
      if(!is.null(get.conf(name="DAGS", dataset = dataset.name)))
      {
        rm.conf(name="DAGS", dataset = dataset.name)
        break
      }
      
      meta.record <- m.tail(meta.path)
      names(meta.record) <-meta.record.name
      
      #目前價位
      Price.curr <- as.numeric(Price.current())
      
      # if(if.MXFSIMU)
      # {
      Price.open <- as.numeric(meta.record$open)
      Price.high <- as.numeric(meta.record$high)
      Price.low <- as.numeric(meta.record$low)
      price.ma5 <- as.numeric(meta.record$ma5)
      price.ma10 <- as.numeric(meta.record$ma10)
      price.ma20 <- as.numeric(meta.record$ma20)
      price.rsi <- as.numeric(meta.record$RSI)
      #
      extremes_Line_Upper <- as.numeric(meta.record$EL.Upper)
      extremes_Line_lower <- as.numeric(meta.record$EL.lower)
      Bolling_Line_upper <- as.numeric(meta.record$b.upper)
      Bolling_Line_lower <- as.numeric(meta.record$b.lower)
      
      currentbar.num <- as.numeric(meta.record$currentBar)        
      # }
      
      
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
      Price.buyin.PRE <- as.numeric( ifelse(!is.null(get.conf(name = "price.Buyin", dataset = dataset.name))
                                            , get.conf(name = "price.Buyin", dataset = dataset.name)
                                            , 0) )
      Price.PCL <- as.numeric( ifelse(!is.null(get.conf(name = "price.PCL", dataset = dataset.name))
                                      , get.conf(name = "price.PCL", dataset = dataset.name)
                                      , 0) )      
      Price.diff <- Price.curr -Price.buyin.PRE
      
      #艦橋指令讀取
      ##已全平倉 
      if(!is.null(get.conf(name = "close.ALLPOSITION", dataset = dataset.name)) || #緊急平倉
         !is.null(get.conf(name = "RESET_AGENT.SERVERE", dataset = dataset.name)) || #AGS重啟動
         !INIT.ENABLE)
      {
        
        m_msg("[設定] 回復系統預設值...")
        PT.data.reset()
        # rm.conf(name = "close.ALLPOSITION", dataset = dataset.name)
        # rm.conf(name = "RESET_AGENT.SERVERE", dataset = dataset.name)
        # 
        # rm.conf(name = "price.Buyin", dataset = dataset.name)
        # rm.conf(name = "price.PCL", dataset = dataset.name)
        # rm.conf(name = "OrderNO", dataset = dataset.name)
        # rm.conf(name = "create.positionLONG", dataset = dataset.name)
        # rm.conf(name = "create.positionSHORT", dataset = dataset.name)
        
        #
        alarm.msg <-NULL
        switch.create.positionLONG <-FALSE
        switch.create.positionSHORT <-FALSE
        ENABLE.ByMA <-FALSE
        create.price <-0
        INIT.ENABLE <-TRUE
        Price.buyin.PRE <-0
        Price.PCL <-0
        
        m_msg(paste0("[設定] env.close.ALLPOSITION "
                     , get.conf(name = "close.ALLPOSITION", dataset = dataset.name)))
        m_msg(paste0("[設定] env.RESET_AGENT.SERVERE "
                     , get.conf(name = "RESET_AGENT.SERVERE", dataset = dataset.name)))
        m_msg(paste0("[設定] switch.create.positionLONG "
                     ,switch.create.positionLONG))
        m_msg(paste0("[設定] switch.create.positionSHORT "
                     , switch.create.positionSHORT))
        m_msg(paste0("[設定] INIT.ENABLE "
                     , INIT.ENABLE))
      }
      
      ## 手動遙控切換<switch.create.positionLONG, switch.create.positionSHORT> switch
      m.switch <-c("switch.create.positionLONG", "switch.create.positionSHORT")
      miu <-0
      
      for(miu in 1:length(m.switch))
      {
        m.conf <-get.conf(name = m.switch[miu], dataset = dataset.name)
        if(!is.null(m.conf))
        {
          m.check <-as.logical(m.conf)
          if(!is.null(m.check))
          {
            if(m.check)
            {
              
              if(m.switch[miu] == "switch.create.positionLONG") 
              {
                switch.create.positionLONG <-TRUE
                m_msg(paste("[設定] 手動設定建倉資料", "switch.create.positionLONG", switch.create.positionLONG))
              }
              if(m.switch[miu] == "switch.create.positionSHORT")
              {
                switch.create.positionSHORT <-TRUE
                m_msg(paste("[設定] 手動設定建倉資料", "switch.create.positionSHORT", switch.create.positionSHORT))
              }
              
              Price.buyin.PRE <-get.conf(name="price.Buyin", dataset =dataset.name)
              Price.PCL <-get.conf(name="price.PCL", dataset =dataset.name)
              
              m_msg(paste("[設定] 手動設定建倉資料", "Price.buyin.PRE", Price.buyin.PRE))
              m_msg(paste("[設定] 手動設定建倉資料", "Price.PCL", Price.PCL))
              
            } 
            rm.conf(name = m.switch[miu], dataset = dataset.name)
          }
          
          
        }
        
      }
      
      ##確認交易結果
      ##檔案price.Buyin不存在而price.PCL存在，表示執行建倉但未確認結果
      
      if(is.null(get.conf(name = "price.Buyin", dataset = dataset.name)) &
         !is.null(get.conf(name = "price.PCL", dataset = dataset.name)))
      {
        
        m_msg(paste0("[訊息] 已執行建倉但未確認結果"))
        
        OnOpen.OrderNO <-get.conf(name = "OrderNO", dataset = dataset.name)
        
        if(OnOpen.OrderNO == CODE.SIMU)
        {
          m_msg(paste0("交易序號回傳 :", OnOpen.OrderNO
                       , "，交易結果 :", OnOpen.OrderNO)) 
          
        }else if(length(OnOpen.OrderNO) !=0)
        {
          
          #依下單回傳序號解碼成文字向量，並確認交易結果
          list.RESULT <-PTrading.confirm(OnOpen.OrderNO)
          if(!is.logical(list.RESULT))
          {
            .checkRESULT <-list.RESULT[[1]]
            transaction  <-list.RESULT[[2]]
            
            # #匯出交易紀錄
            append.to.file(data = transaction, path = extra.data(name = "transaction", p.mode = "path"), m.append = FALSE)
            #交易成功則執行後續設定
            if(.checkRESULT)
            {###
              m_msg(paste0("[訊息] 交易成功，執行後續設定"))
              
              
              OnOpen.pcl  <- BorS2PCL(x=account.info(by.name = "bors", info = transaction), .mode="zh")
              OnOpen.price <- as.numeric(account.info(by.name = "price", info = transaction))
              #匯出交易價位
              set.conf(name="price.Buyin", value =OnOpen.price, dataset =dataset.name)
              #匯出交易PCL
              set.conf(name="price.PCL", value =OnOpen.pcl, dataset =dataset.name)
              
              switch (as.character(OnOpen.pcl),
                      #買進B
                      "1" = {
                        # create.price <- OnOpen.price
                        switch.create.positionLONG =TRUE
                        alarm.msg <- "CR.PL"
                      },
                      #賣出S
                      "-1" = {
                        # create.price <- OnOpen.price *-1
                        switch.create.positionSHORT =TRUE
                        alarm.msg <- "CR.PL"
                      }
              )
            }###
            
            m_msg(paste0("交易序號回傳 :", OnOpen.OrderNO
                         , "，交易結果 :", account.info(by.name = "status", info = transaction)))
            
          }else{
            m_msg(paste0("[錯誤] 交易結果有誤，序號回傳:", OnOpen.OrderNO))
          }
          
          beep(sound = 2)  
        }        
      }
      
      ##均線停利點
      if(!is.null(get.conf(name = "switch_to.ma", dataset = dataset.name)))
        
      {
        switch.stopPORT <-as.numeric(get.conf(name = "switch_to.ma", dataset = dataset.name))
        rm.conf(name = "switch_to.ma", dataset = dataset.name)
        
        beep(sound = 2)
        m_msg(paste("[設定] 均線停利點重設 :", switch.stopPORT))
        
      } 
      ##RSI停利點
      if(!is.null(get.conf(name = "switch_to.rsi", dataset = dataset.name)))
      {
        switch.stopPORT_RSI <-as.numeric(get.conf(name = "switch_to.rsi", dataset = dataset.name))
        # unlink(path.switch_to.rsi)
        rm.conf(name = "switch_to.rsi", dataset = dataset.name)
        
        beep(sound = 2)
        m_msg(paste("[設定] RSI停利點重設 :", switch.stopPORT_RSI))
        
      }
      
      #msg.lite switch
      if(!is.null(get.conf(name = "msg.lite", dataset = dataset.name)))
      {
        msg.lite_ =as.logical(get.conf(name = "msg.lite", dataset = dataset.name))
        rm.conf(name = "msg.lite", dataset = dataset.name)
        
        m_msg(paste("[設定] 切換顯示訊息類型 :", msg.lite_))
        beep(sound = 2)
        
      }
      
      #計算價格變動
      Price.buyin.PRE <- as.numeric( ifelse(!is.null(get.conf(name = "price.Buyin", dataset = dataset.name))
                                            , get.conf(name = "price.Buyin", dataset = dataset.name)
                                            , 0) )
      Price.PCL <- as.numeric( ifelse(!is.null(get.conf(name = "price.PCL", dataset = dataset.name))
                                      , get.conf(name = "price.PCL", dataset = dataset.name)
                                      , 0) )
      
      ##自訂價位建倉
      ###[create.price價格]
      if(!is.null(get.conf(name = "CUSTOM.CREATE.LONG", dataset = dataset.name)))
      {
        create.price <- as.numeric(get.conf(name = "CUSTOM.CREATE.LONG", dataset = dataset.name))
        if(create.price >0)
        {
          alarm.msg <- paste0("PC.PL.", create.price)
          m_msg(paste("[設定] 自訂價位建多倉 :", create.price))
          beep(sound = 2)        
        }else{
          m_msg(paste("[錯誤] 自訂價位有誤 :", create.price))
          create.price <-0
        }
        rm.conf(name = "CUSTOM.CREATE.LONG", dataset = dataset.name)
        
      }
      
      if(!is.null(get.conf(name = "CUSTOM.CREATE.SHORT", dataset = dataset.name)))
      {
        create.price <- as.numeric(get.conf(name = "CUSTOM.CREATE.SHORT", dataset = dataset.name))
        if(create.price >0)
        {
          alarm.msg <- paste0("PC.PS.", create.price)
          m_msg(paste("[設定] 自訂價位建空倉 :", create.price))
          beep(sound = 2)
          create.price <-create.price*-1
        }else{
          m_msg(paste("[錯誤] 自訂價位有誤 :", create.price))
          create.price <-0
        }
        
        rm.conf(name = "CUSTOM.CREATE.SHORT", dataset = dataset.name)
        
      }
      
      ##均線服從建倉
      ###[create.price均線價格]
      if(!is.null(get.conf(name = "MA5.CREATE.LONG", dataset = dataset.name)))
      {
        create.price <- as.numeric(get.conf(name = "MA5.CREATE.LONG", dataset = dataset.name))
        rm.conf(name = "MA5.CREATE.LONG", dataset = dataset.name)
        
        beep(sound = 2)
        
        alarm.msg <- paste0("PMAs.5PL.", create.price)
      }
      
      if(!is.null(get.conf(name = "MA10.CREATE.LONG", dataset = dataset.name)))
        
      {
        create.price <- as.numeric(get.conf(name = "MA10.CREATE.LONG", dataset = dataset.name))
        rm.conf(name = "MA10.CREATE.LONG", dataset = dataset.name)
        
        beep(sound = 2)
        
        alarm.msg <- paste0("PMAs.10PL.", create.price)
        
      }
      if(!is.null(get.conf(name = "MA20.CREATE.LONG", dataset = dataset.name)))
      {
        create.price <- as.numeric(get.conf(name = "MA20.CREATE.LONG", dataset = dataset.name))
        rm.conf(name = "MA20.CREATE.LONG", dataset = dataset.name)
        
        beep(sound = 2)
        
        alarm.msg <- paste0("PMAs.20PL.", create.price)
      }
      if(!is.null(get.conf(name = "MA5.CREATE.SHORT", dataset = dataset.name)))
        
      {
        create.price <- as.numeric(get.conf(name = "MA5.CREATE.SHORT", dataset = dataset.name)) *-1
        rm.conf(name = "MA5.CREATE.SHORT", dataset = dataset.name)
        
        beep(sound = 2)
        
        alarm.msg <- paste0("PMAs.-5PL.", create.price)
      }
      
      if(!is.null(get.conf(name = "MA10.CREATE.SHORT", dataset = dataset.name)))
        
      {
        create.price <- as.numeric(get.conf(name = "MA10.CREATE.SHORT", dataset = dataset.name)) *-1
        rm.conf(name = "MA10.CREATE.SHORT", dataset = dataset.name)
        
        beep(sound = 2)
        
        alarm.msg <- paste0("PMAs.-10PL.", create.price)
      }
      if(!is.null(get.conf(name = "MA20.CREATE.SHORT", dataset = dataset.name)))
      {
        create.price <- as.numeric(get.conf(name = "MA20.CREATE.SHORT", dataset = dataset.name)) *-1
        rm.conf(name = "MA20.CREATE.SHORT", dataset = dataset.name)
        
        beep(sound = 2)
        
        alarm.msg <- paste0("PMAs.-20PL.", create.price)
      }
      
      #切換模擬/真實設定
      m.REMOTE.SWITCH.SIMULATION <-get.conf(name = "REMOTE.SWITCH.SIMULATION", dataset = dataset.name)
      if(!is.null(m.REMOTE.SWITCH.SIMULATION))
      {
        if(as.logical(m.REMOTE.SWITCH.SIMULATION) !=simu)
        {
          simu <-TF.Switch(simu)
          m_msg(paste0("[設定] 切換模擬/真實設定 :"
                       , trans.lang(mode=CODE.SIMU, param = simu)))
          beep(sound = 2)        
        }
        
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
              abs(create.price.dyn) ==ma.all[miu] )
          {
            if(create.price.dyn ==0){create.price.dyn ==ma.all[miu]}
            
            if(ma.all[miu] ==5)
            {
              .sig <-p_n.sig(create.price)
              create.price <-price.ma5 *.sig
              alarm.msg <- paste0("PMA.",.sig, ".5PL.", create.price)
            }
            if(ma.all[miu] ==10)
            {
              .sig <-p_n.sig(create.price)
              create.price <-price.ma10 *.sig
              alarm.msg <- paste0("PMA.", .sig, ".10PL.", create.price)
              
            }
            if(ma.all[miu] ==20)
            {
              .sig <-p_n.sig(create.price)
              create.price <-price.ma20 *.sig
              alarm.msg <- paste0("PMA.", .sig, ".20PL.", create.price)
              
            }
            
          }          
          
        }
        
        #檢查建倉條件
        if(create.price >0 && 
           !switch.create.positionLONG && #非標示已手動完成建倉
           !Finished.create.positionLONG) #非標示已AGENT function完成建倉
        {
          if(Price.curr >create.price -MatchBUFFER &&
             Price.curr <create.price &&
             !ENABLE.ByMA)
          {
            ENABLE.ByMA <-TRUE
            beep(sound = 2)
            m_msg(paste("[設定] 待命多頭均線服從建倉，價位 :", Price.curr))
          }
          else if(Price.curr >create.price && 
                  ENABLE.ByMA &&
                  !Finished.create.positionLONG)
          {
            m_msg(paste("[動作] 執行多頭均線服從建倉，價位 :", Price.curr))
            
            BorS <- "B"
            Price <- Price.current()
            #執行交易並回傳交易序號
            OrderNO <-PTrading.MGR(.BorS=BorS
                                   , .Price=Price
                                   , .Qty=Qty
                                   , .Daytrade=Daytrade
                                   , .simu=simu)
            
            Sys.sleep(1)
            
            if(Auto.positionCLOSE)
            {
              next.step <- "7"
            }               
            
            Finished.create.positionLONG <-TRUE
          }
        }###
        
        if(create.price <0 && 
           !switch.create.positionSHORT &&
           !Finished.create.positionSHORT)
        {
          if(Price.curr <create.price +MatchBUFFER &&
             Price.curr >create.price &&
             !ENABLE.ByMA)
          {
            ENABLE.ByMA <-TRUE
            beep(sound = 2)
            m_msg(paste("[設定] 待命空頭均線服從建倉，價位 :", Price.curr))
            
          }
          else if(Price.curr <create.price && 
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
            
            Sys.sleep(1)
            
            if(Auto.positionCLOSE)
            {
              next.step <- "7"
            }               
            
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
          msg.file  <- set.conf(name="close.ALLPOSITION", value = "", dataset = dataset.name)
          OrderNO <- ClosePositionAll()
          
          m_msg(paste("[動作] 執行多頭停損價位 :", Price.buyin.PRE, ">", Price.curr
                      , switch.create.positionLONG
                      , Price.PCL
                      , Price.diff
                      , .stopLOSS.price.LONG))
          beep(sound = 7)
          CROSS.Stop.PORT.LINE <-0
          stop.PORT.MAPrice <-0
          stop.PORT.RSIPrice <-0
          
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
          msg.file  <- set.conf(name="close.ALLPOSITION", value = "", dataset = dataset.name) 
          OrderNO <- ClosePositionAll()
          
          m_msg(paste("[動作] 執行多頭停利價位 :", Price.buyin.PRE, ">", Price.curr
                      , switch.create.positionLONG
                      , Price.PCL
                      , Price.diff
                      , .stopPORT.price.LONG))
          beep(sound = 8)
          CROSS.Stop.PORT.LINE <-0
          stop.PORT.MAPrice <-0 
          stop.PORT.RSIPrice <-0
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
          msg.file  <- set.conf(name="close.ALLPOSITION", value = "", dataset = dataset.name) 
          OrderNO <- ClosePositionAll()
          
          m_msg(paste("[動作] 執行空頭停損價位 :", Price.curr, "<", Price.buyin.PRE
                      , switch.create.positionSHORT
                      , Price.PCL
                      , Price.diff
                      , .stopLOSS.price.SHORT))
          beep(sound = 7)
          CROSS.Stop.PORT.LINE <-0
          stop.PORT.MAPrice <-0
          stop.PORT.RSIPrice <-0
          
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
          msg.file  <- set.conf(name="close.ALLPOSITION", value = "", dataset = dataset.name) 
          OrderNO <- ClosePositionAll()
          
          m_msg(paste("[動作] 執行空頭停利價位 :", Price.curr, "<", Price.buyin.PRE
                      , switch.create.positionSHORT
                      , Price.PCL
                      , Price.diff
                      , .stopPORT.price.LONG))
          beep(sound = 8)
          CROSS.Stop.PORT.LINE <-0
          stop.PORT.MAPrice <-0
          stop.PORT.RSIPrice <-0
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
          ENABLE.ByMA, Finished.create.positionLONG, Finished.create.positionSHORT
        )   
        extra.msg2 <- paste(
          "<", round(extremes_Line_Upper, digits = 2), round(extremes_Line_lower, digits = 2)
          , round(Bolling_Line_upper, digits = 2), round(Bolling_Line_lower, digits = 2) 
          , switch.stopPORT, stop.PORT.MAPrice, switch.stopPORT_RSI, stop.PORT.RSIPrice, ">"
        ) 
      }
      
      m_msg(paste("[", alarm.msg, Price.PCL, "]", ifelse(Price.buyin.PRE ==0, 0, Price.diff)
                  , Price.buyin.PRE, ">>", Price.curr
                  , extra.msg1
                  , "+", round(.stopPORT.price.LONG, digits = 2), round(.stopPORT.price.SHORT, digits = 2), "-"
                  , "+", round(.stopLOSS.price.LONG, digits = 2), round(.stopLOSS.price.SHORT, digits = 2), "-"
                  , extra.msg2
                  ,  switch.create.positionLONG, switch.create.positionSHORT
                  , trans.lang(mode=CODE.SIMU, param=simu), currentbar.num)
      ) 
      
      Price.curr.PRE <-Price.curr
      
    }#by action="E"
    
  }#by repeat loop
}

