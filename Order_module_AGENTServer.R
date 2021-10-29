#### AGENT ####
Position.AGENT<-function()
{
  
  del.count <-0
  del.count.limited <-100
  create.price <-0
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
  path.OrderNO <-extra.data(name="OrderNO", p.mode = "path")
  path.del.count.limited <- extra.data(name="del.count.limited", p.mode = "path")
    
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
  unlink(path.OrderNO)
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
      if(file.exists(DAGS.path))
      {
        unlink(DAGS.path)
        break
      }
      
      #目前價位
      Price.curr <- as.numeric(Price.current())
      Price.open <- extra.data(name="OP")
      Price.high <- extra.data(name="HI")
      Price.low <- extra.data(name="LO")

      price.ma5 <- as.numeric(extra.data(name="MA5"))
      price.ma10 <- as.numeric(extra.data(name="MA10"))
      price.ma20 <- as.numeric(extra.data(name="MA20"))
      price.rsi <- as.numeric(extra.data(name="RSI"))
      #
      extremes_Line_Upper <- as.numeric(extra.data(name="extremes_Line_Upper"))
      extremes_Line_lower <- as.numeric(extra.data(name="extremes_Line_lower"))
      Bolling_Line_upper <- as.numeric(extra.data(name="B_UP"))
      Bolling_Line_lower <- as.numeric(extra.data(name="B_LO"))
      
      currentbar.num <- as.numeric(extra.data(name="currentBar")) 

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
      if(file.exists(path.price.Buyin)) {Price.buyin.PRE <- as.numeric(extra.data(name="price.Buyin"))}
      if(file.exists(path.price.PCL)) {Price.PCL <- as.numeric(extra.data(name="price.PCL"))}
      
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
        unlink(path.OrderNO)
        unlink(path.create.positionLONG)
        unlink(path.create.positionSHORT)
        unlink(RAGS.path)
        alarm.msg <-NULL
        switch.create.positionLONG =FALSE
        switch.create.positionSHORT =FALSE
        ENABLE.ByMA <-FALSE
        create.price <-0
        # create.price.dynamic <-0
        INIT.ENABLE <-TRUE
        
      }
      
      ##確認交易結果
      ##檔案price.Buyin不存在而price.PCL存在，表示執行建倉但未確認結果
      # m_msg(paste("[", alarm.msg, Price.PCL, "]", ifelse(Price.buyin.PRE ==0, 0, Price.diff)
      #             , Price.buyin.PRE, ">>", Price.curr
      if(!file.exists(path.price.Buyin) &&
         file.exists(path.price.PCL))
      {
        m_msg(paste0("[訊息] 已執行建倉但未確認結果"))
        
        # OnOpen.data <-portfolio.monitor()
        OnOpen.OrderNO <-extra.data(name="OrderNO")
        
        if(OnOpen.OrderNO == CODE.SIMU)
        {
          m_msg(paste0("交易序號回傳 :", OnOpen.OrderNO
                       , "，交易結果 :", OnOpen.OrderNO))          
        }
        if(length(OnOpen.OrderNO) !=0)
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
            {
              m_msg(paste0("[訊息] 交易成功，執行後續設定"))
              
              .price.path <- extra.data(name="price.Buyin", p.mode = "path")
              .PCL.path <- extra.data(name="price.PCL", p.mode = "path")
              #Price.buyin
              append.to.file(data= transaction[5]
                             , path=.price.path, m.append = FALSE)
              #PCL
              append.to.file(data=BorS2PCL(x=transaction[4], .mode="zh") 
                             , path=.PCL.path, m.append = FALSE)
              
              OnOpen.bors <-account.info(by.name = "bors", info = transaction)
              OnOpen.price <- as.numeric(account.info(by.name = "price", info = transaction))
              switch (OnOpen.bors,
                      B = {
                        create.price <- OnOpen.price
                        switch.create.positionLONG =TRUE
                        alarm.msg <- "CR.PL"
                      },
                      S = {
                        create.price <- OnOpen.price *-1
                        switch.create.positionSHORT =TRUE
                        alarm.msg <- "CR.PL"
                      }
              )
            }
            
            m_msg(paste0("交易序號回傳 :", OnOpen.OrderNO
                         , "，交易結果 :", transaction[2]))
            
          }else{
            m_msg(paste0("[錯誤] 交易結果有誤，序號回傳:", OnOpen.OrderNO))
          }
          
          beep(sound = 2)  
        }        
      }
      
      ##均線停利點
      if(file.exists(path.switch_to.ma))
      {
        switch.stopPORT <-as.numeric(extra.data(name="switch_to.ma"))
        unlink(path.switch_to.ma)   
        
        beep(sound = 2)
        m_msg(paste("[設定] 均線停利點重設 :", switch.stopPORT))
        
      } 
      ##RSI停利點
      if(file.exists(path.switch_to.rsi))
      {
        switch.stopPORT_RSI <-as.numeric(extra.data(name="switch_to.rsi"))
        unlink(path.switch_to.rsi)
        
        beep(sound = 2)
        m_msg(paste("[設定] RSI停利點重設 :", switch.stopPORT_RSI))
        
      }
      
      #msg.lite switch
      if(file.exists(path.msg.lite))
      {
        msg.lite_ =as.logical(extra.data(name="msg.lite"))
        unlink(path.msg.lite)
        beep(sound = 2)
        
      }
      
      #del.count.limited reset
      if(file.exists(path.del.count.limited))
      {
        del.count.limited =as.numeric(extra.data(name="del.count.limited"))
        unlink(path.del.count.limited)
        m_msg(paste0("[設定] ˇ價格資料重製間隔 :", del.count.limited))
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
          m_msg(paste("[設定] 自訂價位建多倉 :", create.price))
          beep(sound = 2)        
        }else{
          m_msg(paste("[錯誤] 自訂價位有誤 :", create.price))
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
          m_msg(paste("[設定] 自訂價位建空倉 :", create.price))
          beep(sound = 2)
          create.price <-create.price*-1
        }else{
          m_msg(paste("[錯誤] 自訂價位有誤 :", create.price))
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
        m_msg(paste("[動作] 切換模擬/真實設定"))
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
          
          if( abs(create.price) ==ma.all[miu])
          {
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
            #匯出交易序號
            append.to.file(data = OrderNO, path = extra.data(name = "OrderNO", p.mode = "path"), m.append = FALSE)             
            #匯出交易PCL
            append.to.file(data = BorS2PCL(BorS), path = extra.data(name = "price.PCL", p.mode = "path"), m.append = FALSE) 
            
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
            #匯出交易序號
            append.to.file(data = OrderNO, path = extra.data(name = "OrderNO", p.mode = "path"), m.append = FALSE)             
            #匯出交易PCL
            append.to.file(data = BorS2PCL(BorS), path = extra.data(name = "price.PCL", p.mode = "path"), m.append = FALSE) 
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
          msg.file  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
          file.create(msg.file)
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
          # create.price.dynamic <-0
          
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
          
          m_msg(paste("[動作] 執行多頭停利價位 :", Price.buyin.PRE, ">", Price.curr
                      , switch.create.positionLONG
                      , Price.PCL
                      , Price.diff
                      , .stopPORT.price.LONG))
          beep(sound = 8)
          CROSS.Stop.PORT.LINE <-0
          stop.PORT.MAPrice <-0 
          stop.PORT.RSIPrice <-0
          # create.price.dynamic <-0
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
          
          m_msg(paste("[動作] 執行空頭停損價位 :", Price.curr, "<", Price.buyin.PRE
                      , switch.create.positionSHORT
                      , Price.PCL
                      , Price.diff
                      , .stopLOSS.price.SHORT))
          beep(sound = 7)
          CROSS.Stop.PORT.LINE <-0
          stop.PORT.MAPrice <-0
          stop.PORT.RSIPrice <-0
          # create.price.dynamic <-0
          
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
          
          m_msg(paste("[動作] 執行空頭停利價位 :", Price.curr, "<", Price.buyin.PRE
                      , switch.create.positionSHORT
                      , Price.PCL
                      , Price.diff
                      , .stopPORT.price.LONG))
          beep(sound = 8)
          CROSS.Stop.PORT.LINE <-0
          stop.PORT.MAPrice <-0
          stop.PORT.RSIPrice <-0
          # create.price.dynamic <-0
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
                  , trans.lang(mode=CODE.SIMU, param=simu), currentbar.num)
      ) 
      
      #定期刪除價位及指標相關資料檔，以避免拖慢系統速度
      del.count <-del.count +1
      if(del.count >=del.count.limited)
      {
        del.count =0
        multi.file.remove(.path = price.path, .pattern = ".csv", .tail_keep = 1)
      }
    }#by action="E"
    
  }#by repeat loop
}

