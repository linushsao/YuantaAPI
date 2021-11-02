
stop.Portfolio.lite <- function()
{
  SP.RSI <-35
  SP.MA <-20
  SP.MA.Price <-0
  simu <-TRUE
  price.path <- "C:/Temp/msg/"
  meta.path <- paste0(price.path, "meta_record.csv")
  .pause <-FALSE
  AUTO.STOPLOSS <-FALSE
  
  repeat
  {
    
    #選擇停利方式
    while(TRUE)
    {
      menu_0()
      action.PRE <- get.conf(name="StopPORTFOLIO.action.PRE", dataset = dataset.name)
      if(is.null(action.PRE)){action.PRE <-""}
      
      action <- readline(paste0("PLS. Enter INDEX <", action.PRE,"> :"))
      if(action.PRE !="" || action ==""){action <- action.PRE}
      set.conf(name="StopPORTFOLIO.action.PRE", value = action , dataset = dataset.name)
      
      if(action =="S"){simu <- TF.Switch(simu)}
      
      if(action =="1")
      {
        SP.RSI <- as.numeric(readline(paste0("PLS. Enter RSI.SP ( ",SP.RSI, " ):")))
        break
      }
      
      if(action =="2")
      {
        while(TRUE)
        {
          SP.MA <- as.numeric(readline(paste0("PLS. Enter MA5/10/20 ( MA.",SP.MA, " ):")))
          if(SP.MA %in% c(5,10,20))
          {
            break
          }else{print(paste0("Wrong Param <", SP.MA, ">"))}          
        }

      }
      
      if(action =="QQ")
      {
        .pause <-TRUE
        break
      }
    }
    
    #主程式
    while(TRUE)
    {##WHILE
      
      #檢查主控台命令
      ##檢查停止
      if(!is.null(get.conf(name="DSPL", dataset = dataset.name)))
      {
        rm.conf(name="DSPL", dataset = dataset.name)
        .pause <-TRUE
        break
      }
 
      #切換停損開啟
      m.AUTO.STOPLOSS <-get.conf(name = "AUTO.STOPLOSS"
                                            , dataset = dataset.name)
      
      if(!is.null(m.AUTO.STOPLOSS))
      {
        rm.conf(name = "AUTO.STOPLOSS", dataset = dataset.name)
        AUTO.STOPLOSS <-TF.Switch(AUTO.STOPLOSS)
        m_msg(paste("[設定] 切換主動停損設定 :"
                    , trans.lang(mode="AUTO.STOPLOSS", param=AUTO.STOPLOSS)))
        beep(sound = 2)        
      } 
      
      #切換模擬/真實設定
      m.REMOTE.SWITCH.SIMULATION <-get.conf(name = "REMOTE.SWITCH.SP.SIMULATION"
                                            , dataset = dataset.name)
      
      if(!is.null(m.REMOTE.SWITCH.SIMULATION))
      {
        rm.conf(name = "REMOTE.SWITCH.SP.SIMULATION", dataset = dataset.name)
        simu <-TF.Switch(simu)
        m_msg(paste("[設定] 切換模擬/真實設定 :"
                    , trans.lang(mode="SIMU", param=simu)))
        beep(sound = 2)        
      }  
      
      #切換停利設定
      m.REMOTE.SWITCH.SP <-get.conf(name = "REMOTE.SWITCH.StopPORTFOLIO"
                                    , dataset = dataset.name)
      if(!is.null(m.REMOTE.SWITCH.SP))
      {
        rm.conf(name = "REMOTE.SWITCH.StopPORTFOLIO", dataset = dataset.name)
        action <-as.numeric(m.REMOTE.SWITCH.SP)
        m_msg(paste("[設定] 切換切換停利設定 :"
                    , trans.lang(mode="stop.Portfolio.MODE", param=action)))
        beep(sound = 2)        
      } 
      
      #即時價格路徑
      meta.record <- m.tail(meta.path)
      names(meta.record) <-meta.record.name
      
      #目前價位
      Price.curr <- as.numeric(Price.current())
      
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
      #計算價格變動
      price.Buyin <- as.numeric( ifelse(!is.null(get.conf(name = "price.Buyin", dataset = dataset.name))
                                        , get.conf(name = "price.Buyin", dataset = dataset.name)
                                        , 0) )
      price.PCL <- as.numeric( ifelse(!is.null(get.conf(name = "price.PCL", dataset = dataset.name))
                                      , get.conf(name = "price.PCL", dataset = dataset.name)
                                      , 0) ) 
      
      #主動停損(系統預設布林通道上下軌)
      if(AUTO.STOPLOSS)
      {
        switch (price.PCL,
                
                "1" = {#多頭停損
                  if(Price.curr <=Bolling_Line_lower)
                  {
                    if(!simu)
                    {
                      OrderNO <- ClosePositionAll()
                      
                      m_msg(paste("[動作] 執行多頭停損價位 :", Price.curr))
                      m_msg(paste("[訊息] 回傳交易序號 :", OrderNO))  
                      beep(sound = 7)
                      break
                    }else{
                      m_msg(paste("[模擬] 執行多頭停損價位 :", Price.curr))
                      beep(sound = 2)
                      break
                    }
                    
                  }
                },
                
                "-1" = {#空頭停損
                  if(Price.curr >=Bolling_Line_upper)
                  {
                    OrderNO <- ClosePositionAll()
                    
                    m_msg(paste("[動作] 執行空頭停損價位 :", Price.curr))
                    m_msg(paste("[訊息] 回傳交易序號 :", OrderNO))
                    
                    beep(sound = 8)
                    break
                  }else{
                    m_msg(paste("[模擬] 執行空頭停損價位 :", Price.curr))
                    beep(sound = 2)
                    break
                  }
                }
        )        
      }
      
      
      #RSI停利
      if(action ==1)
      {
        
        switch (price.PCL,
                
                "1" = {#多頭平倉
                  if(price.rsi >SP.RSI)
                  {
                    if(!simu)
                    {
                      OrderNO <- ClosePositionAll()
                      
                      m_msg(paste("[動作] 執行多頭平倉價位 :", Price.curr))
                      m_msg(paste("[訊息] 回傳交易序號 :", OrderNO))  
                      beep(sound = 8)
                      break
                    }else{
                      m_msg(paste("[模擬] 執行多頭平倉價位 :", Price.curr))
                      beep(sound = 2)
                      break
                    }
                    
                  }
                },
                
                "-1" = {#空頭平倉
                  if(price.rsi <SP.RSI *-1)
                  {
                    OrderNO <- ClosePositionAll()
                    
                    m_msg(paste("[動作] 執行空頭平倉價位 :", Price.curr))
                    m_msg(paste("[訊息] 回傳交易序號 :", OrderNO))
                    
                    beep(sound = 8)
                    break
                  }else{
                    m_msg(paste("[模擬] 執行空頭平倉價位 :", Price.curr))
                    beep(sound = 2)
                    break
                  }
                }
        )
        
      }
 
      #MA停利
      if(action ==2)
      {
        # price.ma5 <- as.numeric(meta.record$ma5)
        # price.ma10 <- as.numeric(meta.record$ma10)
        # price.ma20 <- as.numeric(meta.record$ma20)
        switch (SP.MA,
          "5" = {SP.MA.Price <-price.ma5},
          "10" = {SP.MA.Price <-price.ma10},
          "20" = {SP.MA.Price <-price.ma20}
        )
        
        switch (price.PCL,
                "1" = {#多頭平倉
                  if(Price.curr() <SP.MA.Price)
                  {
                    if(!simu)
                    {
                      OrderNO <- ClosePositionAll()
                      
                      m_msg(paste("[動作] 執行多頭平倉價位 :", Price.curr))
                      m_msg(paste("[訊息] 回傳交易序號 :", OrderNO))  
                      beep(sound = 8)
                      break
                    }else{
                      m_msg(paste("[模擬] 執行多頭平倉價位 :", Price.curr))
                      break
                    }
                    
                    
                  }
                },
                
                "-1" = {#多頭平倉
                  if(Price.curr() >SP.MA.Price)
                  {
                    OrderNO <- ClosePositionAll()
                    
                    m_msg(paste("[動作] 執行空頭平倉價位 :", Price.curr))
                    m_msg(paste("[訊息] 回傳交易序號 :", OrderNO))
                    
                    beep(sound = 8)
                    break
                  }
                }
        )
        
      }
      m_msg(paste("[", price.PCL, "]"
                  , trans.lang(mode="stop.Portfolio.MODE", param=action)
                  , Price.curr
                  , price.rsi
                  , auto.stopLOSS
                  , round(Bolling_Line_upper, digits = 2)
                  , round(Bolling_Line_lower, digits = 2)
                  , trans.lang(mode=CODE.SIMU, param=simu), currentbar.num))
      Sys.sleep(0.2)
      
    }#WHILE    
    
    if(.pause){break}
  }
  
  
}