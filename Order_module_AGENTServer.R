#### AGENT ####
Position.AGENT<-function()
{
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
  
  DAGS.path <- extra.data(name="DAGS", p.mode = "path") 
  
  unlink(path.closeALLPosition)
  unlink(path.price.Buyin)
  unlink(path.price.PCL)
  unlink(path.create.positionLONG)
  unlink(path.create.positionSHORT)
  
  repeat
  {
    #產生MENU
    print(paste0("Simulation         : ", simu))       
    # 
    print(" ")
  
    # print("(EDPC)ENABLE.MXFSIMU.Server")
    # print("(SSDS)SWITCH.MXFSIMU.source")
    # print("(PF)Price Freq. ")
    # print("(VS)Verbose SWITCH ") 
    # print("(DS)DATA Generator ")   
    # print("(FT)gen FromTIME(XX:XX:XX) ")
    # print("(RE)RESET ") 
    
    #
  action <- readline("ENABLE AGENT.SERVER(Y):")   

  while(action == "Y")
  {

    #計算目前狀況
    if(file.exists(DAGS.path))
    {
      unlink(DAGS.path)
      unlink(path.closeALLPosition)
      unlink(path.price.Buyin)
      action =NULL
      break
    }
    
    #目前價位
    Price.curr <- as.numeric(Price.current())
    # Price.curr <- extra.data(name="CL")
    # Price.open <- extra.data(name="OP")
    # Price.high <- extra.data(name="HI")
    # Price.low <- extra.data(name="LO")
    
    if(switch.DATA.Source)
    {
      Research_Line_Upper <- extra.data(name="Research_Line_Upper")
      Research_Line_lower <- extra.data(name="Research_Line_lower")
      extremes_Line_Upper <- extra.data(name="extremes_Line_Upper")
      extremes_Line_lower <- extra.data(name="extremes_Line_lower")
      Bolling_Line_upper <- extra.data(name="B_UP")
      Bolling_Line_lower <- extra.data(name="B_LO")
    }else{
      #儘量不用
      Research_Line_Upper <- max(c(ifelse(Research_Line_Upper ==0, Price.curr, Research_Line_Upper), Price.curr.PRE))
      Research_Line_lower <- min(c(ifelse(Research_Line_lower ==0, Price.curr, Research_Line_lower), Price.curr.PRE))
      
      Price.culster.limited <- push.pull(Price.culster.limited, p.mode=FALSE)
      Price.culster.limited[1] <- Price.curr
      extremes_Line_Upper <- max(Price.culster.limited)
      extremes_Line_lower <- min(Price.culster.limited)    
    }
    
    .stopLOSS.price.LONG <- max(c(extremes_Line_lower, Bolling_Line_lower))
    .stopLOSS.price.SHORT <- min(c(extremes_Line_Upper, Bolling_Line_upper)) 
    .stopPORT.price.LONG <- max(c(extremes_Line_Upper, Bolling_Line_upper))
    .stopPORT.price.SHORT <- min(c(extremes_Line_lower, Bolling_Line_lower))  
    
    #計算價格變動
    Price.buyin.PRE <- extra.data(name="price.Buyin")
    Price.PCL <- extra.data(name="price.PCL")
    
    Price.diff <- Price.curr -Price.buyin.PRE
    

    #已全平倉
    if(file.exists(path.closeALLPosition))
    {
      unlink(path.closeALLPosition)
      unlink(path.price.Buyin)
      unlink(path.price.PCL)
      unlink(path.create.positionLONG)
      unlink(path.create.positionSHORT)
      
      switch.create.positionLONG =FALSE
      switch.create.positionSHORT =FALSE
      # Research_Line_Upper <- 0
      # Research_Line_lower <- 0
      # extremes_Line_Upper <- 0
      # extremes_Line_lower <- 0

    }    
    #已開多倉
    if(file.exists(path.create.positionLONG))
    {
      unlink(path.create.positionLONG)
      switch.create.positionLONG =TRUE
      alarm.msg <- "SL.PL"
      # .stopLOSS.price.LONG <- min(c(Research_Line_lower, extremes_Line_lower)) 
      # .stopLOSS.price.LONG <- min(c(extremes_Line_lower, Bolling_Line_lower))
      
      # Research_Line_Upper <- 0
      # Research_Line_lower <- 0
      # extremes_Line_Upper <- 0
      # extremes_Line_lower <- 0
    }
    #已開空倉
    if(file.exists(path.create.positionSHORT))
    {
      unlink(path.create.positionSHORT)
      switch.create.positionSHORT =TRUE
      alarm.msg <- "SL.PS"
      # .stopLOSS.price.SHORT <- max(c(Research_Line_Upper, extremes_Line_Upper))
      # .stopLOSS.price.SHORT <- max(c(extremes_Line_Upper, Bolling_Line_upper)) 
      
      # Research_Line_Upper <- 0
      # Research_Line_lower <- 0
      # extremes_Line_Upper <- 0
      # extremes_Line_lower <- 0
    } 
    
    #AGENT FUNCTION
    ##
    
    if(switch.create.positionLONG && 
       Price.PCL ==for.LONG &&
       !TRENDMark.LONG)
    {
      if(Price.curr <.stopLOSS.price.LONG)
      {
        msg.file  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
        file.create(msg.file)
        result <- ClosePositionAll()
        print(paste("[動作] 執行多頭停損價位 :", Price.curr, "<", Price.buyin.PRE
                    , switch.create.positionLONG
                    , Price.PCL
                    , Price.diff
                    , .stopLOSS.price.LONG))
        break        
      }
      if(Price.curr >=.stopPORT.price.LONG)
      {
        msg.file  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
        file.create(msg.file)
        result <- ClosePositionAll()
        print(paste("[動作] 執行多頭停利價位 :", Price.curr, "<", Price.buyin.PRE
                    , switch.create.positionLONG
                    , Price.PCL
                    , Price.diff
                    , .stopPORT.price.LONG))
        break        
      }

      
    }
    if(switch.create.positionSHORT && 
       Price.PCL ==for.SHORT &&
       !TRENDMark.SHORT
       )
    {
      if(Price.curr >.stopLOSS.price.SHORT)
      {
        msg.file  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
        file.create(msg.file)
        result <- ClosePositionAll()
        print(paste("[動作] 執行空頭停損價位 :", Price.curr, "<", Price.buyin.PRE
                    , switch.create.positionSHORT
                    , Price.PCL
                    , Price.diff
                    , .stopLOSS.price.SHORT))  
        break        
      }
      if(Price.curr <=.stopPORT.price.LONG)
      {
        msg.file  <- extra.data(name="close.ALLPOSITION", p.mode = "path") 
        file.create(msg.file)
        result <- ClosePositionAll()
        print(paste("[動作] 執行空頭停利價位 :", Price.curr, "<", Price.buyin.PRE
                    , switch.create.positionSHORT
                    , Price.PCL
                    , Price.diff
                    , .stopPORT.price.LONG))  
        break        
      }
    }    

    if(Price.curr.PRE ==0){Price.curr.PRE <-Price.curr}
    
    print(paste("[", alarm.msg, Price.PCL, "]", ifelse(Price.buyin.PRE ==0, 0, Price.diff)
                , Price.buyin.PRE, ">>", Price.curr
                , "+",.stopLOSS.price.LONG
                , round(ifelse(.stopLOSS.price.LONG ==0,0,.stopLOSS.price.LONG -Price.buyin.PRE), digits = 2)
                , round(ifelse(.stopLOSS.price.SHORT ==0,0,.stopLOSS.price.SHORT -Price.buyin.PRE), digits = 2)
                , .stopLOSS.price.SHORT,"-"
                , "+", .stopPORT.price.LONG, .stopPORT.price.SHORT, "-"
                , "<", Research_Line_Upper, Research_Line_lower
                , extremes_Line_Upper, extremes_Line_lower
                , round(Bolling_Line_upper, digits = 2), round(Bolling_Line_lower, digits = 2), ">"
                , switch.DATA.Source)
                )    
     
    }
  
  }
  
}

