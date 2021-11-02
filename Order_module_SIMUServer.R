#
SIMU.DATA.Server <- function()
{
  
  VERBOSE <- TRUE
  date.reset <-NULL
  f.time <- NULL
  .sl.time.PRE <- NULL
  
  #功能函式
  ##將向量組合成單一字串
  msg.price <-function(x)
  {
    .msg <-as.character(x)
    leng <- length(.msg)
    .msg.all <-c()
    
    for(miu in 1:leng)
    {
      .msg.all <-paste(.msg.all, .msg[miu])
    }
    
    return(.msg.all)
    
  }
  
  #設定虛擬伺服器資料源
  MXFSIMU.switch <-function()
  {
    repeat
    {
      
      .Product.file <- filename.gen(name=date.reset)
      MXFSIMU.source.data.path <-finacial.dataparg.gen(realdata.path, date.reset, Product, .Product.file)
      if(if.Valid.file(MXFSIMU.source.data.path))
      {
        
        file.copy(MXFSIMU.source.data.path, msg.path)
        sourc.filename <- paste0(msg.path, filename.gen(x="product", date.reset))
        desti.filename <- paste0(msg.path, filename.gen(x=MXFSIMU.Name))
        
        file.rename(sourc.filename, desti.filename)
        # set.conf(name="MXFSIMU.source.data.path"
        #          , value =  MXFSIMU.source.data.path 
        #          , dataset = dataset.name)
        return(TRUE)
        break
      }
      else{
        return(FALSE)
      }
    }
  }
  
  DATA.Generator <- function(verbos =FALSE)
  {
    #虛擬伺服器價格原始檔案位置
    desti.filename <- paste0(msg.path, filename.gen(x=MXFSIMU.Name))
    fromTIME.math <- FALSE
    .forward.index <-c()
    
    if(if.Valid.file(desti.filename))
    {
      #讀取資料源
      data.source <- read.csv(desti.filename, header=FALSE)
      colnames(data.source) <- NULL
      data.row <- nrow(data.source)
      
      if(!is.null(f.time) && is.null(.forward.index)) #設定快轉時間 未尋找該時間索引編號
      {
        .forward.index <- as.numeric(rownames(data.source[grep(f.time, data.source[,2]), ]))
      }
      
      .start <-ifelse(is.null(.forward.index), 1, .forward.index[1])
      
      for(miu in .start:data.row)
      {
        
        if(!is.null(get.conf(name="DMSS", dataset = dataset.name)))
        {
          rm.conf(name="DMSS", dataset = dataset.name)
          break
        }
        
        single.line <- data.source[miu, ]
        .col.data <-as.character(single.line[2]) #時間
        
        #計算訊息顯示間隔
        .sl.time <- strsplit(.col.data, ":")
        .sl.time <- as.numeric(.sl.time[[1]][3])
        .sl.time <- .sl.time -floor(.sl.time)
        
        # if(!is.null(f.time) && !fromTIME.math) #設定快轉時間 未到該時間
        # {
        #   .forward.index <- grep(f.time, .col.data)
        # 
        #   if(.check !=0){fromTIME.math <-TRUE}
        #   
        # }else{ 
          # if(is.null(f.time) || fromTIME.math) #無設定快轉 或 達快轉點
          # {
            
          #寫入模擬檔案
          while(TRUE)
          {
            if(file.access(MXFSIMU.forSERVER.filename, mode=2) ==0) #for write permission
            {
              #匯入檔案<只有一行>讓其他程式分析
              append.to.file(data = single.line, path= MXFSIMU.forSERVER.filename, m.append = FALSE)
              break
            }
            
          }
             
          if(VERBOSE)
          {
            #顯示價位跳動在螢幕上目視確認
            v.single.line <-single.line
            v.single.line[3] <- paste0("< ", v.single.line[3])
            v.single.line[4] <- paste0(v.single.line[4], " >")
            .msg <-msg.price(v.single.line)
            
            print(.msg)
          }
            
            if(is.null(.sl.time.PRE))
            {.sl.time.PRE <-.sl.time}
            
            .sl.time.CURR <- abs(.sl.time -.sl.time.PRE)
            Sys.sleep(.sl.time.CURR) 
            
            .sl.time.PRE <-.sl.time
          # }
          # else{
          #   print(paste(.col.data, .check))
          # }
        # }
        
      }
    }
    else{
      m_msg(paste(MXFSIMU.SOURCE.UNAVILABLE, desti.filename))
      return(FALSE)
    }
    
  }
  
  ##
  #主程式
  
  MXFSIMU.filename <- paste0(msg.path, filename.gen(x=MXFSIMU.Name))
  
  pre.date.reset <-get.conf(name="date.reset", dataset = dataset.name)
  pre.date.reset <-ifelse(is.null(pre.date.reset), "", pre.date.reset)
  
  action <- readline(paste0("[訊息] 虛擬資料伺服器 :是否重設原始資料位置 <", pre.date.reset, "> (y/N)"))
  if(action =="Y" || action =="y")
  {
    repeat
    {
      m_msg(paste("[動作] 虛擬資料伺服器:", "產生虛擬資料"))
      date.reset <- readline(paste0("請輸入原始資料日期(YYMMDD ", pre.date.reset, ") :"))
      #無輸入則以預設資料日期代替
      if(date.reset ==""){date.reset <-pre.date.reset}
      
      if(MXFSIMU.switch())
      {
        m_msg(paste("[動作] 虛擬資料伺服器:", "資料準備完畢!!!"))
        set.conf(name="date.reset", value = date.reset, dataset = dataset.name)
        m.action <- readline(COMMON.ANYKEY.TO.EXIST) 
        break          
      }else{
        m_msg(paste(MXFSIMU.SOURCE.UNAVILABLE, "< ", date.reset, ">"))
        m.action <- readline(COMMON.ANYKEY.TO.EXIST)
      }
    }
  }else if(!file.exists(MXFSIMU.filename))
  {
    repeat
    {
      m_msg(paste(MXFSIMU.SOURCE.UNAVILABLE))
      m_msg(paste("[動作] 虛擬資料伺服器:", "開始產生虛擬資料"))
      date.reset <- readline(paste0("請輸入原始資料日期(YYMMDD) :"))
      if(MXFSIMU.switch())
      {
        m_msg(paste("[動作] 虛擬資料伺服器:", "資料準備完畢!!!"))
        m.action <- readline(COMMON.ANYKEY.TO.EXIST) 
        break          
      }else{
        m_msg(paste(MXFSIMU.SOURCE.UNAVILABLE, csv.desti.filename))
      }
    }
    
  }else if(file.exists(MXFSIMU.filename))
  {
    date.reset <-get.conf(name="date.reset",dataset = dataset.name)
    m_msg(paste("[動作] 虛擬資料伺服器:", "資料準備完畢!!!"))
    m.action <- readline(COMMON.ANYKEY.TO.EXIST) 
    
  }
  
  repeat
  {   
    #產生MENU
    print(" ")   
    print(" ")   
    print(paste0("-----assign( c(O, I, C, R), MA )-----"))
    
    print(paste0("DataTIME           : ", ifelse(!is.null(date.reset)
                                                 ,date.reset
                                                 ,date.format))) 
    print(paste0("Product            : ", Product)) 
    # print(paste0("Price Freq.        : ", sleep.time)) 
    print(paste0("Verbose            : ", VERBOSE))
    print(paste0("FROM TIME          : ", f.time))      
    print(paste0("MODE.XFSource      : ", trans.lang(mode="SECURTIES"
                                                     , get.conf(name="switch.DATA.Source", dataset = dataset.name))))
    print(paste0("PATH.XFSource      : ", data.source.switch(get.conf(name="switch.DATA.Source"
                                                                      , dataset = dataset.name))))

    print(" ")   
    
    print("(SSDS)SWITCH.MXFSIMU.source")
    print("(PF)Price Freq. ")
    print("(VS)Verbose SWITCH ") 
    print("(DG)DATA Generator ")   
    print("(FT)gen FromTIME(XX:XX:XX) ")
    print("(RE)RESET ") 
    
    #
    action <- readline("[COMMAND] :")
    
    #
    if (action != "")
    {
      switch(action,
             
             EDPC ={},
             SSDS ={MXFSIMU.switch()},
             # PF ={
             #   sleep.time <- readline(paste0("Price Freq.(", sleep.time,") :"))
             # },
             VS ={VERBOSE <-TF.Switch(VERBOSE)},
             DG ={DATA.Generator()},
             FT ={
               f.time <- readline("fromTIME(HH:MM:SS) :")
             },
             RE ={
               VERBOSE <-FALSE
               f.time <- NULL
             },
             QQ ={break},
             
             print(paste0("Command is not correct. [", action, "]"))
             
      )
    }    
    
  }  
  
  
}

# print(paste(MXFSIMU.SOURCE.REDIFINE))
# 
# #從最近日期回頭尋找可替代之卷商資料源
# alldir.list <-list.dirs(path=realdata.path, recursive=FALSE)
# alldir.list <- gsub("//", "/", alldir.list)
# leng <- length(alldir.list)
# while(TRUE)
# {
#   allfile.list <- list.files(path = alldir.list[leng]  , recursive = TRUE)
#   if(length(allfile.list) !=0)
#   {
#     print(paste("[訊息] 資料源切換模式 :", switch.DATA.Source, data.path))
#     break
#     }
#   if(leng >1)
#   {
#     leng <-leng -1
#   }else{
#     print(paste("[訊息] 資料源切換模式 :", switch.DATA.Source, data.path))
#   }
#   
# }
# switch.DATA.Source <- TF.Switch(switch.DATA.Source)
# print(paste(MXFSIMU.SOURCE.UNAVILABLE, data.path.tmp))
# print(paste("[訊息] 目前模式 :", switch.DATA.Source, data.path))
