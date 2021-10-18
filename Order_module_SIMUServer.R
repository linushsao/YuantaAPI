#


SIMU.DATA.Server <- function()
{

    # sleep.time <- 0.5
    VERBOSE <- FALSE
    date.reset <-NULL
    f.time <- NULL
    .sl.time.PRE <- NULL
    
    # log.filename <- paste0(msg.path, filename.gen(x="log"))
    unlink(MXFSIMU.forSERVER.filename)
    file.create(MXFSIMU.forSERVER.filename)
    
    #功能函式
    #設定虛擬伺服器資料源
    MXFSIMU.switch <-function()
    {
      
      repeat
      {
        
        .Product.file <- filename.gen(name=date.reset)
        MXFSIMU.source.data.path <-finacial.dataparg.gen(realdata.path, date.reset, Product, .Product.file)
        if(file.exists(MXFSIMU.source.data.path))
        {
          
          file.copy(MXFSIMU.source.data.path, msg.path)
          sourc.filename <- paste0(msg.path, filename.gen(x="product", date.reset))
          desti.filename <- paste0(msg.path, filename.gen(x=MXFSIMU.Name))
          unlink(desti.filename)
          file.rename(sourc.filename, desti.filename)
          return(TRUE)
          break
        }
        else{
          print(paste("[錯誤] 虛擬資料伺服器:", "原始資料檔案不存在 ", MXFSIMU.source.data.path))
          return(FALSE)
        }
      }
    }
    
    DATA.Generator <- function(verbos =FALSE)
    {
      #虛擬伺服器價格原始檔案位置
      desti.filename <- paste0(msg.path, filename.gen(x=MXFSIMU.Name))
      fromTIME.math <- FALSE
      # csv.desti.filename <- gsub(".txt", ".csv", desti.filename)
      # file.rename(desti.filename, csv.desti.filename)
      # print(csv.desti.filename)
      if(file.exists(desti.filename))
      {
        
        data.source <- read.csv(desti.filename, header=FALSE)
        colnames(data.source) <- NULL
        data.row <- nrow(data.source)

        for(miu in 1:data.row)
        {
          
          if(file.exists(DMSS.path))
          {
            unlink(DMSS.path)
            break
          }
          
          single.line <- data.source[miu, ]
          .col.data <-as.character(single.line[2])

          .sl.time <- strsplit(.col.data, ":")
          .sl.time <- as.numeric(.sl.time[[1]][3])
          .sl.time <- .sl.time -floor(.sl.time)
          
          if(!is.null(f.time) && !fromTIME.math) #設定快轉時間 未到該時間
          {
            # .col.data <-as.character(single.line[2])
            .check <- length(grep(f.time, .col.data))
            # .sl.time <- strsplit(.col.data, ":")
            # .sl.time <- as.numeric(.sl.time[[1]][3])
            # .sl.time <- .sl.time -floor(.sl.time)
            
            if(.check !=0){fromTIME.math <-TRUE}
          }
          if(VERBOSE)
          {

            if(is.null(f.time) || fromTIME.math) #無設定快轉或 達快轉點
            {
              print(single.line)
              append.to.file(data = single.line, path= MXFSIMU.forSERVER.filename)
              
              if(is.null(.sl.time.PRE))
              {
                .sl.time.PRE <-.sl.time}
              
              .sl.time.CURR <- abs(.sl.time -.sl.time.PRE)
              Sys.sleep(.sl.time.CURR) 
              
              .sl.time.PRE <-.sl.time
            }
            else{
              print(paste(.col.data, .check))
              
            }
          }

        }
      }
      else{
        print(paste("[錯誤] 虛擬資料伺服器:", "原始資料檔案不存在 ", csv.desti.filename))
        return(FALSE)
      }
      
    }
    
    append.to.file <- function(data, path)
    {
      write.table(data, file=path, sep=","
                  , row.names=F, na = "NA", 
                  append=TRUE,col.names=FALSE)
    }

    ##
    #主程式
    
    MXFSIMU.filename <- paste0(msg.path, filename.gen(x=MXFSIMU.Name))
    
    if(!file.exists(MXFSIMU.filename))
    {
      
      repeat
      {
        print(paste("[錯誤] 虛擬資料伺服器:", "未設定原始資料或檔案不存在"))
        print(paste("[動作] 虛擬資料伺服器:", "產生虛擬資料"))
        date.reset <- readline(paste0("NEW SOURCE DATA.NAME? :"))
        if(MXFSIMU.switch())
        {
          print(paste("[動作] 虛擬資料伺服器:", "資料準備完畢!!!"))
          break          
        }
        
      }

    }
  
    repeat
    {   
      #產生MENU
      print(paste0("DataTIME           : ", ifelse(!is.null(date.reset)
                                                   ,date.reset
                                                   ,date.format))) 
      print(paste0("Product            : ", Product)) 
      # print(paste0("Price Freq.        : ", sleep.time)) 
      print(paste0("Verbose            : ", VERBOSE))
      print(paste0("FROM TIME          : ", f.time))      
      
      print(" ")   
      
      # print("(EDPC)ENABLE.MXFSIMU.Server")
      print("(SSDS)SWITCH.MXFSIMU.source")
      print("(PF)Price Freq. ")
      print("(VS)Verbose SWITCH ") 
      print("(DS)DATA Generator ")   
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
