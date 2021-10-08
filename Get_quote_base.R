#設置當天日期，取用當天的檔案名稱
Date <- gsub("-","",Sys.Date())
#檔案位置
DataPath <- "C:/Users/linus/Documents/Project/9.Shared.Data/8.forSmartAPI/"
#tail執行檔位置
ExecPath <- "C:/Users/linus/Documents/Project/6.APITols/YuantaSmart API/"

#取得成交資訊
GetMatchData <- function(DataPath,Date,Prodid)
{
  data <- system(paste0(ExecPath,'tail.exe -n1 ',DataPath,Date,"/",Prodid,"/",Date,"_Match.txt"),intern=TRUE)
  mdata <- strsplit(data,",")
  return(mdata)
}

#取得上下五檔價資訊
GetUpDn5Data <- function(DataPath,Date,Prodid)
{
  data <- system(paste0(ExecPath,'tail.exe -n1 ',DataPath,Date,"/",Prodid,"/",Date,"_UpDn5.txt"),intern=TRUE)
  mdata <- strsplit(data,",")
  return(mdata)
}

#判斷成交資訊是否更新
isMatchUpdate <- function(DataPath,Date,Prodid,lastTime,freq)
{
  data <- tryCatch(system(paste0(ExecPath,'tail.exe -n1 ',DataPath,Date,"/",Prodid,"/",Date,"_Match.txt"),intern=TRUE)
                   ,error=function(e) return("nodata"), warning=function(w) return("nodata"))
  # data <- system(paste0(tailPath,'tail.exe -n1 ',DataPath,Date,"/",Prodid,"/",Date,"_Match.txt"),intern=TRUE)
  mdata <- strsplit(data,",")[[1]]
  
  if(data=="nodata"){
    return("nodata")
  }else{
    # 小時k,分k,分k
    if(freq=="hour"){
      newTime <- as.numeric(substr(mdata[2],1,2))
    }else if(freq=="min"){
      newTime <- as.numeric(paste0(substr(mdata[2],1,2),substr(mdata[2],4,5)))
    }else if(freq=="sec"){
      newTime <- as.numeric(paste0(substr(mdata[2],1,2),substr(mdata[2],4,5),substr(mdata[2],7,8)))
    }
    
    if(lastTime!=newTime){
      return(TRUE) # 資料有更新
    }else{
      return(FALSE) # 資料無更新
    }
  }
}

#判斷上下五檔資訊是否更新
isUpDn5Update <- function(DataPath,Date,lastTime,freq)
{
  data <- tryCatch(system(paste0(tailPath,'tail.exe -n1 ',DataPath,Date,"/",Prodid,"/",Date,"_UpDn5.txt"),intern=TRUE)
                   ,error=function(e) return("nodata"), warning=function(w) return("nodata"))
  mdata <- strsplit(data,",")[[1]]
  
  if(data=="nodata"){
    return("nodata")
  }else{
    # 小時k,分k,分k
    if(freq=="hour"){
      newTime <- as.numeric(substr(mdata[2],1,2))
    }else if(freq=="min"){
      newTime <- as.numeric(paste0(substr(mdata[2],1,2),substr(mdata[2],4,5)))
    }else if(freq=="sec"){
      newTime <- as.numeric(paste0(substr(mdata[2],1,2),substr(mdata[2],4,5),substr(mdata[2],7,8)))
    }
    
    if(lastTime!=newTime){
      return(TRUE) # 資料有更新
    }else{
      return(FALSE) # 資料無更新
    }
  }
}