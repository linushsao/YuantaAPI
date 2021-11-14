
shortcut.key <-function()
{
  print("")
  print("")
  
  Price <- Price.current()
  
  print(paste0("-----assign( c(O, I, C, R), MA )-----"))
  
  print(paste0("DataTIME           : ", date.format)) 
  print(paste0("Product            : ", ifelse(switch.DATA.Source, Product, MXFSIMU.Name) )) 
  print(paste0("Price              : ", Price)) 
  print(paste0("Quantity           : ", Qty)) 
  print(paste0("BorS               : ", BorS))
  print(paste0("MODE.XFSource      : ", trans.lang(mode="SECURTIES", switch.DATA.Source)))  
  print(paste0("PATH.XFSource      : ", data.source.switch(get.conf(name="switch.DATA.Source"
                                                                    , dataset = dataset.name))))  
  
  print("")
  
  print("-----COMMON FUNCTION-----")
  print("(QR)QueryRight")
  print("(CP)ChangePRodid")
  print("(QA)QueryAllOrder")
  print("(QO)QueryOnOpen")
  print("(QU)QueryUnfinished")
  print("(OL)Place.OrderLMT")
  print("(OM)Place.OrderMKT")
  print("(PR)oduct bundle")
  print("(P)rice bundle")
  print("(Q)uantity bundle")
  print("(BS)Buy|Sell bundle")
  print("(SPT)StopPORT.TYPE")
  print("(SLT)StopLOSS.TYPE")
  print("(DT)_switch_DayTRADE")  
  print("(PRB)Price.buyin")
  print("(PCL)PCL")
  print("(SPUT)S&P Unbreaked times")
  print("(APC)switch_Auto.pos.CLOSE")
  print("(SDP)switch_defaultPORT") 
  print("(SDP)switch_defaultPORT")
  print("(SS)switch_Simulation") 
  print("(RSS)REMOTE switch_Simulation")
  print("(EPPT)EXPORT PTConf")
  print("(SSPM)SWITCH StopPORT.MA")
  print("(SSPR)SWITCH StopPORT.RSI")
  print("")
  
  print("-----AGENT.SERVER FUNCTION-----")
  print("(EAS)ENABLE_AGENT.SERVER") 
  print("(DAGS)DISABLE_AGENT.SERVER") 
  print("(RAGS)RESET_AGENT.SERVER") 
  print("(RSS)REMOTE.SWITCH.SIMU_AGENT.SERVER") 
  print("")
  
  print("-----PORTFOLIO.MINITOR FUNCTION-----")
  print("(EPM)ENABLE_PORTFOLIO.MINITOR") 
  print("")
  
  print("-----MXFSIMU.SERVER FUNCTION-----")
  print("(EMSS)ENABLE_MXFSIMU.SERVER") 
  print("(DMSS)DISABLE_MXFSIMU.SERVER")
  print("(SMS)SWITCH MFXSource")
  print("")
  
  action <- readline("[COMMAND] :")
  return(action)
}

#測試連線結果
m.action <-readline(paste("[是否測試卷商連線品質...[y/N]"))
if(m.action =="Y" | m.action =="y")
{
  if(connect.test())
  {
    check.result <-(paste("[訊息] 聯線正常."))
  }else{
    check.result <- (paste("[錯誤] 無法建立聯線."))
  }
  
  action <- readline(COMMON.ANYKEY.TO.EXIST) 
}


##主程式
#ChangeProd()

main.menu <-function()
{
  
  Price <- Price.current()
  print(" ")
  print(" ")
  
  print(paste0("-----assign( c(O, I, C, R), MA )-----"))
  print(paste0("DataTIME           : ", date.format)) 
  print(paste0("Product            : ", ifelse(switch.DATA.Source, Product, MXFSIMU.Name) )) 
  print(paste0("Price              : ", Price)) 
  print(paste0("Quantity           : ", Qty)) 
  print(paste0("BorS               : ", BorS))
  
  print(paste0("STABLE S.P.        : ", default.enable_stopPORTFOLIO)) 
  print(paste0("AUTO.S.P.          : ", Stop_portfolio)) 
  print(paste0("Auto.pos.CLOSE     : ", Auto.positionCLOSE))
  print(paste0("Max.DDM            : ", Max.DDM))
  print(paste0("DayTRADE           : ", Daytrade))
  print(paste0("Price.buyin        : ", get.conf(name="price.Buyin", dataset = dataset.name)))
  print(paste0("PCL                : ", get.conf(name="price.PCL", dataset = dataset.name))) 
  print(paste0("switch.stopPORT.MA : ", switch.stopPORT))
  print(paste0("switch.stopPORT.RSI: ", switch.stopPORT_RSI))
  
  print(paste0("S&P Unbreaked time : ", Price.reachLIMITED.times.Limited))
  print(paste0("MODE.XFSource      : ", trans.lang(mode="SECURTIES", switch.DATA.Source))) 
  print(paste0("PATH.XFSource      : ", data.source.switch(get.conf(name="switch.DATA.Source"
                                                                    , dataset = dataset.name))))  
  print(paste0("Simulation         : ", simu))  
  
  print(" ")
  print("(SCK) ShortCUT KET MAP")
  print("(CNT) CONNECTION TEST")

}

