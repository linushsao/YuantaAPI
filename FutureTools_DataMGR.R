
extra.data <-function(name="CL")
{
  
  #
  meta.path <- paste0(price.path, "meta_record.csv")
  meta.record <- m.tail(meta.path)
  names(meta.record) <-meta.record.name

  operator <- name
  
  switch(operator,
         
         OP ={return(as.character(meta.record[4]))},
         HI ={return(as.character(meta.record[5]))},
         CL ={return(as.character(meta.record[7]))},
         LO ={return(as.character(meta.record[6]))},
         MA5 ={return(as.character(meta.record[9]))},
         MA10 ={return(as.character(meta.record[10]))},
         MA20 ={return(as.character(meta.record[11]))},
         Research_Line_Upper ={return(as.character(meta.record[12]))},
         Research_Line_Mid ={return(as.character(meta.record[13]))},
         Research_Line_lower ={return(as.character(meta.record[14]))},
         extremes_Line_Upper ={return(as.character(meta.record[15]))},
         extremes_Line_Mid ={return(as.character(meta.record[16]))},
         extremes_Line_lower ={return(as.character(meta.record[17]))},
         B_UP ={return(as.character(meta.record[18]))},
         B_LO ={return(as.character(meta.record[19]))},
         RSI ={return(as.character(meta.record[20]))},
         BSRate ={return(as.character(meta.record[21]))},

         N.ploar_star ={return(as.character(meta.record[22]))},
         N.ploar_star_price ={return(as.character(meta.record[24]))},
         N.ploar_star_stopLoss ={return(as.character(meta.record[25]))},

         S.ploar_star ={return(as.character(meta.record[23]))},
         S.ploar_star_price ={return(as.character(meta.record[26]))},
         S.ploar_star_stopLoss ={return(as.character(meta.record[27]))},

         currentBar ={return(as.character(meta.record[8]))},
         transaction ={
                       m.path <-paste0(price.path, "transaction", ".csv")
                       m.value <- as.character(m.tail(m.path))
                       return(m.value)
                     }

  )
}

