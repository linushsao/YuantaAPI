
source.path <-"C:/Temp/0_Group_RAW"


title.name <-c(
  "date"
  , "time"
  , "barInterval"
  , "open"
  , "high"
  , "low"
  , "close"

  , "currentBar"

  , "sma5"
  , "sma10"
  , "sma20"
  
  , "bupper"
  , "blower"
  
  , "RSI1"
  , "BSRate"
)

Group.name.df <-data.frame(
  
  stock.no=c("TSE12.TW",
             "TSE18.TW",
             "TSE20.TW",
             "TSE15.TW",
             "TSE11.TW",
             "TSE25.TW",
             "TSE99.TW",
             "TSE16.TW",
             "TSE28.TW",
             "TSE14.TW",
             "TSE19.TW",
             "TSE17.TW",
             "TSE22.TW",
             "TSE27.TW",
             "TSE13.TW",
             "TSE29.TW",
             "TSE21.TW",
             "TSE26.TW"),
  
  stock.gname=c("食品",
             "玻璃陶瓷",
             "鋼鐵",
             "電機機械",
             "水泥",
             "建材營造",
             "其他",
             "電器電纜",
             "金融保險",
             "紡織纖維",
             "造紙",
             "化學生技醫療",
             "汽車",
             "觀光",
             "塑膠",
             "貿易百貨",
             "橡膠",
             "航運業")
  
)


