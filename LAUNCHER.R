#source("C:/Users/linus/Documents/Project/6.APITols/LAUNCHER.R")
#
#¥þ­±±Ò°Ê

print("[1] FutureTools_LITE")
print("[2] PT.OS")
print("")
action <- readline("Pls Enter INDEX :")

switch (action,
  "1" ={
    source("C:/Users/linus/Documents/Project/6.APITols/FutureTools_LITE.R")
    FutureTools_LITE()
  },
  "2" ={
    source("C:/Users/linus/Documents/Project/8.Research.Material/NEW_GENERATION/PT.init.R")
    source("C:/Users/linus/Documents/Project/8.Research.Material/NEW_GENERATION/PT.MicroKERNEL.R")
    MicroKernal()
    rm(MicroKernal)
  }
)

#


