#' A enchaned env Function


m_env <- function(name=NULL,value=NULL,mode="init_list",dataset="env") {

    #dataset: 
    #1. = default,means handle environment variables
    #2. dataset could be defined to another custom dataset name        
    #         
    FLAG_add <- TRUE
    name.name <- "name"
    name.value <- "value"

    env.add.colname <- function(v){
        config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)] 
        new.col <- rep("_", length(config[,1]))
        names(new.col) <- name
        config <- cbind(config, new.col)
        result <- config
        write.csv(result,file=name)
        
        return(result)
        }
    
    env.db.merge <- function(be.merged,name){
            #backup original conf
            file.copy(name,paste(name,format(Sys.time(), "%b-%d_%X"),sep="."))
            
            be.merged.file <- read.csv(be.merged,header=FALSE)[-c(1),-c(1)] 
                config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)] 
                config <- rbind(config,be.merged.file)
                result <- config
                names(result) <- c(name.name ,name.value)
                
            write.csv(result,file=name)

        return(result)
    }

    env.db.replace <- function(be.replaced,name){
            #backup original conf
            file.copy(name,paste(name,format(Sys.time(), "%b-%d_%X"),sep="."))
            
            config <- read.csv(be.replaced,header=FALSE)[-c(1),-c(1)] 
                result <- config
                names(result) <- c(name.name ,name.value)
            
            write.csv(result,file=name)

        return(result)
    }
    
    env.reset <- function(v,name){
            #backup original conf
            file.copy(name,paste(name,format(Sys.time(), "%b-%d_%X"),sep="."))
            
            unlink(name)
            file.create(name)
            
            result <- data.frame(a=c(v[1]),b=c(v[2]))
            names(result) <- c(name.name ,name.value)
            write.csv(result,file=name)
            
        return(result)
    }

    env.read <- function(x){
        config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)] 
        r.name <- as.vector(t(config[,1]))
        r.value <- as.vector(t(config[,2]))
        for( y1 in 1:length(r.name)) {
            if( x == r.name[y1] ) { 
            result <- r.value[y1]
            return(result)
            }
        }
    }
    
    env.modify <- function(){
            config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)] 
            filter <- as.vector(t(config[,1]))
            for( y1 in 1:length(filter)) {
                if( name == filter[y1] ) { 
                temp <- as.vector(t(config[,2]))
                temp[y1] <- value
                config[,2] <- temp
                write.csv(config,file=conf_name)
                FLAG_add <- FALSE
                    }
                }
            if( FLAG_add ) { # add record
                temp <- data.frame(a=c(name),b=c(value))
                names(config) <- c(name.name ,name.value)
                names(temp) <- c(name.name ,name.value)
                config <- rbind(config,temp)
                write.csv(config,file=conf_name)

            }
        return(config)
    }
    
    env.del <- function(config) {
        config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)] 
        filter <- as.vector(t(config[,1]))
        for( y1 in 1:length(filter)) {
            if( name == filter[y1]  ) { 
            config[y1,] <- NA
            config <- na.omit(config)
            write.csv(config,file=conf_name)
#             FLAG_add <- FALSE
                }
            }
    }

    env.dataset.path.exist <- function(name.env.file_path)
    {

            if (! file.exists(name.env.file_path) ) { # dataset existed
                    env.reset(c(env.file_name,env.file_value),name=name.env.file_path) 

                }
                
            result <- name.env.file_path
            return(result)
    }
        

# main function
#     dataset <- "env"
    env.file_name <- paste(dataset ,"_file" ,sep="")
    env.file_value <- m_paste(c(".",dataset,".Configure"),op="")
    extension <- ".csv"
    default.env.file_path <- m_paste(c(get.sysinfo(),env.file_value,extension),op="")
    default.env.file_path <-  winpath.trans(default.env.file_path, home.dir=c('/home/linus/', 'C:\\Users\\linus\\Documents\\'))
    conf_name <- env.dataset.path.exist(default.env.file_path)
    
    if(mode == "init_reset") {
        config <- env.reset(c(env.file_name,env.file_value),name=conf_name ) #add first record
        result <- config
    }else if(mode == "init_list") {
         #do nothing
    }else if(mode == "add.colname") {
        config <- env.add.colname(name)
        result <- config
        }else if(mode == "db_list") {
            result  <- dataset
            return(result)
            break
        }else if(mode == "db_merge") {
            be.merged <- value
            print(c("1",be.merged ,conf_name))
            config <- env.db.merge(be.merged, name=conf_name)
            result <- config

        }else if(mode == "db_replace") {
            be.replaced <- value
            config <- env.db.replace(be.replaced, name=conf_name)
            result <- config

    }else if(mode == "w") {
        config <- env.modify()
    }else if(mode == "r") {
        return(env.read(name))
        break
     }else if(mode == "d") {
        config <- env.del(config)
    }

        config <- read.csv(conf_name,header=FALSE)[-c(1),-c(1)]
        rownames(config) <- NULL
        names(config) <- c("name","value")
        result <- config
        return(result)
}

# for testing
# a <- "TESTING"
# 
# stop()
# env(name="raw.data.listname",mode="r")
#
# name="ORDER"
# value="FRUIT"
# mode="w"
# v <- c(env.file_name,env.file_value)
rm(list=ls())
# v[2]
m_env()
m_env(mode="db_list")

m_env(value="/home/linus/ProjectStock/all_stocks/.env.Configure.csv",mode="db_merge")
m_env(value="/home/linus/ProjectStock/all_stocks/.env.Configure.csv",mode="db_replace")

m_env(dataset="env")
m_env(mode="db_list")
m_env(dataset="new")
m_env(mode="db_list",dataset="new")

m_env(mode="init_reset")
m_env(mode="init_reset",dataset="new")

(m_env(name="list",value="NO",mode="w"))
(m_env(name="list",value="YES",mode="w"))
(m_env(name="ORDER",value="FRUIT",mode="w"))
(m_env(name="ORDER",value="BANANA",mode="w"))

(m_env(name="ORDER",value="BANANA",mode="d"))
(m_env(name="list",mode="d"))

m_env(dataset="new")
(m_env(name="list",value="NO",mode="w",dataset="new"))
(m_env(name="list",mode="r",dataset="new"))
(m_env(name="list",value="YES",mode="w",dataset="new"))
(m_env(name="ORDER",value="FRUIT",mode="w",dataset="new"))
(m_env(name="ORDER",value="BANANA",mode="w",dataset="new"))

(m_env(name="ORDER",value="BANANA",mode="d",dataset="new"))
(m_env(name="list",mode="d",dataset="new"))

(m_env(name="index.yahoo",mode="r",dataset="dataset.MGR"))
#

head(m_env(name="prefix.raw.data.name",mode="d"))
head(m_env(mode="init_list"))

head(m_env(mode="init_reset"))



