###########################
# STEP 1: Load the data
###########################

#get last n char of string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
#Load all data

tfv_file <- list.files("..\\Data\\")
tfv_file2 <- list.files("..\\Dropbox\\Data\\")

buses <- data.frame()

if(length(tfv_file)>0) {
  setwd("..\\Data\\")
  data_dir <- getwd()
  for(i in 1:length(tfv_file))
  {
    #handle zipped files
    if (substrRight(tfv_file[i], 4) == ".zip"){
      tmp <- tempfile()
      unzip(tfv_file[i], exdir = tmp)
      file.remove(tfv_file[i])
      tmpfiles <- list.files(tmp)
      setwd(tmp)
      for(j in 1:length(tmpfiles)){
        file.rename(tmpfiles[j],paste0(gsub("[ :-]","",Sys.time())," ",tmpfiles[j]))
        newtmpfiles <- list.files(tmp)
        file.move(newtmpfiles[1],data_dir)
      }
      Sys.sleep(1)
    }
  }
  setwd(data_dir)
  tfv_file <- list.files()
  for(i in 1:length(tfv_file))
  {
    new_file <- suppressWarnings(fread(tfv_file[i], 
                                       select = c(1:7)))
    names(new_file)[1] <- "Resource Name"
    buses <- rbind(buses,new_file)
  }
}

if(length(tfv_file2)>0) {
  setwd("..\\Dropbox\\Data\\")
  data_dir <- getwd()
  for(i in 1:length(tfv_file2))
  {
    #handle zipped files
    if (substrRight(tfv_file2[i], 4) == ".zip"){
      tmp <- tempfile()
      unzip(tfv_file2[i], exdir = tmp)
      file.remove(tfv_file2[i])
      tmpfiles <- list.files(tmp)
      setwd(tmp)
      for(j in 1:length(tmpfiles)){
        file.rename(tmpfiles[j],paste0(gsub("[ :-]","",Sys.time())," ",tmpfiles[j]))
        newtmpfiles <- list.files(tmp)
        file.move(newtmpfiles[1],data_dir)
      }
      Sys.sleep(1)
    }
  }
  setwd(data_dir)
  tfv_file2 <- list.files()
  for(i in 1:length(tfv_file2))
  {
    buses <- rbind(buses,suppressWarnings(fread(tfv_file2[i], 
                                                select = c(1:7))))
  }
  setwd("..\\")
}

if (length(buses)>0) {
  
  names(buses) <- gsub("[ ^[:blank:]:()+?&/\\-]", ".", names(buses))
  buses <- unique(buses)
  
  cred = fromJSON(file = "..//dbCred.json")
  
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = "NIMP",
                   host = cred$host, port = cred$port,
                   user = cred$user, password = cred$password)
  rm(cred) # removes connection info
  routes <- dbGetQuery(con,'Select * from master.busroutes')
  RPostgreSQL::dbDisconnect(con)
  routes$interchange <- ifelse(routes$interchange=="","-",routes$interchange)
}

names(buses)[1] <- "Resource.Name"