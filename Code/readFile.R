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
  for(i in 1:length(tfv_file))
  {
    if (substrRight(tfv_file[i], 4) == ".zip"){
      tmp <- tempfile()
      unzip(tfv_file[i], exdir = tmp)
      file.remove(tfv_file[i])
      tmpfiles <- list.files(tmp)
      for(j in 1:length(tmpfiles)){
        file.rename(tmpfiles[i],paste0(tmpfiles[i],Sys.time()))
        file.move(tmpfiles[i],getwd())
      }
      Sys.sleep(1)
    }
  }
  tfv_file <- list.files()
  for(i in 1:length(tfv_file))
  {
    buses <- rbind(buses,suppressWarnings(fread(tfv_file[i], 
                                                select = c(1:7))))
  }
}

if(length(tfv_file2)>0) {
  setwd("..\\Dropbox\\Data\\")
  for(i in 1:length(tfv_file2))
  {
    if (substrRight(tfv_file2[i], 4) == ".zip"){
      tmp <- tempfile()
      unzip(tfv_file2[i], exdir = tmp)
      file.remove(tfv_file2[i])
      tmpfiles <- list.files(tmp)
      for(j in 1:length(tmpfiles)){
        file.rename(tmpfiles[i],paste0(tmpfiles[i],Sys.time()))
        file.move(tmpfiles[i],getwd())
      }
      Sys.sleep(1)
    }
  }
  tfv_file2 <- list.files()
  for(i in 1:length(tfv_file2))
  {
    buses <- rbind(buses,suppressWarnings(fread(tfv_file2[i], 
                                                select = c(1:7))))
  }
}
  setwd("..\\")
}

if (length(buses)>0) {
  
  names(buses) <- gsub("[ ^[:blank:]:()+?&/\\-]", ".", names(buses))
  buses <- unique(buses)
  
  routes <- read.xlsx("..\\Input\\BusRoutes.xlsx",
                      sheetName = "BusRoutes", 
                      stringsAsFactors=FALSE,
                      as.data.frame = T) %>% na.omit()
}