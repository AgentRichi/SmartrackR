###########################
# STEP 1: Load the data
###########################

#Load all data

tfv_file <- list.files("..\\Data\\")
tfv_file2 <- list.files("..\\Dropbox\\Data\\")
buses <- data.frame()

if(length(tfv_file)>0) {
  setwd("..\\Data\\")
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
    buses <- rbind(buses,suppressWarnings(fread(tfv_file2[i], 
                                                select = c(1:7))))
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