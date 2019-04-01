###########################
# STEP 1: Load the data
###########################

#Load all data
setwd("..\\Data\\")

tfv_file <- list.files()
buses <- data.frame()
numFiles <- length(tfv_file)

if(numFiles>0) {
  
  for(i in 1:numFiles)
  {
    buses <- rbind(buses,suppressWarnings(fread(tfv_file[i], 
                                                select = c(1:7))))
    file.move(tfv_file[i], "..\\Data_hist")
  }
  names(buses) <- gsub("[ ^[:blank:]:()+?&/\\-]", ".", names(buses))
  
  buses <- unique(buses)
  
  routes <- read.xlsx("..\\Input\\BusRoutes.xlsx",
                      sheetName = "BusRoutes", 
                      stringsAsFactors=FALSE,
                      as.data.frame = T) %>% na.omit()
}

remove(numFiles)