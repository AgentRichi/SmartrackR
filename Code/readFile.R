###########################
# STEP 1: Load the data
###########################

setwd("..\\Data\\")

tfv_file <- list.files()
buses <- data.frame()

for(i in 1:length(tfv_file))
{
  
  buses <- rbind(buses,suppressWarnings(fread(tfv_file[i], 
                                              select = c(1:7))))
}

names(buses) <- gsub("[ ^[:blank:]:()+?&/\\-]", ".", names(buses))

setwd("..\\Code")

routes <- read.xlsx("..\\Input\\BusRoutes.xlsx",
                    sheetName = "BusRoutes", 
                    stringsAsFactors=FALSE,
                    as.data.frame = T) %>% na.omit()