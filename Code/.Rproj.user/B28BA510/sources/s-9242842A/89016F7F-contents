setwd("..\\Data_processed\\")

tfv_file <- list.files()
if (!exists('railRep')) {railRep <- data.frame()}

if(length(tfv_file)>0) {
  for(i in 1:length(tfv_file))
  {
    railRep <- rbind(railRep,readRDS(tfv_file[i]))
    railRep <- unique(railRep)
  }
}

if(exists('splitData')) {
  for(thisName in names(splitData)){
    saveRDS(splitData[[thisName]], file = paste0('..\\Data_processed\\',thisName, '.rds'))
  }
  remove(splitData)
}

remove(tfv_file)