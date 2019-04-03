# #Refresh/create files with yesterdays and todays Peak travel information
write.csv(filter(journey_times,
                 (peak=="AM Peak" | peak=="PM Peak"),
                 Date == Sys.Date()-1),
          paste0("..\\Output\\RRBJourneyTimes_",Sys.Date()-1,".csv"))

write.csv(filter(journey_times,
                 (peak=="AM Peak" | peak=="PM Peak"),
                 Date == Sys.Date()),
          paste0("..\\Output\\RRBJourneyTimes_",Sys.Date(),".csv"))

# Remove processed files
setwd("..\\Data\\")

tfv_file <- list.files()
numFiles <- length(tfv_file)

if(numFiles>0) {
  
  for(i in 1:numFiles)
  {
    file.rename(tfv_file[i],paste0(Sys.Date(),"_",i,".csv"))
    file.move(paste0(Sys.Date(),"_",i,".csv"),"..\\Data_hist")
  }
}