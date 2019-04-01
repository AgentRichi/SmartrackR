# #Refresh/create files with yesterdays and todays Peak travel information
write.csv(filter(journey_times,
                 (peak=="AM Peak" | peak=="PM Peak"),
                 Date == Sys.Date()-1),
          paste0("..\\Output\\RRBJourneyTimes_",Sys.Date()-1,".csv"))

write.csv(filter(journey_times,
                 (peak=="AM Peak" | peak=="PM Peak"),
                 Date == Sys.Date()),
          paste0("..\\Output\\RRBJourneyTimes_",Sys.Date(),".csv"))