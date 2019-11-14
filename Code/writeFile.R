# # #Refresh/create files with yesterdays and todays Peak travel information
# write.csv(filter(journey_times,
#                  (peak=="AM Peak" | peak=="PM Peak"),
#                  Date == Sys.Date()-1),
#           paste0("..\\Output\\RRBJourneyTimes_",Sys.Date()-1,".csv"))
# 
# write.csv(filter(journey_times,
#                  (peak=="AM Peak" | peak=="PM Peak"),
#                  Date == Sys.Date()),
#           paste0("..\\Output\\RRBJourneyTimes_",Sys.Date(),".csv"))

#Save tables as flat files for powerBI
# fwrite(railRep %>% mutate(departure=as.character(departure),arrival=as.character(arrival)),"..\\PowerBI_flat_files\\railRep.csv",dateTimeAs = "ISO")
# fwrite(travel_times %>% mutate(Departure=as.character(Departure),Arrival=as.character(Arrival)),"..\\PowerBI_flat_files\\travel_times.csv")
# fwrite(journey_times %>% mutate(Date=as.character(Date)),"..\\PowerBI_flat_files\\journey_times.csv")

# loads the PostgreSQL driver and credentials
drv <- dbDriver("PostgreSQL")
cred = fromJSON(file = "..//dbCred.json")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "NIMP",
                 host = cred$host, port = cred$port,
                 user = cred$user, password = cred$password)
# dbWriteTable(con,"railrep",railRep,overwrite=TRUE)
# dbWriteTable(con,"journey_times",journey_times,overwrite=TRUE)
# dbWriteTable(con,"travel_times",travel_times,overwrite=TRUE)

if(!"railrep" %in% dbListTables(con)) {
  rr.create_query = paste0("CREATE TABLE master.railrep (",
                           rr.colnames[1]," varchar, ",
                           rr.colnames[2]," numeric, ",
                           rr.colnames[3]," varchar, ",
                           rr.colnames[4]," numeric, ",
                           rr.colnames[5]," varchar, ",
                           rr.colnames[6]," varchar, ",
                           rr.colnames[7]," varchar, ",
                           rr.colnames[8]," varchar, ",
                           rr.colnames[9]," varchar UNIQUE, ",
                           rr.colnames[10]," varchar, ",
                           rr.colnames[11]," varchar, ",
                           rr.colnames[12]," varchar, ",
                           rr.colnames[13]," varchar, ",
                           rr.colnames[14]," varchar, ",
                           rr.colnames[15]," varchar, ",
                           rr.colnames[16]," timestamp, ",
                           rr.colnames[17]," timestamp, ",
                           rr.colnames[18]," numeric, ",
                           rr.colnames[19]," numeric, ",
                           rr.colnames[20]," numeric, ",
                           rr.colnames[21]," varchar, ",
                           rr.colnames[22]," varchar, ",
                           rr.colnames[23]," integer, ",
                           rr.colnames[24]," integer, ",
                           rr.colnames[25]," numeric, ",
                           "CONSTRAINT unique_rr_id UNIQUE (tripID));"
                           )
  dbBegin(con)
  dbSendQuery(con,rr.create_query)
  dbCommit(con)
}

dbDisconnect(con)

rr.colnames <- sub("[.]","_",names(railRep))
jt.colnames <- sub("[.]","_",names(journey_times))
tt.colnames <- sub("[.]","_",names(travel_times))

# replace all blank values
railRep$interchange[railRep$interchange==""] <- "NA"
railRep$legTime <- as.numeric(railRep$legTime, unit='mins')
railRep$AdditionalJourneyTime <- as.numeric(railRep$AdditionalJourneyTime, unit='mins')

con <- dbConnect(drv, dbname = "NIMP",
                 host = cred$host, port = cred$port,
                 user = cred$user, password = cred$password)

apply(railRep, 1, function(df_row,df_names) {
  insert_query = paste0('INSERT INTO ','master.railrep',' VALUES (',
                        "'",df_row[1],"'",", ",
                        df_row[2],", ",
                        "'",df_row[3],"'",", ",
                        df_row[4],", ",
                        "'",df_row[5],"'",", ",
                        "'",df_row[6],"'",", ",
                        "'",df_row[7],"'",", ",
                        "'",df_row[8],"'",", ",
                        "'",df_row[9],"'",", ",
                        "'",df_row[10],"'",", ",
                        "'",df_row[11],"'",", ",
                        "'",df_row[12],"'",", ",
                        "'",df_row[13],"'",", ",
                        "'",df_row[14],"'",", ",
                        "'",df_row[15],"'",", ",
                        "'",df_row[16],"'",", ",
                        "'",df_row[17],"'",", ",
                        df_row[18],", ",
                        df_row[19],", ",
                        df_row[20],", ",
                        "'",df_row[21],"'",", ",
                        "'",df_row[22],"'",", ",
                        df_row[23],", ",
                        df_row[24],", ",
                        df_row[25],
                    ") ON CONFLICT ON CONSTRAINT unique_rr_id DO UPDATE SET ",
                     df_names[1]," = excluded.",df_names[1],",",
                     df_names[2]," = excluded.",df_names[2],",",
                     df_names[3]," = excluded.",df_names[3],",",
                     df_names[4]," = excluded.",df_names[4],",",
                     df_names[5]," = excluded.",df_names[5],",",
                     df_names[6]," = excluded.",df_names[6],",",
                     df_names[7]," = excluded.",df_names[7],",",
                     df_names[8]," = excluded.",df_names[8],",",
                     df_names[9]," = excluded.",df_names[9],",",
                     df_names[10]," = excluded.",df_names[10],",",
                     df_names[11]," = excluded.",df_names[11],",",
                     df_names[12]," = excluded.",df_names[12],",",
                     df_names[13]," = excluded.",df_names[13],",",
                     df_names[14]," = excluded.",df_names[14],",",
                     df_names[15]," = excluded.",df_names[15],",",
                     df_names[16]," = excluded.",df_names[16],",",
                     df_names[17]," = excluded.",df_names[17],",",
                     df_names[18]," = excluded.",df_names[18],",",
                     df_names[19]," = excluded.",df_names[19],",",
                     df_names[20]," = excluded.",df_names[20],",",
                     df_names[21]," = excluded.",df_names[21],",",
                     df_names[22]," = excluded.",df_names[22],",",
                     df_names[23]," = excluded.",df_names[23],",",
                     df_names[24]," = excluded.",df_names[24],",",
                     df_names[25]," = excluded.",df_names[25],";")
  
  dbBegin(con)
  dbSendQuery(con,insert_query)
  dbCommit(con)

}, df_names = rr.colnames)

dbDisconnect(con)
rm(cred) # removes connection info

# Remove processed files
setwd("..\\Data\\")

tfv_file <- list.files()
numFiles <- length(tfv_file)

if(numFiles>0) {
  
  for(i in 1:numFiles)
  {
    timenow <- Sys.time()
    file.rename(tfv_file[i],paste0(timenow,"_",i,"_OneDrive.csv"))
    tfv_file <- list.files()
    file.move(tfv_file[1],"..\\Data_hist")
  }
}

setwd("..\\Dropbox\\Data\\")

tfv_file <- list.files()
numFiles <- length(tfv_file)

if(numFiles>0) {
  
  for(i in 1:numFiles)
  {
    timenow <- Sys.time()
    file.rename(tfv_file[i],paste0(timenow,"_",i,"_DropBox.csv"))
    tfv_file <- list.files()
    file.move(tfv_file[1],"..\\Data_hist")
  }
}

setwd("..\\")