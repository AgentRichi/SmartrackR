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

# loads the PostgreSQL driver and credentials
drv <- dbDriver("PostgreSQL")
cred = fromJSON(file = "..//dbCred.json")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "NIMP",
                 host = cred$host, port = cred$port,
                 user = cred$user, password = cred$password)
# dbWriteTable(con,"railRep",railRep,overwrite=TRUE)
# dbWriteTable(con,"journey_times",journey_times,overwrite=TRUE)
# dbWriteTable(con,"travel_times",travel_times,overwrite=TRUE)

if(!"railrep" %in% dbListTables(con)) {
  rr.colnames <- sub("[.]","_",names(railRep))
  rr.create_query = paste0("CREATE TABLE master.railRep (",
                           rr.colnames[1]," varchar, ",
                           rr.colnames[2]," numeric, ",
                           rr.colnames[3]," varchar, ",
                           rr.colnames[4]," numeric, ",
                           rr.colnames[5]," varchar, ",
                           rr.colnames[6]," varchar, ",
                           rr.colnames[7]," varchar, ",
                           rr.colnames[8]," varchar, ",
                           rr.colnames[9]," varchar NOT NULL, ",
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
                           rr.colnames[26]," varchar NOT NULL, ",
                           "PRIMARY KEY (tripid,path_order),",
                           " CONSTRAINT unique_rr_id UNIQUE (tripid,path_order));"
                           )
  dbBegin(con)
  dbSendQuery(con,rr.create_query)
  dbCommit(con)
}

dbDisconnect(con)

# #replace special characters
# railRep$des <- railRep$des %>% sapply(FUN = function(x) gsub(pattern = "/",replacement = " ",x=x))
# railRep$org <- railRep$org %>% sapply(FUN = function(x) gsub(pattern = "/",replacement = " ",x=x))
# railRep$od <- railRep$od %>% sapply(FUN = function(x) gsub(pattern = "/",replacement = " ",x=x))
# railRep$od.int <- railRep$od.int %>% sapply(FUN = function(x) gsub(pattern = "/",replacement = " ",x=x))
# railRep$origin <- railRep$origin %>% sapply(FUN = function(x) gsub(pattern = "/",replacement = " ",x=x))
# railRep$destination <- railRep$destination %>% sapply(FUN = function(x) gsub(pattern = "/",replacement = " ",x=x))


railRep.split <- split.data.frame(railRep.setdiff,railRep.setdiff$type)

# n <- 300
# nr <- nrow(df)
# split(df, rep(1:ceiling(nr/n), length.out=nr))
# try statement inside apply
cred = rjson::fromJSON(file = "..//dbCred.json")
drv <- DBI::dbDriver("PostgreSQL")
con <- DBI::dbConnect(drv, dbname = "NIMP",
                      host = cred$host, port = cred$port,
                      user = cred$user, password = cred$password)


sapply(railRep.split, function(rr.split) {
  rr.colnames <- sub("[.]","_",names(rr.split))
  # replace all blank values
  rr.split$interchange[rr.split$interchange==""] <- "NA"
  # rr.split$legTime <- as.numeric(rr.split$legTime, unit='mins')
  # rr.split$AdditionalJourneyTime <- as.numeric(rr.split$AdditionalJourneyTime, unit='mins')
  #nested apply:
  apply(rr.split, 1, function(df_row,df_names) {
    df_row <- sapply(df_row, trimws)
    insert_query = paste0('INSERT INTO ','master.railRep',' VALUES (',
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
                          df_row[25],", ",
                          "'",df_row[26],"'",
                          ") ON CONFLICT ON CONSTRAINT unique_rr_id DO NOTHING",
                          ";")
    DBI::dbBegin(con)
    DBI::dbSendQuery(con,"SET client_encoding = WIN1252;")
    DBI::dbCommit(con)
    DBI::dbBegin(con)
    DBI::dbSendQuery(con,insert_query)
    DBI::dbCommit(con)
    
  }, df_names = rr.colnames)
  print(paste('data successfully loaded for',rr.split$type[1]))
})

dbDisconnect(con)
rm(cred) # removes connection info

# Remove processed files
setwd("..\\Data\\")

tfv_file <- list.files()
numFiles <- length(tfv_file)

if(numFiles>0) {
  
  for(i in 1:numFiles)
  {
    timenow <- gsub("[ :-]","",Sys.time())
    file_name <- paste0(gsub(".csv","",tfv_file[i]),timenow,"_",i,"_OneDrive.csv")
    file.rename(tfv_file[i],file_name)
    file.move(file_name,"..\\Data_hist")
  }
}

setwd("..\\Dropbox\\Data\\")

tfv_file <- list.files()
numFiles <- length(tfv_file)

if(numFiles>0) {
  
  for(i in 1:numFiles)
  {
    timenow <- gsub("[ :-]","",Sys.time())
    file_name <- paste0(gsub(".csv","",tfv_file[i]),timenow,"_",i,"DropBox.csv")
    file.rename(tfv_file[i],file_name)
    file.move(file_name,"..\\Data_hist")
  }
}

setwd("..\\")

#Save tables as flat files for powerBI
fwrite(railRep %>% mutate(departure=as.character(departure),arrival=as.character(arrival)),"..\\PowerBI_flat_files\\railRep.csv",dateTimeAs = "ISO")
fwrite(travel_times %>% mutate(departure=as.character(departure),arrival=as.character(arrival)),"..\\PowerBI_flat_files\\travel_times.csv",dateTimeAs = "ISO")
fwrite(journey_times %>% mutate(travdate=as.character(travdate)),"..\\PowerBI_flat_files\\journey_times.csv")
