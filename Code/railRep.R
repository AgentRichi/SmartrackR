setwd("..\\Data_processed\\")

# tfv_file <- list.files()
# if (!exists('railRep')) {railRep <- data.frame()}
# 
# if(length(tfv_file)>0) {
#   for(i in 1:length(tfv_file))
#   {
#     railRep <- rbind(railRep,readRDS(tfv_file[i]))
#     railRep <- unique(railRep)
#   }
# }
# remove(tfv_file)

#make sure railRep is a data.table and cols are in the right order
railRep <- as.data.table(railRep)
railRep <- railRep[,.(od.int,AvgTrainTimeInt,od,AvgTrainTime,ID,Resource.Name,Registration,project,
                      tripID,type,peak,direction,origin,destination,interchange,departure,arrival,
                      legTime,dwellTime,onTime,org,des,Seq.Org,Seq.Des,AdditionalJourneyTime,path_order)]
names(railRep) <- sub("[.]","_",names(railRep)) %>% tolower()
railRep$id  <- as.character(railRep$id)

railRep$legtime <- as.numeric(railRep$legtime, unit = 'mins')
railRep$additionaljourneytime <- as.numeric(railRep$additionaljourneytime, unit = 'mins')

# loads the PostgreSQL driver and credentials
drv <- dbDriver("PostgreSQL")
cred = fromJSON(file = "..//dbCred.json")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "NIMP",
                 host = cred$host, port = cred$port,
                 user = cred$user, password = cred$password)

#special case where railrep table exists but is empty will cause an error
if("railrep" %in% dbListTables(con)) {
  rr <- dbGetQuery(con,'Select * from master.railRep') %>% as.data.table() %>% unique()
  setkey(rr,tripid,org,des)
  setkey(railRep,tripid,org,des)
  railRep.setdiff <- railRep[!tripid %in% rr$tripid]
  # make sure new values are not duplicated
  rr <- rr[!railRep]
  railRep <- rbind(railRep,rr)
} else {railRep.setdiff <- railRep}

rm(cred) # removes connection info
RPostgreSQL::dbDisconnect(con)

if(exists('splitData')) {
  #save new data into backup rds files
  for(thisName in names(splitData)){
    saveRDS(splitData[[thisName]], file = paste0('..\\Data_processed\\',thisName, '.rds'))
  }
  remove(splitData)
}