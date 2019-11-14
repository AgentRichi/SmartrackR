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

# # loads the PostgreSQL driver and credentials
# drv <- dbDriver("PostgreSQL")
# cred = fromJSON(file = "..//dbCred.json")
# # creates a connection to the postgres database
# # note that "con" will be used later in each connection to the database
# con <- dbConnect(drv, dbname = "NIMP",
#                  host = cred$host, port = cred$port,
#                  user = cred$user, password = cred$password)
# 
# if("railRep" %in% dbListTables(con)) {
#   rr <- dbGetQuery(con,'Select * from master.railRep')
#   railRep <- rbind(railRep,rr)
#   railRep <- unique(railRep)
# }
# 
# rm(cred) # removes connection info
# RPostgreSQL::dbDisconnect(con)

if(exists('splitData')) {
  #save new data into backup rds files
  for(thisName in names(splitData)){
    saveRDS(splitData[[thisName]], file = paste0('..\\Data_processed\\',thisName, '.rds'))
  }
  remove(splitData)
}