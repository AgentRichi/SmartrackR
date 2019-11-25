###########################
# STEP 4: Calculate Metrics
###########################

#remove non RRP Buses and unnecessary columns
railRep <- buses.valid %>% 
  select(ID,Resource.Name,Registration,project,tripID,type,peak,
         direction,origin,destination,interchange,departure,arrival,legTime,dwellTime,onTime) %>%
  arrange(tripID,arrival)

# remove trips with unrealistic travel times
to_remove <- railRep %>% filter((legTime < 0 | legTime > 110) 
                                | (
                                  (origin=='Arts Centre' | origin=='Moorabbin' | destination=='Arts Centre' | destination=='Moorabbin') & 
                                    (legTime>60) & (arrival < as.Date("2019-01-31"))
                                )
) %>% select(tripID) %>% unique()

tt_remove <- railRep  %>% arrange(tripID,arrival) %>%
  group_by(tripID) %>%
  summarise(TripTime = difftime(last(arrival),first(departure), tz = "AEST", units = "mins")) %>% 
  filter(TripTime < 0 | TripTime > 180) %>% select(tripID)

to_remove <- rbind(to_remove,tt_remove)

railRep <- railRep %>% filter(!(tripID %in% to_remove$tripID))

####### 
# DELETE ONCE VIZ IS IN TABLEAU, NO LONGER NEEDED


# join station data for stop order when drawing arcchart
nodes <- read.csv("..\\Input\\stops.csv",stringsAsFactors = F)
nodes <- cbind(sequence=1:nrow(nodes),nodes)[,1:2]

nodes$label <- tolower(nodes$label)
railRep$org <- tolower(railRep$origin)
railRep$des <- tolower(railRep$destination)
railRep <- railRep %>% left_join(nodes,by=(c('org'='label'))) %>% 
  left_join(nodes,by=(c('des'='label'))) %>% rename(Seq.Org=sequence.x,Seq.Des=sequence.y)

railRep$Seq.Des[is.na(railRep$Seq.Des)] <- 0
railRep$Seq.Org[is.na(railRep$Seq.Org)] <- 0
#######################
# Delay Metric (additional journey time)
#######################

# load data

########
# Rewrite to use "GTFS".odtimes table instead

# train.od <- fread("..\\Input\\mean_stopping_times\\mean_stopping_times.csv") %>% 
#   select(from,time,to) %>% unique()

# loads the PostgreSQL driver and credentials
drv <- dbDriver("PostgreSQL")
cred = fromJSON(file = "..//dbCred.json")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "postgis_TAA",
                 host = cred$host, port = cred$port,
                 user = cred$user, password = cred$password)
# dbWriteTable(con,"railRep",railRep,overwrite=TRUE)
# dbWriteTable(con,"journey_times",journey_times,overwrite=TRUE)
# dbWriteTable(con,"travel_times",travel_times,overwrite=TRUE)
rm(cred) # removes connection info
train.od <- dbGetQuery(con,'Select * from "GTFS".odtime')
RPostgreSQL::dbDisconnect(con)

## MOVE THESE TO EXTERNAL FILE
# fix station names that don't match
# railRep$org <- ifelse((!(railRep$interchange %in% c("","-")) & railRep$direction=="DOWN"),
#                       railRep$interchange,railRep$origin) %>% tolower()
# railRep$org[railRep$org=="flemington"] <- "showgrounds"
# railRep$org[railRep$org=="arts centre"] <- "flinders street"
# railRep$org[railRep$org=="federation square"] <- "flinders street"
# railRep$org[railRep$org=="jolimont"] <- "jolimont-mcg"
# 
# railRep$des <- ifelse((!(railRep$interchange %in% c("","-")) & railRep$direction=="UP"),
#                       railRep$interchange,railRep$destination) %>% tolower()
# railRep$des[railRep$des=="flemington"] <- "showgrounds"
# railRep$des[railRep$des=="arts centre"] <- "flinders street"
# railRep$des[railRep$des=="federation square"] <- "flinders street"
# railRep$des[railRep$des=="jolimont"] <- "jolimont-mcg"

#manually overwrite incorrect names from conversion file
name_conv <- read.csv('..//Input//name_conversions.csv')
name_conv <- as.data.table(name_conv) %>% setkey(org)
railRep <- as.data.table(railRep) %>% setkey(org)
railRep <- name_conv[railRep]

railRep$org <- apply(railRep, 1, function(x) {
  if (is.na(x['match'])) {
    return(x['org'])
  } else {
    #ifelse check is needed because 'fawkner' is both a tram and train stop
    return(ifelse(x['type']=="UFD-FGS LTD EXP",x['org'],x['match']))
  }
})

railRep <- railRep[,!"match"]
names(name_conv) <- c("des","match")
setkey(name_conv,des)
setkey(railRep,des)
railRep <- name_conv[railRep]

railRep$des <- apply(railRep, 1, function(x) {
  if (is.na(x['match'])) {
    return(x['des'])
  } else {
    return(ifelse(x['type']=="UFD-FGS LTD EXP",x['des'],x['match']))
  }
})
railRep <- railRep[,!"match"]

# BBC-MBN is reversed in direction for some reason
railRep$direction[railRep$od=="brighton beachmoorabbin"] <- "UP"
railRep$direction[railRep$od=="moorabbinbrighton beach"] <- "DOWN"

#create od keys
railRep$od <- paste0(railRep$org,railRep$des)
railRep$od.int <- ifelse(railRep$direction=="DOWN",
                         paste0(railRep$origin,railRep$interchange),
                         paste0(railRep$interchange,railRep$destination)) %>% tolower()

#remove suburb name and Railway Station from org and des lookup
train.od$origin <- lapply(strsplit(train.od$origin,split = "[(]"), `[[`,1) %>% unlist() %>% tolower()
train.od$origin[which(nchar(train.od$origin)>6)] <- 
  lapply(strsplit(train.od$origin[which(nchar(train.od$origin)>6)],split = "railway"), `[[`,1) %>% 
  unlist() %>% trimws()
train.od$destination <- lapply(strsplit(train.od$destination,split = "[(]"), `[[`,1) %>% unlist() %>% tolower()
train.od$destination[which(nchar(train.od$destination)>6)] <- 
  lapply(strsplit(train.od$destination[which(nchar(train.od$destination)>6)],split = "railway"), `[[`,1) %>% 
  unlist() %>% trimws()

railRep <- as.data.table(railRep) %>% setkey(od)
#get unique list of od combos
rr.unique <- railRep[,.(org=ifelse(interchange %in% c("","-"),org,
                                   tolower(ifelse(direction=="DOWN",
                                                  interchange,org)
                                           )
                                   ),
                        des=ifelse(interchange %in% c("","-"),des,
                                   tolower(ifelse(direction=="DOWN",
                                                  des,interchange)
                                           )
                                   ),od)] %>% unique() %>% setkey(od)

rr.unique$AvgTrainTime <- apply(rr.unique, 1, function(x) {
  #match by origin/destination in both direction
  org.match <- train.od[
    (train.od$origin %in% x['org'] & train.od$destination %in% x['des'])|
      (train.od$destination %in% x['org'] & train.od$origin %in% x['des']),
  ]$time %>% mean(na.rm=TRUE)
})

rr.uniqueInt <- railRep[,.(org=tolower(ifelse(direction=="DOWN",origin,interchange)),
                           des=tolower(ifelse(direction=="DOWN",interchange,destination)),od.int)] %>% unique() %>% setkey(od.int)

rr.uniqueInt$AvgTrainTimeInt <- apply(rr.uniqueInt, 1, function(x) {
  #match by origin/destination in both direction
  org.match <- train.od[
    (train.od$origin %in% x['org'] & train.od$destination %in% x['des'])|
      (train.od$destination %in% x['org'] & train.od$origin %in% x['des']),
  ]$time %>% mean(na.rm=TRUE)
})

#join results
railRep <- rr.unique[,.(od,AvgTrainTime=AvgTrainTime/60)][railRep]
setkey(railRep,od.int)
railRep <- rr.uniqueInt[,.(od.int,AvgTrainTimeInt=AvgTrainTimeInt/60)][railRep]
railRep$AvgTrainTimeInt[is.na(railRep$AvgTrainTimeInt)]  <- 0

# remove NAs and save in separate table
railRep.fix <- railRep[is.na(AvgTrainTime)]

if (nrow(railRep.fix) > 0 ){
  railRep <- railRep[!tripID %in% railRep.fix$tripID]
  
  railRep.fix <- railRep.fix[,!"AvgTrainTime"]
  railRep.fix <- as.data.table(railRep.fix) %>% setkey(od)
  
  rr.unique.fix <- railRep.fix[,.(org=ifelse(interchange %in% c("","-"),org,
                                             tolower(ifelse(direction=="DOWN",
                                                            interchange,org)
                                             )
  ),
  des=ifelse(interchange %in% c("","-"),des,
             tolower(ifelse(direction=="DOWN",
                            des,interchange)
             )
  ),od)] %>% unique() %>% setkey(od)
  
  # find average travel times using fuzzy matching
  # agrep is very slow on such a big lookup table...
  rr.unique.fix$AvgTrainTime <- apply(rr.unique.fix, 1, function(x) {
    #match by origin
    org.match <- train.od[agrep(x['org'],train.od$origin,ignore.case = TRUE),]
    #filter to match destination as well
    org.match <- org.match[agrep(x['des'],org.match$destination,ignore.case = TRUE),]
    #going the opposite way
    des.match <- train.od[agrep(x['des'],train.od$origin,ignore.case = TRUE),]
    des.match <- des.match[agrep(x['org'],des.match$destination,ignore.case = TRUE),]
    
    return(rbind(org.match,des.match)$time %>% mean(na.rm=TRUE))
    
  })
  
  railRep.fix <- rr.unique.fix[!is.na(rr.unique.fix$AvgTrainTime),.(od,AvgTrainTime=AvgTrainTime/60)][railRep.fix]
  setcolorder(railRep.fix,names(railRep))
  
  railRep <- rbind.data.frame(railRep,railRep.fix)
}

#Calculate travel times for Buses going across train lines
railRep$AdditionalJourneyTime <- railRep$legTime + railRep$AvgTrainTimeInt - railRep$AvgTrainTime
railRep$AdditionalJourneyTime[is.na(railRep$AdditionalJourneyTime)] <- 0

#######
#Prepare new data to save to folder
splitData <- split.data.frame(railRep,paste(year(railRep$departure),month(railRep$departure),day(railRep$departure),sep = '-'))
