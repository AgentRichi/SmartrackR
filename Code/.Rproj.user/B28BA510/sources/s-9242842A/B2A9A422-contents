###########################
# STEP 4: Calculate Metrics
###########################

#remove non RRP Buses and unnecessary columns
railRep <- buses.valid %>% 
  select(ID,Resource.Name,Registration,project,tripID,type,peak,
         direction,origin,destination,interchange,departure,arrival,legTime,dwellTime,onTime) %>%
  arrange_('tripID','arrival')

# remove trips with unrealistic travel times
to_remove <- railRep %>% filter((legTime < 0 | legTime > 110) 
                                | (
                                  (origin=='Arts Centre' | origin=='Moorabbin' | destination=='Arts Centre' | destination=='Moorabbin') & 
                                    (legTime>60) & (arrival < as.Date("2019-01-31"))
                                )
) %>% select(tripID) %>% unique()

tt_remove <- railRep  %>% arrange_('tripID','arrival') %>%
  group_by(tripID) %>%
  summarise(TripTime = difftime(last(arrival),first(departure), tz = "AEST", units = "mins")) %>% 
  filter(TripTime < 0 | TripTime > 180) %>% select(tripID)

to_remove <- rbind(to_remove,tt_remove)

railRep <- railRep %>% filter(!(tripID %in% to_remove$tripID))


# join station data for stop order when drawing arcchart
nodes <- read.csv("..\\Input\\stops.csv",stringsAsFactors = F)
nodes <- cbind(sequence=1:nrow(nodes),nodes)[,1:2]

nodes$label <- tolower(nodes$label)
railRep$org <- tolower(railRep$origin)
railRep$des <- tolower(railRep$destination)
railRep <- railRep %>% left_join(nodes,by=(c('org'='label'))) %>% 
  left_join(nodes,by=(c('des'='label'))) %>% rename(Seq.Org=sequence.x,Seq.Des=sequence.y)


#######################
# Delay Metric (additional journey time)
#######################

# load data
train.OD <- fread("..\\Input\\mean_stopping_times\\mean_stopping_times.csv") %>% 
  select(from,time,to) %>% unique()

# fix station names that don't match
railRep$org <- ifelse((railRep$interchange != "-" & railRep$direction=="DOWN"),
                      railRep$interchange,railRep$origin) %>% tolower()
railRep$org[railRep$org=="flemington"] <- "newmarket"
railRep$org[railRep$org=="arts centre"] <- "flinders street"
railRep$org[railRep$org=="federation square"] <- "flinders street"

railRep$des <- ifelse((railRep$interchange != "-" & railRep$direction=="UP"),
                      railRep$interchange,railRep$destination) %>% tolower()
railRep$des[railRep$des=="flemington"] <- "newmarket"
railRep$des[railRep$des=="arts centre"] <- "flinders street"
railRep$des[railRep$des=="federation square"] <- "flinders street"

railRep$OD.int <- ifelse(railRep$direction=="DOWN",
                         paste0(railRep$origin,railRep$interchange),
                         paste0(railRep$interchange,railRep$destination)) %>% tolower()

#Calculate travel times for Buses going across train lines

# create unique timetable and join with Rail Rep
train.OD$OD <- paste0(train.OD$from,train.OD$to) %>% tolower()
railRep$OD <- paste0(railRep$org,railRep$des)

railRep <- as.data.table(railRep) %>% setkey(OD)
train.OD <- as.data.table(train.OD)[!duplicated(OD)] %>% setkey(OD)
railRep <- train.OD[,.(OD,AvgTrainTime=time/60)][railRep]

setkey(railRep,OD.int)
railRep <- train.OD[,.(OD,AvgTrainTimeInt=time/60)][railRep]
railRep$AvgTrainTimeInt[is.na(railRep$AvgTrainTimeInt)]  <- 0

railRep$AdditionalJourneyTime <- railRep$legTime + railRep$AvgTrainTimeInt - railRep$AvgTrainTime
railRep$AdditionalJourneyTime[is.na(railRep$AdditionalJourneyTime)] <- 0

#Prepare new data to save to folder
splitData <- split.data.frame(railRep,paste(year(railRep$departure),month(railRep$departure),day(railRep$departure),sep = '-'))