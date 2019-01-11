rm(list=ls(all=TRUE))

library(dplyr)
library(magrittr)
library(lubridate)
library(xlsx)
library(data.table)

#function to append rows during bus validation
dt.append <- function(x1, x2) {
  obj <- deparse(substitute(x1)) # get the actual object name as a string
  assign(obj, value = data.table::rbindlist(list(x1, x2)), envir = .GlobalEnv)
  
}

work_dir <- "C:\\Users\\vicxjfn\\OneDrive - VicGov\\NIMP\\Smartrack"
#work_dir <- "D:\\OneDrive - VicGov\\NIMP\\Smartrack"
setwd(paste0(work_dir, ".\\Data\\"))

#Function to determine which peak period the route belongs to
peak <- function(x,am.start,am.end,pm.start,pm.end) {
  
  x <- x %>% format("%H%M%S") %>% as.numeric()
  am.start <- am.start %>% format("%H%M%S") %>% as.numeric()
  am.end <- am.end %>% format("%H%M%S") %>% as.numeric()
  pm.start <- pm.start %>% format("%H%M%S") %>% as.numeric()
  pm.end <- pm.end %>% format("%H%M%S") %>% as.numeric()
  
  if(between(x,am.start,am.end)) "AM Peak"
  else if(between(x,pm.start,pm.end)) "PM Peak"
  else if(between(x,am.end,pm.start)) "Inter Peak"
  else "Off Peak"
  
}

###########################
# STEP 1: Load the data
###########################

tfv_file <- list.files()
buses <- data.frame()

for(i in 1:length(tfv_file))
{
  buses <- rbind(buses,
                 read.csv(tfv_file[i], 
                          header = TRUE,
                          stringsAsFactors = F, 
                          skip=0))
}

setwd(work_dir)

routes <- read.xlsx("Input/BusRoutes.xlsx",
                    sheetName = "BusRoutes", 
                    stringsAsFactors=FALSE,
                    as.data.frame = T) %>% na.omit()


###########################
# STEP 2: Clean the data
###########################

routes[c("start.datetime","end.datetime","start.time.filter","end.time.filter")] <- force_tz(
  routes[c("start.datetime","end.datetime","start.time.filter","end.time.filter")],tz="UTC")

buses <- buses %>% filter(Geofence.Name != "")

#Enter.Time is in a different format when loading csv compared to converting from xlsx
# for csv direct download use as.POSIXct(strptime(gsub('[\\.]','',Enter.Time), format = '%d/%m/%Y %H:%M'))
# for converted csv use as.POSIXct(Enter.Time), format = '%d/%m/%Y %I:%M:%S %p'
# format arrival time and separate geofence name into columns, then order by bus and timestamp
arrival.1 = as.POSIXct(strptime(gsub('[\\.]','',buses$Enter.Time), 
                              format = '%d/%m/%Y %I:%M:%S %p'))
arrival.2 = as.POSIXct(strptime(gsub('[\\.]','',buses$Enter.Time), 
                                format = '%d/%m/%Y %H:%M'))
arrival.1[is.na(arrival.1)] <- arrival.2[is.na(arrival.1)]
arrival.1 <- force_tz(arrival.1,tz="UTC")

buses <- buses %>% mutate(arrival = arrival.1,
                          stop.order = as.numeric(unlist(lapply(strsplit(Geofence.Name," - "),'[[',3))),
                           destination = unlist(lapply(strsplit(Geofence.Name," - "),'[[',4))) %>%
  arrange(Resource.Name, arrival)

#format dwell time into seconds
tmp <- strsplit(buses$Time.Inside.Geofence..hh.mm.ss., split=":") %>% lapply(as.numeric,1)
dwellTime <- do.call(rbind, lapply(tmp, rbind))
dwellTime <- dwellTime[,1]*60*60 + dwellTime[,2]*60 + dwellTime[,3]
buses$dwellTime <- dwellTime

#calculate departure time (arrival+dwell) and get origin from preceding row
buses <- buses %>% mutate(departure = lag(dwellTime,1)+lag(arrival,1),
                          origin = ifelse(lag(Resource.Name,1)==Resource.Name,lag(destination,1),"0")) %>%
  mutate(legTime = difftime(arrival,departure, tz = "AEST", units = "mins")+(dwellTime/60),2)
buses$origin[1] <- "0"

#assign ID
buses <- cbind("ID" = sprintf("%07d", 1:nrow(buses)), buses)

###########################
# STEP 3: Assign Bus Routes
###########################

#define peak periods
am.start <- as.POSIXct("1899-12-30 7:00:00") %>% force_tz(tz="UCT")
am.end <- as.POSIXct("1899-12-30 9:00:00 UTC") %>% force_tz(tz="UCT")
pm.start <- as.POSIXct("1899-12-30 15:30:00 UTC") %>% force_tz(tz="UCT")
pm.end <- as.POSIXct("1899-12-30 19:00:00 UTC") %>% force_tz(tz="UCT")

#compare bus stopping pattern to route and assign name
#for loop variables
buses$tripID <- c(rep(0,nrow(buses)))
buses$type <- c(rep(0,nrow(buses)))
buses$direction <- c(rep(0,nrow(buses)))
buses$peak <- c(rep(0,nrow(buses)))
buses$onTime <- c(rep(0,nrow(buses)))
buses$project <- c(rep(0,nrow(buses)))
buses$interchange <- c(rep("-",nrow(buses)))

buses.valid <- buses[0,]
for(i in 1:nrow(routes)){
  
  #route stopping patterns
  route <- routes[i,]
  pattern <- unlist(strsplit(route$stops,",")) %>% trimws() %>% toupper()
  pattern.rev <- unlist(strsplit(route$stops,",")) %>% trimws() %>% toupper() %>% rev()
  ignore <- unlist(strsplit(route$passes,",")) %>% trimws() %>% toupper()
  
  #Filter dataset to dates and times
  railRep <- buses %>% filter(!(toupper(destination) %in% ignore)) %>% 
    mutate(departure = lag(dwellTime,1)+lag(arrival,1),
           origin = ifelse(lag(Resource.Name,1)==Resource.Name,lag(destination,1),"0")) %>%
    filter(origin != destination) %>%
    filter(departure >= route$start.datetime &
             arrival <= route$end.datetime &
             between(as.numeric(format(arrival,"%H%M%S")),
                     as.numeric(format(route$start.time.filter,"%H%M%S")),
                     as.numeric(format(route$end.time.filter,"%H%M%S")))) %>%
    filter(origin != destination,legTime < 120, legTime > 0) %>% arrange(Resource.Name,departure)
  
  #adjust peak periods
  peak.adjust <- minutes(route$peak.adjust)
  am.start.route <- (am.start + peak.adjust)
  am.end.route <- (am.end + peak.adjust)
  pm.start.route <- (pm.start + peak.adjust)
  pm.end.route <- (pm.end + peak.adjust)
  
  #while loop variables
  stops <- 1
  tripID <- 1
  org <- railRep$origin %>% trimws() %>% toupper()
  des <- railRep$destination %>% trimws() %>% toupper()
  resource <- railRep$Resource.Name
  resource
  N <- length(pattern)
  while(stops <= (nrow(railRep)-(N-2))) {
    #check that bus name is the same for all legs
    if(length(unique(resource[stops:(stops+N-2)]))==1){
    #if bus is travelling in UP direction
    if((all.equal(org[stops:(stops+N-2)],pattern[1:(N-1)])==T) && 
       (all.equal(des[stops:(stops+N-2)],pattern[2:N])==T)) {
      
      railRep$tripID[stops:(stops+N-2)] <- paste(gsub(" ","",route$name),
                                                 railRep$Resource.Name[stops],
                                                 railRep$departure[stops])
      railRep$type[stops:(stops+N-2)] = route$name
      railRep$direction[stops:(stops+N-2)] <- "UP"
      railRep$onTime[stops:(stops+N-2)] <- railRep$onTime[stops:(stops+N-2)] + route$additional.time
      railRep$peak[stops:(stops+N-2)] <- peak(railRep$arrival[(stops+N-2)],
                                              am.start.route,am.end.route,
                                              pm.start.route,pm.end.route)
      railRep$project[stops:(stops+N-2)] <- route$project
      railRep$interchange[(stops+N-2)] <- route$interchange
      #remove dwelltime from final destination
      railRep$dwellTime[(stops+N-2)] <- 0
      
      tripID <- tripID+1
      stops <- stops+(N-1)
    }
    
    #if bus is travelling in DOWN direction
    else if((all.equal(org[stops:(stops+N-2)],pattern.rev[1:(N-1)])==T) && 
            (all.equal(des[stops:(stops+N-2)],pattern.rev[2:N])==T)) {
      
      railRep$tripID[stops:(stops+N-2)] <- paste(gsub(" ","",route$name),
                                                  railRep$Resource.Name[stops],
                                                  railRep$departure[stops])
      railRep$type[stops:(stops+N-2)] <- route$name
      railRep$direction[stops:(stops+N-2)] <- "DOWN"
      railRep$onTime[stops:(stops+N-2)] <- railRep$onTime[stops:(stops+N-2)] + route$additional.time
      railRep$peak[stops:(stops+N-2)] <- peak(railRep$departure[stops],
                                              am.start.route,am.end.route,
                                              pm.start.route,pm.end.route)
      railRep$project[stops:(stops+N-2)] <- route$project
      railRep$interchange[stops] <- route$interchange
      #remove dwelltime from final destination
      railRep$dwellTime[(stops+N-2)] <- 0
      
      tripID <- tripID+1
      stops <- stops+(N-1)
    }
    
    #else not a replacement bus
    else(stops = stops+1)
    } else(stops = stops+1)
    }
  railRep <- railRep %>% filter(type != "0")
  buses.valid <- dt.append(buses.valid,railRep)
}

###########################
# STEP 4: Calculate Metrics
###########################

#remove non RRP Buses and unnecessary columns
railRep <- buses.valid %>% 
  select(ID,Resource.Name,Registration,project,tripID,type,peak,
         direction,origin,destination,interchange,departure,arrival,legTime,dwellTime,onTime) %>% arrange_('tripID','arrival')


# join station data for stop order when drawing arcchart
nodes <- read.csv("C:/Users/vicxjfn/OneDrive - VicGov/NIMP/Smartrack/Input/stops.csv",stringsAsFactors = F)
nodes <- cbind(sequence=1:nrow(nodes),nodes)[,1:2]

railRep <- railRep %>% left_join(nodes,by=(c('origin'='label'))) %>% 
  left_join(nodes,by=(c('destination'='label'))) %>% rename(Seq.Org=sequence.x,Seq.Des=sequence.y)


#######################
# Delay Metric (additional journey time)
#######################

# load data
train.OD <- fread("Input/mean_stopping_times.csv")

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

#######################
# Punctuality Metric (percent on time)
#######################

#CODE FOR TRAVEL TIMES TABLE
travel_times <- railRep  %>% arrange_('tripID','arrival') %>%
  group_by(tripID, type, direction, peak, Resource.Name) %>%
  summarise(Origin = first(origin), Destination = last(destination),
            Departure = first(departure), Arrival = last(arrival),
            TripTime = sum(legTime), TrainTime = sum(AvgTrainTime),
            XtraTime = first(onTime), XtraTrain = sum(AvgTrainTimeInt))

travel_times$Punctual <- ifelse(((travel_times$TripTime+travel_times$XtraTrain) <= (travel_times$TrainTime+travel_times$XtraTime)),1,0)


#CODE FOR JOURNEY TIMES TABLE
journey_times <- railRep  %>% arrange_('tripID','arrival') %>%
  group_by(tripID, type, direction, peak, Resource.Name) %>%
  summarise(Origin = first(origin), Destination = last(destination),
            Departure = first(departure), Arrival = last(arrival),
            TripTime = sum(legTime), TrainTime = sum(AvgTrainTime),
            XtraTime = first(onTime), XtraTrain = sum(AvgTrainTimeInt),
            Date = as.Date(first(departure))) %>% 
  group_by(type, peak, direction, Date) %>%
  mutate(Punctual = ifelse(((TripTime+XtraTrain) <= (TrainTime+XtraTime)),1,0)) %>%
  summarise(TravelTimes = mean(TripTime),
            ExpectedTravelTime = mean((TripTime+XtraTrain)-TrainTime), 
            # ^this is actually delay, name is to avoid breaking references
            AdditionalTravelTime = mean(XtraTime),
            Difference = mean((TripTime+XtraTrain) - (TrainTime+XtraTime)),
            Punctuality = sum(Punctual),
            NumBuses = n()) %>% arrange(Date)

journey_times$Punctuality <- round(journey_times$Punctuality / journey_times$NumBuses,2)

#Refresh/create files with yesterdays and todays Peak travel information
write.csv(filter(journey_times,
                 (peak=="AM Peak" | peak=="PM Peak"),
                 Date == Sys.Date()-1),
          paste0("Output/RRBJourneyTimes_",Sys.Date()-1,".csv"))

write.csv(filter(journey_times,
                 (peak=="AM Peak" | peak=="PM Peak"),
                 Date == Sys.Date()),
          paste0("Output/RRBJourneyTimes_",Sys.Date(),".csv"))