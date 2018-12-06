rm(list=ls(all=TRUE))

library(dplyr)
library(magrittr)
library(lubridate)
library(xlsx)
library(data.table)

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
                          project = unlist(lapply(strsplit(Geofence.Name," - "),'[[',2)),
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
                          origin = ifelse(lag(Resource.Name,1)==Resource.Name,lag(destination,1),"0"))
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

for(i in 1:nrow(routes)){
  
  #route stopping patterns
  route <- routes[i,]
  pattern <- unlist(strsplit(route$stops,",")) %>% trimws() %>% toupper()
  pattern.rev <- unlist(strsplit(route$stops,",")) %>% trimws() %>% toupper() %>% rev()
  
  #Filter dataset to dates and times
  railRep <- buses %>% filter(departure >= route$start.datetime &
                                arrival <= route$end.datetime &
                                between(as.numeric(format(arrival,"%H%M%S")),
                                        as.numeric(format(route$start.time.filter,"%H%M%S")),
                                        as.numeric(format(route$end.time.filter,"%H%M%S"))))
  
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
  N <- length(pattern)
  while(stops <= (nrow(railRep)-(N-2))) {
    
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
      #remove dwelltime from final destination
      railRep$dwellTime[(stops+N-2)] <- 0
      
      tripID <- tripID+1
      stops <- stops+(N-1)
    }
    
    #else not a replacement bus
    else(stops = stops+1)
  }
  
  #assign replacement buses according to ID
  buses$tripID[match(railRep$ID,buses$ID)] <- railRep$tripID
  buses$type[match(railRep$ID,buses$ID)] <- railRep$type
  buses$direction[match(railRep$ID,buses$ID)] <- railRep$direction
  buses$peak[match(railRep$ID,buses$ID)] <- railRep$peak
  buses$dwellTime[match(railRep$ID,buses$ID)] <- railRep$dwellTime
  buses$onTime[match(railRep$ID,buses$ID)] <- railRep$onTime
}

###########################
# STEP 4: Calculate Metrics
###########################

#remove non RRP Buses and unnecessary columns
railRep <- buses %>% filter(type != "0") %>% 
  select(ID,Resource.Name,Registration,project,tripID,type,peak,
         direction,origin,destination,departure,arrival,dwellTime,onTime) %>%
  mutate(legTime = round(difftime(arrival,departure, tz = "AEST", units = "mins")+(dwellTime/60),2)) %>%
   filter(legTime < 120, legTime > 0) %>% arrange_('tripID','arrival')


# join station data for stop order when drawing arcchart
nodes <- read.csv("C:/Users/vicxjfn/OneDrive - VicGov/NIMP/Smartrack/Input/stops.csv",stringsAsFactors = F)
nodes <- cbind(sequence=1:nrow(nodes),nodes)[,1:2]

railRep <- railRep %>% left_join(nodes,by=(c('origin'='label'))) %>% 
  left_join(nodes,by=(c('destination'='label'))) %>% rename(Seq.Org=sequence.x,Seq.Des=sequence.y)


#######################
# Delay Metric (additional journey time)
#######################

# load data
train.OD <- fread("C:/Data/SUM/ServiceUSageModel_Train_May2017_People_Trip_OD_MAtrix_15Min.csv")
train.OD <- train.OD[transfer_point=="NULL"]
# create unique timetable [O-D travel time x time of day (intervals)]

timetable <- train.OD[,.(AvgTrainTime=mean(avg_trip_time)),
                      by=.(day_type,org=origin,des=destination,origin_departure_hour)] %>% 
  setkeyv(c("org","des","day_type","origin_departure_hour"))

# compare bus leg times to TT & calculate additional travel time
railRep$origin_departure_hour <- hour(railRep$departure)

wkdays <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
wkends <- c("Saturday","Sunday")

railRep$day_type <- weekdays(railRep$departure)
railRep$day_type[railRep$day_type %in% wkdays] <- "weekday"
railRep$day_type[railRep$day_type %in% wkends] <- "weekend"

railRep$org <- tolower(railRep$origin)
railRep$org[railRep$org=="flemington"] <- "newmarket"
railRep$des <- tolower(railRep$destination)
railRep$des[railRep$des=="flemington"] <- "newmarket"

railRep <- railRep %>% as.data.table() %>% 
  setkeyv(c("org","des","day_type","origin_departure_hour"))
railRep <- timetable[railRep,roll = T, rollends = c(T, T)]
railRep$AdditionalJourneyTime <- railRep$legTime - railRep$AvgTrainTime

#railRep <- railRep %>% select(6:ncol(railRep))

#######################
# Punctuality Metric (percent on time)
#######################

travel_times <- railRep  %>% arrange_('tripID','arrival') %>%
  group_by(tripID, type, direction, peak, Resource.Name) %>%
  summarise(Origin = first(origin), Destination = last(destination),
            Departure = first(departure), Arrival = last(arrival),
            TripTime = sum(legTime), TrainTime = sum(AvgTrainTime),
            XtraTime = first(onTime))
travel_times$Punctual <- ifelse(travel_times$TripTime <= (travel_times$TrainTime+travel_times$XtraTime),1,0)


journey_times <- travel_times %>% group_by(type, peak, direction) %>%
  summarise(TravelTimes = round(mean(TripTime),2),
            Punctuality = sum(Punctual),
            NumBuses = n())

journey_times$Punctuality <- journey_times$Punctuality / journey_times$NumBuses

# write.csv(railRep,"./Output/railRep.csv")
# write.csv(travel_times,"./Output/travel_times.csv")
# write.csv(journey_times,"./Output/journey_times.csv")
#metrics to calculate:

# Bus OD journey time : Average journey time. 
# Suggest metric is daily average split by direction, time band 
# (AM Peak, PM Peak, Non-Peak, Weekend, Public Holiday) and stopping pattern.

# Additional journey time : Establish by time band and direction 
# the average journey time through the affected area that a normal train/tram service 
# would take and then measure the differential from actual bus journey time.

# % Increased journey time : additional journey time / 'normal' journey time
# represented as an average with the same aggregations as the OD journey times.

# Bus sample size : # Valid trips / # Buses allocated 

# % Services on-time : # Buses delivered with an OD Journey time < 4 minutes and 59 seconds 
# more than expected / # Valid trips observed