rm(list=ls(all=TRUE))

library(dplyr)
library(data.table)
library(magrittr)
library(lubridate)

work_dir <- "C://Data//SmartrackR//"
setwd(paste0(work_dir, ".//Data//"))


# Load the data
tfv_file <- list.files()
buses <- data.frame()

for(i in 1:length(tfv_file))
{
  buses <- rbind(buses,
                     read.csv(tfv_file[i], header = TRUE,stringsAsFactors = F, skip=1))
}

setwd(work_dir)
# buses <- read.csv("C://Data/Smartrack/Data/TFV - CTD_13062018.csv", header = T, stringsAsFactors = F) %>% as.tbl()
# validLegs <- read.csv("C://Data/Smartrack/ValidLegs.csv", stringsAsFactors = F)

buses <- buses %>% filter(Geofence.Name != "")

#Enter.Time is in a different format when dowloading directly compared to converting from xlsx
# for csv direct download use as.POSIXct(strptime(gsub('[\\.]','',Enter.Time), format = '%d/%m/%Y %H:%M'))
# for converted csv use as.POSIXct(Enter.Time), format = '%d/%m/%Y %I:%M:%S %p'
# format arrival time and separate geofence name into columns, then order by bus and timestamp
buses <- buses %>% mutate(arrival = as.POSIXct(strptime(gsub('[\\.]','',Enter.Time), format = '%d/%m/%Y %I:%M:%S %p')),
                         project = unlist(lapply(strsplit(Geofence.Name," - "),'[[',2)),
                         stop.order = as.numeric(unlist(lapply(strsplit(Geofence.Name," - "),'[[',3))),
                         destination = unlist(lapply(strsplit(Geofence.Name," - "),'[[',4))) %>%
  arrange(Resource.Name, seconds(Enter.Time))

#format dwell time into seconds
tmp <- strsplit(buses$Time.Inside.Geofence..hh.mm.ss., split=":") %>% lapply(as.numeric,1)
dwellTime <- do.call(rbind, lapply(tmp, rbind))
dwellTime <- dwellTime[,1]*60*60 + dwellTime[,2]*60 + dwellTime[,3]
buses$dwellTime <- dwellTime %>% seconds()


#calculate departure time (arrival+dwell) and get origin from preceding row
buses <- buses %>% mutate(departure = lag(dwellTime,1)+lag(arrival,1),
                          origin = ifelse(lag(Resource.Name,1)==Resource.Name,lag(destination,1),"0"))
buses$origin[1] <- "0"


#VALID TRIPS:
# Caulfield - Carnegie - LTD EXP Flag - Full Exp Flag - Westall
# Malvern - Carnegie - LTD EXP Flag - Oakleigh - Huntingdale - Clayton - Westall
# Caulfield - Carnegie - Murrumbeena - Hughesdale - Oakleigh - Huntingdale - Clayton - Westall

#NEW VALID TRIPS:
#Stopping all Stations (SAS): Newmarket - Ascot Vale - Moonee Ponds - Essendon
#Limited Express (LTD EXP): Flemington Racecourse - Essendon - Glenbervie - Strathmore - Pascoe Vale - Oak Park - Glenroy - Broadmeadows
#Express (EXP): Flemington Racecourse - EXP Flag - Broadmeadows

express <- c("Flemington","Full Exp Flag","Broadmeadows") #-2
express_2 <- c("Flemington","Newmarket","Full Exp Flag","Broadmeadows") #-1
ltd_express <- c("Flemington","Essendon","Glenbervie","Strathmore","Pascoe Vale","Oak Park","Glenroy","Broadmeadows") #+1
sas <- c("Newmarket","Ascot Vale","Moonee Ponds","Essendon") #-4


#Variables for while loop
stops <- 1
buses$type <- c(rep(0,length(buses$Resource.Name)))


#Iterate to assign bus type based on stopping pattern

############
#ISSUE!!: Currently if a bus enters/exits the same geofence more than once (happening at Oakleigh) - 
#the trip is filtered out, even though it might be a legitimate replacement bus
############

while(stops < length(buses$origin)) {
  org = buses$origin
  dest = buses$destination
  
  #EXPRESS BUS
  if ((
      (org[stops] == first(express) && dest[stops] == express[2]) &&
      (lead(org,1)[stops] == express[2] && lead(dest,1)[stops] == last(express))
  )
      ||
    (
      (org[stops] == last(express) && dest[stops] == express[2]) &&
      (lead(org,1)[stops] == express[2] && lead(dest,1)[stops] == first(express))
     )
      ||
  (
    (org[stops] == first(express_2) && dest[stops] == express_2[2]) &&
    (lead(org,1)[stops] == express_2[2] && lead(dest,1)[stops] == express_2[3]) &&
    (lead(org,1)[stops] == express_2[2] && lead(dest,1)[stops] == last(express_2))
  )
  ||
  (
    (org[stops] == last(express_2) && dest[stops] == express_2[3]) &&
    (lead(org,1)[stops] == express_2[3] && lead(dest,1)[stops] == express_2[2]) &&
    (lead(org,1)[stops] == express_2[2] && lead(dest,1)[stops] == first(express_2))
  ))
    {
      buses$type[stops:(stops+length(express)-2)] <- "Express"
      stops <- stops+length(express)-1
  }
  
  #LTD EXPRESS BUS
  else if ((
    (org[stops] == first(ltd_express) && dest[stops] == ltd_express[2]) &&
    (lead(org,1)[stops] == ltd_express[2] && lead(dest,1)[stops] == ltd_express[3]) &&
    (lead(org,2)[stops] == ltd_express[3] && lead(dest,2)[stops] == ltd_express[4]) &&
    (lead(org,3)[stops] == ltd_express[4] && lead(dest,3)[stops] == ltd_express[5]) &&
    (lead(org,4)[stops] == ltd_express[5] && lead(dest,4)[stops] == ltd_express[6]) &&
    (lead(org,5)[stops] == ltd_express[6] && lead(dest,5)[stops] == ltd_express[7]) &&
    (lead(org,5)[stops] == ltd_express[7] && lead(dest,6)[stops] == last(ltd_express))
  )
  ||
  ( #last = 8
    (org[stops] == last(ltd_express) && dest[stops] == ltd_express[7]) &&
    (lead(org,1)[stops] == ltd_express[7] && lead(dest,1)[stops] == ltd_express[6]) &&
    (lead(org,2)[stops] == ltd_express[6] && lead(dest,2)[stops] == ltd_express[5]) &&
    (lead(org,3)[stops] == ltd_express[5] && lead(dest,3)[stops] == ltd_express[4]) &&
    (lead(org,4)[stops] == ltd_express[4] && lead(dest,4)[stops] == ltd_express[3]) &&
    (lead(org,5)[stops] == ltd_express[3] && lead(dest,5)[stops] == ltd_express[2]) &&
    (lead(org,6)[stops] == ltd_express[2] && lead(dest,6)[stops] == first(ltd_express))
  ))
  {
    buses$type[stops:(stops+length(ltd_express)-2)] <- "LTD ltd_express"
    stops <- stops+length(ltd_express)-1
  }

  #SAS BUS
  else if ((
    (org[stops] == first(sas) && dest[stops] == sas[2]) &&
    (lead(org,1)[stops] == sas[2] && lead(dest,1)[stops] == sas[3]) &&
    (lead(org,2)[stops] == sas[3] && lead(dest,2)[stops] == sas[4])
  )
  ||
  ( #last = 4
    (org[stops] == last(sas) && dest[stops] == sas[3]) &&
    (lead(org,1)[stops] == sas[3] && lead(dest,1)[stops] == sas[2]) &&
    (lead(org,2)[stops] == sas[2] && lead(dest,2)[stops] == sas[1])
  ))
  {
    buses$type[stops:(stops+length(sas)-2)] <- "SAS"
    stops <- stops+length(sas)-1
  }
  else {
    buses$type[stops] <- "None"
    stops <- stops+1
    }
}

#remove non RRP Buses and unnecessary columns
railRep <- buses %>% filter(type != "None") %>% 
  select(project,Resource.Name,Registration,type,origin,destination,departure,arrival,dwellTime)


#variables needed to work out trip ID
startpoints <- c("Broadmeadows","Flemington")
sas_startpoints <- c("Essendon","Newmarket")
new_trips <- railRep$origin %in% startpoints
railRep$tripId <- c(rep(0,length(railRep$Resource.Name)))
id = 0

# logic: if there is no origin (i.e. this is the first data point for the bus) OR 
# if the origin is a startpoint, then the leg is part of a new trip
for(leg in seq(1,length(railRep$origin))) {
  if(railRep$type[leg] == "SAS" & railRep$origin[leg] %in% sas_startpoints) {
    id = id+1
    railRep$tripId[leg] <- id
  } else if(railRep$type[leg] != "SAS" & railRep$origin[leg] %in% startpoints) {
    id = id+1
    railRep$tripId[leg] <- id
  } else {
  railRep$tripId[leg] <- id
  }
}

# ifelse(
#   (railRep$type == "SAS" & railRep$origin %in% sas_startpoints),
#   id+1, ifelse(
#     railRep$type != "SAS" & railRep$origin %in% startpoints,
#     id+1, id
#   )
# )

# Determine the peak time
tripDeparture <- railRep %>% filter(!duplicated(tripId)) %>%
  select(tripId,departure) %>% mutate(tripDeparture = hour(departure)) %>%
  select(tripId,tripDeparture)

railRep <- railRep %>% left_join(tripDeparture,"tripId")

railRep <- railRep %>% mutate(peak = sapply(tripDeparture,function(x){
  if(x>6 & x<10){"AM Peak"}
  else if (x>9 & x<16){"Intra Peak"}
  else if (x>15 & x<19){"PM Peak"}
  else {"Off Peak"}
}))

# if the destination OR origin is a start/end point then dwelltime = 0, else dwelltime/60 to get minutes
#NOTE this is not entirely accurate, but to check dwell times at start/end, we need more validation
#to ensure the bus was not sitting idle and actually picking up/dropping off passengers
for(i in seq(1:length(railRep$dwellTime))) {
  if (
    ((railRep$destination[i] %in% startpoints || railRep$origin[i] %in% startpoints) && 
     railRep$type[i] !="SAS") ||
    ((railRep$destination[i] %in% sas_startpoints || railRep$origin[i] %in% sas_startpoints) && 
     railRep$type[i] == "SAS")) {
    railRep$dwellAdj[i] <- 0
  } else {railRep$dwellAdj[i] <- railRep$dwellTime[i]/60}
}

##
#REMOVE EXPRESS FLAGS
##

#Step 1: separate express buses
exp_buses <- data.table(railRep[railRep$type=="Express",c("origin",
                                                          "destination",
                                                          "departure",
                                                          "arrival",
                                                          "tripId",
                                                          "dwellAdj")],
                        key = "tripId")

#Step 2: For each trip, select org and dest, and sum up travel time
exp_buses <- exp_buses[,list(
  origin=first(origin),
  destination=last(destination),
  departure=first(departure),
  arrival=last(arrival),
  dwellAdj=sum(dwellAdj)),by=tripId] %>% 
  
#Step 3: join other columns from railrep table
  left_join(railRep[,c("project",
                       "Resource.Name",
                       "Registration",
                       "type",
                       "tripId",
                       "tripDeparture",
                       "peak")],
              by = "tripId")

#Step 4: Combine express buses with other buses
railRep <- rbind(railRep[railRep$type != "Express",names(railRep) !="dwellTime"],exp_buses)

  # ifelse((
  # (railRep$destination %in% startpoints && railRep$type !="SAS") 
  # || 
  # (railRep$origin %in% sas_startpoints && railRep$type == "SAS")),
  # 0, railRep$dwellTime/60)
# travel_times <- railRep %>% 
#   filter(difftime(arrival,departure, tz = "AEST", units = "mins") < 50,
#          !(tripId %in% c(77,127))) %>% 
#   group_by(tripId, type, peak) %>%
#   summarise(TripTime = sum(difftime(arrival,departure, tz = "AEST", units = "mins")+dwellAdj))

# travel_times <- travel_times %>% group_by(type, peak) %>%
#   summarise(TravelTimes = mean(TripTime))

# write.csv(travel_times,paste0("test2.csv"))


# write.csv(travel_times,paste0("CTD Bus Travel Times - ",date(Sys.time()-days(1)),".csv"))


###TO DO:
#Add dwell times to total JT - Done
#Calculate time for each leg of journey - Done but need to deal with express flags

leg_times <- railRep %>%
  filter(difftime(arrival,departure, tz = "AEST", units = "mins") < 50) %>%
  group_by(tripId, origin, destination, peak, type, departure, arrival,dwellAdj) %>%
  summarise(TripTime = sum(difftime(arrival,departure, tz = "AEST", units = "mins")+dwellAdj)) %>%
  filter(date(departure) == date(arrival))

# leg_times <- leg_times %>% group_by(origin,destination, peak, type) %>%
#  summarise(TravelTimes = mean(TripTime))

write.csv(leg_times,paste0("Output/CGB Bus Leg Times - ",date(Sys.time()-days(1)),".csv"), row.names = F)

merge_times <- merge(railRep, leg_times, by = c("tripId","type","peak","departure","arrival","origin","destination"), all.x = T)

merge_times <- cbind("VCDI_ID" = sprintf("VCDI_ID_%06d", 1:nrow(merge_times)), merge_times)

merge_times$tripId <- as.character(merge_times$tripId)

# Validation 

tripTime_check <- subset(merge_times, merge_times$TripTime >= 30)

merge_times <- merge_times[c( "VCDI_ID"
                              ,"tripId"
                              ,"project"
                              ,"Resource.Name"
                              ,"Registration"
                              ,"departure"
                              ,"arrival"
                              ,"type"
                              ,"peak"
                              ,"origin"
                              ,"destination" 
                              ,"dwellAdj"
                              ,"TripTime"
)]

write.csv(merge_times,paste0("Output/","CGB Bus Leg Times.csv"), row.names = F)


# Total Travel Time

travel_times <- railRep %>%
  filter(difftime(arrival,departure, tz = "AEST", units = "mins") < 50) %>%
  # !(tripId %in% c(77,127))
  group_by(tripId, type, peak) %>%
  summarise(TripTime = sum(difftime(arrival,departure, tz = "AEST", units = "mins")+dwellAdj))

 travel_times <- travel_times %>% group_by(type, peak) %>%
   summarise(TravelTimes = mean(TripTime))

write.csv(travel_times,paste0("Output/","CGB Bus Trip Times.csv"), row.names = F)
