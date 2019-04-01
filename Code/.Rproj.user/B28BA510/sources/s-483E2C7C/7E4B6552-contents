###########################
# STEP 2: Clean the data
###########################
  
routes[c("start.datetime","end.datetime","start.time.filter","end.time.filter")] <- force_tz(
  routes[c("start.datetime","end.datetime","start.time.filter","end.time.filter")],tz="UTC")
buses_dup <- select(buses,Resource.Name,Registration,
                    Enter.Odometer..km.,Distance.Travelled..km.,
                    Geofence.Name) %>% duplicated(fromLast=T)
buses <- buses %>% filter(Geofence.Name != "", !buses_dup)

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
  mutate(legTime = difftime(arrival,departure, tz = "AEST", units = "mins")+(dwellTime/60))

buses$origin[1] <- "0"

#assign ID
buses <- cbind("ID" = sprintf("%07d", 1:nrow(buses)), buses)