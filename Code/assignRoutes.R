###########################
# STEP 3: Assign Bus Routes
###########################

#function to append rows during bus validation
dt.append <- function(x1, x2) {
  obj <- deparse(substitute(x1)) # get the actual object name as a string
  assign(obj, value = data.table::rbindlist(list(x1, x2)), envir = .GlobalEnv)
  
}

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
  
##
# ADD FILTER: buses to validate > time of last bus in database
##


buses.valid <- buses[0,]

for(i in 1:nrow(routes)){
  
  #route stopping patterns
  route <- routes[i,]
  pattern <- gsub("[{}\"]","",route$stops) %>% strsplit(split = ",") %>% unlist() %>% trimws() %>% toupper()
  pattern.rev <- rev(pattern)
  ignore <- gsub("[{}\"]","",route$passes) %>% strsplit(split = ",") %>% unlist() %>% trimws() %>% toupper()
  
  # all replacement buses are now checked, then dates are evaluated later
  railRep <- buses %>% filter(!(toupper(destination) %in% ignore)) 

  railRep <-  railRep %>%
    mutate(departure = lag(dwellTime,1)+lag(arrival,1),
           origin = ifelse(lag(Resource.Name,1)==Resource.Name,lag(destination,1),"0")) %>%
    mutate(legTime = difftime(arrival,departure, units = "mins")+(dwellTime/60)) %>%
    arrange(Resource.Name,departure) 
  
  railRep <- railRep %>% filter(origin != destination)
  #adjust peak periods
  peak_adjust <- minutes(route$peak_adjust)
  am.start.route <- (am.start + peak_adjust)
  am.end.route <- (am.end + peak_adjust)
  pm.start.route <- (pm.start + peak_adjust)
  pm.end.route <- (pm.end + peak_adjust)
  
  #while loop variables
  stops <- 1
  tripID <- 1
  org <- railRep$origin %>% trimws() %>% toupper()
  des <- railRep$destination %>% trimws() %>% toupper()
  resource <- railRep$Resource.Name
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
        railRep$onTime[stops:(stops+N-2)] <- railRep$onTime[stops:(stops+N-2)] + route$additional_time
        railRep$peak[stops:(stops+N-2)] <- peak(railRep$arrival[(stops+N-2)],
                                                am.start.route,am.end.route,
                                                pm.start.route,pm.end.route)
        railRep$project[stops:(stops+N-2)] <- route$project
        railRep$interchange[(stops+N-2)] <- route$interchange
        #remove dwelltime from final destination
        railRep$dwellTime[(stops+N-2)] <- 0
        
        #Adjust start and end of trip by 2 minutes to account for time b/n entering Geofence and actually stopping
        railRep$departure[stops] <- railRep$departure[stops] - minutes(2)
        railRep$arrival[(stops+N-2)] <- railRep$arrival[(stops+N-2)] + minutes(2)
        
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
        railRep$onTime[stops:(stops+N-2)] <- railRep$onTime[stops:(stops+N-2)] + route$additional_time
        railRep$peak[stops:(stops+N-2)] <- peak(railRep$departure[stops],
                                                am.start.route,am.end.route,
                                                pm.start.route,pm.end.route)
        railRep$project[stops:(stops+N-2)] <- route$project
        railRep$interchange[stops] <- route$interchange
        #remove dwelltime from final destination
        railRep$dwellTime[(stops+N-2)] <- 0
        
        #Adjust start and end of trip by 2 minutes to account for time b/n entering Geofence and actually stopping
        railRep$departure[stops] <- railRep$departure[stops] - minutes(2)
        railRep$arrival[(stops+N-2)] <- railRep$arrival[(stops+N-2)] + minutes(2)
        
        tripID <- tripID+1
        stops <- stops+(N-1)
      }
      
      #else not a replacement bus
      else(stops = stops+1)
    } else(stops = stops+1)
  }
  railRep <- railRep %>% filter(type != "0") %>% 
    mutate(legTime = difftime(arrival,departure, tz = "AEST", units = "mins")+(dwellTime/60))
  buses.valid <- dt.append(buses.valid,railRep)
}
