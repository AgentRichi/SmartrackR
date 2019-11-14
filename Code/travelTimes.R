#CODE FOR TRAVEL TIMES TABLE
travel_times <- railRep  %>% arrange(tripID,arrival) %>%
  group_by(tripID, type, direction, peak, Resource.Name) %>%
  summarise(Origin = first(origin), Destination = last(destination),
            Departure = first(departure), Arrival = last(arrival),
            TripTime = difftime(last(arrival),first(departure), tz = "AEST", units = "mins"), 
            TrainTime = sum(AvgTrainTime),
            XtraTime = first(onTime), XtraTrain = sum(AvgTrainTimeInt))

travel_times$Punctual <- ifelse(((travel_times$TripTime+travel_times$XtraTrain) <= (travel_times$TrainTime+travel_times$XtraTime)),1,0)

to_remove <- lapply(strsplit(travel_times$Resource.Name," "), '[[', 1) == "TDV" & 
  travel_times$TripTime > 60 & travel_times$Departure > as.POSIXct('2019-09-28 00:00:00',tz = 'UTC')
remove_id <- travel_times$tripID[to_remove]

manual_remove <- fread('..\\Input\\removeTrips.csv', sep = ',')

railRep <- railRep %>% filter(!tripID %in% remove_id) %>% filter(!tripID %in% manual_remove$tripID)
travel_times <- travel_times %>% filter(!tripID %in% remove_id) %>% filter(!tripID %in% manual_remove$tripID)

remove('to_remove','remove_id','manual_remove')
