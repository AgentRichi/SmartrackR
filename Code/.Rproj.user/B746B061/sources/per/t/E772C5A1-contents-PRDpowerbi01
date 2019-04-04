#CODE FOR TRAVEL TIMES TABLE
travel_times <- railRep  %>% arrange_('tripID','arrival') %>%
  group_by(tripID, type, direction, peak, Resource.Name) %>%
  summarise(Origin = first(origin), Destination = last(destination),
            Departure = first(departure), Arrival = last(arrival),
            TripTime = difftime(last(arrival),first(departure), tz = "AEST", units = "mins"), 
            TrainTime = sum(AvgTrainTime),XtraTime = first(onTime), XtraTrain = sum(AvgTrainTimeInt))

travel_times$Punctual <- ifelse(((travel_times$TripTime+travel_times$XtraTrain) <= (travel_times$TrainTime+travel_times$XtraTime)),1,0)