#CODE FOR TRAVEL TIMES TABLE
travel_times <- railRep  %>% arrange(tripid,arrival) %>%
  group_by(tripid, type, direction, peak, resource_name) %>%
  summarise(origin = first(origin), destination = last(destination),
            departure = first(departure), arrival = last(arrival),
            triptime = difftime(last(arrival),first(departure), tz = "AEST", units = "mins"), 
            traintime = sum(avgtraintime),
            expected_delay = first(ontime), traintimeint = sum(avgtraintimeint))

travel_times$punctual <- ifelse(((travel_times$triptime+travel_times$traintimeint) <= (travel_times$traintime+travel_times$expected_delay)),1,0)

to_remove <- lapply(strsplit(travel_times$resource_name," "), '[[', 1) == "TDV" & 
  travel_times$triptime > 60 & travel_times$departure > as.POSIXct('2019-09-28 00:00:00',tz = 'UTC')
remove_id <- travel_times$tripid[to_remove]

manual_remove <- fread('..\\Input\\removeTrips.csv', sep = ',')

railRep <- railRep %>% filter(!tripid %in% remove_id) %>% filter(!tripid %in% manual_remove$tripID)
railRep.setdiff <- railRep.setdiff %>% filter(!tripid %in% remove_id) %>% filter(!tripid %in% manual_remove$tripID)
travel_times <- travel_times %>% filter(!tripid %in% remove_id) %>% filter(!tripid %in% manual_remove$tripID)

remove('to_remove','remove_id','manual_remove')
