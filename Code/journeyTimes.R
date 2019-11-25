#CODE FOR JOURNEY TIMES TABLE
journey_times <- railRep  %>% arrange(tripid,arrival) %>%
  group_by(tripid, type, direction, peak, resource_name) %>%
  summarise(origin = first(origin), destination = last(destination),
            departure = first(departure), arrival = last(arrival),
            triptime = difftime(last(arrival),first(departure), tz = "AEST", units = "mins"),
            traintime = sum(avgtraintime),
            expected_delay = first(ontime), traintimeint = sum(avgtraintimeint),
            travdate = as.Date(first(departure))) %>%
  group_by(type, peak, direction, travdate) %>%
  mutate(punctual = ifelse(((triptime+traintimeint) <= (traintime+expected_delay)),1,0)) %>%
  summarise(traveltimes = mean(triptime),
            delay = mean((triptime+traintimeint)-traintime),
            additionaltraveltime = mean(delay),
            difference = mean((triptime+traintimeint) - (traintime+expected_delay)),
            punctuality = sum(punctual),
            numbuses = n()) %>% arrange(travdate)

journey_times$punctuality <- round(journey_times$punctuality / journey_times$numbuses,2)
