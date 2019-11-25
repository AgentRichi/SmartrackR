library(rjson)
library(DBI)
library(data.table)
library(magrittr)
library(dplyr)

cred = fromJSON(file = "..//dbCred.json")

# loads the PostgreSQL driver
drv <- DBI::dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "NIMP",
                 host = cred$host, port = cred$port,
                 user = cred$user, password = cred$password)
rm(cred) # removes connection info
routes <- dbGetQuery(con,'Select * from master.railrep')
RPostgreSQL::dbDisconnect(con)

routes <- as.data.table(routes)
types <- c("OAK-EMV EXP","WTL-EMV LTD EXP","OAK-BLY LTD EXP")
railRep <- routes[type %in% types]

# compare travel times
#CODE FOR JOURNEY TIMES TABLE
travel_times <- railRep  %>% arrange(tripid,arrival) %>%
  group_by(tripid, type, direction, peak, resource_name) %>%
  summarise(origin = first(origin), destination = last(destination),
            departure = first(departure), arrival = last(arrival),
            triptime = difftime(last(arrival),first(departure), tz = "AEST", units = "mins"),
            traintime = sum(avgtraintime),
            expected_delay = first(ontime), traintime_interchange = sum(avgtraintimeint),
            date = as.Date(first(departure)))

journey_times$punctuality <- round(journey_times$punctuality / journey_times$numbuses,2)

fwrite(railRep,"..//Output/BLY-EMV_analysis_legtimes.csv")
fwrite(travel_times,"..//Output/BLY-EMV_analysis_traveltimes.csv")
