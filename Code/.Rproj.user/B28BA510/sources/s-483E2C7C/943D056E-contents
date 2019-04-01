rm(list=ls(all=TRUE))

library(dplyr)
library(magrittr)
library(lubridate)
library(xlsx)
library(data.table)

# # Run next 2 lines only if using source() to run the script
# work_dir <- dirname(parent.frame(2)$ofile)
# setwd(work_dir)
setwd(getwd())

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

###########################
# STEP 1: Load the data
###########################

source('..\\Code\\readFile.R')

###########################
# STEP 2: Clean the data
###########################

source('..\\Code\\cleanBuses.R')

###########################
# STEP 3: Assign Bus Routes
###########################

source('..\\Code\\assignRoutes.R')

###########################
# STEP 4: Calculate Metrics
###########################

source('..\\Code\\calculateMetrics.R')

#######################
# STEP 5: Load Tables
#######################

#CODE FOR JOURNEY TIMES TABLE
source('..\\Code\\journeyTimes.R')

#CODE FOR TRAVEL TIMES TABLE
source('..\\Code\\travelTimes.R')

#remove uneccessary tables
remove(nodes, route, routes, to_remove, tt_remove, train.OD, buses, buses.valid, tmp)

# #Refresh/create files with yesterdays and todays Peak travel information
source('..\\Code\\writeFile.R')
