library(data.table)

# load data
trainSum <- fread("C:/Data/SUM/ServiceUsageModel_Train_May2017.csv")

# create timetable [O-D travel time x time of day (intervals)]
timetable <- trainSum[,c(1:5,8,11)]

# compare bus leg times to TT & calculate additional travel time