library(data.table)
library(magrittr)
library(reshape)

railRep <- fread('../PowerBI_flat_files/railRep.csv')
travel_times <- fread('../PowerBI_flat_files/travel_times.csv')
journey_times <- fread('../PowerBI_flat_files/journey_times.csv')

railRep <- railRep[departure>as.Date('2018-12-01')]
travel_times <- travel_times[Departure>as.Date('2018-12-01')]
journey_times <- journey_times[Date>as.Date('2018-12-01')]

railRep$period <- ifelse(railRep$departure < as.Date('2019-02-01'),'Dec/Jan19','Apr19')
travel_times$period <- ifelse(travel_times$Departure < as.Date('2019-02-01'),'Dec/Jan19','Apr19')
journey_times$period <- ifelse(journey_times$Date < as.Date('2019-02-01'),'Dec/Jan19','Apr19')

join_project <- railRep[,.(type,project)] %>% unique() %>% setkey(type)

group <- rep('0',nrow(join_project))
for (i in 1:nrow(join_project)) {
  if(join_project$type[i] %in% c('BER-WES LTD EXP','BER-WES SAS','CRB-WES EXP','CRB-WES LTD EXP',
                                 'DNG-WES EXP','DNG-WES SAS','PKH-WES EXP','PKH-WES LTD EXP')){
    group[i] <- 'Cran/Pak Outer'
  } else if(join_project$type[i] %in% c('CFD-ART EXP','CFD-FED EXP','CFD-PAR SAS')){
    group[i] <- 'Cran/Pak Inner'
  } else if(join_project$type[i] %in% c('EPP-BEL LTD EXP','EPP-TBR LTD EXP','SMG-WAT','TMT-BEL SAS','TMT-TBR SAS')){
    group[i] <- 'Mernda'
  } else {
    group[i] <- 'Weekend Inner'
  }
}

join_project <- join_project[,"group" := group]
setkey(travel_times,type)
travel_times <- travel_times[join_project]

#RRB count by route
#fields needed: type, time period, count, project
figure_1 <- travel_times[,.(n=.N),.(type,period)]
fwrite(figure_1,'..\\Charts\\busCount.csv')

#TT by time of day
#fields needed: type, peak, travel times
figure_2 <- travel_times[,.(time=mean(TripTime)),.(type,peak)]
fwrite(figure_2,'..\\Charts\\travelTime.csv')

#boxnwhisker
#fileds: type, 90th, 10th, median
# figure_3 <- travel_times[,.(median_tt=median(TripTime),
#                             '10th'=quantile(TripTime,.1),
#                             '90th'=quantile(TripTime,.9)),
#                          .(type)]
# 
# #
# fwrite(figure_3,'..\\Charts\\boxnwhisker.csv')

#june vs april route comparison
figure_4 <- melt(travel_times,id=c('period','type','peak'),measure="TripTime")
figure_4.1 <- cast(figure_4,type+peak~period,fun.aggregate = mean)
figure_4.1 <- figure_4.1[complete.cases(figure_4.1),]
fwrite(figure_4.1,'..\\Charts\\junaprcomp.csv')