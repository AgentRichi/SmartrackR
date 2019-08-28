import gtfstk as gt
import pandas as pd
pd.set_option('display.max_rows', 30)
pd.set_option('display.max_columns', 30)
pd.set_option('display.width', 1000)
from pathlib import Path
import numpy as np
import sys
from itertools import combinations

# set your locations
GTFS_LOCATION = r'C:\Users\vicxjfn\VicGov\Transport Analysis and Assessment - NAPO\NIMP\User Experience Research\Datasets\code\PTV API\SmartrackR\Input'
train = Path(GTFS_LOCATION, r'gtfs_train.zip')
stopping = Path(GTFS_LOCATION, r'BusRoutesDict.csv')
stopping = Path(GTFS_LOCATION, r'TrainOD.xlsx')
output_path = Path(GTFS_LOCATION, r'mean_stopping_times.csv')

# read gtfs and stopping patterns spreadsheets
feed = gt.read_gtfs(train, dist_units='m')
sp = pd.read_excel(stopping)
# sp = pd.read_csv(stopping)

# changes to sp:
# change names of city stops to match
# work out times in both directions
# add interchange stops

stop_times = feed.stop_times
stop_times['departure_time'] = stop_times['departure_time'].apply(lambda x: pd.Timedelta(x).total_seconds())

# merge stop times with trips
stop_times = pd.merge(stop_times, feed.trips, on='trip_id')

# turn comma separated into a list
sp['Stopping Pattern'] = sp['Stopping Pattern'].str.split(',')

# explode the list
sp2 = sp['Stopping Pattern'].apply(pd.Series) \
    .merge(sp, right_index=True, left_index=True) \
    .drop(["Stopping Pattern"], axis=1) \
    .melt(id_vars=['Route', ], value_name="Stopping Pattern").dropna()

# get the simple name because gtfs has Railway Station written in them
stop_ids = feed.stops
stop_ids['simple'] = stop_ids['stop_name'].str.split(' Rail').str[0]
stop_ids = stop_ids[['stop_id', 'simple']]

# get every combination of stops as a list of tuples
stop_matrix = []
for combo in combinations(stop_ids['simple'], 2):
    stop_matrix = stop_matrix+[combo]
stop_matrix
# merge the 2 datasets so we can get the stop ids attached to OD data
sp3 = pd.merge(sp2, stop_ids, left_on='Stopping Pattern', right_on='simple')

# sort by route and variable so we can get stop order
sp3 = sp3.sort_values(by=['Route', 'variable'])

# shift all the stops up 1 item to get the next stop
sp3[['next_stop_id', 'next_route', 'next_stop']] = sp3[['stop_id', 'Route', 'simple']].shift(-1)

# remove all items where the items arent the same route and arent the same stop
sp3 = sp3.loc[(sp3['Route'] == sp3['next_route']) & (sp3['simple'] != sp3['next_stop'])]

# iterate over this and create the output values, im sure there is a quicker way, but cant be bothered
# working it out
some_list = []
for values in sp3.itertuples():
    # get to filters and join on the trip id
    value = (values.stop_id, values.next_stop_id)
    filtered1 = stop_times.loc[stop_times['stop_id'] == value[0]]
    filtered2 = stop_times.loc[stop_times['stop_id'] == value[1]]
    tj = pd.merge(filtered1, filtered2, on='trip_id')

    # get absolute time difference between the two stops for each trip
    tj['total_time'] = abs(tj['departure_time_x'] - tj['departure_time_y'])

    # create a record and add it to a list, this has the mean time of the tj dataset above
    some_dict = {'from_id': values.stop_id, 'to_id': values.next_stop_id,
                 'from': values.simple, 'to': values.next_stop,
                 'time': tj['total_time'].mean(), 'route': values.Route, 'index': values.variable}
    some_list.append(some_dict)

# convert the list to a dataframe
output = pd.DataFrame(some_list)
# fill the NaT with zeros. This happens when 2 stations are never on the same trip.
output['time'] = output['time'].fillna(0)

# sort the values for easy reading
output = output.sort_values(['route', 'index'])

# export to csv
output.to_csv(output_path, index=False)
