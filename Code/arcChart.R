library(magrittr)
library(dplyr)
library(tibble)
library(ggraph)
library(igraph)
library(plotly)

sources <- leg_times[3] %>% distinct(origin) %>% rename(label=origin)

destinations <- leg_times[4] %>% distinct(destination) %>% rename(label=destination)

nodes <- stations

edges <- leg_times %>% group_by(origin,destination) %>% 
  summarise(weight=mean(TripTime))

edges <- edges %>% 
  inner_join(nodes, by = c("origin" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  inner_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges <- edges[c(4,5,3)]

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = weight,color=1/weight), alpha = 0.8, arrow = arrow(length = unit(3, 'mm'))) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), color="#000000", size=4) +
  labs(edge_width = "Travel Times") +
  theme_graph()