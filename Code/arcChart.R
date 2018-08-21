############################################################################
# Title:        arcChart.R
# Description:  heavily modified version of arc-diagram function by Gaston Sanchez
# Author:       Richard Rossmann
#               https://www.linkedin.com/in/rossmannrichard/
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2018, Richard Rossmann
#               All rights reserved
############################################################################
############################################################################
# Title:        arcDiagram.R
# Description:  function to plot a basic arc-diagram
# Author:       Gaston Sanchez
#               www.gastonsanchez.com
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
############################################################################

library(plotly)
library(magrittr)
library(dplyr)
library(tibble)
library(RColorBrewer)

color.gradient <- function(x, colors=c("#c9cba3","#ffe1a8","#e26d5c"), colsteps=length(x)) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

arcDiagram <- function(
  edgelist, dat=5, categories = 1, sorted=TRUE, decreasing=FALSE, lwd=NULL,
  col=NULL, cex=NULL, col.nodes=NULL, lend=1, ljoin=2, lmitre=1,
  las=2, bg=NULL, mar=c(4,1,3,1))
{
  # ARGUMENTS
  # edgelist:   two-column matrix with edges
  # sorted:     logical to indicate if nodes should be sorted
  # decreasing: logical to indicate type of sorting (used only when sorted=TRUE)
  # lwd:        widths for the arcs (default 1)
  # col:        color for the arcs (default "gray50")
  # cex:        magnification of the nodes labels (default 1)
  # col.nodes:  color of the nodes labels (default "gray50")
  # lend:       the line end style for the arcs (see par)
  # ljoin:      the line join style for the arcs (see par)
  # lmitre:     the line mitre limit fort the arcs (see par)
  # las:        numeric in {0,1,2,3}; the style of axis labels (see par)
  # bg:         background color (default "white")
  # mar:        numeric vector for margins (see par)
  
  # make sure edgelist is a two-col matrix
  if (!is.matrix(edgelist) || ncol(edgelist)!=2)
    stop("argument 'edgelist' must be a two column matrix")
  edges = edgelist
  # how many edges
  ne = nrow(edges)
  # get nodes
  nodes = unique(as.vector(edges))
  nums = seq_along(nodes)
  # how many nodes
  nn = length(nodes)  
  # ennumerate
  if (sorted) {
    nodes = sort(nodes, decreasing=decreasing)
    nums = order(nodes, decreasing=decreasing)
  }
  # check default argument values
  if (is.null(lwd)) lwd = rep(1, ne)
  if (length(lwd) != ne) lwd = rep(lwd, length=ne)
  if (is.null(col)) col = rep("gray50", ne)
  if (length(col) != ne) col = rep(col, length=ne)
  if (is.null(cex)) cex = rep(1, nn)
  if (length(cex) != nn) cex = rep(cex, length=nn)
  if (is.null(col.nodes)) col.nodes = rep("gray50", nn)
  if (length(col.nodes) != nn) col.nodes = rep(col.nodes, length=nn)
  if (is.null(bg)) bg = "white"
  dat <- as.data.frame(dat) %>% sapply(as.numeric)
  if (length(dat)==1) wd = rep(dat,nrow(edges))
  # scale the weight
  wd <- (dat-min(dat))/(max(dat)-min(dat))*10 +1
  wd.col <- color.gradient(wd)
  
  # wrap coordinates in 
  
  dataset <- as.data.frame(edges)
  dataset$category <- categories
  nodes <- unique(as.vector(dataset[,1:2]))
  dataset <- split(dataset,f=dataset[,length(dataset)])
  print(dataset)
  centers <- data.frame(origin=as.character(),
                        x.coord=as.numeric(),
                        y.coord=as.numeric())
  off.set <- 0
  for (category in dataset) {
    category <- category[1] %>% as.matrix()
    nodes <- unique(as.vector(category))
    nodes <- nodes[!(nodes %in% centers$origin)]
    nn = length(nodes)
    if (nn>0) {
      # node labels coordinates
      nf = rep(1 / nn, nn)
      # node labels center coordinates
      fin = cumsum(nf)
      ini = c(0, cumsum(nf)[-nn])
      x.coord = (ini + fin) / 2
      print(x.coord)
      y.coord = off.set
      print(y.coord)
      origin = nodes
      print(origin)
      centers.new <- data.frame(origin=origin,
                                x.coord=x.coord,
                                y.coord=y.coord)
      print(centers.new)
      centers <- rbind(centers,centers.new)
      off.set = off.set-1
    }
  }
  
  edges.3 <- as.data.frame(edges)
  edges.3 <- merge(edges.3,centers)
  edges.3 <- merge(edges.3,centers,by.x = "destination", by.y = "origin")
  edges.3$midpoint.x <- (edges.3$x.coord.x+edges.3$x.coord.y)/2
  edges.3$midpoint.y <- (edges.3$y.coord.x+edges.3$y.coord.y)/2
  
  radios <- sqrt(
    (edges.3$midpoint.x - edges.3$x.coord.x)^2 +
      (edges.3$midpoint.y - edges.3$x.coord.y)^2 
  )
  
  arc.start <- atan((edges.3$y.coord.x - edges.3$midpoint.y) /
                      (edges.3$x.coord.x - edges.3$midpoint.x))
  arc.end <- atan((edges.3$y.coord.y - edges.3$midpoint.y) /
                    (edges.3$x.coord.y - edges.3$midpoint.x))
  

  # nn = length(nodes)
  # # node labels coordinates
  # nf = rep(1 / nn, nn)
  # # node labels center coordinates
  # fin = cumsum(nf)
  # ini = c(0, cumsum(nf)[-nn])
  # centers = (ini + fin) / 2
  #   names(centers) = nodes
  # # arcs coordinates
  # # matrix with numeric indices
  # e_num = matrix(0, nrow(edges), ncol(edges))
  # for (i in 1:nrow(edges))
  # {
  #   e_num[i,1] = centers[which(nodes == edges[i,1])]
  #   e_num[i,2] = centers[which(nodes == edges[i,2])]
  # }
  # max arc radius
  # multiply by -1 to flip arcs
  #radios = ((e_num[,1] - e_num[,2]) / 2) * -1
  # max_radios = which(radios == max(radios))
  # max_rad = unique(radios[max_radios] / 2)
  # min_radios = which(radios == min(radios))
  # min_rad = unique(radios[min_radios] / 2)
  # arc locations
  locs.x = edges.3$midpoint.x
  locs.y = edges.3$midpoint.y
  #colors
  cols <- brewer.pal(8,"Set3")
  # plot
  # par(mar = mar, bg = bg)
  # plot.new()
  # plot.window(xlim=c(-0.025, 1.025), ylim=c(1*min_rad*2, 1*max_rad*2))
  p <- plot_ly()
  # plot connecting arcs
  for (i in 1:nrow(edges))
  {
    z = seq(arc.start[i]-pi, arc.end[i]+pi, l=100)
    print("z")
    print(z)
    #   radio = radios[i]
    #   x = locs[i] + radio * cos(z)
    #   y = radio * sin(z)
    #   lines(x, y, col=col[i], lwd=lwd[i], 
    #         lend=lend, ljoin=ljoin, lmitre=lmitre)
    radio = radios[i]
    print("radio")
    print(radio)
    x = locs.x[i] + radio * cos(z)
    print(head(x))
    y = locs.y[i] + radio * sin(z)
    print(head(y))
    #y = y + ifelse(y[[2]]>0,0.01,-0.01) #move y up/down to show label
    width <- wd[i]
    color <- wd.col[i]
    txt <- paste0(colnames(dat),": ",format(dat[i],digits = 2))
    p <- add_trace(p,
                   x = x,
                   y = y, 
                   hoverinfo = "text",
                   text = txt,
                   line = list(color = color, shape = "spline", width = width),
                   mode = "lines",
                   name = "", 
                   type = "scatter")
  }
  
  axis_template <- list(showgrid = F , zeroline = F, showline = F, showticklabels = F)
  
  
  p <- p %>%  add_text(x=edges.3$x.coord.x,
                       y=edges.3$y.coord.x,
                       hoverinfo = "none",
                       text = edges.3$origin, 
                       textfont = list(color = '#000000', size = 14))  %>%
    layout(xaxis = axis_template,
           yaxis = axis_template,
           showlegend = F)
  p
  # add node names
  # mtext(nodes, side=1, line=0, at=centers, cex=cex, 
  #       col=col.nodes, las=las)
}






################################################################
#   Charting
################################################################
# RUN Smartrack.R FIRST!!

sources <- leg_times[3] %>% distinct(origin) %>% rename(label=origin)
sources
destinations <- leg_times[4] %>% distinct(destination) %>% rename(label=destination)

nodes <- stations

edges <- leg_times %>% group_by(origin,destination) %>% 
  summarise("Average Travel Time"=mean(TripTime))

edges <- edges %>% 
  inner_join(nodes, by = c("origin" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  inner_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

#sort
edges <- edges[with(edges, order(from,to)),]
edges
testcat <- c(1,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
arcDiagram(as.matrix(edges[1:2]), dat = edges[3], categories = testcat, sorted = F, lwd = 3,cex = 0.5)
# trace1 <- list(
#   x = c(2.0, 1.99305941144, 1.98574643661, 1.97805434163, 1.96997690531, 1.96150847788, 1.95264404104, 1.94337926902, 1.93371059014, 1.92363524842, 1.91315136476, 1.90225799707, 1.89095519865, 1.87924407431, 1.86712683348, 1.85460683947, 1.84168865435, 1.82837807854, 1.81468218442, 1.80060934326, 1.78616924477, 1.77137290855, 1.75623268698, 1.74076225889, 1.72497661366, 1.70889202541, 1.69252601703, 1.675897314, 1.65902578797, 1.64193239031, 1.62463907603, 1.60716871832, 1.58954501452, 1.57179238419, 1.55393586006, 1.536000973, 1.51801363194, 1.5, 1.48198636806, 1.463999027, 1.44606413994, 1.42820761581, 1.41045498548, 1.39283128168, 1.37536092397, 1.35806760969, 1.34097421203, 1.324102686, 1.30747398297, 1.29110797459, 1.27502338634, 1.25923774111, 1.24376731302, 1.22862709145, 1.21383075523, 1.19939065674, 1.18531781558, 1.17162192146, 1.15831134565, 1.14539316053, 1.13287316652, 1.12075592569, 1.10904480135, 1.09774200293, 1.08684863524, 1.07636475158, 1.06628940986, 1.05662073098, 1.04735595896, 1.03849152212, 1.03002309469, 1.02194565837, 1.01425356339, 1.00694058856, 1.0), 
#   y = c(0.0, 0.0117008799697, 0.0233885330354, 0.0350490995641, 0.0466680356158, 0.0582301236222, 0.0697194879132, 0.0811196154134, 0.0924133818105, 0.103583083462, 0.114610475273, 0.125476814724, 0.136162912176, 0.14664918753, 0.156915733214, 0.166942383435, 0.176708789515, 0.186194501058, 0.1953790526, 0.204242055282, 0.212763293013, 0.220922822464, 0.228701076161, 0.236078967845, 0.243037999191, 0.249560366887, 0.255629069045, 0.261228009841, 0.266342101259, 0.27095736081, 0.275061004089, 0.278641531075, 0.281688805103, 0.284194123531, 0.28615027919, 0.287551611814, 0.288394048777, 0.288675134595, 0.288394048777, 0.287551611814, 0.28615027919, 0.284194123531, 0.281688805103, 0.278641531075, 0.275061004089, 0.27095736081, 0.266342101259, 0.261228009841, 0.255629069045, 0.249560366887, 0.243037999191, 0.236078967845, 0.228701076161, 0.220922822464, 0.212763293013, 0.204242055282, 0.1953790526, 0.186194501058, 0.176708789515, 0.166942383435, 0.156915733214, 0.14664918753, 0.136162912176, 0.125476814724, 0.114610475273, 0.103583083462, 0.0924133818105, 0.0811196154134, 0.0697194879132, 0.0582301236222, 0.0466680356158, 0.0350490995641, 0.0233885330354, 0.0117008799697, 0.0), 
#   hoverinfo = "none", 
#   line = list(
#     color = "#6b8aca", 
#     shape = "spline", 
#     width = 0.5
#   ), 
#   mode = "lines", 
#   name = "", 
#   type = "scatter"
# )
# p <- plot_ly()
# p <- add_trace(p, x=trace1$x, y=trace1$y, hoverinfo=trace1$hoverinfo, line=trace1$line, mode=trace1$mode, name=trace1$name, type=trace1$type)
# p %>% add_text(x=mean(trace1$x),y=max(trace1$y),text = "12")

# #Colorbrewer
# wd <- as.data.frame(wed) %>% sapply(as.numeric)
# wd <- (wd-min(wd))/(max(wd)-min(wd))*10 +1
# rand.data <- wd
# br.range <- seq(min(rand.data),max(rand.data),length.out=10)
# results <- sapply(1:ncol(rand.data),function(x) hist(rand.data[,x],plot=F,br=br.range)$counts)
# cols <- brewer.pal(8,"Set3")
# lapply(1:ncol(results),function(x) print(cols[x]))




# # DRAW A ROUNDED RECTANGLE:
# p <- plot_ly()
# e_num <- matrix(c(0.04545455,0.04545455,0.13636364,0.59090909),ncol = 2)
# # r = 0.025
# # locs = rowSums(e_num) + r
# height.in = ((e_num[,1] - e_num[,2]) / 4) * -1
# radios = height.in/3
# height.out = height.in + radios
# radio = radios[1]
# # line_1
# x <- c(e_num[1,1],e_num[1,1])
# y <- c(0,height.in[1])
# # circle_1
# z1 = seq(pi, pi/2, l=50)
# c1.x = (e_num[1,1] + radio) + (radio * cos(z1))
# x = c(x,c1.x)
# c1.y = height.in[1] + (radio * sin(z1))
# y = c(y,c1.y)
# # line_horizontal
# lh.x = (tail(x,1)+(tail(x,1)-head(x,1))/2)
# x = c(x,lh.x)
# y = c(y,tail(y,1))
# # circle_2
# # z1 = seq(0, pi/2 , l=50)
# # c1.x = (e_num[1,1] + r) + r * cos(z1)
# # x = c(x,c1.x)
# # c1.y = height.in[1] + r * sin(z1)
# # y = c(y,c1.y)
# # line_2
# p <- add_trace(p,x=x,y=y,                   mode = "lines",
#                name = "", 
#                type = "scatter")  %>%
#   layout(
#     xaxis = list(range = c(0, 0.2)),
#     yaxis = list(range = c(0, 0.2)))
# p
# 
# 
# radios = ((e_num[,1] - e_num[,2]) / 2) * -1
# locs = rowSums(e_num) / 2
# # p <- plot_ly()
# # plot connecting arcs
# z = seq(0, pi, l=100)
# radio = radios[1]
# x = locs[1] + radio * cos(z)
# y = 2 + radio * sin(z)
# p <- plot_ly() %>% add_trace(x=x,y=y,                   mode = "lines",
#                name = "",
#                type = "scatter")
# p
# 
# edges.2 <- as.data.frame(edges)
# edges.2$category <- testcat
# nodes <- unique(as.vector(edges.2[,1:2]))
# dataset <- split(dataset,f=dataset[,length(dataset)])
# print(dataset)
# centers <- data.frame(origin=as.character(),
#                       x.coord=as.numeric(),
#                       y.coord=as.numeric())
# off.set <- 0
# for (category in dataset) {
#   category <- category[1] %>% as.matrix()
#   nodes <- unique(as.vector(category))
#   nodes <- nodes[!(nodes %in% centers$origin)]
#   nn = length(nodes)
#   if (nn>0) {
#     # node labels coordinates
#     nf = rep(1 / nn, nn)
#     # node labels center coordinates
#     fin = cumsum(nf)
#     ini = c(0, cumsum(nf)[-nn])
#     x.coord = (ini + fin) / 2
#     print(x.coord)
#     y.coord = off.set
#     print(y.coord)
#     origin = nodes
#     print(origin)
#     centers.new <- data.frame(origin=origin,
#                               x.coord=x.coord,
#                               y.coord=y.coord)
#     print(centers.new)
#     centers <- rbind(centers,centers.new)
#     off.set = off.set-1
#   }
# }
# 
# edges.3 <- as.data.frame(edges[1:2])
# edges.3 <- merge(edges.3,centers)
# edges.3 <- merge(edges.3,centers,by.x = "destination", by.y = "origin")
# edges.3$midpoint.x <- (edges.3$x.coord.x+edges.3$x.coord.y)/2
# edges.3$midpoint.y <- (edges.3$y.coord.x+edges.3$y.coord.y)/2
# radios <- sqrt(
#   (edges.3$midpoint.x - edges.3$x.coord.x)^2 +
#     (edges.3$midpoint.y - edges.3$x.coord.y)^2 
# )
# 
# arc.start <- atan((edges.3$y.coord.x - edges.3$midpoint.y) /
#                     (edges.3$x.coord.x - edges.3$midpoint.x))
# arc.end <- atan((edges.3$y.coord.y - edges.3$midpoint.y) /
#                     (edges.3$x.coord.y - edges.3$midpoint.x))
# 
# z = seq(0, pi, l=100)