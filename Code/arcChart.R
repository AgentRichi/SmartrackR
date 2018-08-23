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
  edgelist, group=1, edgeweight=5, sorted=FALSE, decreasing=FALSE, lwd=NULL,
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
  categ <- unique(cbind(edges[,1],group))
  names(categ) <- c("origin","line")
  print(categ)
  categ <- aggregate(line ~ origin,categ, paste, collapse = "/")
  print("categ")
  print(categ)
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
  
  edgeweight <- as.data.frame(edgeweight) %>% sapply(as.numeric)
  wd <- edgeweight
  if (length(wd)==1) wd = rep(wd,nrow(edges))
  # scale the weight
  wd <- (wd-min(wd))/(max(wd)-min(wd))*10 +1
  wd.col <- color.gradient(wd)
  # node labels coordinates
  nf = rep(1 / nn, nn)
  # node labels center coordinates
  fin = cumsum(nf)
  ini = c(0, cumsum(nf)[-nn])
  centers = (ini + fin) / 2
  names(centers) = nodes
  print(centers)
  # arcs coordinates
  # matrix with numeric indices
  e_num = matrix(0, nrow(edges), ncol(edges))
  for (i in 1:nrow(edges))
  {
    e_num[i,1] = centers[which(nodes == edges[i,1])]
    e_num[i,2] = centers[which(nodes == edges[i,2])]
  }
  # max arc radius
  # multiply by -1 to flip arcs
  radios = ((e_num[,1] - e_num[,2]) / 2) * -1
  max_radios = which(radios == max(radios))
  max_rad = unique(radios[max_radios] / 2)
  min_radios = which(radios == min(radios))
  min_rad = unique(radios[min_radios] / 2)
  # arc locations
  locs = rowSums(e_num) / 2
  #node colors
  cols <- 
  # plot
  par(mar = mar, bg = bg)
  # plot.new()
  # plot.window(xlim=c(-0.025, 1.025), ylim=c(1*min_rad*2, 1*max_rad*2))
  p <- plot_ly(x=locs,
               y=0,
               type='scatter',
               mode = 'markers',
               marker=list(size=1, opacity=0),
               color=edgeweight, 
               colors=color.gradient(c(1,2,3)),
               hoverinfo = "none")
  # plot connecting arcs
  z = seq(0, pi, l=100)
  for (i in 1:nrow(edges))
  {
    #   radio = radios[i]
    #   x = locs[i] + radio * cos(z)
    #   y = radio * sin(z)
    #   lines(x, y, col=col[i], lwd=lwd[i], 
    #         lend=lend, ljoin=ljoin, lmitre=lmitre)
    radio = radios[i]
    x = locs[i] + radio * cos(z)
    y = radio * sin(z)
    y = y + ifelse(y[[2]]>0,0.03,-0.01) #move y up/down to show label
    width <- wd[i]
    color <- wd.col[i]
    txt <- paste0(edges[i,1]," to ",edges[i,2],"\n",colnames(edgeweight),": ",format(edgeweight[i],digits = 2))
    p <- add_trace(p,
                   x = x,
                   y = y, 
                   hoverinfo = "text",
                   text = txt,
                   line = list(color = color, shape = "spline", width = width),
                   mode = "lines",
                   name = txt, 
                   type = "scatter")
  }
  
  axis_template <- list(showgrid = F , zeroline = F, showline = F, showticklabels = F)
  
  p <- p %>%  add_text(x=centers,
                       y=0.02,
                       text = names(centers),
                       textfont = list(color = '#000000', size = 12, weight="bold")) %>% 
    add_trace(
      x = centers,
      y = 0,
      marker = list(
        color = 'rgb(255, 255, 255)',
        size = 15,
        opacity=1,
        line = list(
          color = 'rgb(0, 0, 0)',
          width = 2
        )
      ),
      showlegend = F
    ) %>%
    # add_trace(x=centers,
    #           y=0,
    #           marker = list(
    #             color = 'rgb(17, 157, 255)',
    #             size = 20,
    #             line = list(
    #               color = 'rgb(231, 99, 250)',
    #               width = 2
    #             ))) %>%
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

sources <- leg_times[4] %>% distinct(origin) %>% rename(label=origin)
sources
destinations <- leg_times[5] %>% distinct(destination) %>% rename(label=destination)

nodes <- stations

edges <- leg_times %>% group_by(line,origin,destination) %>% 
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
arcDiagram(as.matrix(edges[2:3]), edgeweight = edges[4], group=edges[1], sorted = F, lwd = 3,cex = 0.5)

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

#Colorbrewer
# wd <- as.data.frame(wed) %>% sapply(as.numeric)
# wd <- (wd-min(wd))/(max(wd)-min(wd))*10 +1
# rand.data <- wd
# br.range <- seq(min(rand.data),max(rand.data),length.out=10)
# results <- sapply(1:ncol(rand.data),function(x) hist(rand.data[,x],plot=F,br=br.range)$counts)
# cols <- brewer.pal(8,"Set3")
# lapply(1:ncol(results),function(x) print(cols[x]))
