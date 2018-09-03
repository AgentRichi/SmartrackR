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
  edgelist, edgeweight=5, edgecol=5, edgeseq=1, cols=c("#c9cba3","#ffe1a8","#e26d5c"), sorted=FALSE, decreasing=FALSE, lwd=NULL,
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
  
  edges <- edgelist
  print(edges)
  #SORT EDGES
  
  edgecol <- as.data.frame(edgecol) %>% sapply(as.numeric)
  edgeweight <- edgeweight %>% as.data.frame() %>% sapply(as.numeric)
  if (length(edgeweight)==1) edgeweight = rep(edgeweight,nrow(edges))
  if (length(edgecol)==1) edgecol = rep(edgecol,nrow(edges))
  if (length(edgeseq)==1 & is.null(nrow(edgeseq))) edgeseq = 1:nrow(edges)
  #colnames(edgeseq) <- 'edgeseq'
  # edgesort <- cbind(edges,edgeseq,edgeweight,edgecol) %>% as.data.frame()
  # edgesort <- edgesort %>%
  #   left_join(edgesort[c('origin','edgeseq')], by = c("destination" = "origin")) %>%
  #   rename(from = edgeseq.x, to = edgeseq.y) %>% unique()
  # #sort
  # edgesort <- edgesort[with(edgesort, order(from,to)),]
  # edges <- as.matrix(edgesort[1:2])
  # 
  # edgeweight <- edgesort[4] %>% as.data.frame() %>% sapply(as.numeric)
  # edgecol <- edgesort[5] %>% as.data.frame() %>% sapply(as.numeric)
  # 
  # how many edges
  ne = nrow(edges)
  # get nodes
  nodes = unique(as.vector(edges))
  nums = unique(as.vector(edgeseq))
  # how many nodes
  nn = length(nodes) 
  # ennumerate
  if (sorted) {
    nodes = nodes[order(nums)]
    # nums = order(nodes, decreasing=decreasing)
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
  wd <- edgeweight
  # scale the weight
  wd <- (wd-min(wd))/(max(wd)-min(wd))*10 +1
  print("wd")
  print(str(wd))
  wd.col <- color.gradient(edgecol,colors = cols)
  # node labels coordinates
  nf = rep(1 / nn, nn)
  # node labels center coordinates
  fin = cumsum(nf)
  ini = c(0, cumsum(nf)[-nn])
  centers = (ini + fin) / 2
  names(centers) = nodes
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
  # plot
  par(mar = mar, bg = bg)
  # plot.new()
  # plot.window(xlim=c(-0.025, 1.025), ylim=c(1*min_rad*2, 1*max_rad*2))
  p <- plot_ly(x=locs,
               y=0,
               type='scatter',
               mode = 'markers',
               marker=list(size=1, opacity=0),
               color=edgecol, 
               colors=color.gradient(1:length(cols),colors = cols),
               hoverinfo = "none")
  print("cols")
  print(cols)
  print(color.gradient(1:length(cols),colors = cols))
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
    y = y + ifelse(y[[2]]>0,0.05,-0.01) #move y up/down to show label
    width <- wd[i]
    print(width)
    color <- wd.col[i]
    txt <- paste0(edges[i,1]," to ",edges[i,2],"\n",
                  colnames(edgecol),": ",format(edgecol[i],digits = 2),"\n",
                  colnames(edgeweight),": ",edgeweight[i])
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
  m <- list(l = 0, r = 0, b = 0, t = 0, pad = 0)
  p <- p %>%  add_text(x=centers,
                       y=0.03,
                       text = paste0(substr(names(centers),1,4),"."),
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
           showlegend = F,
           margin = m
           )
  p
  # add node names
  # mtext(nodes, side=1, line=0, at=centers, cex=cex,
  #       col=col.nodes, las=las)
}




################################################################
#   Charting
################################################################
# RUN Smartrack.R FIRST!!

# sources <- leg_times[4] %>% distinct(origin) %>% rename(label=origin)
# sources
# destinations <- leg_times[5] %>% distinct(destination) %>% rename(label=destination)

stations <- read.csv("C:/Users/vicxjfn/OneDrive - VicGov/NIMP/Smartrack/Input/CRANPAK.csv",stringsAsFactors = F)

nodes <- cbind(id=1:nrow(stations),stations)[,1:2]

edges <- railRep %>% group_by(origin,destination,Seq.Org,Seq.Des) %>% 
  filter(project=="CGB") %>%
  summarise("Number of Trips (sample)"=n(),
            "Average Travel Time"=mean(legTime))

edges <- edges %>%
  inner_join(nodes, by = c("origin" = "label")) %>%
  rename(Sequence = id)

# edges <- edges %>%
#   inner_join(edges[c('origin','Sequence')], by = c("destination" = "origin")) %>%
#   rename(from = Sequence.x, to = Sequence.y)
# 
# #sort
# edges <- edges[with(edges, order(from,to)),]

arcDiagram(as.matrix(edges[c('origin','destination')]), 
           edgeweight = edges[c('Number of Trips (sample)')],
           edgecol = edges[c('Average Travel Time')],
           edgeseq = as.matrix(edges[c('Seq.Org','Seq.Des')]),
           cols=c("#91A4A9","#C5001C"),
           sorted = T)

