\name{level.plot}
\alias{level.plot}
\title{ Exploratory visualization of information spread of biological networks }
\description{
Generates a level plot by selecting a set of nodes at the initial level '0'. Outgoing nodes are plotted on higher levels than level '0', and the incoming nodes are plotted on lower levels. 
 This process is repeated for the nodes at each new level until all nodes are plotted. For this function, the graph can be plotted at each step, like a sequence, by using the 'plotstep' option. Edge colors show a connection between different levels. If the level difference is one, than the edgecolor is "grey" (default). For a level difference of zero, the default edge color is "brown". For level differences greater than one (upward edges), the edgecolor is "blue". If the level difference is less than zero (downward edges) the edgecolor is "red". The level plot function helps to see the information spread in a network, starting from a set of nodes (initial_nodes).

}
\usage{

level.plot(x, layout.function=NULL, type=1, initial_nodes=NULL,
init_nodes=0, order_degree = "in", plotsteps = FALSE, 
saveplots=FALSE, dirname=NULL, vertex.colors=NULL, 
edge.col=NULL, tkplot=FALSE, nodeset=NULL,path.col="green", 
col.s1="red", col.s2="yellow", nodes.on.path=TRUE,v.size=2, 
e.size=.5, v.lab=FALSE, bg="black", v.lab.cex=0.5, 
v.lab.col="skyblue",sf=4,e.path.width=1,e.curve=.5, 
level.spread=FALSE)  

}
\arguments{
  \item{x}{\code{x} is a graph object created from an adjacency matrix or from a tabular data of two columns using the \code{graph()} function available in \code{igraph}.  }
  \item{layout.function}{\code{layout.function} is a function class which genetates 2D coordinates for an input graph, this function is used to adjust x coordinates of the plot}
  \item{type}{\code{type} is a numerical value, it can either be 1 or 2, it is used to adjust coordinates generated from \code{layout.function} option. }
  \item{initial_nodes}{\code{initial_nodes} is a vector of integers of the vertex ids of the graph, from which the plot of a graph starts.}
  \item{init_nodes}{ \code{init_nodes} is an integer which provides the total numbers of vertices to initialize the plot of a graph randomly. If \code{initial_node} is NULL and \code{init_nodes} is \code{0} then \code{level.plot} selects 3 vertices randomly.}
  \item{order_degree}{\code{order_degree} is used to arrange nodes in a given order, based on their degrees from left to right in increasing order. Possible options for the ordering are 'in' and 'out'. }
  \item{plotsteps}{\code{plotsteps} is logical; if 'TRUE', shows a sequence of plots of the nodes found at each steps. Possible options are 'TRUE' and 'FALSE'.}
  \item{saveplots}{\code{saveplots} is logical; if 'TRUE' than saves plots of each steps in a pdf file inside a folder with the name \code{plot_steps_dir} in the current working directory.}
  \item{dirname}{\code{dirname} it is a directory path option if this option is not null then plots of each steps are saved in the given directory.}
  \item{vertex.colors}{ is a  vector of color names or hexadecimal codes of three colors. The first element is for vertex color at level 0, second element is for vertex color at level above level 0, third element is for vertex color lower than level 0.}
  \item{edge.col}{ is a 4 dimensional vector of colors. The first element is for edges that have a level difference between nodes greater than one. The second element is for edges that are connected to nodes with a level difference less than one. The third element is for edges connecting nodes on the same level. The fourth element is for edges with a level difference between connected nodes equal to one. }   
\item{nodeset}{is a list object containing two vector. The first vector contains a set of start nodes and the second vector contains the end nodes. Shortest paths are visualized between the start nodes and end nodes.}
\item{path.col}{is a character value for a color used to color the shortest paths between start nodes and end nodes, specified in\code{nodeset} option. }
\item{col.s1}{ color of start nodes specified in \code{nodeset} option, possible values can be a color name or a hexadecimal code of a color.}
\item{col.s2}{ color of end nodes specified in \code{nodeset} option, possible values can be a color name or a hexadecimal code of a color.}
\item{nodes.on.path}{is a logical value It shows the nodes which connect start nodes and end nodes specified in \code{nodeset} option.}
\item{v.size}{is a numeric value or a numeric vector which contains numeric values to assign the size of nodes.}
\item{e.size}{is a numeric value or a numeric vector which contains numeric values to assign the width of edges.}
\item{v.lab}{is a logical value to show vertex label.}
\item{v.lab.cex}{is a numerical value to set the font size of vertex labels.}
\item{v.lab.col}{is a color name or a hexadecimal color code to assign colors to vertex labels.}
\item{sf}{A numeric value, which is a scaling factor. This is used to adjust the vertex size when \code{v.size} input is a numeric vector. }  
\item{e.path.width}{is a numeric value. This is used to adjust the edge-size of the shortest paths between start nodes and end nodes. The start nodes and end nodes are given as an input as a list object for the "nodeset" option.} 
\item{e.curve}{is a positive numeric value that adjusts the curvature of the edges which connect nodes that are on same level.} 
\item{level.spread}{is a logical value, if it is 'TRUE' then the width of each level is the same.}
\item{bg}{name of a color or a hexadecimal code of a color. This option is used to color the background of the plot. The default option is "black".}
\item{tkplot}{is logical value, if TRUE use the tkplot function of igraph for plotting the network.}
\item{...}{\code{...} parameter for other inputs.}
}
\details{
  This function starts plotting nodes at \code{level 0}. These nodes are either user-defined or randomly selection. The outgoing nodes of the initial node set are plotted on the upper level and incoming nodes are plotted on the lower level. These steps are repeated until all nodes are plotted.     
}
\value{
  This function plots a graph using 'tkplot' or 'plot' function available in 'igraph'.
  This function returns a list. The first component of the list contains the x and y coordinates and  color ids of the nodes of the graph and the second component contains a vector of edge colors.
}
\references{ \url{http://bio-complexity.com/} }
\author{Shailesh Tripathi, Frank Emmert-Streib}

\examples{
data("PPI_Athalina")

### Example 1 #####
#####% 5 initial nodes are picked randomly #####
id <- level.plot(g1, init_nodes =5 ,tkplot=FALSE) 

### Example 2 #####
#####% initial nodes are given as an input #####
id <- level.plot(g1, initial_nodes =c(1,4,5,7,9,11,25,27,29,100,101),
tkplot=FALSE) 

### Example 3 #####
#####% width of each level is same #####
id <- level.plot(g1, initial_nodes 
=c(1,4,5,7,9,11,25,27,29,100,101), 
level.spread = TRUE,tkplot=FALSE) 

### Example 4 #####
#####% plot of graph when nodes are not arranged based on the degree #####
id <- level.plot(g1, initial_nodes 
=c(1,4,5,7,9,11,25,27,29,100,101),
order_degree= NULL, level.spread = TRUE,
tkplot=FALSE) 

### Example 5 #####
#####% plot of graph when nodes are not arranged based on the degree #####
id <- level.plot(g1, initial_nodes 
=c(1,4,5,7,9,11,25,27,29,100,101),
order_degree= NULL, level.spread = TRUE,
tkplot=FALSE) 

### Example 6 #####
###% plot of graph using a forcebased algorithm 
###in igraph package #####
xx <- level.plot(g1, layout.function=layout.reingold.tilford,
initial_nodes=c(1,4,5,7,9,11,25,27,29,100,101))

xx <- level.plot(g1, layout.function=layout.fruchterman.reingold,type=2, 
init_nodes=20)

xx <- level.plot(g1, layout.function=layout.kamada.kawai,
type=1, init_nodes=20)

### Example 7 #####
## Shortest path between initial set of nodes at level '0'
## to a set of nodes with vertex id 10, 1000, 1001, 1002 ##%
id <- level.plot(g1, initial_nodes=c(101,1,5,7),
vertex.colors=c("white", "white", "white"),
edge.col=c("grey", "grey", "grey", "grey"),
nodeset= list(c(101,1,5,7),c(10,1000,1001,1002)),
tkplot=FALSE, level.spread=TRUE,
order_degree=NULL)

### Example 8 #####
###  A directed network is plotted with level.plot function ##%
g <- barabasi.game(300, power=0)
id <-  level.plot(g, tkplot=FALSE,
level.spread=TRUE, order_degree=NULL)
 
}

