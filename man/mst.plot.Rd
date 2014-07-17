\name{mst.plot}
\alias{mst.plot}
\title{ Network plot}
\description{
  Large complex networks can be plotted using the fruchterman-reingold algorithm or the Kamada-kawai algorithm or any given input function on minimum spanning tree of each network. The edges of the minimum spanning tree (MST) are shown in black color, the rest of the edges are shown in a different colors which are a function of distances between the coordinates of the nodes.
 
}
\usage{
 mst.plot(x, layout.function=NULL,colors=NULL,mst.edge.col="white",
vertex.color = "skyblue",tkplot=FALSE,expression=NULL, v.size=FALSE,
e.size=FALSE, mst.e.size=1, edge.col.wt=NULL, v.lab=FALSE,
e.lab=NULL, bg="black",v.lab.cex=0.5, e.lab.cex=0.5,
v.lab.col="blue",lab.dist=0, e.lab.col="blue",
v.sf=c(3,12),e.arrow=.2)

}
\arguments{
\item{x}{\code{x} is a graph object, created using the igraph package. }
\item{layout.function}{\code{layout.function} is a function class. The user can pass his own function to plot the graph. The input function passed to \code{layout.function} should return a 2D matrix of coordinates with rows equal to the number of vertices of the input graph and two columns representing x and y coordinates. }
\item{mst.edge.col}{This option assigns a color to the edges of the minimum spanning tree of graph 'g'. The default color is black for 'tkplot=TRUE'. If 'tkplot=FALSE' it is white. }
\item{colors}{\code{colors} is a vector for the edge colors to color the remaining edges of a graph. The  remaining edges are colored based on the distance between the nodes connecting them.}
\item{vertex.color}{\code{vertex.colors}  is a vector of colors for the vertices of a graph.}
\item{expression}{\code{expression} is a numeric vector which represents properties of genes or proteins, e.g., their mean expression values or p-values. If this option is given, the nodes are colored with a range of colors from red to blue (red for low expression, blue for high expression) depending on the expression values.}  
\item{tkplot}{\code{tkplot} is a boolean variable. If it is FALSE, the graph is plotted with the plot function with a black background. }
\item{v.size}{is a numeric value or a numeric vector which contains values to assign the size of the nodes.}
\item{e.size}{is a numeric value or a numeric vector which contains values to assign the width of edgesother than the minimum spanning tree edges of the graph.}
\item{mst.e.size}{is a numeric value which assigns the edge width to the edges of minimum spanning tree of the input graph.}
\item{v.lab}{is logical value to show vertex labels.}
\item{e.lab}{is logical value to show edge labels.}
\item{bg}{is a character value to color the background.}
\item{v.lab.cex}{is a numerical value to set the font size of the vertex labels.}
\item{e.lab.cex}{is a numerical value to set the font size of the edge labels.}
\item{lab.dist}{is a numerical value to adjust the distance of labels from the nodes.}
\item{v.lab.col}{is a hexadecimal character value to assign colors to the vertex labels.}
\item{e.lab.col}{is a hexadecimal character value to assign colors to the edge labels.}
\item{e.arrow}{is a inumerical value to set the arrow width in a directed network.}
\item{v.sf}{is a 2 dimensional numeric vector, which represents the minimum and maximum limits of the size of vertices. This is used to adjust the vertex size when the \code{v.size} input is a numeric vector. }  
\item{edge.col.wt}{is a vector of numeric values provided for each edge. This is used to color the edges from blue to red from high to low values. }  
\item{...}{\code{...} parameter for other inputs.}
}
\value{
  This function plots the graph object given as an input using 'tkplot' or 'plot' function available in 'igraph' package.
  This function returns a \code{netbiov} class  object.
}
\examples{
  data("PPI_Athalina")

  ## Example 1 ####
  kk <- mst.plot(g1) %or

  ## Example 2 ####
  ## When expression values of genes or nodes are given and 
  ## to be plotted as ai color of vertices ###
  id <- mst.plot(g1, expression=rnorm(vcount(g1)), v.size=1)

  ## Example 3 ####
  ## When expression values of genes or nodes are given and to be
  ## plotted as a 
  ##color of vertices, also the degree of nodes to be shown
  ## as their vertex-size ###
  id <- mst.plot(g1, expression=rnorm(vcount(g1)),
 v.size=degree(g1), v.sf=c(1,5))

  ## Example 4 ####
  ## When MST edges are highlighted in purple color
## and rest of the edges are plotted with a range of 
## heat colors depending on the distance between nodes ###
  id <- mst.plot(g1, mst.edge.col="purple",
 colors=heat.colors(20), vertex.color="yellow",
 v.size=1)
  
  
  ## Example 5 ####
  ## Plotting a graph with kamada-kawai layout algorithm ###
  id <- mst.plot(g1, mst.edge.col="purple", 
colors=heat.colors(20), vertex.color="white", v.size=1, 
layout.function=layout.kamada.kawai)


  ## Example 6 ####
  ## Plotting a graph with when weights of edges are given ###
  id <- mst.plot(g1, mst.edge.col="purple", edge.col.wt = 
runif(ecount(g1), min=1, max=10), vertex.color="yellow",
 v.size=1, layout.function=layout.kamada.kawai)


}
