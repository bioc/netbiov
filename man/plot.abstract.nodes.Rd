\name{plot.abstract.nodes}
\alias{plot.abstract.nodes}
\title{Exploratory visualization of information spread of biological networks}
\description{
Modular layout style:

  Visualization of modules in an abstract way. In the function \code{plot.abstract.nodes}, nodes in a module are replaced by a single node and the relative size of a node is proportional to the total number of nodes in a module. In the functions \code{plot.abstract.module}, \code{plot.abstract.nodes}, edges between two modules are replaced by a single edge and the total number of edges between two modules are reflected by the relative edge-width.
  
}
\usage{

\method{plot}{abstract.nodes}(x, mod.list=NULL,  rest.module=TRUE, 
color.random=FALSE, nodes.color = NULL, edge.colors=NULL,
layout.function=NULL,tkplot=FALSE,v.sf = 0, e.sf = 0,lab.color=NULL,
lab.cex=NULL, lab.dist=NULL, bg="black", ... )
}
\arguments{
  \item{x}{\code{x} is a graph object created from an adjacency matrix or from a tabular data of two columns using \code{graph()} function available in \code{igraph}  
}
  \item{mod.list}{
  \code{mod.list} is a list object each components of mod.list consists a vector of module to be plotted.
}
  \item{random}{
  is a logical value, if modules information is not provided then random nodes for the modules are selected.
}
  \item{rest.module}{
  is a logical value, rest of the nodes to be plotted as a one module (if TRUE) or independently (if FALSE)
}
 \item{color.random}{is a logical value, if this option is TRUE it will assign random color to each module or nodes.}
 \item{nodes.color}{assigns colors to nodes of recreated graph}  
 \item{edge.colors}{ is a vector of  colors, assigned to color edges depending on their width from small to large width. }  
 \item{layout.function}{is a function class, this corrosponds to a layout function given as an input to plot a graph. Input function should return a two column matrix which has same no of rows as the modules in the graph }
 \item{tkplot}{ is a boolean variable, if it is true function will use this option to plot a graph, if it is false function will use plot function with a black background.}
 \item{v.sf}{is a numeric value, this option to increase or decrease the  size of nodes}
 \item{e.sf}{is a numeric value, this option to increase or decrease width of edges}
 \item{lab.color}{colors the vertices and edge labels}
 \item{lab.cex}{is anumeric value, increase or decrease the font size of labels}
 \item{lab.dist}{is a numeric value, adjust vertex labels}  
\item{bg}{is a color value to adjust background color of the plot. }  
\item{...}{\code{...} parameter for other inputs.}
}
\references{ \url{http://bio-complexity.com/} }
\author{Shailesh Tripathi, Frank Emmert-Streib}
\examples{
  require(igraph)
  data("PPI_Athalina")
  data("modules_PPI_Athalina")
  plot.abstract.nodes(g1, mod.list = lm, 
edge.colors = c("red","green","blue","orange"))
}
