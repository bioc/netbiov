\name{plot.spiral.graph}
\alias{plot.spiral.graph}
\title{
  Exploratory visualization of information spread of biological networks
}
\description{
  Plots a graph in a spirical fashion, highly connected  nodes are placed at center.
}
\usage{

\method{plot}{spiral.graph}(x, tp=61, vertex.color=NULL, 
color.random=FALSE, rank.function=NULL,tkplot=FALSE,
v.size=2, e.size=.5,e.curve=.5, v.lab=FALSE, bg="black",
e.col="grey", skip=0, ...)
}
\arguments{
  \item{x}{\code{x} is a graph object created from an adjacency matrix or from a tabular data of two columns using \code{graph()} function available in \code{igraph}  
}
  \item{tp}{
  is a numeric value, a tuning parameter to get different spirical shapes.
}
 \item{color.random}{is alogical value, if TRUE, picks colors randomly and assign to nodes depending on their degree}
  \item{rank.function}{
   is a 'function' class returns a two column matrix with number of rows equal to the total nodes in the graph. 
}
  \item{tkplot}{
  it is a boolean variable, if it is true, function will use 'tkplot' function to plot a graph, if it is false function will use plot function with the black background.
}
\item{v.size}{is a numeric value or a numeric vector which contains numeric values to assign the size of nodes.}
\item{e.size}{is a numeric value to assign the width to edges.}
\item{e.curve}{is a positive numeric value that adjusts the curvature of the edges.} 
\item{v.lab}{is a logical value to show vertex label.}
\item{bg}{name of a color or a hexadecimal code of a color. This option is used to color the background of the plot. The default option is "black".}
\item{e.col}{is a character or a hexadecimal color value to set the color of edges.}
 \item{vertex.color}{is a vector of colors assign colors to nodes depending on their degree, from high to low.}    
 \item{skip}{is an integer value to plot nodes away from the center.}    

\item{...}{\code{...} parameter for other inputs.}
}
\details{
  This layout function plots network in a spiral fashion.
}
\value{
  returns a list object of 'netbiov' class
}
\references{ \url{http://bio-complexity.com/} }
\author{Shailesh Tripathi, Frank Emmert-Streib}
\examples{
  g <- barabasi.game(500)
  x <- plot.spiral.graph(g, 121 )
  x <- plot.spiral.graph(g, 120,rank.function=layout.reingold.tilford,
vertex.color="red", e.col="green")
}
