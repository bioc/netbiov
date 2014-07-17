\name{plot.NetworkSperical.startSet}
\alias{plot.NetworkSperical.startSet}
\title{
  Exploratory visualization of information spread of biological networks
}
\description{
Global layout style:

  A visualization of network in a 'star-like' form. An initial set of nodes are placed in the corner of polygons. The neighbors are then plotted above these nodes in an arc form. Whenever a node is encountered with multiple neighbors, neighbors are plotted in the direction of that node in an arc form. This process continues until all the nodes are placed. This gives a star-like view of the network.   
}
\usage{
\method{plot}{NetworkSperical.startSet}(x, mo="in", nc=5, tkplot=FALSE,
v.lab=FALSE, v.size=2, bg="black", ...)
}
\arguments{
  \item{x}{\code{x} is a graph object created from an adjacency matrix or from a tabular data of two columns using \code{graph()} function available in \code{igraph}.  }
  \item{mo}{
  \code{mo}  represents the mode of nodes. Options are \code{in} or \code{out}.
}
  \item{nc}{
  \code{nc}  is a numeric value representing the total number of corners to be plotted, initially at the center position of a polygon.
} 
  \item{tkplot}{
  it is a boolean variable, if it is true, function will use 'tkplot' function to plot a graph, if it is false function will use plot function with the black background.
}
\item{...}{\code{...} parameter for other inputs.}
\item{v.lab}{\code{v.lab} is a logical value to show vertex label.}
\item{v.size}{\code{v.size} is a numeric value to assign the size of nodes.}
\item{bg}{\code{bg}is a color value to adjust background color of the plot.}


}
\value{
  Plots the input graph object using \code{tkplot} function.
}


\references{ \url{http://bio-complexity.com/} }
\author{Shailesh Tripathi, Frank Emmert-Streib}
\examples{
   n <- 500
  g <- barabasi.game(n, directed = TRUE)
  plot.NetworkSperical.startSet(g, mo = "in", nc = 5, tkplot=FALSE)
}
