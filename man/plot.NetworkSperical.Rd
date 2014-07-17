\name{plot.NetworkSperical}
\alias{plot.NetworkSperical}
\title{
  Exploratory visualization of information spread of biological networks
}
\description{
Global layout style:

  A visualization of a network in a spherical form. The node with the highest degree is placed in the center, and its neighbors are plotted around this node in a circular manner. Whenever a node is encountered with multiple neighbors, the neighbors are plotted into the direction of that node. This process continues until all nodes are placed. This gives a compact spherical view of the network.   
}
\usage{
\method{plot}{NetworkSperical}(x, mo="in", tkplot = FALSE, 
v.lab=FALSE, v.size=1, bg="black", \dots)
}
\arguments{
  \item{x}{\code{x} is a graph object created from an adjacency matrix or from a tabular data of two columns using \code{graph()} function available in \code{igraph}.  }
  \item{mo}{
  \code{mo} represents the mode of nodes. Can be either \code{in} or \code{out}.
}
 \item{tkplot}{
  it is a boolean variable, if it is true, function will use 'tkplot' function to plot a graph, if it is false function will use plot function with the black background.
}
\item{v.lab}{\code{v.lab} is a logical value to show vertex label.}
\item{v.size}{\code{v.size} is a numeric value to assign the size of nodes.}
\item{bg}{\code{bg}is a color value to adjust background color of the plot.}
\item{\dots}{\code{\dots} parameter for other inputs.}
}
\value{
  Plots the input graph object using \code{tkplot} function.
}


\references{ \url{http://bio-complexity.com/} }
\author{Shailesh Tripathi, Frank Emmert-Streib}
\examples{
  #Example 1
   g <- barabasi.game(500, directed = TRUE)
   xx <- plot.NetworkSperical(g,  mo = "in", tkplot=FALSE)

  # Example 2
  g <- erdos.renyi.game(100, p=.1)
  xx <- plot.NetworkSperical(g)



}
