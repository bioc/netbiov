\name{splitg.mst}
\alias{splitg.mst}
\alias{split.mst}
\title{ Network plot}
\description{
Modular layout style:

  A large complex network is plotted by splitting it into its modules. The positions of the vertices in each subnetwork are determined by using the
  fruchterman-reingold algorithm or the Kamada-kawai algorithm for the minimum spanning tree of each subnetwork. The edges of the minimum spanning tree are shown in black color. 
 
}
\usage{

splitg.mst(x,layout.function=NULL, mod.list=NULL, 
colors=NULL,mst.edge.col="white", 
vertex.color = c("red","green","blue","orange"),
random.v.color=FALSE,in.con.ed.col=NULL,tkplot=FALSE,
v.size=2, e.size=.5, mst.e.size=1, v.lab=FALSE, 
bg="black", v.lab.cex=0.5, e.lab.cex=0.5, 
v.lab.col="skyblue", lab.dist=0, v.sf=4, sf.modules = 5, ...)
}
\arguments{
  \item{x}{\code{x} is a graph object, created using igraph package. }
  \item{layout.function}{
   is a 'function' class or a vector of functions to plot the layout of each module by a function in 'layout.function'. 
}
  \item{mod.list}{
  \code{mod.list} is a list object, which provides a modular information about the graph, each components of mod.list contains a vector of nodes to be plotted.
}

\item{v.size}{is a numeric value or a numeric vector which contains values to assign the size of the nodes.}
\item{e.size}{is a numeric value to assign the width to edges.}
  \item{e.lab.cex}{
  is a numeric variable; this determines the size of the labels of the vertices  or the modules.
}
  \item{lab.dist}{
  is a numeric variable; this adjusts labels of vertices.
}
  \item{sf.modules}{
  is an integer variable is used to scale up or scale down the graph plot.
}
\item{v.sf}{is a numeric value. This is used to adjust vertex size when \code{v.size} input is a numeric vector. }  
\item{mst.edge.col}{This option assigns a color to the edges of the minimum spanning tree of each module of graph 'g'. The default color is black for 'tkplot=TRUE'. If 'tkplot=FALSE' it is white. }
  \item{random}{ \code{random} is a logical value, this option is used to choose nodes of split graphs randomly }
\item{colors}{\code{colors} is a vector of colors. This option is a vector of the edge colors to assign colors to the edges of the graph.}
\item{vertex.color}{\code{vertex.colors} is a vector of colors to assign colors to the vertices of the modules of the graph.}
\item{random.v.color}{is a logical value, this option is used to assign colors to the vertices colors of the modules, colors for modules are picked randomly.}
\item{in.con.ed.col}{is a scaler, assign colors to the edges which are showing connections between the modules.}
\item{tkplot}{it is a boolean variable, if it is true function will use tkplot function to plot a graph, if it is false function will use plot function with a black background.}
\item{mst.e.size}{is a numeric value which assigns the edge width to the edges of minimum spanning tree of the input graph.}
\item{v.lab}{is a logical value to show vertex label.}
\item{v.lab.cex}{is a numerical value to set the font size of vertex labels.}
\item{v.lab.col}{is a hexadecimal character value to assign colors to vertex labels.}
\item{sf}{is a numeric value. This is used to adjust vertex size when \code{v.size} input is a numeric vector.}  
\item{bg}{ is a color value for background.}

\item{...}{\code{...} parameter for other inputs.}
}
\value{
  This function plots a graph using 'tkplot' function available in the 'igraph'.
  This function returns a list,first component of list is a graph object, second component of the list contains x and y coordinates, third component of list contains color ids of edges of the graph etc.
}
\examples{
  data("PPI_Athalina")
  data("modules_PPI_Athalina")
        id <- splitg.mst(g1, mod.list=lm, random.v.color=TRUE, tkplot=FALSE )
}

