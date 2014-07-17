\name{plot.abstract.module}
\alias{plot.abstract.module}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Visualization of  large biological networks.
}
\description{
  Visualization of graphs in a modular form. In this plot, connecting edges between modules are replaced by a single edge and its width is propotional to the number of connections between modules.  
}
\usage{

\method{plot}{abstract.module}(x, layout.function=NULL,mod.list=NULL, 
module.function=FALSE, split.graph=7, color.random=FALSE, 
modules.color = NULL, col.grad=NULL, mod.edge.col=NULL,
ed.color=NULL,edge.col.random=FALSE, expression = NULL,
exp.by.module=FALSE, tkplot=FALSE, layout.overall = NULL,
sf=0,arrange.bydegree=FALSE,mod.lab=FALSE,node.lab=FALSE, 
lab.cex = NULL,lab.color=NULL, lab.dist=NULL, v.size=FALSE,
nodeset=NULL,path.col="green", col.s1="red", col.s2="yellow",
nodes.on.path=TRUE,e.path.width=1, scale.module=NULL,v.sf=5,
e.width=.5,bg="black", e.sf=15, abstract.graph=TRUE,
modules.name.num=TRUE,  v.size.path=TRUE, ...)
}
\arguments{
  \item{x}{\code{x} is a graph object created from an adjacency matrix or from a tabular data of two columns using \code{graph()} function available in \code{igraph}
}  
  \item{layout.function}{
  It is a 'function' class or a vector of functions to plot the layout of each module by a function in 'layout.function'. 
}
  \item{mod.list}{
  \code{mod.list} is a list object, which provides a modular information about the graph, each components of mod.list contains a vector of nodes to be plotted.
}
  \item{module.function}{
   is a logical value for obtaining modules in the network. 
}
  \item{random}{
  It is a boolean variable, if 'mod.list' is null, it picks the nodes for modules randomly.
}
  \item{split.graph}{
  \code{split.graph} if random is TRUE, it provides no. of modules that a graph to be split.
}
  \item{color.random}{If this option is TRUE it will assign random colors to modules.}
  \item{modules.color}{ Is a vector of colors to assign a color to each module by the the user.}  

  \item{col.grad}{
     is a vector of colors or can be a list of vectors of colors to assign the colors to the nodes for each modules based on their degree from low to high.
}
  \item{mod.edge.col}{
  is a  vector of color to assign the edge color to  the edges of a modules.  
}
  \item{ed.color}{
    is a scaler of color and assign colors to the edges between modules.
  
}
  \item{edge.col.random}{
  is a boolean variable and assign colors to the edges of each module randomly.
}
 \item{expression}{\code{expression} is a numeric vector which represents properties of genes or proteins like mean expression values or p-values, If this option is given, the nodes are colored with a range of colors from red to blue (red for low expression, blue for high expression) depending on the expression values.}  
\item{exp.by.module}{
  this option is a boolean or a numeric vector which represents the order of modules given as an input in the mod.list option. This option is used to see the variation in expression values of nodes in a particular module by using a range of color from red to blue, red indicate low expression value and blue indicates high expression values.    
}
  \item{tkplot}{
  it is a boolean variable, if it is true, function will use 'tkplot' function to plot a graph, if it is false function will use plot function with the black background.
}
  \item{layout.overall}{this option belongs to the class 'function', for this option any function which returns a two column matrix which should have rows equal to the no of modules for the placement of the modules.}

  \item{sf}{
  is an integer variable is used to scale up or scale down the graph plot.
}
  \item{arrange.bydegree}{
  is a boolean variable; if true the coordinates of nodes are assigned by their degree, higher degree nodes are plotted towards center and lower degree nodes are plotted outside.
}
  \item{mod.lab}{
  is a boolean variable; prints module labels at center of each module if the module's names are available in 'mod.list' as list names.
}
  \item{node.lab}{
  is a boolean variable; this option plots  the vertex label, if option is TRUE.
}
  \item{lab.cex}{
  is a numeric variable; this determines the size of the label of the vertices  or the modules.
}
  \item{lab.color}{
    is a string variable; this assigns colors to the label of vertices or the modules.
}
  \item{lab.dist}{
  is a numeric variable; this adjusts vertices label.
}
\item{nodeset}{is a list object contains two vector, it can be a numeric vector also.  First vector contains set of start nodes and second vector contains end nodes. In the case of numeric vector which  indicates the module id, it is used to show shortest path between two modules.  Shortest paths are visualized between start nodes and end nodes.}
\item{path.col}{is a color vector for coloring the shortest path between start nodes and end nodes.}
\item{col.s1}{ is a color vector to color start nodes or modules.}
\item{col.s2}{ is a color vector to color end nodes or modules.}
\item{nodes.on.path}{is a logical value which shows nodes which connect start nodes and end nodes.}
\item{e.path.width}{is a vector containing of size 2. This option sets the edge width of the shortest paths between two modules or two set of nodes.}
\item{scale.module}{is a numeric vector of the size of total number of modules. This option scales the size of each module independently.}
\item{v.size}{is a numeric value or a numeric vector which contains numeric values to assign the size of nodes.}
\item{e.width}{is a numeric value which contains numeric values to assign the width to edges.}
%\item{v.lab}{is a logical value to show vertex label.}
%\item{v.lab.cex}{is a numerical value to set the font size of vertex labels.}
%\item{v.lab.col}{is a hexadecimal character value to assign colors to vertex labels.}
\item{v.sf}{is a numeric value. This is used to adjust vertex size when \code{v.size} input is a numeric vector. }  
\item{bg}{is a color value to adjust background color of the plot. }  
\item{e.sf}{is a numeric vector of size 2 to adjust the edge width between a range of minimum and maximum. }  
\item{abstract.graph}{is a logical value which adjusts the abstract view of modular plot using force-based algorithm or any input function given in \code{layout.overall} option. }  
\item{modules.name.num}{is a logical value for displaying module name or its number}
\item{v.size.path}{is a numeric or logical value which adjusts the size of nodes between which the shortest path to be shown. }
\item{...}{\code{...} parameter for other inputs.}
}

\value{
  returns a list object of 'netbiov' class
}
\references{ \url{http://bio-complexity.com/} }
\author{Shailesh Tripathi, Frank Emmert-Streib}
\seealso{
  plot.abstract.nodes, plot.abstract.module
}
\examples{
  data("PPI_Athalina")
  data("modules_PPI_Athalina")

  ## Example 1 #######%
  ##% Abstract modular layout plot of A. Thalina PPI network, 
##modules are colored randomly, module information is given as a 
##list object ###%
  id <- plot.abstract.module(g1,mod.list = lm, 
layout.function=layout.graphopt, color.random = TRUE , 
tkplot=FALSE,node.lab=FALSE,v.size=1)


  ## Example 2 #######%
  ##% Abstract modular layout plot of A. Thalina PPI network,
## modules are colored randomly, module information is  predicted 
##using 'fastgreedy' algorithm ###%
  id <- plot.abstract.module(g1, 
layout.function=layout.graphopt, color.random = TRUE , 
tkplot=FALSE,node.lab=FALSE,v.size=1)


  ## Example 3 #######%
  ##% Abstract modular layout plot of A. Thalina PPI network
## when expression value of genes are given ###%
  id <- plot.abstract.module(g1, layout.function=layout.graphopt,
 color.random = TRUE,cexpression=rnorm(vcount(g1)), tkplot=FALSE,
node.lab=FALSE,v.size=1)
  
  ## Example 4 #######%
  ##% Abstract modular layout plot of A. Thalina PPI network 
##when expression value of modules 1, 2 and 3 are shown by colors 
##by ranking independently from each other ###%
  id <- plot.abstract.module(g1, 
layout.function=layout.graphopt, modules.color="grey", 
expression=rnorm(vcount(g1)), tkplot=FALSE,node.lab=FALSE,v.size=
1, exp.by.module=c(1,2,3))

  ## Example 5 #######%
  ##% Abstract modular layout plot of A. Thalina PPI network 
##by emphasizing module labels ###%
  id <- plot.abstract.module(g1, mod.list=lm, 
layout.function=layout.graphopt, modules.color="grey", 
tkplot=FALSE, mod.lab=TRUE  ,v.size=1, lab.color="green" )

  ## Example 6 #######%
  ##% Abstract modular layout plot of A. Thalina PPI network 
##highlighting shortest paths between modules 1, 5 and 7, 18 ###%
  id <- plot.abstract.module(g1, mod.list=lm, 
layout.function=layout.graphopt, modules.color="grey", 
tkplot=FALSE, nodeset=c(1,5,7,18), sf=-10, v.size=1)  

  ## Example 7 #######%
  ##% Abstract modular layout plot of A. Thalina PPI network 
##combining two layouts ###%
  fn <- function(g)layout.star(g, 
center=which.max(degree(g))-1)
  id <- plot.abstract.module(g1,mod.list = lm, 
layout.function=layout.graphopt, layout.overall=fn,
color.random = TRUE , tkplot=FALSE,node.lab=FALSE,v.size=1)

}  
