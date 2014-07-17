\name{plot.netbiov}
\alias{plot.netbiov}
\alias{tkplot.netbiov}
\title{
  Visualization of  large biological networks.
}
\description{
  Plots netbiov object
}
\usage{
\method{plot}{netbiov}(x, ...)
\method{tkplot}{netbiov}(x, ...) 

}
\arguments{
  \item{x}{\code{x} is a netbiov object contains different properties of graph}
\item{...}{\code{...} parameter for other inputs.}
}  

\value{
  returns invisible 'NULL' value.
}
\references{ \url{http://bio-complexity.com/} }
\author{Shailesh Tripathi, Frank Emmert-Streib}
\examples{
  data("PPI_Athalina")
  id <- plot.modules(g1)
  plot.netbiov(id)
}  
