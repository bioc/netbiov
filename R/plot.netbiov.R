#################################
plot.netbiov <- function(x,...){
  if(class(x)=="netbiov"){
    gparm <- x
    rm(x)
    #print(gparm$edge.label)
    par(bg=gparm$bg,mar=c(0,0,0,0),oma=c(0,0,0,0))
    plot(gparm$g, layout = gparm$layout, 
vertex.color=gparm$vertex.color, edge.color=gparm$edge.color,
vertex.label.color=gparm$lab.color, vertex.size=gparm$vertex.size,
edge.arrow.size=gparm$edge.arrow.size, vertex.label.cex=gparm$vertex.label.cex,
vertex.label=gparm$vertex.label,vertex.frame.color=gparm$vertex.frame.color,
edge.width=gparm$edge.width, edge.label.color=gparm$edge.label.color,
edge.label.cex=gparm$edge.label.cex, edge.curved=gparm$edge.curved,
vertex.label.family=gparm$vertex.label.family, edge.label=gparm$edge.label)
  }

}
tkplot.netbiov <- function(x, ...){
  if(class(x)=="netbiov"){
    gparm <- x
    rm(x)
    par(bg=gparm$bg,mar=c(0,0,0,0),oma=c(0,0,0,0))
    tkplot(gparm$g, layout = gparm$layout, 
vertex.color=gparm$vertex.color, edge.label=gparm$edge.label,
edge.color=gparm$edge.color,vertex.size=gparm$vertex.size, 
edge.arrow.size=gparm$edge.arrow.size, 
vertex.label.cex=gparm$vertex.label.cex, vertex.label=gparm$vertex.label,
vertex.frame.color=gparm$vertex.frame.color, edge.width=gparm$edge.width)
}

}

