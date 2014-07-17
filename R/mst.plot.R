
 mst.plot <- function(x, layout.function=NULL,colors=NULL,mst.edge.col="white",
 vertex.color = "skyblue",tkplot=FALSE,expression=NULL, v.size=FALSE, 
e.size=FALSE, mst.e.size=1, edge.col.wt=NULL, v.lab=FALSE, e.lab=NULL, 
bg="black",v.lab.cex=0.5, e.lab.cex=0.5,v.lab.col="blue",lab.dist=0, 
e.lab.col="blue",v.sf=c(3,12),e.arrow=.2){
  ##print(degree(g))
  #print("h1")  
  if(is.null(x)){
    stop("please input a valid igraph object")
  }
  if(!is.igraph(x)){
    stop("please input a valid igraph object")
  }
  gtmp1 <- .set.mst.graph.attributes(x, v.lab)
  #print("h2")  
  rm(x)

  g <- gtmp1[[1]]
  #print(get.edgelist(g))
  v.label <- gtmp1[[2]] 
  temp1 <- .set.mst.size(g, e.size, v.size, v.sf)
  #print("h3")  

  e.s <- temp1[[1]]
  v.s <- temp1[[2]] 
  gtemp <- .get.mst.crd(g, layout.function)
  crd <- gtemp[[1]]
  mst <- gtemp[[3]]
  gt <- gtemp[[2]]
  gtemp <- g
  if(is.null(edge.col.wt) && (length(edge.col.wt)<ecount(g))){
    temp2 <- .set.edge.color_mst(crd, mst, gt, colors)  
    colid <- temp2[[1]]
    gt <- temp2[[2]]
    if(length(colors)==0){    
            cole <- colors()[colid]
    }
    if(length(colors)>0){cole <- colid}
    e.s[which(cole=="grey19")] <- mst.e.size
    if(!is.null(mst.edge.col)){
      cole[which(cole=="grey19")] <- mst.edge.col
    }
  }
  else{
   cole <- .set.edge.col.wt_mst(g,gt,mst,edge.col.wt,mst.e.size,e.size)
   gt <- cole[[3]]
   e.s <- cole[[2]]
   #print(e.s)
   cole <- cole[[1]]
  }
  v.colors <- .set.mst.node.col(vertex.color, gt, expression)
  if(is.null(e.lab)|| (e.lab==FALSE)){
    e.lab <- NA
  }
  else{
    if(e.lab==TRUE){
      e.lab <- NULL
    }
  }

  

  
  gparm <- list(g= gt, layout = crd[,1:2], vertex.color=v.colors,
        edge.color=cole,vertex.size=v.s,edge.arrow.size=0.3,
        vertex.label.cex= v.lab.cex, vertex.label=v.label,
        lab.color=v.lab.col,lab.dist=lab.dist, vertex.frame.color=v.colors,
        edge.width=e.s, bg=bg, edge.label=e.lab)
        class(gparm) <- "netbiov"
  if(tkplot){
         tkplot.netbiov(gparm)
       }
       else{
         plot.netbiov(gparm)
       }
       gparm
}
       
.dist.mst <- function(x1,y1,x2,y2){
        return(sqrt( (x1 - x2 )^2 + (y1 - y2)^2 ))
}

.rgbToHex.mod <- function(n){
        #R <-  G 
        #B  <- 255
        #G <- 255
  #R=(n-1)
  if(n<255){
    R = 255
    G = 0
    B = n
  }
  else{
    R=255-(n-255)
    G=0
    B=255
  }

  #G=(n-20)
  #R=255-(n-10); 
  #B=13


        k1 <- .toHex.mod(R)
        k2 <- .toHex.mod(G)
        k3 <- .toHex.mod(B)
        k <- paste("#",k1,k2,k3, sep="")
        return(k)
}

.toHex.mod <- function(n){
        hexstr <- "0123456789ABCDEF"
        if(is.na(n)) {return("00")}
        n = max(0,min(n,255));
        k1 <- strsplit(hexstr,"")[[1]][((n-n%%16)/16)+1]
        k2 <- strsplit(hexstr,"")[[1]][(n%%16)+1]
        return(paste(k1,k2,sep=""))
}
############################################################

.set.mst.graph.attributes <- function(g,v.lab ){
  if(is.directed(g)){
    mode = "directed"
  }
  else{
    mode="undirected"
  }
  adj <- get.adjacency(g,names=FALSE)
  gname <- V(g)$name
  if(!is.null(V(g)$label)){
    v.label <- V(g)$label
  }
  if(vcount(g)==length(v.lab)){
    v.label <- v.lab
  }
  if(class(v.lab)=="logical"){
    if(v.lab==TRUE){
      if(!is.null(V(g)$label)){
        v.label <- V(g)$label
      }
      else{
        v.label <- V(g)$name
        if(length(gname)<vcount(g) )
        {
          v.label <- c(1:(vcount(g)))
        }   
      }
      
    }
    else{
      v.label <- NA
    }
  }
  ##print(v.label)
  #g <- graph.adjacency(adj,mode=mode)
  #g <- simplify(g)
  if(!is.null(v.label)&&(length(v.label)==vcount(g))){
    V(g)$label <- v.label
  }
  E(g)$label <-E(g)
  #print(g)
  list(g, v.label)
}

.set.mst.size <- function(g, e.size, v.size, v.sf){
  v.s = 2.5
  if(class(v.size)=="logical"){
    v.s=2.5
  }
  if((class(v.size)=="numeric")||(class(v.size)=="integer")){
    if(length(v.size)< vcount(g)){
      v.s = v.size[1]
    }
    else{
    if(length(v.sf)==1){
      v.s = (v.size/max(v.size))*v.sf
      v.s[which(v.s<=.5)] <- .5
    }
    else{
    v.s <- v.size
    v.s = min(v.sf)+((v.s-min(v.s))/(max(v.s)-min(v.s)))*
    (max(v.sf)-min(v.sf))

      ##print(v.s)
    }
    }
  }
  if(class(e.size)=="logical"){
                e.s=rep(.5,ecount(g))
     }
        if(class(e.size)=="numeric"){
                if(length(e.size)< ecount(g)){
                        e.s = rep(e.size[1],ecount(g))
                }
                else{
                        e.s = (e.size[1:ecount(g)]/max(e.size[1:ecount(g)]))*12
                        e.s[which(e.s<=.5)] <- .5
                }
        }
          
  list(e.s, v.s)

}
.get.mst.crd <- function(g, layout.function){
  mst <- minimum.spanning.tree(g)
  ##print(ecount(mst))
  #print(E(mst)$label)

  gt <- graph.difference(g,mst)
  ##print(ecount(gt))
  lab <- sort(setdiff(E(g)$label,E(mst)$label))
  if(class(layout.function)=="function"){
    crd <- layout.function(mst) 
  }
  else{
    params <- list(niter=200)
    crd <-layout.fruchterman.reingold(mst, params)
  }
  crd <- cbind(crd, c(1:(length(crd[,1]))))
  ##print(ecount(gt))
  #print(E(gt)$label)
  #print(E(mst)$label)

  list(crd, gt, mst)
  
}

.set.edge.color_mst <- function(crd=NULL, mst=NULL, gt=NULL, colors=NULL){
  #print(E(gt)$label)
  if(length(colors)==0){
    colcode<- grep("orange",colors())
    #colcode <- (heat.colors(20))
    ##print(colcode)
    colid1 <-rep(280, ecount(mst))
  }
  if(length(colors)>0){
      colcode <- c(1:length(colors))
      names(colcode) <- colors
      colid1 <-rep("grey19", ecount(mst))
  }
  colid <- c()
  if(ecount(gt)==1){
    if(length(colors)==0){
      colid = colcode[1]
    }
    if(length(colors)>0){
                     colid <- append(colid, names(colcode)[1])
                }
  }
  if(ecount(gt)>1){
    edgelist <- get.edgelist(gt, names=FALSE)
    d <-c()
    colid<-c()
    for(i in 1:length(edgelist[,1])){
      e1 <- which(crd[,3]==edgelist[i,1])
      e2 <- which(crd[,3]==edgelist[i,2])
         d<- append(d,.dist.mst(crd[e1,1],crd[e1,2],crd[e2,1],crd[e2,2]))
    }
    ##print(d)
    col <-rep(round(((max(d))-(min(d)))/(length(colcode)),digits=5),
    (length(colcode)))
    ##print(col)
    col <- cumsum(col)
    col <- append(min(d),col+min(d))
    ##print(col)
    if(col[length(col)]< max(d))
    {
      #col[length(col)] <- col[length(col)]+2
      col[length(col)] <- max(d)+1
    }
    for(i in 1:length(d))
    {
      for(j in 1:(length(col)-1)){
        if((d[i]>=col[j])&&(d[i]<col[j+1])){
          if(length(colors)==0){
                 colid <- append(colid, colcode[j])
          }
          if(length(colors)>0){
           colid <- append(colid, names(colcode)[j])
          }
              }
       }
    }
  }
    tmpmst <- get.edgelist(mst, names=FALSE)
    rownames(tmpmst) <- (E(mst)$label)
    tmplab <- c(E(gt)$label)
    gt <- add.edges(gt, t(tmpmst))
    #E(gt)$label <- c(tmplab, E(mst)$label)
    #print(E(gt)$label)
    list(c(colid, colid1), gt)
}

.set.mst.node.col <- function(vertex.color, gt, expression){
  if(length(vertex.color)<vcount(gt)){
                        v.colors <-  rep(vertex.color, vcount(gt))[1:vcount(gt)]
              }
              else{V(gt)$color <- v.colors }
              if(!is.null(expression)&&(length(expression)==vcount(gt))){
    rank.wt <- expression
    rank.wt <- ((rank.wt-min(rank.wt))/
    (max(rank.wt)-min(rank.wt)))*509+1
    rank.wt <- round(rank.wt, digits=0)
      #print(as.vector(rank.wt))
      exp.col <- sapply(rank.wt,.rgbToHex.mod)
                     vertex.color <- exp.col
                     if(is.null(vertex.color)||(length(vertex.color)<2))
      {vertex.color <- heat.colors(max(rank.wt))}
                     v.colors <- vertex.color[rank(rank.wt)]
                 }

  v.colors
}
.set.edge.col.wt_mst <- function(g, gt, mst, edge.col.wt, mst.e.size, e.size){
  gt <- add.edges(gt, t(get.edgelist(mst, names=FALSE)))
  #print(get.edgelist(g, names=FALSE))
  l1 <-  apply(get.edgelist(g, names=FALSE), 1,
  function(x)paste(as.vector((x)), collapse="_"))
  l2 <-  apply(get.edgelist(gt, names=FALSE), 1,
  function(x)paste(as.vector((x)), collapse="_"))
  l3 <-  apply(get.edgelist(mst, names=FALSE), 1,
  function(x)paste(as.vector((x)), collapse="_"))
  names(edge.col.wt) <- l1
  #print(l1)
  #print(l2)
  #print(l3)
  edge.col.wt <- edge.col.wt[l2]
  rank.wt <- (edge.col.wt)
  rank.wt <- ((rank.wt-min(rank.wt))/(max(rank.wt) - min(rank.wt)))*509+1
  rank.wt <- round(rank.wt, digits=0)
  e.wt.col <- sapply(rank.wt,.rgbToHex.mod)  
  e.col <- e.wt.col
  if(class(e.size)=="logical"){
                e.s=rep(.5,ecount(g))
      }
      if(class(e.size)=="numeric"){
               if(length(e.size)< vcount(g)){
                       e.s = rep(e.size[1],ecount(g))
               }
               else{
                        e.s = (e.s/max(e.s))*12
                        e.s[which(e.s<=.5)] <- .5
                }
       }
   names(e.s) <- l1
   e.s <- e.s[l2]  
   e.s[l3] <- mst.e.size
       list(e.col, e.s, gt) 
} 
