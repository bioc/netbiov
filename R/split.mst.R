splitg.mst <- function(x,layout.function=NULL, mod.list=NULL,
 colors=NULL,mst.edge.col="white",
vertex.color = c("red","green","blue","orange"),random.v.color=FALSE,
in.con.ed.col=NULL,tkplot=FALSE, v.size=2, e.size=.5, mst.e.size=1,
v.lab=FALSE, bg="black", v.lab.cex=0.5, e.lab.cex=0.5, v.lab.col="skyblue",
lab.dist=0, v.sf=4, sf.modules = 5, ...){
    
    if(is.null(x)){
      stop("please input a valid igraph object")
    }
    if(!is.igraph(x)){
      stop("please input a valid igraph object")
    }
    g <- x
    rm(x)
    dgg1 <- degree(g)
    v.s <- .set.split.mst.size(v.size, g, v.sf)

    tmp1 <- .set.split.graph.attributes(g, v.lab)

    g <- tmp1[[1]]
    ##print(ecount(g))
    glabel <- tmp1[[2]]
    mod.list <- .set.split.mst.modules(g, mod.list)

    colcode <- .set.split.ecolor(colors)

    tmp2 <- .set.split.vector(g, mod.list)

    split.vector <- tmp2[[1]]
    v.id <- tmp2[[2]]
    #print(v.id)
    lgsplit <- .set.split.subgraph(g, split.vector, v.id)
    colm <- .set.split.vertex.color(g, lgsplit, v.id, vertex.color,
 split.vector, random.v.color)
    vertex.color <- colm[[2]]
    colm <- colm[[1]]
    crd <- .set.split.mst.crd(lgsplit, layout.function, sf.modules)
    clabt <- crd[[2]]
    lab <- crd[[3]]
    crd <- crd[[1]]
    cole <- .set.split.edge.col(g, in.con.ed.col, lab, clabt,
 crd, colcode)
    #print(length(cole))
    tmp3 <- .reset.split.attributes(g, dgg1, colm, glabel, cole,
vertex.color,random.v.color=random.v.color, lgsplit, mst.edge.col, e.size,
mst.e.size)
    g1 <- tmp3[[1]]
    cole1 <- tmp3[[2]]
    e.s <- tmp3[[3]]
    vlab <- tmp3[[4]]
    gparm <- list(g= g1, layout = crd, vertex.color=V(g1)$color,
edge.color=cole1,vertex.size=v.s, edge.arrow.size=0.3, 
vertex.label.cex= v.lab.cex, vertex.label=vlab, lab.color=v.lab.col,
lab.dist=lab.dist, vertex.frame.color=V(g1)$color, edge.width=e.s, bg=bg)
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
  x1 <- as.numeric(x1)    
  x2 <- as.numeric(x2)    
  y1 <- as.numeric(y1)    
  y2 <- as.numeric(y2)    
        return(sqrt( (x1 - x2 )^2 + (y1 - y2)^2 ))
}



##################################################################
.set.split.mst.size <- function(v.size, g, v.sf){
  if(class(v.size)=="logical"){
    v.s=2
  }
   if(class(v.size)=="numeric" && (length(v.size)<vcount(g))){
    v.s=v.size[1]
   }
   else{
  v.s= ((v.size- min(v.size))/(max(v.size)-min(v.size)))*(v.sf-.5)+.5
  v.s[which(v.s<.5)] <- .5
   }  
   v.s
}
.set.split.graph.attributes <- function(g, v.lab){
  adj <- get.adjacency(g,names=FALSE)
  gname <- V(g)$name
  if(is.null(gname)){
    gname <- c(1:(vcount(g)))
  }
  if(!is.null(V(g)$label)){
    glabel <- V(g)$label
  }
  else{
    glabel <- V(g)$name
  }
  if(length(v.lab)==vcount(g)){
    glabel <- v.lab
  }
  if(is.directed(g)){
    g <- graph.adjacency(adj)
  }
  else{
    g <- graph.adjacency(adj, mode="undirected")
  }
     E(g)$label <- c(1:(ecount(g)))
  if(is.null(V(g)$label)){
         V(g)$label <- (seq(vcount(g)))

  }
  if(length(v.lab)!=vcount(g)){
      glabel <- NA
  }
  else{
      glabel <- v.lab
  }
  list(g, glabel)


}


.set.split.mst.modules <- function(g, mod.list){
  if(!is.null(V(g)$name)){
    if((class(mod.list[[1]])=="character")){
      mln <- list()
      for(i in 1:length(mod.list)){
        km <- match(mod.list[[i]],V(g)$name)
        km <- km[!is.na(km)]  
        mln[[i]] <- km
      }
      names(mln) <- mod.list
      mod.list = mln
    }
  }
  if(is.null(mod.list) || (length(unlist(mod.list))!=vcount(g))){
    fc <- multilevel.community(g)
           memb <- fc$membership
           un <- unique(memb)
           mod.list <- sapply(sort(un), function(x)which(x==memb))
         
  }
     mod.list   
}

.set.split.ecolor <- function(colors){
  colcode<- grep("green",colors())
  if(length(colors)==0){
    cl<- grep("green",colors())[16:40]
    colors <- colors()[cl]
    
  }
  colcode <- c(1:length(colors))
  names(colcode) <- colors
  colcode
}

.set.split.vector <- function(g, mod.list){
   if(length(mod.list)>0)
  {
    grpnodes <- c()
    nodeids <- c()
    for(i in 1:length(mod.list))
    {
      grpnodes <- append(grpnodes, length(mod.list[[i]]))
      nodeids <- append(nodeids, mod.list[[i]])
    }
    leftnodes <- setdiff(V(g), unique(nodeids))
    if(length(leftnodes)> 0 )
    {
      grpnodes <- append(grpnodes, length(leftnodes))
      nodeids <- append(nodeids, leftnodes)
            
    } 
    split.vector <- grpnodes
    split.vector <- c(0,cumsum((split.vector)))
    v.id <- nodeids
  }
  list(split.vector, v.id)
}

.set.split.subgraph <- function(g, split.vector, v.id){
  lgsplit <- list()
  for(i in 1:(length(split.vector)-1))
   {
    ##print(v.id [ ( split.vector[i]+1 ) : ( split.vector[i+1] ) ])
    lgsplit[[i]] <- induced.subgraph( g, 
v.id [ ( split.vector[i]+1 ) : ( split.vector[i+1])])
    ##print(E(lgsplit[[i]])$label)
   }

  lgsplit
}


.set.split.vertex.color <- function(g, lgsplit, v.id, vertex.color,
split.vector, random.v.color){
if(length(lgsplit)==length(vertex.color)){
      colm <- rep("skyblue",vcount(g))
      vid <- v.id 
      for(i in 1:(length(split.vector)-1)){
      colm[vid [ ( split.vector[i]+1):(split.vector[i+1] )]]<- vertex.color[i]
      }
 }
else{
vertex.color <- rep(vertex.color, length(lgsplit))[1:length(lgsplit)]
colm <- rep("skyblue",vcount(g))
  vid <- v.id
  for(i in 1:(length(split.vector)-1)){
  colm[vid [ ( split.vector[i]+1 ):(split.vector[i+1])]]<-vertex.color[i]
  }
}      
 if(random.v.color || (is.null(vertex.color))){
  grtemp <- colors()[grep("gray",colors())]
  grtemp <- c(grtemp,colors()[grep("grey",colors())])
  coltemp <- setdiff(colors(), grtemp)
  vertex.color <- sample(coltemp, length(lgsplit))
  colm <- rep("skyblue",vcount(g))
  vid <- v.id 
  for(i in 1:(length(split.vector)-1)){
    colm[vid[(split.vector[i]+1):(split.vector[i+1])]]<-vertex.color[i]
  }
}
list(colm, vertex.color)
}

.set.split.mst.crd <- function(lgsplit, layout.function, sf.modules = 5){
   lmst <- list()
   clabt <- c()
   lab <- c()   
   lablist <- list()
  clablist <- list()
   for(i in 1:(length(lgsplit)))
   {
    mst <- minimum.spanning.tree(lgsplit[[i]])
    lablist[[i]] <- E(mst)$label
    ##print(E(mst)$label)  
    lab <- append(lab,E(mst)$label)
    clablist[[i]] <- setdiff( E( lgsplit[[i]] )$label, lablist[[i]])
    lmst[[i]] <- mst 
    clabt <-append(clabt,clablist[[i]])
  }
  if(class(layout.function)=="function"){
    layouts <- lapply(lmst, layout.function)  
  }  
  else{
    layouts <- lapply(lmst, layout_with_fr)  
  }
  lorder <- c() 
  for(i in 1:(length(lgsplit)))
  {
    lorder <- append(lorder, V(lgsplit[[i]])$label)
  }
  layouts <- lapply(layouts, 
function(x)layout.norm(x, (max(x[,1])+sf.modules),(min(x[,1])-sf.modules), 
max(x[,2]+sf.modules),min(x[,2]-sf.modules)))

  crd<- layout.merge(lmst, layouts)
  crd <- cbind(crd,lorder)  
  crd <- crd[order(crd[,3]),]
  crd <- crd[,1:2]
  crd <- matrix(as.numeric(crd),nrow=nrow(crd),byrow=FALSE)  
  list(crd, clabt, lab)
}

.set.split.edge.col <- function(g, in.con.ed.col, lab, clabt, crd, colcode){
  ##print(ecount(g))
  edge.list <-  get.edgelist(g)
#print(nrow(edge.list))
  clab <- setdiff(E(g)$label,sort(lab))
  c1 <- setdiff(clab, sort(clabt)) 
  clab <- setdiff(clab,c1) 
  ##print(clab)
  d <-rep(-2, length(E(g)))
  
  d[lab] <- -1 
  
  
  
  if(length(clab) > 0){
    for(i in 1:length(clab))
     {
      x1 <- crd[edge.list[(clab[i]),1],1]
      y1 <- crd[edge.list[(clab[i]),1],2]
         x2 <- crd[edge.list[(clab[i]),2],1]
      y2 <- crd[edge.list[(clab[i]),2],2]
      d[(clab[i])] <- .dist.mst(x1,y1,x2,y2)
    }
     col <- rep((((max(d[which(d>0)])+1) - (min(d[which(d>0)])-1))/
    length(colcode)),length(colcode))
     col <- cumsum(col)
     col <- append(0,col)
    if(col[length(col)]< max(d))
    {
            col[length(col)] <- col[length(col)]+2
    }
  }  
           colid<-c()
          for(i in 1:length(d))
          {
    if(d[i]==-1)
    {
      colid <- append(colid, "black")
    }
    if(d[i]==-2)
    {
      colid <- append(colid, "grey79")
    }
    
    if(length(clab) > 0)
    {
    
            for(j in 1:(length(col)-1)){
                    if((d[i]>=col[j])&&(d[i]< col[j+1])){
                       colid <- append(colid,names(colcode)[j])
                break
                    }          
            }
    }
          }
    ##print(i)
    
      cole <- colid
    ##print(colcode)
  if(!is.null(in.con.ed.col)){
    cole[which(cole=="grey79")] <- in.con.ed.col
  }  
  
  cole
}
.reset.split.attributes <- function(g, dgg1, colm, glabel, cole,
vertex.color,random.v.color=TRUE, lgsplit, mst.edge.col, e.size, mst.e.size){
  edge.list <-  get.edgelist(g)
  ##print(length(cole))
  g1<- delete.edges(g,E(g)) 
  k1 <- which(cole=="black")
  k2<- which(cole!="black")
  #print(length(cole))
  newe <- rbind(edge.list[k2,], edge.list[k1,])
  g1<-add.edges(g1,t(newe) )
  cole1 <- c(cole[k2],cole[k1])
  V(g1)$color <- vertex.color[1]
  if((length(lgsplit)==length(vertex.color))|| (random.v.color)){
    V(g1)$color <- colm
  }
  if(!is.null(mst.edge.col)){
    cole1[which(cole1=="black")] <- mst.edge.col
  }
  vlab <-NA
  
  if(sum(abs(degree(g1)-dgg1))==0){
      vlab <- glabel
  }
  e.s <- rep(e.size,ecount(g1))
  e.s[which(cole1==mst.edge.col)] <- mst.e.size
  
  list(g1, cole1, e.s, vlab)

} 









