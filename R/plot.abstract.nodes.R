plot.abstract.nodes <- function(x, mod.list=NULL,  rest.module=TRUE, 
color.random=FALSE, nodes.color = NULL, edge.colors=NULL,layout.function=NULL,
tkplot=FALSE,v.sf = 0, e.sf = 0,lab.color=NULL, lab.cex=NULL, 
lab.dist=NULL, bg="black", ... ){
  if(is.null(x)){
    stop("please input a valid igraph object")
  }
  if(!is.igraph(x)){
    stop("please input a valid igraph object")
  }
  g <- x
  rm(x)
  cv <- NA
  if(is.null(V(g)$name)){
    V(g)$name <- paste("g",c(1:vcount(g)),sep="")
  }
  else{  
    if((class(mod.list[[1]])=="character")){
      mln <- list()
      for(i in 1:length(mod.list)){
        km <- match(mod.list[[i]],V(g)$name)
        km <- km[!is.na(km)]  
        mln[[i]] <- km
      }
      names(mln) <- names(mod.list)
      mod.list = mln
      
    }
    
  }
  if(is.null(mod.list)){
      memb <- .mod.function.absnode(g)
      un <- unique(memb)
      mod.list <- lapply(sort(un), function(x)which(x==memb))
  }

  if(rest.module){
  vid <- as.numeric(unlist(mod.list))
  v.rest <- setdiff(as.vector(V(g)), vid)
        v.id <- c(vid, v.rest)
  if(length(v.rest)>0){
         split.vector <- c(0,unlist(lapply(mod.list, length)),length(v.rest))
        }
        else{
         split.vector <- c(0,unlist(lapply(mod.list, length)))
        }       
  split.vector <- cumsum(split.vector)
  l.subgraph <- .get.modules.nodes(g, v.id, split.vector)
  cn <- .count.connection.nodes(g, v.id, split.vector)
  }
  else{  
  vid <- as.numeric(unlist(mod.list))
        v.rest <- setdiff(as.vector(V(g)), vid)
        v.id <- c(vid, v.rest)
  if(length(v.rest)>0){
  split.vector<-c(0,unlist(lapply(mod.list,length)),rep(1,length(v.rest)))
        }
        else{
          split.vector <- c(0,unlist(lapply(mod.list, length)))
        }       

        split.vector <- cumsum(split.vector)
  l.subgraph <- .get.modules.nodes(g, v.id, split.vector)
  cn <- .count.connection.nodes(g, v.id, split.vector)
  }

  if(!is.null(names(mod.list))){
    rownames(cn) <- colnames(cn) <- names(mod.list)
  }
  else{
    rownames(cn) <- colnames(cn) <- paste("m", 
c(1:length(l.subgraph)),sep="")
  }
  count.modules <- unlist(lapply(l.subgraph, .size.module.nodes))
  names(count.modules) <- rownames(cn)
    color.in <- NULL
    if(color.random){
       color.in <- TRUE  
       cols <- .set.rank.nodes(count.modules, color.in)
    }
    else{  
      if((!is.null(nodes.color))){
      if(length(nodes.color)<length(count.modules)){
        color.in <- rep(nodes.color,length(count.modules))
        color.in <- color.in[1:(length(count.modules))]
        cols <- color.in
        names(cols) <- names(count.modules)
      }
      else{
        color.in <- nodes.color[1:(length(count.modules))]
        cols <- color.in
        names(cols) <- names(count.modules)
      }
      }
      else{
        color.in = FALSE
         cols <- .set.rank.nodes(count.modules, color.in)
      }
    }


  if(!is.directed(g)){
    gm <- graph.adjacency(cn,mode="undirected", weighted=TRUE)
    cv <- FALSE
  }
  else{
    gm <- graph.adjacency(cn, weighted=TRUE)
    cv <- TRUE
  }
  V(gm)$label <- paste(V(gm)$name,"\ntotal nodes:",count.modules)
  ##print(V(gm))
  ew <- E(gm)$weight
  ##print(ew)
  rank.edge <- round(rank(ew))
  ##print(rank.edge)
  ew <- (ew/max(ew))*abs(10 + e.sf)
  cm1 <- (count.modules/max(count.modules))*abs(50+v.sf)
  names(cm1) <- names(count.modules)
  count.modules <- cm1 + 4 
  crdf <- .getcrd.mod.nodes(count.modules,layout.function, gm)
  crdf <- crdf[V(gm)$name,]
  cols <- cols[V(gm)$name]
  ecl <- c()
  if(is.null(edge.colors)){
    ecl <- "black"
  }
  if(length(edge.colors)>1&&(length(edge.colors)<ecount(gm))){
  rank.edge <- round((rank.edge*(length(edge.colors)))/max(rank.edge))+1
  edge.colors <- c(edge.colors[1],edge.colors)
  ecl <- edge.colors[rank.edge]
  }
  else{
    ecl <- edge.colors
  }
        if(is.null(lab.color)){
                lab.color="blue"
        }
        if(is.null(lab.cex)){
                lab.cex=1
        }
        if(is.null(lab.dist)){
                lab.dist = .2
        }

  gparm <- list(g= gm, layout = crdf, vertex.color=cols, 
edge.color=ecl,vertex.size=5 + (count.modules),edge.arrow.size=0.3, 
vertex.label.cex=lab.cex, vertex.label=V(gm)$label, lab.color=lab.color,
lab.dist=lab.dist, vertex.frame.color=cols, edge.width=ew, bg=bg, 
vertex.label.family="serif", edge.label.color = lab.color, 
edge.label.cex = lab.cex, edge.curved=cv)
  class(gparm) <- "netbiov"

  if(tkplot){
          tkplot.netbiov(gparm)
  }
  else{
    plot.netbiov(gparm)      
  }  
  gparm  

}




#######################################################
.size.module.nodes <- function(x){return(vcount(x))}


.get.modules.nodes <- function(g, v.id, split.vector){
        split.module <- list()
        for(i in 1:(length(split.vector)-1))
        {
        if((split.vector[i]+1)<split.vector[i+1]){
        split.module[[i]] <-  induced.subgraph(g, 
  v.id[( split.vector[i]+1) : ( split.vector[i+1] ) ])
        }
        else{
        split.module[[i]] <-  induced.subgraph(g, 
  v.id[( split.vector[i]+1) : ( split.vector[i]+1 ) ])
        }
        }
        split.module
}
.count.connection.nodes <- function(g, v.id, split.vector){
        split.module <- list()
        for(i in 1:(length(split.vector)-1))
        {
  if((split.vector[i]+1)<split.vector[i+1]){
         split.module[[i]] <-  v.id[(split.vector[i]+1):(split.vector[i+1])]
  }
  else{
  split.module[[i]] <-  v.id[(split.vector[i]+1):(split.vector[i]+1)]
  }
        }

  ##print(split.module)
        conn <-matrix(0,nrow=length(split.vector)-1,ncol=length(split.vector)-1)
  adjtmp <- get.adjacency(g)
        for(i in 1:(length(split.module)-1)){
                v.set1 <-  split.module[[i]]
                for(j in (i+1):length(split.module) ){
                        v.set2 <- split.module[[j]]

                         tmp <- .count.edges.nodes(adjtmp,v.set1, v.set2)

      conn[i,j] = tmp[[1]]
      conn[j,i] = tmp[[2]]
    }
         }

        conn
}
.count.edges.nodes <- function(adjtmp, vid1, vid2){
        ew = 0
        #for(i in 1:length(vid1)){
        #        for(j in 1:length(vid2)){
        #                if((are.connected(g, vid1[i], vid2[j]))){
        #                        ew = ew + 1
        #                }
        #        }
        #}
  
  e1 <- sum(adjtmp[(vid1), (vid2)]) 
  e2 <- sum(adjtmp[(vid2), (vid1)])
        return(list(e1, e2))
}
.set.rank.nodes <- function(d, color.in){
  ##print((d))
        col <- hist(d,breaks=round(max(d)), plot=FALSE)$breaks
  ##print(col)
  ##print("####")
        col[which.max(col)] <- col[which.max(col)] + 1
  if(color.in){
    grtemp <- colors()[grep("gray",colors())]
                grtemp <- c(grtemp,colors()[grep("grey",colors())])
                coltemp <- setdiff(colors(), grtemp)
     #colcode <- (colors()[grep(sample(coltemp,length(d)),colors())])
     colcode <- sample(coltemp,length(d))  
    colid <- colcode
    ##print((colcode))
    names(colid) <- names(d)
  }
  else{  
          colcode <- rev(heat.colors(length(col),alpha=.75))
    if(length(colcode)>4){
      colcode[c(1, 2,3,4,5) ] <- colcode[c(6,7,8,9,10)] 
    }
          colid <- c()
          for(i in 1:length(d))
          {
                  for(j in 1:(length(col)-1)){
                          if((d[i]>=col[j])&&(d[i]<col[j+1])){
                                  colid <- append(colid, (colcode)[j])

                          }  
                  }
          }
          names(colid) <- names(d)
  }
        colid
}

.getcol.nodes <- function(lst){
        col <- c()
        for(i in 1:length(lst)){
                col <- rbind(col,lst[[i]][,c(1,5)])
        } 
        col
}    
.getcrd.mod.nodes <- function(count.module,layout.function=NULL,g){
  dst <- function(x){return(dist(rbind(c(0,0), x)))}
  if(is.null(layout.function)||(class(layout.function)!="function")){
          crdf <- layout_with_graphopt(graph.empty(length(count.module)))
  }
  else{
    crdf <- layout.function(g)
  }
  crdf <- layout.norm(crdf, xmax=100, xmin=-100, ymax=100, ymin=-100)
        rownames(crdf) <- paste("m", c(1:length(count.module)),sep="")
        count.module  <- sort(count.module, decreasing=TRUE)
  if(is.null(layout.function)){
    d  <- apply(crdf,1, dst)  
          names(d) <- rownames(crdf) 
          d <- sort(d, decreasing=FALSE)
          crdf <- crdf[names(d),]
  }
  rownames(crdf) <- (V(g)$name)
  count.module <- count.module[rownames(crdf)]
  crdf
}

.mod.function.absnode <- function(g){
if(!is.directed(g)){
fc <- multilevel.community(g)
memb <- fc$membership
}
else{
memb <- walktrap.community(g)$memb
}
memb
}
