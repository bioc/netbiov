plot.modules <- function(x, layout.function=NULL,mod.list=NULL,
module.function=FALSE, split.graph=7, color.random=FALSE, modules.color = NULL,
col.grad=NULL, mod.edge.col=NULL,ed.color=NULL,edge.col.random=FALSE,
expression = NULL,exp.by.module=FALSE, tkplot=FALSE, layout.overall = NULL,
sf=0,arrange.bydegree=FALSE,mod.lab=FALSE,node.lab=FALSE, lab.cex = NULL,
lab.color=NULL, lab.dist=NULL, v.size=FALSE, nodeset=NULL,path.col="green",
col.s1="red", col.s2="yellow", nodes.on.path=TRUE,e.path.width=c(1,1),
scale.module=NULL,v.sf=5,e.width=.5,bg="black", abstract.graph=TRUE,
modules.name.num = TRUE, v.size.path=TRUE,...){
  if(is.null(x)){
    stop("please input a valid igraph object")
  }
  if(!is.igraph(x)){
    stop("please input a valid igraph object")
  }
  labx=NA;lab=NA;
  tmp <- .set.attributes(x, mod.list)  
       g <- tmp[[1]]
  rm(x)
  mod.list <- tmp[[2]]
  labx <- tmp[[3]]
  tmpn <- .set.expression(g, expression, exp.by.module)
  exp.col <- tmpn[[1]]
  expmod <- tmpn[[2]] 
  expression <- tmpn[[3]]
  v.size <- .set.vertex.size(g, v.size, v.sf=v.sf)
  tmpn0 <- .set.module.info(g=g, split.graph=split.graph, 
  mod.list=mod.list, module.function=module.function, modules.name.num)

  split.vector <- tmpn0[[1]]
  v.id <- tmpn0[[2]]
  mod.list <- tmpn0[[3]]
  ######print(mod.list)
       tmpn1 <- .set.module.col(color.random, split.vector, modules.color, col.grad)

  color.in <- tmpn1[[1]]
  col.grad <- tmpn1[[2]]
  layout.function <- .set.layout.function(layout.function, split.vector)
       tempn2  <- .set.graph.attributes(g, layout.function, split.vector, v.id,
 exp.by.module, expression, color.in, col.grad, arrange.bydegree) 

  layouts <- tempn2[[1]]
  lgsplitx <- tempn2[[2]] 
       vcols <- .getcol.mod(layouts)
       vcols <- vcols[V(g)$name,]

  if(class(abstract.graph)=="logical"){
    if(abstract.graph){
      abstract.graph <- .absg(g, mod.list)
    }
    else{
      abstract.graph=NULL
    }
  } 
  else{
    abstract.graph=NULL
  }
       vcrd<-.getcrd.mod(layouts,layout.overall,sf,scale.module,abstract.graph)
       crd <- vcrd[V(g)$name,]
  ed.color <- .set.edge.col(g, ed.color,edge.col.random,
mod.edge.col, lgsplitx)
  if((length(expression)==vcount(g))&& (!expmod)){
    vcols[V(g)$name,2] <- exp.col
  }
       latattr <- .set.label(g, mod.list, mod.lab,node.lab,
 layouts, labx, lab.color, lab.cex, lab.dist )  
       tempn3 <- .set.shortest.paths(g, nodeset, ed.color, 
vcols, e.width, nodes.on.path, path.col, e.path.width, 
col.s1, col.s2, mod.list, v.size.path, v.size) 
  vcols <- tempn3[[1]]
  e.width <- tempn3[[2]]
  ed.color <- tempn3[[3]]
  v.size <- tempn3[[4]]
     
  ###################################################
  gparm <- list(g= g, layout = crd, vertex.color=vcols[,2], 
edge.color=ed.color,vertex.size=v.size,edge.arrow.size=0.3, 
vertex.label.cex=latattr[[3]], vertex.label=latattr[[1]], 
lab.color=latattr[[2]],lab.dist=latattr[[4]], vertex.frame.color=vcols[,2],
edge.width=e.width, bg=bg)
  class(gparm) <- "netbiov"

  if(tkplot){
          tkplot.netbiov(gparm)
  }
  else{
    plot.netbiov(gparm)      
  }  
  gparm  
}



.get.coord.mod <- function(g){
        g1 <- g$g
        dg <- g$dg
  arrange.bydegree=FALSE
  col.i <- g$color.input
  col.grad <- g$col.grad
  exp.mod = g$exp.mod
  layout.function <- g$layout.function
  arrange.bydegree <- g$arrange.bydegree
        g <- g1
        dst <- function(x){return(dist(rbind(c(0,0), x)))}
        mx <- function(x){ return(x[which.max(abs(x))])}
        chngdr <- function(x){if(x[2]<0){x[2] <- abs(x[2])};return(x)}
  if(is.null(layout.function)||(class(layout.function)!="function")){
    crd1 <- layout_as_tree(g, 
root=as.vector(V(g)[which.max(degree(g))-1]), circular=TRUE)

    crd1[(is.nan(crd1))] <- 0
  }
  else{
    tryCatch({ 
      crd1 <- layout.function(g)

  
    }, error = function(ex) {
      crd1 <- layout_as_tree(g, 
root=as.vector(V(g)[which.max(degree(g))-1]), circular=TRUE)
      crd1[(is.nan(crd1))] <- 0
    })
    
  }

        d  <- apply(crd1,1, dst)
        names(d) <- V(g)$name
  detail <- cbind(d, crd1)
  
        rownames(detail) <- V(g)$name
  if((nrow(detail)>1)&&(arrange.bydegree)){
          detail <- detail[order(detail[,1], decreasing = TRUE),]
          #detail <- detail[order(detail[,1], decreasing = FALSE),]
  }

        rownames(crd1) <- V(g)$name
        d <- rev(sort(d))
        names(dg) <- V(g)$name
        cols <- .set.rank.mod(dg, color.input=col.i, col.grad=col.grad, exp.mod)
  if(length(dg)>1 && (arrange.bydegree)){
          dg <- sort(rev(dg))
  }

        dg1 <- cbind(dg, detail)
  if(nrow(dg1)>1 && (arrange.bydegree)){
          rownames(dg1) <- names(dg)
          dg1 <- dg1[rownames(crd1),]
          #########print(dg1)
	  ######## print("crd1")	
            cols <- cols[rownames(dg1)]
  }
        crd <- cbind(dg1, cols)
        crd

}
.getcol.mod <- function(lst){
        col <- c()
  nm <- c()
         for(i in 1:length(lst)){
    nm <- append(nm,rownames(lst[[i]]))
                 col <- rbind(col,lst[[i]][,c(1,5)])
    
        }
  rownames(col) <- nm
        col
}
##########################
.getcrd.mod <- function(lst, layout.overall=NULL,sf=0,
scale.module=NULL, abstract.graph=NULL){
     ############print(is.null(abstract.graph))  
        cnt <- c()
        for(i in 1:length(lst)){
                cnt <- c(cnt, nrow(lst[[i]]))
        }
        cnt <- cbind(c(1:length(cnt)), cnt)
     ######print(is.null(abstract.graph))
     if(is.null(abstract.graph)){    
          cnt <- cnt[order(cnt[,2], decreasing = TRUE),]
          lstnew <- list()
          for(i in 1:length(lst)){
                lstnew[[i]] <- lst[[cnt[i,1]]]
          }
    lst <- lstnew

     }  
        scale.module <- scale.module[cnt[,1]]
  #################print(head(scale.module))

        dst <- function(x){return(dist(rbind(c(0,0), x)))}
        k  <- c()
        minn <- function(x){return(min(x,50))}
        mxx <- function(x){if(x<.99){return(1)}else{return(x)}}
  if(is.null(scale.module)){
          for(i in 1:length(lst)){
                  k <- append(k, nrow(lst[[i]]))
          }
    k <- sqrt(k)
    #k <- sapply(k,minn)
          #################print(k)
  }
  else{
    if(length(scale.module)!=length(lst)){
      k <- rep(scale.module, length(lst))
      k <- k[1:length(lst)]
    }
    else{
      k <- scale.module
    }
  }
  k <- sapply(k,minn)
  
        max.k <- max(k)
  if(is.null(layout.overall)||(class(layout.overall)!="function")){
    if(!is.null( abstract.graph)){
    gx <- graph.adjacency( abstract.graph, mode="undirected") 
           crdf <- layout_with_fr(gx)
    }
    else{  
    crdf <- layout_with_fr(graph.empty(length(lst)))}
  }
  else{
    if(!is.null( abstract.graph)){
    gx <- graph.adjacency( abstract.graph, mode="undirected") 
          crdf <- layout.overall(gx)
    }
          else{crdf <- layout.overall(graph.empty(length(lst)))}
  }
        scale.x <- max(abs(crdf[,1])); scale.y <- min(abs(crdf[,1]))
        scale.x <- scale.y <- max(scale.x, scale.y)
        crdf <- layout.norm(crdf, 
  xmax=scale.x+max.k+20+sf, xmin=-1*(scale.x+max.k+20+sf), 
  ymax=scale.y+max.k+20+sf, ymin=-1*(scale.y+max.k+20+sf) )
        rownames(crdf) <- paste("g", c(1:length(lst)),sep="")
        d  <- apply(crdf,1, dst)
        names(d) <- rownames(crdf)
     if(is.null( abstract.graph)){  
          d <- sort(d, decreasing=FALSE)
    crdf <- crdf[names(d),]

     }
        crd <- list()
        crdall <- c()
        for(i in 1:length(lst)){
                tmp <- lst[[i]]
                tmp1 <- matrix(as.numeric(tmp[,c(3:4)]), nrow=nrow(tmp))
                rownames(tmp1) <- rownames(tmp)
                tmp1 <- layout.norm(tmp1, (crdf[i,1]-k[i]),(crdf[i,1]+k[i]),
(crdf[i,2]-k[i]),(crdf[i,2]+k[i]))

    tmp1[(is.nan(tmp1[,1])),1] <- min(tmp1[!is.nan(tmp1[,1]),1])
    tmp1[(is.nan(tmp1[,2])),2] <- min(tmp1[!is.nan(tmp1[,2]),2])

    kk1 <- which(tmp1[,1]==Inf)
    kk2 <- which(tmp1[,2]==Inf)
    if(length(kk1)>0){
      tmp1[kk1,1] <- crdf[i,1]+rnorm(1)
    }
    if(length(kk2)>0){
                        tmp1[kk2,2] <- crdf[i,2]+rnorm(1)
                }
                crd[[i]] <- tmp1
                crdall <- rbind(crdall, tmp1)
        }
        crdall
}
###########

.set.rank.mod <- function(d, color.input=NULL, col.grad=NULL,exp.mod=FALSE){
  #########print(col.grad)

  col <- c(min(d), min(d)+1, min(d)+2, max(d)+min(d)+3)
  if(length(d)>2){
    if(max(d)==0){
      dtemp <- rep(1,length(d))
      names(dtemp) <- names(d)
      d <- dtemp
    }
    #########print((d))
          col <- hist(d,breaks=round(max(abs(d))), plot=FALSE)$breaks
          col[which.max(col)] <- col[which.max(col)] + 1
  }
  
  grtemp <- colors()[grep("gray",colors())]
  grtemp <- c(grtemp,colors()[grep("grey",colors())])
  coltemp <- setdiff(colors(), grtemp)

  #          color_type="warm",random=TRUE, total_color = 20

  
  if(is.null(color.input)){
    if(is.null(col.grad)||(length(col.grad)<2)){
            colcode <- rev(heat.colors(length(col), alpha = 1))
    }
    else{
      if(length(col.grad)<length(col)){
        tmp1 <- unique(col.grad)
        tmp1 <- tmp1[1]  
        col.tmp <- rep(tmp1, (length(col)-length(col.grad)))
        colcode <- c(col.tmp,col.grad)
      }
      else{colcode <- col.grad}
    }
  }
  if(class(color.input)=="logical"){
    if(color.input){
    colcode <-.colfn_mod(color_type="all",random=TRUE,total_color=1)
    if(length(colcode)<length(col)){
      nn <- length(col) - length(colcode)
      colcode <- append(rep(colcode[1],nn),colcode)
    }
    }
    else{
      
    if(is.null(color.input)){
    if(is.null(col.grad)||(length(col.grad)<2)){
            colcode <- rev(heat.colors(length(col), alpha = 1))
    }
    else{
      if(length(col.grad)<length(col)){
      tmp1 <- unique(col.grad)
      tmp1 <- tmp1[1]  
      col.tmp <- rep(tmp1, (length(col)-length(col.grad)))
      colcode <- c(col.tmp,col.grad)
      }
      else{colcode <- col.grad}
    }
    }
    }
  }
  if(class(color.input)=="character"){
    colcode <- rep(color.input, length(col))
    #########print()
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
  if(exp.mod){
    
    colid <- sapply(d,.rgbToHex.mod) 
    #########print(head(colcode))
  }
  #########print(length(colid))
        names(colid) <- names(d)
  #########print(paste(length(col),">>> ", length(colcode)))
        colid

}
##############
.edge.col.mod <- function(g, col = NULL){
  ecol <- c()
  if(is.null(col)){
    ecol <- rep("gray", ecount(g))    
  }
  else{
    ecol <- rep(col, ecount(g))
  }
  names(ecol) <- E(g)$name
  return(ecol)
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
.get.lab.module1 <- function(g){
  
  d <- degree(g)
  #########print(d)
  d <- which.max(d)
  return(V(g)[d-1]$name)
}
.get.lab.module <- function(d){
  d <- d[,2]
  d1 <- as.numeric(d)
  names(d1) <- names(d) 
  #d <- degree(g)
  #########print(d)
  dm <- which.min(d1)
  return(names(d1)[dm])
}
.colfn_mod <- function(color_type="warm",random=TRUE, 
total_color = 20, start = 1 ){
        R=0;G=0;B=0
        #if(color_type == "warm"){
                R <- c(rep(0,256),(c(0:255)),rep(255,256))
                G <- c(rep(255,256), rep(255,256),rev(c(0:255)))
                B <- c(rev(c(0:255)), rep(0,256), rep(0,256))
                Rhx <- sapply(R,.toHex.mod)
                Ghx <- sapply(G,.toHex.mod)
                Bhx <- sapply(B,.toHex.mod)
                colcode1 <- paste("#",Rhx,Ghx,Bhx,sep="")
        #}
        #if(color_type == "cold"){
                R <- c(rep(255,256),c(0:255),c(0,256))
                G <- c(rep(0,256), rep(0,256),c(0:255))
                B <- c(c(0:255), rep(255,256), rep(255,256))
                Rhx <- sapply(R,.toHex.mod)
                Ghx <- sapply(G,.toHex.mod)
                Bhx <- sapply(B,.toHex.mod)
                colcode2 <- paste("#",Rhx,Ghx,Bhx,sep="")
        #}
    if(color_type=="warm"){
      colcode <- colcode1
    }
    if(color_type=="cold"){
      colcode <- colcode2
    }
    if(color_type=="all"){
      colcode <- c(colcode1,colcode2)
    }
  
  
        if(random){
                colcode <- sample(colcode,total_color)
        }
        else{
                if((start+total_color-1)< length(colcode)){

                        colcode <- colcode[start:(start+total_color)]
                }
        }

        return(colcode)
}

.rgbToHex.mod1 <- function(n){
        #R <-  G 
        #B  <- 255
        #G <- 255
  #R=(n-1)
  if(n<255){
    R = 255
    G = n
    B = 0
  }
  else{
    R=255-(n-255)
    G=255
    B=0
  }

  #G=(n-20)
  #R=255-(n-10); 
  #B=13


        k1 <- .toHex.mod(R)
        k2 <- .toHex.mod(G)
        k3 <- .toHex.mod(B)
        k <- paste("#",k2,k1,k3, sep="")
        return(k)
}

.get.paths.nodes <- function(s1, s2, g){
  path.list <- list()
  for(i in 1:length(s1)){
    path.list[[i]] <- get.shortest.paths(g, s1[i],to = s2)$vpath  
  }
  kp <- c()
  for(i in 1:length(path.list)){
    for(j in 1:length(path.list[[i]])){
      tmp <- path.list[[i]][[j]]
      if(length(tmp)>1){
        for(k in 1:(length(tmp)-1)){
          kp <- rbind(kp, c(tmp[k], tmp[k+1]))
        }
      }
    }  
  }
    tmp.node <- as.vector(kp)
    tmp.node <- unique(setdiff(unique(tmp.node), c(s1,s2)))


  kp1 <- kp
  if(!is.null(V(g)$name)){
    kp[,1] <- V(g)$name[kp1[,1]+1]  
    kp[,2] <- V(g)$name[kp1[,2]+1]
  }
  tmpel <- get.edgelist(g)
  tmpel <- paste(tmpel[,1],"###",tmpel[,2],sep="")
  kpx1 <- paste(kp[,1],"###",kp[,2],sep="")
  kpx2 <- paste(kp[,2],"###",kp[,1],sep="")
  el.match1 <- match(kpx1, tmpel)
  el.match2 <- match(kpx2, tmpel)
  el.match <- unique(c(el.match1, el.match2))
  el.match<- el.match[!is.na(el.match)]
  rm(tmpel,kpx1, kpx2,el.match1, el.match2  )
  list(el.match, tmp.node)
}

############# Modules internal ###############
.set.attributes <- function(g=g, mod.list=mod.list){
  if(is.null(V(g)$name)){
    V(g)$name <- paste("g", c(1:vcount(g)), sep="")
    labx <- as.character(c(1:(vcount(g))))
  }
  else{  
    if((class(mod.list[[1]])=="character")){
      mln <- list()
      for(i in 1:length(mod.list)){
        km <- match((mod.list[[i]]),V(g)$name)
        km <- km[!is.na(km)]  
        mln[[i]] <- km
        #########print(km)
      }
      names(mln) <- names(mod.list)
      mod.list = mln
      
    };labx <- V(g)$name
  }
  if(!is.null(V(g)$label)){
    labx = V(g)$label
  }
  if(is.null(E(g)$name)){
    E(g)$name <- paste("e", c(1:ecount(g)), sep="")
  }
  
  list(g, mod.list, labx)

}

.set.expression <- function(g, expression, exp.by.module){
  expmod = FALSE
  exp.col <- NULL
  if((length(expression)==vcount(g))){
    names(expression) <- V(g)$name
     rnk <- (((510 - 1)*(abs(expression)-
min(abs(expression))))/(max(abs(expression))-min(abs(expression)))) + 1 
    #rnk <- (abs(expression)*510)/max(abs(expression))
    #exp.col <- sapply(rank(expression),.rgbToHex.mod)
    exp.col <- sapply(rnk,.rgbToHex.mod)  
    if(exp.by.module||(class(exp.by.module)=="integer")){
      expmod=TRUE
    }
  }
  list(exp.col, expmod, expression)

}

.set.vertex.size <- function(g, v.size, v.sf=2){

  if(class(v.size)=="logical" || 
(class(v.size)=="numeric" && length(v.size)!=vcount(g))){
    if(class(v.size)=="numeric"){
      v.size=v.size[1]
    }
    else{
      v.size=2
    }
  }
  if(class(v.size)=="numeric" && length(v.size)==vcount(g)){
      
    v.size <- (rank(v.size)*v.sf)/max(rank(v.size))
    #v.size <- (((12 - 1)*(abs(v.size)-
    #min(abs(v.size))))/(max(abs(v.size))-min(abs(v.size)))) + 1 
    v.size[which(v.size<=.5)] <- .5
          
    
  }
  v.size


}


.set.module.info <- function(g, split.graph, module.function, mod.list, 
modules.name.num){
  if( (is.null(mod.list))){
    #if(class(module.function))
      memb <- .mod.function.modules(g)
      un <- unique(memb)
      mod.list <- sapply(sort(un), function(x)which(x==memb))
      #mod.list <- sapply(mod.list, function(x)x-1)
  }
  if(is.null(names(mod.list))){
    if(modules.name.num){
      names(mod.list) <- c(1:length(mod.list))
    }
    else{
      names(mod.list)<-paste("module",c(1:length(mod.list)))

    }
  }
  #else{
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
  #}
  #print(split.vector)
  list(split.vector, v.id, mod.list)

}


.set.module.col <- function(color.random, split.vector,modules.color,col.grad){
  #########print(split.vector)
  color.in <- NULL
  if(color.random){
     color.in <- rep(TRUE, (length(split.vector)-1))  
    #########print(color.in)
  }
  else{  
    if((!is.null(modules.color))){
    if(length(modules.color)<(length(split.vector)-1)){
      color.in <- rep(modules.color,(length(split.vector)-1))
      color.in <- color.in[1:(length(split.vector)-1)]
    }
    else{
      color.in <- modules.color[1:(length(split.vector)-1)]
    }
    }
  }

  if(class(col.grad)!="list"){
    col.grad <- list(col.grad)
  }
  if(class(col.grad)=="list"){
    if(length(col.grad)!=(length(split.vector)-1)){
      col.grad <- rep(col.grad, (length(split.vector)-1))
      col.grad <- col.grad[1:(length(split.vector)-1)]  
    }
  }
  list(color.in, col.grad)


}

.set.layout.function <- function(layout.function, split.vector){
  #########print(v.id)
  if(length(layout.function)<(length(split.vector)-1)){
    if(class(layout.function)=="function"){
      layout.function <- c(layout.function)
    }
    ly.tmp <- as.list(layout.function)
    #########print(length(ly.tmp))
    layout.function <- rep(ly.tmp, (length(split.vector)-1))
    ########print(length(layout.function))
    layout.function <- layout.function[1:(length(split.vector)-1)]
    
  }
  layout.function

}

.set.graph.attributes <- function(g,layout.function, split.vector, v.id, 
exp.by.module, expression, color.in, col.grad, arrange.bydegree){
        lgsplit <-list()
        lgsplitx <- list();
        for(i in 1:(length(split.vector)-1))
        {
  if((split.vector[i]+1)<split.vector[i+1]){
        g.temp<-induced.subgraph(g,v.id[( split.vector[i]+1):(split.vector[i+1])])
        }
        else{
        g.temp<-induced.subgraph(g,v.id[(split.vector[i]+1):(split.vector[i]+1)])
        }
        lgsplitx[[i]] <- g.temp
  dg = degree(g, V(g.temp)$name)
  exp.mod = FALSE
  if(class(exp.by.module)=="logical"){
    if(exp.by.module && !(is.null(expression))){
    dg <- expression[V(g.temp)$name]
    if(length(dg)< vcount(g.temp)){
      dg <- degree(g, V(g.temp)$name)
    }
    dg <- (((510 - 1)*(abs(dg)-min(abs(dg))))/(max(abs(dg))-min(abs(dg)))) + 1 
    exp.mod = TRUE
    }
  }
  if(class(exp.by.module)=="numeric"){
    if((length(exp.by.module)>0) && !(is.null(expression))){
    if(i %in% exp.by.module){
       dg <- expression[V(g.temp)$name]
      exp.mod=TRUE  
    }
    if(length(dg)< vcount(g.temp)){
      dg <- degree(g, V(g.temp)$name)
    }
    dg <- (((510 - 1)*(abs(dg)-min(abs(dg))))/(max(abs(dg))-min(abs(dg)))) + 1 
    }
  }
                lgsplit[[i]] <- list(g = g.temp, dg = dg, 
color.input = color.in[i], col.grad = col.grad[[i]], 
layout.function = layout.function[[i]],arrange.bydegree=arrange.bydegree,
exp.mod = exp.mod)
        }
        layouts <- lapply(lgsplit, .get.coord.mod)
        list(layouts, lgsplitx)  
}


.set.edge.col <- function(g, ed.color, edge.col.random, mod.edge.col,lgsplitx){
  if(is.null(ed.color)){
    ed.color <- rep("lightgray", ecount(g))
  }
  else{
    ed.color <- rep(ed.color[1],ecount(g))
  }
  names(ed.color) <- E(g)$name
  if((edge.col.random)){
          smpcol <- .colfn_mod(color_type="warm",random=TRUE,
    total_color=length(lgsplitx)) 
    #smpcol <- sample(coltemp, length(lgsplitx))
    ectmp <- c()
    for(i in 1:length(lgsplitx)){
      ectmp <- c(ectmp,.edge.col.mod(lgsplitx[[i]],smpcol[i]))
    }
    #########print(ectmp)
    ed.color[names(ectmp)] <- ectmp
  }
  else{
  if(!is.null(mod.edge.col)){
  if(length(mod.edge.col)< length(lgsplitx)){
  mod.edge.col <-rep(mod.edge.col, length(lgsplitx)) 
  }
  ectmp <- c()
        for(i in 1:length(lgsplitx)){
          ectmp <- c(ectmp, .edge.col.mod(lgsplitx[[i]], mod.edge.col[i]))
        }
  ed.color[names(ectmp)] <- ectmp
  }
  }
           ed.color

}

.set.label <- function(g, mod.list, mod.lab,node.lab, layouts, 
labx, lab.color, lab.cex, lab.dist ){
  ########print(node.lab)
  lab=NA
  if(!is.null(names(mod.list))&& (mod.lab)){
    lab <- rep("",vcount(g))
    names(lab) <- V(g)$name
    labn <- lapply(layouts,.get.lab.module)
    lab[unlist(labn)] <- names(mod.list)
    #########print(labn)
  }
  if(node.lab){
    lab <- NA
    lab <- labx
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
  ########print(lab)
      list(lab, lab.color, lab.cex, lab.dist)
}

.set.shortest.paths <- function(g, nodeset, ed.color, vcols, e.width, 
nodes.on.path, path.col, e.path.width, col.s1, col.s2, mod.list, 
v.size.path, v.size){
  
  if(!is.null(nodeset)){
    s1 <- c()
    s2 <- c()
    if(class(nodeset)=="list"){
      if(length(nodeset)==2){  
      nodeset <- lapply(nodeset, function(x)as.numeric(x))
      s1 <- nodeset[1]
      s2 <- nodeset[2]

      }
      else{
        
        s1 <- mod.list[nodeset[1]]
        s2 <- mod.list[nodeset[2]]  
        #s1 <- as.numeric(nodeset[[1]])
        #s2 <- setdiff(c(0:(vcount(g)-1)),s1)
      }
    }
    if(class(nodeset)=="numeric"){
    if(length(nodeset) > 2){
    txm <- length(nodeset)%%2
    if(txm==0){
      tmp_1 <- sapply(c(1:length(nodeset)),function(x)x%%2)
      xxp1 <- nodeset[which(tmp_1==1)]
      xxp2 <- nodeset[which(tmp_1==0)]
      s1 <- (mod.list[xxp1])
      s2 <- (mod.list[xxp2]) 
    }
    else{
      nodeset <- c(nodeset, nodeset[length(nodeset)])
      tmp_1 <- sapply(c(1:length(nodeset)),function(x)x%%2)
      xxp1 <- nodeset[which(tmp_1==1)]
      xxp2 <- nodeset[which(tmp_1==0)]
                
      s1 <-   (mod.list[xxp1])
      s2 <-   (mod.list[xxp2]) 
    
    }
    
    }
    else{
    
      s1 <- mod.list[nodeset[1]]
      s2 <- mod.list[nodeset[2]]
    }  
    }
    path.colx <- .get.paths.nodes_l(s1, s2, g)
    path.colx1 <- path.colx[[2]]
    path.colx <- path.colx[[1]]
    ##print(path.colx)

    if((nodes.on.path==TRUE)||(class(nodes.on.path)=="character")){
      if(class(nodes.on.path)=="logical"){
        vcols[(path.colx1),2] <- "purple"
      }
      else{  vcols[(path.colx1),2] <- nodes.on.path }
    }
    ######print(ed.color) 
    ed.color[path.colx] <- path.col[1]
    nmx1 <- c()
    nmx2 <- c()
    if(((length(path.col)-1)<length(s1))&&(length(path.col)>1)){
      pc <- rep(path.col[2:length(path.col)],length(s1))
    }
    if((length(path.col)-1)>=length(s1)){
      pc <- path.col[2:(2+length(s1))]
    }
    if(length(path.col)==1){
      pc <- rep(path.col,length(s1))
    }
    if(length(e.path.width)==1){
      e.path.width <- rep(e.path.width,2)
    }  
    e.width <- rep(e.width,ecount(g))[1:ecount(g)]
    names(e.width) <- paste("e", c(1:length(e.width)), sep="")  
    e.width[path.colx] <- e.path.width[2]
    #print(pc)
    for(ix in 1:length(s1)){
    gxx1 <- induced.subgraph(g, s1[[ix]])
    gxx2 <- induced.subgraph(g, s2[[ix]])
    nmx1 <- E(gxx1)$name  
    nmx2 <- E(gxx2)$name 
    nmx1 <- c(nmx1, intersect(nmx1, names(ed.color[path.colx])))
    nmx2 <- c(nmx2,intersect(nmx2, names(ed.color[path.colx])))
    nmx1 <- unique(nmx1)
    nmx2 <- unique(nmx2) 
    
    if(length(nmx1)>0){
      #print(pc[ix])
      ed.color[nmx1] <- pc[ix]
    }
    if(length(nmx2)>0){
      #print(pc[ix])
      ed.color[nmx2] <- pc[ix]
    }
    if(length(c(nmx1, nmx2))>0){  
      e.width[c(nmx1, nmx2)] <- e.path.width[1]
    }


    }
    
    if(length(col.s1) < length(s1)){
      col.s1 = rep(col.s1, length(s1))
      col.s2 = rep(col.s2, length(s2))
    }
    #print(col.s1)
    for(i in 1:length(s1)){
      vcols[(unlist(s1[[i]])),2] <- col.s1[i]
      vcols[(unlist(s2[[i]])),2] <- col.s2[i]
    }
    v.size <- rep(v.size, vcount(g))
    if(class(v.size.path)=="logical"){
      if(v.size.path){
      v.size[(c(unlist(s1),unlist(s2)))] <- max(v.size) + .5  
      v.size[(path.colx1)] <- max(v.size) + .5
      }
      
    }
    if(class(v.size.path)=="numeric"){
      v.size[(c(unlist(s1),unlist(s2)))] <- v.size.path
    }
      
    
    
  }
  list(vcols, e.width, ed.color, v.size)

}

.get.paths.nodes_l <- function(s1, s2, g){
path.list <- list()
cnt <- 1
for(i in 1:length(s1)){
  for(j in 1:length(s1[[i]])){
  path.list[[cnt]] <- get.shortest.paths(g, s1[[i]][j],to = s2[[i]])$vpath
  cnt <- cnt + 1
  }
}
kp <- c()
for(i in 1:length(path.list)){
  for(j in 1:length(path.list[[i]])){
  tmp <- path.list[[i]][[j]]
  kp <- rbind(kp, cbind(tmp[1:(length(tmp)-1)], tmp[2:length(tmp)]))
  }  
}

  tmp.node <- as.vector(kp)
  tmp.node <- unique(setdiff(unique(tmp.node), c(s1,s2)))


kp1 <- kp
if(!is.null(V(g)$name)){
  kp[,1] <- V(g)$name[kp1[,1]]  
  kp[,2] <- V(g)$name[kp1[,2]]
}
tmpel <- get.edgelist(g)
tmpel <- paste(tmpel[,1],"###",tmpel[,2],sep="")
kpx1 <- paste(kp[,1],"###",kp[,2],sep="")
kpx2 <- paste(kp[,2],"###",kp[,1],sep="")
el.match1 <- match(kpx1, tmpel)
el.match2 <- match(kpx2, tmpel)
el.match <- unique(c(el.match1, el.match2))
el.match<- el.match[!is.na(el.match)]
rm(tmpel,kpx1, kpx2,el.match1, el.match2  )
list(el.match, tmp.node)
}


 .absg <- function(g, mod.list){
  adj <- get.adjacency(g)
  ########print(mod.list)
   m <- matrix(0, nrow=length(mod.list), ncol=length(mod.list)) 
  for(i in 1:(length(mod.list)-1)){
    for(j in (i+1):length(mod.list)){
      if(!is.directed(g)){
        xx <- adj[(mod.list[[i]]), (mod.list[[j]])]
        if(sum(xx) > 0){
          m[i,j]<- m[j,i] <- 1
        }
      }
      else{
        xx1 <-  adj[(mod.list[[i]]), (mod.list[[j]]+1)]
        xx2 <-  adj[(mod.list[[j]]), (mod.list[[i]]+1)]
        if(sum(xx1) > 0){
          m[i,j] <- 1  
        }
        if(sum(xx2) > 0){
          m[j, i] <- 1
        }
      }
    }
  }
  ##print("xxsxss")
  m
}


.mod.function.modules <- function(g){
if(!is.directed(g)){
  fc <- multilevel.community(g)
  memb <- fc$membership
}
else{
  memb <- walktrap.community(g)$memb
}

  memb
}




