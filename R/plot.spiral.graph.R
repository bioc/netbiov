plot.spiral.graph <- function(x, tp=61, vertex.color=NULL, color.random=FALSE,
rank.function=NULL,tkplot=FALSE, v.size=2, e.size=.5,e.curve=.5, v.lab=FALSE,
bg="black", e.col="grey", skip=0, ...){
  
  if(is.null(x)){
    stop("please input a valid igraph object")
  }
  if(!is.igraph(x)){
    stop("please input a valid igraph object")
  }
  g <- x
  rm(x)
  if(is.null(V(g)$name)){
    V(g)$name <- paste("g", c(1:vcount(g)),sep="")
  }
  color.in <- NULL
  if(color.random){
     color.in <- rep(TRUE, (length(vcount(g))))  
    ##print(color.in)
  }
  else{  
    if(!is.null(vertex.color)){
      if(length(vertex.color)<((vcount(g)))){
        color.in <- rep(vertex.color,((vcount(g))))
        color.in <- color.in[1:((vcount(g)))]
      }
      else{
        color.in <- vertex.color[1:(vcount(g))]
      }
    }
  }

  #print(color.in)
  dst <- function(x){return(dist(rbind(c(0,0), x)))}
  mx <- function(x){ return(x[which.max(abs(x))])}
  chngdr <- function(x){if(x[2]<0){x[2] <- abs(x[2])};return(x)}

  crd1 <- .get.spiral(g, a=1, b = 1, tp = tp, rank.function=rank.function, skip=skip)
  
  rownames(crd1) <- V(g)$name
  dg <- degree(g)
  names(dg) <- V(g)$name
  cols <- .set.rank.spiral(dg, color.in)
  cols <- cols[rownames(crd1)]  
  ##print(cols)
  if(v.lab){
    v.lab <- V(g)$label
  }
  else{
    v.lab <- NA
  }
  gparm <- list(g=g, layout=crd1,vertex.color=cols,edge.arrow.size=0.4,
vertex.label=v.lab,vertex.frame.color=cols,vertex.size=v.size, 
edge.width=e.size,edge.curved=e.curve, bg=bg, edge.color=e.col)
  class(gparm) <- "netbiov"
  if(tkplot){
    tkplot.netbiov(gparm)
  }
  else{
    plot.netbiov(gparm)

  }
  #dg1[order(dg1[,1]),]  
  #cols[rownames(d)]
  gparm
      
}
.get.spiral <- function(g, a=1, b = 1, tp = 1, rank.function=NULL, skip=skip){

    
    

        k <- vcount(g)+ skip
        x <- c()
        y <- c()
  tpx <- c()
        theta = 1
        for(i in 1:k ){
    ##print((a^(1/2)*((pi*theta)/180)^(1/2)))
                x <- c(x,(a^(1/2)*((pi*theta)/180)^(1/2))*cos((pi*theta)/180))
                y <- c(y,(a^(1/2)*((pi*theta)/180)^(1/2))*sin((pi*theta)/180))
    ##print(tp)
                #if(theta==360){
                #        theta = 0
                #}
                theta=theta + tp
        }
  #print(tpx)
      m <- cbind(x,y)
  m <- m[c((skip+1):nrow(m)),]
  colnames(m) <- c()
  
  if(!is.null(rank.function)&& class(rank.function)=="function")  {
  #print("fff")
    rnkx <- .get_rank_spiral(g, rank.function=rank.function)
    m <- m[rnkx,] 

  }
      rownames(m) <- V(g)$name
  m
  
}
.get_rank_spiral <- function(g, rank.function=NULL){
  crd <- rank.function(g)
#crd <- layout.reingold.tilford(g, circular=T)

  d <- rank(crd[,1])
  #names(d) <- V(g)$name
  d
  #print(names(sort(d)))
  #names(rev(sort(d)))

}

###########################################################
.getcol.spiral <- function(lst){
  col <- c()
  for(i in 1:length(lst)){
    col <- rbind(col,lst[[i]][,c(1,5)])  
  }
  col
}

############################################################
.set.rank.spiral <- function(d, color.input){
  col <- c(min(d), min(d)+1, min(d)+2, max(d)+min(d)+3)
  if(length(d)>2){
    if(max(d)==0){
      dtemp <- rep(1,length(d))
      names(dtemp) <- names(d)
      d <- dtemp
    }
    ##print((d))
          col <- hist(d,breaks=round(max(abs(d))), plot=FALSE)$breaks
          col[which.max(col)] <- col[which.max(col)] + 1
  }
  
  grtemp <- colors()[grep("gray",colors())]
  grtemp <- c(grtemp,colors()[grep("grey",colors())])
  coltemp <- setdiff(colors(), grtemp)
  
  if(is.null(color.input)){
          colcode <- rev(heat.colors(length(col), alpha = 1))
  }
  if(class(color.input)=="logical"){
    if(color.input){
      colcode <- sample(coltemp, length(col)) 
      if(length(colcode)<length(col)){
        nn <- length(col) - length(colcode)
        colcode <- append(rep(colcode[1],nn),colcode)
      }
    }
    else{
            colcode <- rev(heat.colors(length(col), alpha = 1))
    }
  }
  if(class(color.input)=="character"){
    colcode <- rep(color.input, length(col))
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
  colid
  
}

#g <- barabasi.game(600)
#get.coord(g)
#n = 200
#g <- watts.strogatz.game(1, n, 5, 0.05)
#g <-as.directed(g, mode = "arbitrary")
#g <- simplify(g) 
#n=2
#g <- erdos.renyi.game(n, 2/100)
#n = 100
#g <- barabasi.game(n, m=1)








