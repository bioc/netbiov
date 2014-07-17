plot.NetworkSperical <- function(x, mo="in", tkplot = FALSE, v.lab=FALSE,
v.size=1, bg="black", ...){
   
  if(is.null(x)){
    stop("please input a valid igraph object")
  }
  if(!is.igraph(x)){
    stop("please input a valid igraph object")
  }
  gtemp <- g <- x
  rm(x) 
  n <- vcount(g)
        adj <- get.adjacency(g)
  coord <- matrix(0,nrow = n, ncol = 4)
        if(mo == "in"){
          d <- degree(g, V(g), mode = "in")
        }else{
          d <- degree(g, V(g), mode = "out")
        }
  names(d) <- c(1:(n))
  d1 <- sort(d,decreasing=TRUE)
        nset <- new.env()
  

        ind <- which(max(d) == d)[1] # center node
        #ind <- 1                 # or choose a node sa center of the view
        
        ind_nbs <-which(adj[,ind] == 1)

          #neighbors(g, V(g)[ind-1], mode = "in") + 1
        assign(as.character(ind), ind_nbs, envir = nset)
  
        L <- length(ind_nbs)
  

        if(L > 1){

          nVol <- vector(mode = "numeric", length = L)
          for(j in 1:L){
            if(mo == "in"){
            nVol[j]<-neighborhood.size(g,order=n,nodes=(ind_nbs[j]),mode="in")
            }else{
            nVol[j]<-neighborhood.size(g,order=n,nodes=(ind_nbs[j]),mode="out")
            }
          }
          nVolrankind <- order(nVol, decreasing = TRUE)
          nVolrank <- sort(nVol, decreasing = TRUE)
          SV <- sum(nVol)/4
          Ls <- floor(L/4)
          Lf <- L - 3*Ls
          ind_nbsnew <- vector(mode = "numeric", length = L)
          nVolnew <- vector(mode = "numeric", length = L)
          
        
          cc <- 1
          ind_nbsnew[cc] <- nVolrankind[1]
          nVolnew[cc] <- nVolrank[1]
          cc <- cc + 1
          ccb <- 2
          cce <- L
          #print(cce)
          outp<-.orderNeighbors(SV,ind_nbsnew,nVolnew,nVolrank,
    nVolrankind,(Ls),cc,ccb,cce)
          ind_nbsnew <- get("1", envir = outp)
          nVolnew <- get("2", envir = outp)
          cc <- get("3", envir = outp)
          ccb <- get("4", envir = outp)
          cce <- get("5", envir = outp)
          SVadd <- sum(nVolnew)
          
          ind_nbsnew[cc] <- nVolrankind[ccb]
          nVolnew[cc] <- nVolrank[ccb]
          cc <- cc + 1
          ccb <- ccb + 1
          #print(cce)
          outp <- .orderNeighbors(SV + SVadd, ind_nbsnew, nVolnew, nVolrank,
   nVolrankind, (Ls), cc, ccb, cce)
          ind_nbsnew <- get("1", envir = outp)
          nVolnew <- get("2", envir = outp)
          cc <- get("3", envir = outp)
          ccb <- get("4", envir = outp)
          cce <- get("5", envir = outp)
          SVadd <- sum(nVolnew)
          
          ind_nbsnew[cc] <- nVolrankind[ccb]
          nVolnew[cc] <- nVolrank[ccb]
          cc <- cc + 1
          ccb <- ccb + 1
          #print(cce)
          outp <- .orderNeighbors(SV + SVadd, ind_nbsnew, nVolnew, nVolrank, 
   nVolrankind, (Ls), cc, ccb, cce)
          ind_nbsnew <- get("1", envir = outp)
          nVolnew <- get("2", envir = outp)
          cc <- get("3", envir = outp)
          ccb <- get("4", envir = outp)
          cce <- get("5", envir = outp)
          SVadd <- sum(nVolnew)
          
          ind_nbsnew[cc] <- nVolrankind[ccb]
          nVolnew[cc] <- nVolrank[ccb]
          cc <- cc + 1
          ccb <- ccb + 1
          #print(cce)
          outp <- .orderNeighbors(SV + SVadd, ind_nbsnew, nVolnew, 
   nVolrank, nVolrankind, (Lf), cc, ccb, cce)
          ind_nbsnew <- get("1", envir = outp)
          nVolnew <- get("2", envir = outp)
          cc <- get("3", envir = outp)
          ccb <- get("4", envir = outp)
          cce <- get("5", envir = outp)
          SVadd <- sum(nVolnew)
          

          ind_nbs <- ind_nbs[ind_nbsnew]

        }
        
        angle_pn <- 360/L
        
        a1 <- 0.0
        a2 <- 0.0
        coord[ind,] <- c(0,0,1,1)
     #print(n)

        if(mo == "in"){
          adj[ind,] <- vector(mode = "numeric", length = n)
        }else{
          adj[,ind] <- vector(mode = "numeric", length = n)
        }
        r1 <- 100
        theta <- 0.0
        flag <- 1
        for(j in 1:L){
          if( (-1)^j > 0){
            scaleR <- 1.0
          }else{
            scaleR <- 0.9
          }
          a1=scaleR*r1*cos((pi*theta)/180) 
          a2=scaleR*r1*sin((pi*theta)/180)
          theta <- theta + angle_pn
          if(mo == "in"){
            aux <- neighborhood.size(g, order = n, nodes = (ind_nbs[j]),
    mode= "in")
          }else{
            aux <- neighborhood.size(g, order = n, nodes = (ind_nbs[j]),
   mode= "out")
          }
          if(flag == 1 & aux == 1){
            indcol <- j + 1
            flag <- 0
          }
       #print(n)

          if(aux != 1){
            coord[ind_nbs[j],] <- c(a1,a2,1,j+1)    # 4th coord = color
            if(mo == "in"){
              adj[ind_nbs[j],] <- vector(mode = "numeric", length = n)
            }else{
              adj[,ind_nbs[j]] <- vector(mode = "numeric", length = n)
            }
            
          }else{
            coord[ind_nbs[j],] <- c(a1,a2,1,indcol)
            if(mo == "in"){
              adj[ind_nbs[j],] <- vector(mode = "numeric", length = n)
            }else{
              adj[,ind_nbs[j]] <- vector(mode = "numeric", length = n)
            }
          }
        }

        

        #print(sum(coord[,3]))
        flag <- 1
        cc <- vector(mode ="numeric", length = 30)
        cc[1] <- sum(coord[,3])
        k <- 2
        while(flag){
          outp <- .auxinnersphere(nset,n=n, coord, adj, k, mo)
          nset <- get("1", envir = outp)
          coord <- get("2", envir = outp)
          adj <- get("3", envir = outp)
          cc[k] <- sum(coord[,3])
          #print(c(k, cc[k]))
          if((cc[k] > n | cc[k] == cc[k-1]) & k > 1){
            flag <- 0
          }
          k <- k + 1
        }


        # remove nodes and their edges that have not been reached so far
        adj2 <- get.adjacency(g)
        g <- graph.adjacency(adj2 - adj)

        indaux <- which(coord[,3] == 0)
        coord[indaux,4] <- 1

        
        cind <- sample(1:n, n, replace = FALSE)
        col_rand <- colors()[cind]
        ind <- regexpr("gray", col_rand)
        ind2 <- which(ind == -1)
        col_rand <- col_rand[ind2]
        ind <- regexpr("grey", col_rand)
        ind2 <- which(ind == -1)
        col_rand <- col_rand[ind2]
        ind <- regexpr("white", col_rand)
        ind2 <- which(ind == -1)
        col_rand <- col_rand[ind2]
        ind <- regexpr("light", col_rand)
        ind2 <- which(ind == -1)
        col_rand <- col_rand[ind2]
        ind <- regexpr("1", col_rand)
        ind2 <- which(ind == -1)
        col_rand <- col_rand[ind2]
        #ind <- regexpr("2", col_rand)
        #ind2 <- which(ind == -1)
        #col_rand <- col_rand[ind2]
        ind <- regexpr("3", col_rand)
        ind2 <- which(ind == -1)
        col_rand <- col_rand[ind2]
        ind <- regexpr("4", col_rand)
        ind2 <- which(ind == -1)
        col_rand <- col_rand[ind2]
        
        
        palette(col_rand)
        col_nodes <- palette()[coord[,4]]
        #col_nodes <- colors()[coord[,4]] 
  if(v.lab){
    v.lab <- V(gtemp)$label
  }  
  else{v.lab = NA}
  gparm <- list(g = gtemp,layout = coord[,1:2], vertex.size = v.size, 
edge.arrow.size=0.4,vertex.color = col_nodes, vertex.frame.color = col_nodes,
vertex.label=v.lab, bg=bg, edge.label=NA) 
        class(gparm) <- "netbiov"
        if(tkplot){
                tkplot.netbiov(gparm)
        }
        else{
                plot.netbiov(gparm)
        }
  gparm
        
}       
        

  

############## Function to plot network #############

.auxinnersphere <- function(nset,n, coord, adj, Iter, mo){

  L1 <- length(ls(nset))
  nset2 <- new.env()
  ind_nset <- as.numeric(ls(nset))
  for(j in 1:L1){
    ind_cnodes <- get(as.character(ind_nset[j]), envir = nset)
    L2 <- length(ind_cnodes)
    
    for(k1 in 1:L2){
      cnode <- ind_cnodes[k1]
      ind2_nbs <- which(adj[,cnode] == 1)
      L <- length(ind2_nbs)
      if(L > 0){
        assign(as.character(cnode), ind2_nbs, envir = nset2)
   #print(cnode)
  #print("#####")
   #print(length(cnode))
   #print(n)
        if(mo == "in"){
          adj[cnode,] <- vector(mode = "numeric", length = n)
       
        }else{
          adj[,cnode] <- vector(mode = "numeric", length = n)
        }
        a1 <- coord[cnode,1]
        a2 <- coord[cnode,2]
        r2 <- 100
        if(a1 > 0 & a2 > 0){
          theta_start <- 180*atan( a2/a1 )/pi
        }
        if(a1 < 0 & a2 > 0){
          theta_start <- 180 - 180*atan( a2/abs(a1) )/pi
        }
        if( a1 > 0 & a2 < 0){
          theta_start <- 360 - 180*atan( abs(a2)/a1 )/pi
        }
        if( a1 < 0 & a2 < 0){
          theta_start <- 180 + 180*atan( abs(a2)/abs(a1) )/pi
        }
        if( a1 == 0 & a2 > 0){
          theta_start <- 90
        }
        if( a1 == 0 & a2 < 0){
          theta_start <- 270
        }
        if( a1 > 0 & a2 == 0){
          theta_start <- 0
        }
        if( a1 < 0 & a2 == 0){
          theta_start <- 180
        }

        b <- 10*pi                  # segment per node
        #angle_pn <- 7/(Iter*0.3)
        r <- sqrt(a1^2 + a2^2) + r2
        angle_pn <- b*180/(r*pi)
        for(k in 1:L){
          theta <- theta_start + angle_pn*((k-1)/L)  #*(Iter*0.6)
          c1 <- r*cos((pi*theta)/180) 
          c2 <- r*sin((pi*theta)/180)
          col_cnode <- coord[cnode,4]
          coord[ind2_nbs[k],] <- c(c1,c2,1,col_cnode)
      #print(n)

          if(mo == "in"){
            adj[ind2_nbs[k],] <- vector(mode = "numeric", length = n)
          }else{
            adj[,ind2_nbs[k]] <- vector(mode = "numeric", length = n)
          }
        }
      }
    }
  }

  outp <- new.env()
  assign("1", nset2, envir = outp)
  assign("2", coord, envir = outp)
  assign("3", adj, envir = outp)

  return(outp)

}



.orderNeighbors <- function(SV, ind_nbsnew, nVolnew, nVolrank, nVolrankind,
Ls, cc, ccb, cce=0){
 if(cce!=0){
  for(j in 1:Ls){
    aux <- sum(nVolnew) + nVolrank[ccb]
    if(aux >= SV){
      nVolnew[cc] <- nVolrank[cce]
      ind_nbsnew[cc] <- nVolrankind[cce]
      cce <- cce - 1
      cc <- cc + 1
    }else{
      nVolnew[cc] <- nVolrank[ccb]
      ind_nbsnew[cc] <- nVolrankind[ccb]
      ccb <- ccb + 1
      cc <- cc + 1
    }
  }
}

  outp <- new.env()
  assign("1", ind_nbsnew, envir = outp)
  assign("2", nVolnew, envir = outp)
  assign("3", cc, envir = outp)
  assign("4", ccb, envir = outp)
  assign("5", cce, envir = outp)
  
  return(outp)

}
