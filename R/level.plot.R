
  
    level.plot <- function(x, layout.function=NULL, type=1, initial_nodes=NULL,
    init_nodes=0, order_degree = "in", plotsteps = FALSE, saveplots=FALSE, 
    dirname=NULL, vertex.colors=NULL, edge.col=NULL, tkplot=FALSE, nodeset=NULL,
    path.col="green", col.s1="red", col.s2="yellow", nodes.on.path=TRUE,
    v.size=2, e.size=.5, v.lab=FALSE, bg="black", v.lab.cex=0.5,
    v.lab.col="skyblue",sf=4, e.path.width=1,e.curve=.5, level.spread=FALSE){
  if(is.null(x)){
    stop("please input a valid igraph object")
  }
  if(!is.igraph(x)){
    stop("please input a valid igraph object")
  }
  g <- x
  rm(x)
  if(!(is.connected(g))){
    if(is.directed(g)){    
      g <- .process_graph1(g,k=initial_nodes)
    }
    else{
      g <- .process_graph1(g,k=initial_nodes)
      g <- simplify(as.undirected(g))      
    }
    
  }
  if((length(V(g)$label)<vcount(g))&&is.null(V(g)$name)){
    V(g)$label <- c(1:(vcount(g)))
  }
  else{
    if(length(V(g)$label)<vcount(g)){
      V(g)$label <- c(1:(vcount(g)))
    }
  }
  if((is.null(vertex.colors))||(length(vertex.colors)<3)){
    colcode= c(500,371,62)
  }
  else{
    if(is.character(vertex.colors)){
      #colcode <- match(vertex.colors[1:3], colors())
      colcode <- c(1:3)
    }
  }

  if(class(v.size)=="logical"){
                v.s=2
        }
        if(class(v.size)=="numeric" && (length(v.size)<vcount(g))){
                v.s=v.size[1]
        }
        else{
               v.s= (rank(v.size)/max(rank(v.size)))*sf
               v.s[which(v.s<.5)] <- .5
        }       

  n = vcount(g)
    k <-c()
  if(length(initial_nodes)== 0 && init_nodes == 0){
    k<- sample(n,size=3,replace=FALSE)
  }

  if((length(initial_nodes)!= 0 && init_nodes == 0)||
  (length(initial_nodes)!= 0 && init_nodes != 0)){
    for(i in 1:length(initial_nodes))  
      k<- append(k,(which(V(g)$label==initial_nodes[i])))
      
  }

  if(length(initial_nodes)== 0 && init_nodes != 0){
    k<-sample(n,size=init_nodes,replace=FALSE)
    
  }
  if(plotsteps)
  {
      if(saveplots){  
    if(length(dirname)==0){  
      dirname=getwd()
      #print(dirname)
      #print("*******")
      dirname=paste(dirname,"/plot_steps_dir",sep="")
      #print(dirname)
    }
    if(!file.exists(dirname)){
      dir.create(dirname)
    }
      }        
  }
  #print(paste("initial nodes =>", toString(k)))
  mat <- .getalllevels(g,k,plotsteps=plotsteps,dirname=dirname,
  e.curve=e.curve,level.spread=level.spread)
  #print("hello")
  #print(head(mat))	
  minv <- min(mat[,2])
  maxv <- max(mat[,2])
  mat1 <-c()
  
  for(i in minv:maxv){
    if(i==0)
    col=colcode[1]
    if(i>0)
    col=colcode[2]
    if(i<0)
    col=colcode[3]

    lst <- which(mat[,2]==i)
    mat1 <- rbind(mat1,.gencoord(gtemp=g,mat[lst,1],i,col=col, 
    mod=order_degree))
    
  }
   
  mat2<-matrix(0,length(mat1[,1]),4)
  mat2[,1]<-as.numeric(rownames(mat1))
  mat2[,2]<-mat1[,1]
  mat2[,3]<-mat1[,2]
  mat2[,4]<-mat1[,3]
  if(nrow(mat2)>1){
                mat3<-mat2[order(mat2[,1]),]
        }
        else{
                mat3 <- mat2
        }
        cole <- c()
	#print(edge.col)
        if(length(E(g))> 0){
                cole<-.edgeCol(g,mat3,edge.col)
        }
	#print(cole)
  if(length(vertex.colors)<3){
          col1<-colors()[mat3[,4]]
  }
  else{
    col1 <- vertex.colors[mat3[,4]]
  }
  cre = NA
  cre <- .curve.level(mat3[,2:3], g,e.curve=e.curve)
  e.width <- rep(e.size, ecount(g))
  if(!is.null(nodeset)){
    s1 <- c()
    s2 <- c()
    if(class(nodeset)=="list"){
      if(length(nodeset)==2){  
        s1 <- as.numeric(nodeset[[1]])
        s2 <- as.numeric(nodeset[[2]])

      }
      else{
        s1 <- as.numeric(nodeset[[1]])
        s2 <- setdiff(c(1:(vcount(g))),s1)
      }
    }
    if(class(nodeset)=="numeric"){
      s1 <- nodeset
      s2 <- setdiff(c(1:(vcount(g))),s1)
    }
    path.colx <- .get.paths.nodes_lev(s1, s2, g)
    path.colx1 <- path.colx[[2]]
    path.colx <- path.colx[[1]]

    if((nodes.on.path==TRUE) || (class(nodes.on.path)=="character"))
    {
      if(class(nodes.on.path)=="logical"){
        col1[(path.colx1)] <- "purple"
      }
      else{  col1[(path.colx1)] <- nodes.on.path }
    } 
    cole[path.colx] <- path.col
    #print(path.colx)
    e.width[path.colx] <- e.path.width
    col1[(s1)] <- col.s1
    col1[(s2)] <- col.s2
    
  }
  vlab=""
  if(v.lab){
    vlab=V(g)$label
  }

#############################
  if(!is.null(layout.function)&& (class(layout.function)=="function")){
      tmp_crdx <- layout.function(g)
      mat3[,2] <- tmp_crdx[,type]
      rm(tmp_crdx)
  }
  if(level.spread){
    mat3[,2:3] <- .getcrdlev(mat3[,2:3])
  }



  #rownames(mat3) <- V(g)$label
  gparm <- list(g= g, layout = mat3[,2:3], vertex.color=col1, 
  edge.color=cole,vertex.size=v.s,edge.arrow.size=0.3, 
  vertex.label.cex= v.lab.cex, vertex.label=vlab, 
  lab.color=v.lab.col,lab.dist=0, vertex.frame.color=col1, 
  edge.width=e.width, bg=bg, edge.curved=cre)
  class(gparm) <- "netbiov"


#############################          
        if(length(E(g))> 0){
    if(tkplot){
                  tkplot.netbiov(gparm)
    }
    else{
      plot.netbiov(gparm)
    }
       }
       else{
                if(vcount(g)>1)
                {
      if(tkplot){
                          tkplot.netbiov(gparm)
      }
      else{
        tkplot.netbiov(gparm)
      }
                }
                else{
                        print("graph consists a single node")
                }
        }
  ##print(head(E(g)))
  ##print(V(g)$label)
    
        gparm
}

########## Called by main function, generates levels of nodes of a graph ##################### 

.getalllevels<-function(g,k,plotsteps,dirname=dirname,e.curve=e.curve,
level.spread=FALSE){
 
          #par(mfrow=c(3,4),mar=c(0,0,0,0))
  n=vcount(g)
  ##print(plotsteps)
  ######
  gtemp <- g
  ####### 
  neighbors <- matrix(0,length(k),2)
  neighbors[,1] <- k
  neighbors[,2] <- rep(0,length(k)) 
  ###############
  filename=NULL
  if(plotsteps)
  {  
    if(length(dirname)!=0)
    {
      if(file.exists(dirname)){

        filename<-paste(dirname,"/plot_steps.pdf",sep="")
        pdf(filename)   
      }  

    }
  }
  
  k<-neighbors
  kprev<-k[,1]
  #par(mfrow=c(4,2))
  while(1){
    
    mattemp <- matrix(0,1,2)
    ktemp<-c()
    if(length(k[,1])>0){
      for(i in 1:length(k[,1]))
      {
        gn<-.getneighbors(g,k[i,1])
        g<-gn$g
    
        if(length(gn$outnode)>0){

          for(j in 1:length(gn$outnode)){
            temp<-setdiff(gn$outnode[j],kprev)
            if(length(temp)>0)
            {
             mattemp[1,1] <- gn$outnode[j]
             mattemp[1,2] <- k[i,2]+1
             neighbors <- rbind(neighbors, mattemp)
             kprev <-append(kprev,gn$outnode[j])
             ktemp<-rbind(ktemp,mattemp)
            }
          }
          
        }
  
        if(length(gn$innode)>0){

          for(j in 1:length(gn$innode)){
            temp<-setdiff(gn$innode[j],kprev)
            if(length(temp)>0)
            {
              mattemp[1,1] <- gn$innode[j]
              mattemp[1,2] <- k[i,2]-1
              neighbors <- rbind(neighbors, mattemp)
              kprev<-append(kprev,gn$innode[j])
              ktemp<-rbind(ktemp,mattemp)
              
            }
          }
        }    
      }  
      ktemp<-unique(ktemp)
      ############
      if(plotsteps){
        if(length(ktemp[,1])>0){
          minv <- min(neighbors[,2])
          maxv <- max(neighbors[,2])
          mat1 <-c()
          for(i in minv:maxv){
            if(i==0)
            col=500
            if(i>0)
            col=371
            if(i<0)
            col=62
            lst <- which(neighbors[,2]==i)
            mat1 <- rbind(mat1,.gencoord(gtemp=g,neighbors[lst,1],i,col=col, mod="in"))
    
          }
          
        .ploteachstep(mat1=mat1,m=ktemp[,1],
        gtemp=g,dirname=filename,e.curve=e.curve,
        level.spread=level.spread)
          
          
        }
      }
      ################
      k <- ktemp
      }  
      if(length(kprev)==n)
      {
        break
      }
        
    
  }
  if(plotsteps){
    if(!is.null(dirname)){
      dev.off()
    }
  }
  return(neighbors)
    
  
}    
      
    
  
  


##########################################################
########### Internal function, returns indegree nodes and outdegree nodes of a vector of nodes of a graph object ##################### 
.getneighbors <- function(g,nm, mod = 1,flag=0){
    #V(g)$name <- c(1:vcount(g))   
    k<-nm
    outnode <- c()
    outi <- c()
    outp <- c()
    innode <- c()
    ini <- c()
    

    for(i in 1:length(k))
    {
     #print("xx")
     #print(k[i]) 
     #print(as.vector(neighbors(g, V(g)[k[i]], mode = "out")))
      outnode <- as.vector(neighbors(g, k[i], mode = "out"))
      #print("xx")
      outi <- append(outi,outnode)
      if(length(outnode) > 0){
        for(j in 1:length(outnode)){
            #print(V(g))
            #print(k[i])
            #print(outnode[j])
          if(are.connected(g,k[i],outnode[j]))
          {  
            
            g <- delete.edges(g, E(g, P=c(k[i],outnode[j])))
        #	print("xx")    
	#g <- delete.edges(g, paste(V(g)$name[k[i]],"|",V(g)$name[outnode[j]],sep=""))
          }
        }
      }
    }
    #print("xx2")	
    for(i in 1:length(k))
    {
      
      innode <- as.vector(neighbors(g, V(g)[k[i]], mode = "in"))
      ini <- append(ini,innode)
      if(length(innode) > 0){
        for(j in 1:length(innode)){

          if(are.connected(g,k[i],innode[j]))
          {  
            g <- delete.edges(g, E(g, P=c(k[i],innode[j])))
            #g <- delete.edges(g, paste(V(g)$name[k[i]],"|",V(g)$name[innode[j]],sep=""))
          }
        }
      }
    }
    
    outnode=union(outi,outi)
    innode=union(ini,ini)
    return(list(g=g,outnode=outnode,innode=innode))
      
      
  }    
  


##################################


########## Internal function, used to color edges ###################
.edgeCol <-function(g,mat3,edge.col){  
  edgecol <-c()
  if(is.null(edge.col)||(length(edge.col)< 4)){
    ecr  <- 62
    ecg <-33
    ec1 <- 32
    ec2 <- 226
    edge.col <- colors()[c(ecr,ecg,ec1,ec2)]
  }
  
  if(is.character(edge.col)){
    if(length(edge.col)< 4){
      edge.col <- rep(edge.col,4)[1:4]
    }
    else{
      edge.col <- edge.col[1:4]
    }
    #ec <- match(edge.col, colors())
    #ecr <- ec[1];ecg <- ec[2];ec1 <- ec[3]; ec2 <- ec[4] 
    ecr <- 1; ecg=2; ec1 = 3; ec2 = 4
  }
  for(i in 1:ecount(g)){
    ##print(edgecol)
    ei <- ends(g,i,names=FALSE)
    e1 <- which(mat3[,1]==ei[1])
    e2 <- which(mat3[,1]==ei[2])
    if((length(e1)&&length(e2))>0){
    
      if(abs(mat3[e1,3] - mat3[e2,3])>1 && ((mat3[e1,3] - mat3[e2,3])>1))
      {
        #ec = ec+2
        edgecol <-append(edgecol, ecg)
      }
      if(abs(mat3[e1,3] - mat3[e2,3])>1 && ((mat3[e1,3] - mat3[e2,3])< (-1)))
      {
        #ec = ec+2
        edgecol <-append(edgecol, ecr)
      }
      if(abs(mat3[e1,3] - mat3[e2,3])==0)
      {
        #ec1 = ec1+2
        edgecol <-append(edgecol, ec1)
      }
      if(abs(mat3[e1,3] - mat3[e2,3])==1)
      {
        #ec2 = ec2+2
        edgecol <-append(edgecol, ec2)
      }
      #if(ec>654)
      #{
      #  ec=401
      #}
    }  

  }
  if(!is.na(edge.col[5])){
    edgecolxx <- rep(edge.col[5],length(edgecol))
  }
  else{
    edgecolxx <- rep("grey",length(edgecol))
  }
  edgecolxx[which(edgecol==1)] <- edge.col[1]
  edgecolxx[which(edgecol==2)] <- edge.col[2]
  edgecolxx[which(edgecol==3)] <- edge.col[3]
  edgecolxx[which(edgecol==4)] <- edge.col[4]
  #cole<-colors()[edgecol]
  return(edgecolxx)
}
###################################################

##### internal function, plot each step of a graph #############     

.ploteachstep <- function(mat1,m,gtemp,dirname=NULL,e.curve=0, level.spread=FALSE){
  vec <-c()
  #print(dirname)
  mat2<-matrix(0,length(mat1[,1]),4)
  mat2[,1]<-as.numeric(rownames(mat1))
  mat2[,2]<-mat1[,1]
  mat2[,3]<-mat1[,2]
  mat2[,4]<-mat1[,3]
  mat3<-mat2[order(mat2[,1]),]
  g <- gtemp
  g <-subgraph(g,mat3[,1])
  mat4<-matrix(0,length(mat3[,1]),5)
  mat4[,1]<-c(1:(length(mat3[,1])))
  mat4[,2]<-mat3[,2]
  mat4[,3]<-mat3[,3]
  mat4[,4]<-mat3[,4]
  mat4[,5]<-mat3[,1]
  for(i in 1:length(m)){
    vec <- append(vec,which(mat4[,5]==m[i]))}
  E(g)$color<-"lightgrey"
  V(g)$color<-"grey"
  V(g)[mat4[vec,1]]$color<-"red"
  E(g)[mat4[vec,1]%--%mat4[,1]]$color<-"blue"  
  #tkplot(g,layout=mat4[,2:3],vertex.size=12,edge.arrow.size=0.7, vertex.label=mat4[,5])
  #tkpid<-tkplot(g,layout=mat4[,2:3],vertex.size=12,edge.arrow.size=0.7)
  ##print(head(mat4))
  ##print("####")
  ##print(E(g))
  ##print(V(g)$label)
  crez <- .curve.level1(mat4[,2:3], g,e.curve)
  ##print(crez)
        if(level.spread){
                mat4[,2:3] <- .getcrdlev(mat4[,2:3])
        }
  
  if(length(dirname)==0){
    #tkplot(g)
    #par(mar=c(0,0,0,0))
    if(length(crez)==0)
    plot(g,layout=mat4[,2:3],vertex.size=3,edge.arrow.size=0.2,
    edge.curved=crez,vertex.label.cex=0.01)
  }
  
  if(length(dirname)!=0){
    #par(mar=c(0,0,0,0))
    #tkplot(g,layout=mat4[,2:3],vertex.size=4,edge.arrow.size=0.7)  
    plot(g,layout=mat4[,2:3],vertex.size=3,edge.arrow.size=0.2,
    vertex.label.dist=0.0, vertex.label.cex=0.01, edge.curved=crez)  
  }
}


##################################################

############## Internal function, Generate coordinates for nodes at a given level ########

.gencoord <- function(gtemp,k,level=0,mod =NULL,col=500){
    
    matt<-matrix(0,length(k),2)
    mat1<-c()
    if(length(k)>=2)
    {      
      mat<-matrix(0,1,3)
      if(!is.null(mod)){
        k<-sort(k)
        matt[,1] <- degree(gtemp,V(gtemp)[k], mode = mod)
      }  
      ##print(k)
      else{
        matt[,1] <- degree(gtemp,V(gtemp)[k], mode = "all")
      }
      matt[,2] <- k
      ##print(matt)
      if(!is.null(mod)){
        matt <- matt[order(matt[,1]),]
        k <- matt[,2]
      }
      
      
      if((length(k)%%2) != 0 ){
        ax <- ((length(k) - 1)/2)*(-1)    
        #ax <- ((round(length(k)/2))*(-1))
      }
      if((length(k)%%2) == 0 ){
        ax <- ((round(length(k)/2))*(-1))
      }
        
      for(i in 1:length(k))
      {
        mat[1,1] <- ax
        mat[1,2] <- level
        mat[1,3] <- col
        rownames(mat) <- k[i]  
        mat1 <- rbind(mat1,mat)
        ax = ax + 1
        if((ax == 0) && ((length(k)%%2) == 0))
        {
          ax = ax + 1
        }
      }
      return(mat1)      
     }

     if(length(k)==1)
     {
      mat1<-matrix(0,1,3)
      mat1[,2] <- level
      rownames(mat1)<-k[1]
      mat1[1,3] <- col
      return(mat1)
     }  
    
  }
##################################################

############ internal function, process a given graph object, 
###########if two or more subgraphs of a given graph do not 
###########intersect, then it decomposes graph in to many graphs
########## which do not intersect each other, and 
############ returns a largest node containing sub graph ########### 

.process_graph <- function(g){
  
  adj <- get.adjacency(g, names=FALSE)
  gname <- V(g)$name
  glabel <- V(g)$label

  g <- graph.adjacency(adj)

  if(length(gname)<=0)
  {
    V(g)$label <- c(1:(vcount(g)))
  }   
  else{V(g)$label <- gname}  
  if(length(V(g)$label)==vcount(g)){
    V(g)$label <- glabel
  }  

  #V(g)$label <- (seq(vcount(g)) -1)
  set_graph <- decompose.graph(g)
  max_node_graph <- which.max(sapply(set_graph, vcount))  
  return(set_graph[[max_node_graph]])

}
#################################################

.process_graph1 <- function(g,k){
  
  adj <- get.adjacency(g, names=FALSE)
  gname <- V(g)$name
  glabel <- V(g)$label
  
  id <- c()
  lab <-c()

  #g <- graph.adjacency(adj)

  if(length(gname)<=0 && length(glabel)< vcount(g))
  {
    V(g)$label <- c(1:(vcount(g)))
  }   
  #else{ V(g)$label<- c(0:(vcount(g)-1)) }  
  if(length(gname)==0){
    labelmat <- cbind(V(g)$label,V(g)$label)
  }
  else{
    labelmat <- cbind(V(g)$label,gname)
  }
  
  
  set_graph <- decompose.graph(g)
  
  if(length(k)>0){
  for(i in 1:length(set_graph)){

    for(j in 1:length(k)){
      tmp <- which(V(set_graph[[i]])$label==V(g)$label[k])
      if(length(tmp)>0)
      {
        id <- append(id,i)
      }
    }
  }
  id <- unique(id)
  pg <- graph.disjoint.union(set_graph[id])
  
  for(i in 1:length(id))
  {
    lab <- append(lab, V(set_graph[[id[i]]])$label)
  }
  
  
  #V(pg)$label <- lab
  #V(pg)$name <- labelmat[(lab+1),2]
  }
  else{
      max_node_graph <- which.max(sapply(set_graph, vcount))
      pg <- set_graph[[max_node_graph]]
      #V(pg)$name <- labelmat[(V(pg)$label+1),2]  
  }
    
  
  return(pg)

}
.process_graphxx <- function(g,k){
    adj <- get.adjacency(g, names=FALSE)
    gname <- V(g)$name
  
    id <- c()
    lab <-c()

    g <- graph.adjacency(adj)

    if(length(gname)<=0)
    {
      V(g)$label <- c(1:(vcount(g)))
    }   
    else{ V(g)$label<- c(1:(vcount(g))) }  
    if(length(gname)==0){
      labelmat <- cbind(V(g)$label,V(g)$label)
    }
    else{
      labelmat <- cbind(V(g)$label,gname)
    }
  
  
    set_graph <- decompose.graph(g)
  
    if(length(k)>0){
    for(i in 1:length(set_graph)){

      for(j in 1:length(k)){
        tmp <- which(V(set_graph[[i]])$label==k[j])
        if(length(tmp)>0)
        {
          id <- append(id,i)
        }
      }
    }
    id <- unique(id)
    pg <- graph.disjoint.union(set_graph[id])
  
    for(i in 1:length(id))
    {
      lab <- append(lab, V(set_graph[[id[i]]])$label)
    }
  
  
      V(pg)$label <- lab
      V(pg)$name <- labelmat[(lab+1),2]
    }
    else{
        max_node_graph <- which.max(sapply(set_graph, vcount))
        pg <- set_graph[[max_node_graph]]
        V(pg)$name <- labelmat[(V(pg)$label+1),2]  
     
    }
  
  return(pg)

}
.curve.level <- function(mat=NULL, g, e.curve=.5){
  if(is.null(V(g)$name)){
    V(g)$name <- paste("g",c(1:vcount(g)),sep="")
  }
  el <- get.edgelist(g)
  if(is.null(el[,1])||is.null(el[,2])){
    return(NULL)
  }
  else{
    if(is.null(mat)){
      return(rep(0,ecount(g)))
    }
    else{
      cre <- rep(0,ecount(g))
    #el <- get.edgelist(g)
      #if(!is.numeric(el[,1])){
        tmpm1 <- match(el[,1], V(g)$name)
        tmpm2 <- match(el[,2], V(g)$name)
        m1 <- mat[tmpm1,2]
        m2 <- mat[tmpm2,2]
        ##print(m1)
      #}
      #else{
      #  m1 <- mat[(el[,1]+1),2]
      #  m2 <- mat[(el[,2]+1),2]
      #}
      ##print(m)
      m <- m1 - m2
      ##print(head(m))
      if(length(which(abs(m)>0))>0){
        cre[which(m==0)] <- e.curve 
      }
      return(cre)
    }
  }
}
.curve.level1 <- function(mat=NULL, g, e.curve=.5){
  ##print(head(mat))
  V(g)$name <- c(1:vcount(g))
  #print(V(g)$name)
  el <- get.edgelist(g)
  if(is.na(el[1])||is.na(el[2])){
    return(NULL)
  }
  else{
    if(is.null(mat)){
      return(rep(0,ecount(g)))
    }
    else{
      cre <- rep(0,ecount(g))
    #el <- get.edgelist(g)
      ##print(head(el))
      m1 <- mat[(el[,1]),2]
      m2 <- mat[(el[,2]),2]
  
      m <- m1 - m2
      ##print(head(m))
      if(length(which(m>0))>0){
        cre[which(m==0)] <- e.curve
      }
      return(cre)
    }
  }
}
.get.paths.nodes_lev <- function(s1, s2, g){
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
.getcrdlev <- function(crd){
  ymn=0
  ymx=5
  y1 <- crd[,2]
  x1 <- crd[,1]
  crd.tmp <- matrix(0, ncol=2, nrow=nrow(crd))
  mn.x <- min(x1)
  mx.x <- max(x1)
  unq.y <- (sort(unique(y1)))
  if(nrow(crd)>2){
    for(i in 1:length(unq.y))
    {
      p <- which(y1==unq.y[i])
      if(length(p)>1){
        ##print(head(crd[p,]))
        #crdx <- (layout.circle(graph.empty(length(p))))  
        #tmp <- layout.norm(crdx, ymin=2-i, ymax=2+i, xmin=2-i, xmax=2+i)  
        #tmp <- layout.norm(crdx, ymin=ymn, ymax=ymx, xmin=mn.x, xmax=mx.x)  
        tmp <- layout.norm(crd[p,], ymin=crd[p[1],2], ymax=crd[p[1],2], xmin=mn.x, xmax=mx.x)  
        #tmp[,2] <- unq.y[i]
        ymn <- ymx +5
        ymx <- ymx +10
      }
      else{
        #crd[p,2] <- max(crd.tmp)+20
        tmp <- crd[p,]
      }
      crd.tmp[p,] <- tmp  
    }
  }
  crd.tmp
}  
