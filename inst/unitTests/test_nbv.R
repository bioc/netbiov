test_output_equal <- function() {
    data("PPI_Athalina")
    checkEquals(class(g1), "igraph")
   
    xx <- plot.modules(g1)
    checkEqualsNumeric(degree(xx$g), degree(g1))   		
    
    xx <- mst.plot(g1)
    checkIdentical(degree(xx$g), degree(g1))


    xx <- mst.plot.mod(g1)
    checkIdentical(degree(xx$g), degree(g1))

   		
    xx <- splitg.mst(g1)
    checkIdentical(degree(xx$g), degree(g1))

    
    xx <- plot.spiral.graph(g1)
    checkEqualsNumeric(degree(xx$g), degree(g1))   		

   
    xx <- plot.NetworkSperical(g1)
    checkIdentical(degree(xx$g), degree(g1))
   

    xx <- plot.NetworkSperical.startSet(g1)
    checkIdentical(degree(xx$g), degree(g1))

   
    xx <- plot.abstract.module(g1)
    checkEquals(vcount(xx$g), vcount(g1))
}
