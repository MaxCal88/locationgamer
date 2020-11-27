# Plots the minimum spanning tree derived by function primDistance
plotPrim <- function(shortestPath, coordMat){
  minAx <- min(union(coordMat[,1], coordMat[,2]))
  maxAx <- max(union(coordMat[,1], coordMat[,2]))
  par(pty = "s")
  plot(coordMat[,1], coordMat[,2], xlim = c(minAx,maxAx), ylim = c(minAx,maxAx), xlab = "X", ylab = "Y", main = "Prim - Minimum spanning tree")
  for (i in 1:dim(shortestPath)[1]){
    segments(coordMat[shortestPath[i,1],1],coordMat[shortestPath[i,1],2],coordMat[shortestPath[i,2],1],coordMat[shortestPath[i,2],2])
  }
}
