# Plots the shortest path on the provided network
# Requires the edgematrix (0 if not connected, 1 if connected, and the coordinate matrix)

plotDjikstra <- function(edgeMatrix, coordMatrix, djikstraPath){
  minAx <- min(union(coordMatrix[,1], coordMatrix[,2]))
  maxAx <- max(union(coordMatrix[,1], coordMatrix[,2]))
  par(pty = "s")
  plot(coordMatrix[,1], coordMatrix[,2], xlim = c(minAx,maxAx), ylim = c(minAx,maxAx), xlab = "X", ylab = "Y")
  
  for (i in 1:dim(edgeMatrix)[1]){
    for (j in 1: dim(edgeMatrix)[2]){
      if(edgeMatrix[i,j] == 1){
        segments(coordMatrix[i,1], coordMatrix[i,2], coordMatrix[j,1], coordMatrix[j,2] )
      }
    }
  }
  
  for (i in 1:(length(djikstraPath)-1)){
    segments(coordMatrix[djikstraPath[i],1],coordMatrix[djikstraPath[i],2],coordMatrix[djikstraPath[i+1],1],coordMatrix[djikstraPath[i+1],2], col = 'red')
  }
  
}
