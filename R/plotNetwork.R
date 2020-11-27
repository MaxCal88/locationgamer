plotNetwork <- function(edgeMatrix, coordMatrix){
  minAx <- min(union(coordMatrix[,1], coordMatrix[,2]))
  maxAx <- max(union(coordMatrix[,1], coordMatrix[,2]))
  par(pty = "s")
  plot(coordMatrix[,1], coordMatrix[,2], xlim = c(minAx,maxAx), ylim = c(minAx,maxAx))
  for (i in 1:dim(edgeMatrix)[1]){
    for (j in 1:dim(edgeMatrix)[2]){
      if(edgeMatrix[i,j] == 1){
        segments(coordMatrix[i,1], coordMatrix[i,2], coordMatrix[j,1], coordMatrix[j,2])
      }
    }
  }
}
