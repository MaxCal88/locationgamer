randomCoordinates <- function(nNodes, xMax, xMin, yMax, yMin){
  nodeCoordinates <- as.data.frame(matrix(0, ncol = 2, nrow = nNodes))
  colnames(nodeCoordinates) <- c("xCoordinate", "yCoordinate")
  for (i in 1:nNodes){
    nodeCoordinates[i,1] <- sample(x = seq(from = xMin, to = xMax, by = 1), size = 1, replace = TRUE)
    nodeCoordinates[i,2] <- sample(x = seq(from = yMin, to = yMax, by = 1), size = 1, replace = TRUE)
  }
  nodeCoordinates
}
