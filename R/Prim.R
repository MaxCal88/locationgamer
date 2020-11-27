### Initialize points
A <- randomCoordinates(4,100,0,100,0)
B <- createDistance(A)
distMatrix <- B
C <-primDistance(B)
coordMatrix <- A
shortestPath <-C
plotPrim(C, A)

# Setting up the edgematrix
edgeMatrix <- matrix(0, nrow = 4, ncol = 4)

for (i in 1: (dim(edgeMatrix)[1]-1)){
  edgeMatrix[C[i,1], C[i,2]] <- 1
  edgeMatrix[C[i,2], C[i,1]] <- 1
}

nNodes <- 4
initialNode <- 1
endNode <- 4


newRes <- djikstra(edgeMatrix, coordMatrix, initialNode, endNode, nNodes)
djikstraPath <- newRes[[1]]


plotNetwork(edgeMatrix, coordMatrix)
plotDjikstra(edgeMatrix, coordMatrix, djikstraPath)

edgeMatrix[,1] <- c(0,1,1,1)
edgeMatrix[,2] <- c(1,0,1,1)
edgeMatrix[,3] <- c(1,1,0,1)
edgeMatrix[,4] <- c(1,1,1,0)
