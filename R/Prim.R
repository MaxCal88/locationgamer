### Initialize points
A <- randomCoordinates(5,100,0,100,0)
B <- createDistance(A)
distMatrix <- B
C <-primDistance(B)
coordMatrix <- A
shortestPath <-C
plotPrim(C, A)

# Setting up the edgematrix
nNodes <- 5
initialNode <- 3
endNode <- 4


newRes <- djikstra(edgeMatrix1, coordMatrix, initialNode, endNode, nNodes)
djikstraPath <- newRes[[1]]
plotNetwork(edgeMatrix1, coordMatrix)
plotDjikstra(edgeMatrix1, coordMatrix, djikstraPath)

# matrices for testing
edgeMatrix1 <- matrix(0, nrow = 5, ncol = 5)
edgeMatrix1[,1] <- c(0,1,0,0,0)
edgeMatrix1[,2] <- c(1,0,1,1,1)
edgeMatrix1[,3] <- c(0,1,0,0,0)
edgeMatrix1[,4] <- c(0,1,0,0,0)
edgeMatrix1[,5] <- c(0,1,0,0,0)
newRes1 <- djikstra(edgeMatrix1, coordMatrix, initialNode, endNode, nNodes)
djikstraPath1 <- newRes1[[1]]
plotNetwork(edgeMatrix1, coordMatrix)
plotDjikstra(edgeMatrix1, coordMatrix, djikstraPath1)


edgeMatrix2 <- matrix(0, nrow = 5, ncol = 5)
edgeMatrix2[,1] <- c(0,1,0,0,0)
edgeMatrix2[,2] <- c(1,0,1,1,1)
edgeMatrix2[,3] <- c(0,1,0,0,1)
edgeMatrix2[,4] <- c(0,1,0,0,1)
edgeMatrix2[,5] <- c(0,1,1,1,0)
newRes2 <- djikstra(edgeMatrix2, coordMatrix, initialNode, endNode, nNodes)
djikstraPath2 <- newRes2[[1]]
plotNetwork(edgeMatrix2, coordMatrix)
plotDjikstra(edgeMatrix2, coordMatrix, djikstraPath2)

edgeMatrix3 <- matrix(0, nrow = 5, ncol = 5)
edgeMatrix3[,1] <- c(0,1,1,1,0)
edgeMatrix3[,2] <- c(1,0,0,0,1)
edgeMatrix3[,3] <- c(1,0,0,0,1)
edgeMatrix3[,4] <- c(1,0,0,0,1)
edgeMatrix3[,5] <- c(0,1,1,1,0)
newRes3 <- djikstra(edgeMatrix3, coordMatrix, initialNode, endNode, nNodes)
djikstraPath3 <- newRes3[[1]]
plotNetwork(edgeMatrix3, coordMatrix)
plotDjikstra(edgeMatrix3, coordMatrix, djikstraPath3)


edgeMatrix4 <- matrix(0, nrow = 5, ncol = 5)
edgeMatrix4[,1] <- c(0,1,0,0,0)
edgeMatrix4[,2] <- c(1,0,0,0,0)
edgeMatrix4[,3] <- c(0,0,0,0,1)
edgeMatrix4[,4] <- c(0,0,0,0,1)
edgeMatrix4[,5] <- c(0,0,1,1,0)
newRes4 <- djikstra(edgeMatrix4, coordMatrix, initialNode, endNode, nNodes)
djikstraPath4 <- newRes4[[1]]
plotNetwork(edgeMatrix4, coordMatrix)
plotDjikstra(edgeMatrix4, coordMatrix, djikstraPath4)
