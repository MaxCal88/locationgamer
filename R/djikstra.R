# Finds the shortest path through the connected network using Djikstra's algorithm
# Output is the shortest path and the shortest distance

djikstra <- function(edgeMatrix, coordMat, initialNode, endNode, nNodes){
  unvisitedNodes <- as.data.frame(matrix(NA, nrow = nNodes, ncol = 4))
  colnames(unvisitedNodes) <- c('Node','Status', 'CurrentValue','TentDistance')
  unvisitedNodes$Node <- seq(from = 1, to = nNodes, by = 1)
  unvisitedNodes$Status[initialNode] <- "Current"
  unvisitedNodes$CurrentValue[initialNode] <- 0
  visitedNodes <- NULL
  prev <- matrix(0, nrow = dim(edgeMatrix)[1], ncol = dim(edgeMatrix)[2])
  
  
  while ((endNode %in% visitedNodes$Node) == FALSE){
    neighborNodes <- which(edgeMatrix[unvisitedNodes$Node[which(unvisitedNodes$Status == "Current")],] == 1 )
    prev[which(unvisitedNodes$Status == "Current"), neighborNodes] <- 1
    for (i in 1:length(neighborNodes)){
      idx = which(unvisitedNodes$Node == neighborNodes[i])
      curridx = which(unvisitedNodes$Status == "Current")
      unvisitedNodes$TentDistance[idx] <- unvisitedNodes$CurrentValue[curridx] + euclidDistance(coordMat[unvisitedNodes$Node[curridx],1],coordMat[unvisitedNodes$Node[curridx],2], coordMat[neighborNodes[i],1],coordMat[neighborNodes[i],2])
      
      if (length(idx) != 0){
        unvisitedNodes$CurrentValue[idx] <- min(unvisitedNodes$CurrentValue[idx], unvisitedNodes$TentDistance[idx], na.rm = TRUE)
      }
    }
    # remove current from unvisited set
    
    removalNode <- which(unvisitedNodes$Status == "Current")
    visitedNodes <- rbind(visitedNodes, unvisitedNodes[removalNode, 1:3])
    unvisitedNodes <- unvisitedNodes[-removalNode,]
    newCurrent <- unvisitedNodes$Node[which(unvisitedNodes$CurrentValue == min(unvisitedNodes$CurrentValue, na.rm = TRUE))]
    unvisitedNodes$Status[which(unvisitedNodes$Node == newCurrent)] <- "Current"
  }
  # get neighbors of current node
  
  shortestPath <- matrix(NA, nrow = (dim(visitedNodes)[1])+1, ncol = 1)
  shortestPath[1] <- endNode
  nVisited <- dim(visitedNodes)[1]
  visitedNodes <- visitedNodes[seq(from = nVisited, to = 1),]
  
  
  # Check whether previous nodes are parent to current node
  child <- visitedNodes$Node[1]
  visitedNodes$directParent <- 0
  visitedNodes$directParent[1] <- 1
  
  while (visitedNodes$directParent[which(visitedNodes$Node == initialNode)] == 0){
    directParent <- which(edgeMatrix[,child] == 1)
    distChildParent <- matrix(0, nrow = length(directParent), ncol = 2)
    if(length(directParent) == 1){
      directParent <- directParent
    }else{
      distChildParent[,1] <- directParent
      for (i in 1:length(directParent)){
        distChildParent[i,2] <- euclidDistance(coordMat[distChildParent[i,1],1],coordMat[distChildParent[i,1],2],coordMat[child,1],coordMat[child,2])
      }
      directParentVal <- intersect(visitedNodes$CurrentValue, visitedNodes$CurrentValue[which(visitedNodes$Node == child)] - distChildParent[,2])
      directParent <- visitedNodes$Node[which(visitedNodes$CurrentValue == directParentVal)]
    }
    child <- directParent
    visitedNodes$directParent[which(visitedNodes$Node == child)] <- 1
  }
   djikstraPath <- visitedNodes$Node[which(visitedNodes$directParent == 1)]
  
  result <- list(djikstraPath, visitedNodes[1,3])
  result
}
