# Finds the shortest path through the connected network using Djikstra's algorithm
# Output is the shortest path and the shortest distance

djikstra <- function(edgeMatrix, coordMatrix, initialNode, endNode, nNodes){
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
      unvisitedNodes$TentDistance[idx] <- unvisitedNodes$CurrentValue[curridx] + euclidDistance(coordMatrix[unvisitedNodes$Node[curridx],1],coordMatrix[unvisitedNodes$Node[curridx],2], coordMatrix[neighborNodes[i],1],coordMatrix[neighborNodes[i],2])

      if (length(idx) != 0){
        unvisitedNodes$CurrentValue[idx] <- min(unvisitedNodes$CurrentValue[idx], unvisitedNodes$TentDistance[idx], na.rm = TRUE)
      }
    }
    # remove current from unvisited set

    removalNode <- which(unvisitedNodes$Status == "Current")
    visitedNodes <- rbind(visitedNodes, unvisitedNodes[removalNode, 1:3])
    unvisitedNodes <- unvisitedNodes[-removalNode,]

    if(dim(unvisitedNodes)[1] == 0){
      break
    }else{
      newCurrent <- unvisitedNodes$Node[which(unvisitedNodes$CurrentValue == min(unvisitedNodes$CurrentValue, na.rm = TRUE))]
      unvisitedNodes$Status[which(unvisitedNodes$Node == newCurrent)] <- "Current"
    }
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
    possibleParents <- which(edgeMatrix[,child] == 1)
    containedParents <- possibleParents[(possibleParents %in% visitedNodes$Node)]

    if (length(containedParents) == 1){
      child <- containedParents
      visitedNodes$directParent[which(visitedNodes$Node == child)] <- 1
    } else{
      childToParent <- matrix(0, nrow = length(containedParents), ncol = 1)
      for(i in 1:length(containedParents)){
        childToParent[i] <- euclidDistance(coordMatrix[child,1],coordMatrix[child,2],coordMatrix[containedParents[i],1],coordMatrix[containedParents[i],2])
      }
      childParentComp <- visitedNodes$CurrentValue[which(visitedNodes$Node == child)] - childToParent
      auxiliary <- order(visitedNodes$Node[containedParents])
      currentParentsValues <- (visitedNodes$CurrentValue[containedParents])[auxiliary]
      childParentDeviation <- childParentComp - currentParentsValues
      lowestDeviation <- which(childParentDeviation == min(abs(childParentDeviation)))
      parentNew <- containedParents[lowestDeviation]
      visitedNodes$directParent[which(visitedNodes$Node == parentNew)] <- 1
      child <- parentNew
    }
  }
  djikstraPath <- visitedNodes$Node[which(visitedNodes$directParent == 1)]

  result <- list(djikstraPath, visitedNodes[1,3])
  result
}
