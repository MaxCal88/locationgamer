# Finds the shortest path through the connected network using Djikstra's algorithm
# Output is the shortest path and the shortest distance

djikstra <- function(edgeMatrix, coordMatrix, initialNode, endNode, nNodes){
  unvisitedNodes <- as.data.frame(matrix(NA, nrow = nNodes, ncol = 4))
  colnames(unvisitedNodes) <- c('Node','Status', 'CurrentValue','TentDistance')
  unvisitedNodes$Node <- seq(from = 1, to = nNodes, by = 1)
  unvisitedNodes$Status[initialNode] <- "Current"
  unvisitedNodes$CurrentValue[initialNode] <- 0
  visitedNodes <- NULL
  djikstraPath <- 0

  while ((endNode %in% visitedNodes$Node) == FALSE){
    neighborNodes <- which(edgeMatrix[unvisitedNodes$Node[which(unvisitedNodes$Status == "Current")],] == 1 )

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

    if(any(visitedNodes$Node == endNode) == TRUE){
      break
    } else{
      if(length(visitedNodes[,1]) > 0){
        noNeighborsLeft <- which(edgeMatrix[visitedNodes$Node, -visitedNodes$Node] == 1)
        if(length(noNeighborsLeft) == 0){
          djikstraPath <- NA
          lengthDjikstra <- Inf
          warning("Network is not connected")
          break
        } else{
          newCurrent <- unvisitedNodes$Node[which(unvisitedNodes$CurrentValue == min(unvisitedNodes$CurrentValue, na.rm = TRUE))]
          unvisitedNodes$Status[which(unvisitedNodes$Node == newCurrent)] <- "Current"
        }
      }
    }
  }

  #Recursively solve shortest path

  if(is.na(djikstraPath) == FALSE){
    shortestPath <- matrix(NA, nrow = (dim(visitedNodes)[1])+1, ncol = 1)
    shortestPath[1] <- endNode
    nVisited <- dim(visitedNodes)[1]
    visitedNodes <- visitedNodes[seq(from = nVisited, to = 1),]

    # Check whether previous nodes are parent to current node
    # Recursive solving of shortest Path vertices
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

        ### There is a bug here! REDO!!!
        childToParent <- matrix(0, nrow = length(containedParents), ncol = 3)
        for(i in 1:length(containedParents)){
          childToParent[i,1] <- containedParents[i]
          childToParent[i,2] <- euclidDistance(coordMatrix[child,1],coordMatrix[child,2],coordMatrix[containedParents[i],1],coordMatrix[containedParents[i],2])
        }
        childToParent[,3] <- visitedNodes$CurrentValue[which(visitedNodes$Node == child)] - childToParent[,2]
        parentNewIdx <- which(childToParent[,3] == min(abs(childToParent[,3])))
        visitedNodes$directParent[which(visitedNodes$Node == childToParent[parentNewIdx,1])] <- 1
        child <- childToParent[parentNewIdx,1]
      }
    }
    djikstraPath <- visitedNodes$Node[which(visitedNodes$directParent == 1)]
    lengthDjikstra <- visitedNodes[1,3]
  }
  result <- list(djikstraPath, lengthDjikstra)
  result
}

