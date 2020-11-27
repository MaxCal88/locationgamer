lgsolve <- function(edgeMatrix, coordMatrix, nPlayers, demandLoc){
  shortestDistMatrix <- matrix(0, ncol = dim(edgeMatrix)[2], nrow = dim(edgeMatrix)[1])
  nNodes <- dim(edgeMatrix)[1]
  
  for (i in 1:dim(shortestDistMatrix)[1]){
    for (j in 1:dim(shortestDistMatrix)[2]){
      if (i == j){
        shortestDistMatrix[i,j] <- 0
      } else{
        shortestDistMatrix[i,j] <- djikstra(edgeMatrix, coordMatrix, i, j, nNodes)[[2]]
      }
    }
  }
  
  if (nPlayers == 2){
    returnPlayer1 <- matrix(0, ncol = dim(edgeMatrix)[2], nrow = dim(edgeMatrix)[1])
    returnPlayer2 <- matrix(0, ncol = dim(edgeMatrix)[2], nrow = dim(edgeMatrix)[1])
    
    for (k in 1:dim(returnPlayer1)[1]){
      for (q in 1:dim(returnPlayer1)[2]){
        choice1 <- k
        choice2 <- q
        compFrame <- shortestDistMatrix[,c(choice1, choice2)]
        comparison <- apply(compFrame,1,min)
        idx <- matrix(0, nrow = length(comparison), ncol = 1)
        
        if(k == q){
          idx[i] <- 0
        }else{
          for (i in 1: length(idx)){
            idx[i] = which(compFrame[i,] == min(compFrame[i,]))
            
            if (length(idx[i]) == 2){
              idx[i] <- 0 # equidistant
            }
          }
        }
        
        
        compFrame <- cbind(compFrame,idx)
        demand1 <- sum(demandLoc[which(compFrame[,3] == 1)]) + sum(demandLoc[which(compFrame[,3] == 0)])/2
        demand2 <- sum(demandLoc[which(compFrame[,3] == 2)]) + sum(demandLoc[which(compFrame[,3] == 0)])/2
        
        returnPlayer1[k,q] <- demand1 # Player 1 is rows
        returnPlayer2[k,q] <- demand2 # Player 2 is columns
      }
      maxReturnPlayer1 <- apply(returnPlayer1,2, max)
      maxReturnPlayer2 <- apply(returnPlayer2,1, max)
      
      optChoicePlayer1 <- matrix(0, nrow = dim(edgeMatrix)[1], ncol = dim(edgeMatrix)[2])
      optChoicePlayer2 <- matrix(0, nrow = dim(edgeMatrix)[1], ncol = dim(edgeMatrix)[2])
      
      for (i in 1:dim(edgeMatrix)[2]){
        optChoicePlayer1[which(returnPlayer1[,i] == maxReturnPlayer1[i]),i] <- 1
      }
      
      for (j in 1:dim(edgeMatrix)[2]){
        optChoicePlayer2[j,which(returnPlayer2[j,] == maxReturnPlayer2[j])] <- 1
      }
      
      NashLoc <- matrix(0, nrow = dim(edgeMatrix)[1], ncol <- dim(edgeMatrix)[1])
      
      for (i in 1:dim(NashLoc)[1]){
        for (j in 1:dim(NashLoc)[2]){
          if (optChoicePlayer1[i,j] == 1 && optChoicePlayer2[i,j] == 1){
            NashLoc[i,j] <- 1
          }else{
            NashLoc[i,j] <- 0
          }
        }
      }
      
    }
    
  }else{
    print("Method for multiple players is not implemented yet!")
  }
  
  
}
