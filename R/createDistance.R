### Creates a distance matrix assuming a completely connected network (every vertex is connected to all others)
createDistance <- function(coordMatrix){
  distMat <- matrix(0, nrow = dim(coordMatrix)[1], ncol = dim(coordMatrix)[1])
  for (i in 1:dim(coordMatrix)[1]){
    for (j in 1:dim(coordMatrix)[1]){
      distMat[i,j] = sqrt((coordMatrix[i,1]-coordMatrix[j,1])^2 + (coordMatrix[i,2]-coordMatrix[j,2])^2)
    }
  }
  distMat
}
