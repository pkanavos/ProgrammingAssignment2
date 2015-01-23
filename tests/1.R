hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

inputMatrix    <- hilbert(8); 
identityMatrix <- diag(8)  	

test.that_cacheSolve_returns_inverse <- function() {

  cacheMatrix<-makeCacheMatrix(inputMatrix)
	result<-cacheSolve(cacheMatrix)
  
  ##Multiplying the inverted matrix with the original matrix
  ##should result in the identity matrix
  
  multMatrix<-round(result %*% inputMatrix,3)
	checkEquals(multMatrix,identityMatrix)
}

test.that_cacheSolve_returns_matrix <- function() {
  
  cacheMatrix<-makeCacheMatrix(inputMatrix)
  result<-cacheSolve(cacheMatrix)
  
  ##The result is a matrix
  checkTrue(is(result,"matrix"))  
}


test.that_second_call_returns_cached_value <- function() {

  cacheMatrix<-makeCacheMatrix(inputMatrix)
  
  firstResult <-cacheSolve(cacheMatrix)
  secondResult<-cacheSolve(cacheMatrix)
  
  checkIdentical(firstResult,secondResult)
  
}

test.that_na_returns_na <- function() {
  
  cacheMatrix<-makeCacheMatrix(NA)
  
  firstResult <-cacheSolve(cacheMatrix)
  secondResult<-cacheSolve(cacheMatrix)
  
  checkIdentical(firstResult,secondResult)
  checkTrue(is.na(firstResult[[1]]))
}
