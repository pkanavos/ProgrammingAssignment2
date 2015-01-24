## Set the seed before generating a random 8x8 matrix, to ensure repeatability
set.seed(1)

## Create an 8x8 input matrix to use in all tests
inputMatrix    <- matrix(rnorm(64),nrow=8,ncol=8); 


## Test that cacheSolve returns a matrix, not a function or something else
test.that_cacheSolve_returns_matrix <- function() {
  
  cacheMatrix<-makeCacheMatrix(inputMatrix)
  result<-cacheSolve(cacheMatrix)
  
  ##The result is a matrix
  checkTrue(is(result,"matrix"))  
}

##Test that cacheSolve does return the inverse of the input matrix
test.that_cacheSolve_returns_inverse <- function() {
  ## Calculate the inverse matrix
  cacheMatrix<-makeCacheMatrix(inputMatrix)
	result<-cacheSolve(cacheMatrix)
  
  ##Multiplying the inverted matrix with the original matrix
  ##should result in the identity matrix  
  multMatrix<-round(result %*% inputMatrix,3)
  ## The expected result of the multiplication is the identity matrix
  expected <- diag(8)    
  # Are the multiplication results equal to the expected result?
  checkEquals(multMatrix,expected)
}

## Calling cacheSolve wtice should return the same object, not just an equal object
test.that_second_call_returns_cached_value <- function() {

  cacheMatrix<-makeCacheMatrix(inputMatrix)
  
  firstResult <-cacheSolve(cacheMatrix)
  secondResult<-cacheSolve(cacheMatrix)
  
  # The results of both calls should be the same object
  checkIdentical(firstResult,secondResult)
  
}

## solver returns a 1x1 matrix containing NA. Does cacheSolve return the same?
test.that_na_returns_na <- function() {
  
  cacheMatrix<-makeCacheMatrix(NA)
  
  result <-cacheSolve(cacheMatrix)

  checkTrue(is.na(result[[1]]))
}
