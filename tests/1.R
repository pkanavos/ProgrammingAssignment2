hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

test.examples <- function() {
	unit8<- diag(8)		
	
	h8 <- hilbert(8); 
	cacheMatrix<-makeCacheMatrix(h8)
	cachedInverted<-cacheSolve(cacheMatrix)

	multMatrix<-round(cachedInverted %*% h8,3)

	checkEquals(unit8,multMatrix)
}

