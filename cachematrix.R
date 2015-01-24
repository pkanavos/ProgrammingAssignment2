## cacheSolve and makeCacheMatrix calculate the inverse of a matrix
## and cache the result to avoid recalculating the inverse of the same matrix


## To use, first create a cache of the input data with `makeCacheMatrix()`,
## then call `cacheSolve()` on the cache, eg:
##
##    cache  <- makeCacheMatrix(someMatrix)
##    result <- cacheSolve(cashe)


## Create a list of functions that cache the input matrix
## and a computation result.
## The list of functions is similar to an object in object-oriented languages

makeCacheMatrix <- function(x = matrix()) {
        #inv holds the inverted result. Initialize to NULL
        inv <- NULL
        #The set method caches the input data for lazy evaluation
        #and clears the cached result
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #get returns the cached input data
        get <- function() x
        #stores the inverse matrix
        setinv <- function(inverse) inv <<- inverse
        #retrieve the inverse matrix
        getinv <- function() inv
        #Return a list of named functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Calculates the inverse of a matrix and caches the value in a cache list
## created by `makeCacheMatrix`, unless the cache already contains the result.
## In this case, the result is returned immediatelly

cacheSolve <- function(x, ...) {
        #Does the cache already contain the result?
        inv <- x$getinv()
        if (!is.null(inv)){
          #If yes, return the result immediatelly
        	message("getting cached data")          
        	return(inv)
        }
        #Otherwise, retrieve the data from the cache
        data<-x$get()
        #Calculate the inverse
        inv<-solve(data,...)
        #and store the result in the cache
        x$setinv(inv)
        #before returning it to the caller
        inv
}
