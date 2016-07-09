## makeCacheMatrix takes in a matrix and return a inverse of the matrix. 
## The inverse is cached and the cache is returned


## madeCacheMatrix creates and return a list of three functions
## setMatrix -  Set a matrix and initialise the cache to null
## getMatrix -  Gets the matrix
## setInverse - Solve the matrix and store in cache
## getInverse - Returns the inverse from cache if any. 
##              If cache is empty, it will solve the matrix and store the inverse 
##              in cache and return the cache

makeCacheMatrix <- function(x = matrix()) {
    
    cachedInverse <- NULL
    
    setMatrix <- function(matrixToSet) {
        x <<- matrixToSet
        cachedInverse <<- NULL
    }
    
    getMatrix <- function() {
        x
    }
    
    setInverse <- function(inverseToSet) {
        cachedInverse <<- inverseToSet
    }
    
    getInverse <- function() {
        cachedInverse
    }
    
    return(list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInverse = setInverse, getInverse = getInverse))
}


## cacheSolve takes in a list of makeCacheMatrix's functions.
## If there is already an inverse in cache, it will return the inverse, otherwise it 
## creates a Inverse Matrix using the solve function and set it to the cache using
## makeCacheMatrix setInverse function 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if (!is.null(x$getInverse())) {
            message("Getting Matrix Inverse from Cache")
        } else {
            x$setInverse(solve(x$getMatrix()))
        }
        return(x$getInverse())
}
