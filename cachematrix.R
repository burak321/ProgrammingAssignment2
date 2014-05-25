## Two functions for 
##1 - generating a special matrix to perform cached 
## inverse calculation operations
##2 - calculating the inverse of the given matrix
##if it is not already calculated or
##retrieving the inverse from cache and setting
##the inverse of the given matrix based on the 
##retrieved data if it is already calculated 
##and cached.

## Generates a special vector that makes possible
## caching the calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseMat) invMat <<- inverseMat
        getinverse <- function() invMat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calcualtes the inverse of the input matrix if it 
## is not already calculated. Retrieves inverse matrix
## from cache if already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invMat <- x$getinverse()
        if(!is.null(invMat)) {
                message("getting cached inverse matrix")
                return(invMat)
        }
        data <- x$get()
        invMat <- solve(x, ...)
        x$setinverse(invMat)
        invMat
}
