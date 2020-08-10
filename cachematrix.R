## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a list containing a function to 
# set/get value of matrix and set/get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invr <<- inverse
        getinverse <- function() invr
        list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# The below function returns the inverse of the matrix if the cached inverse is available
# iif not, cacheSolve computes, caches, and returns it.

cacheSolve <- function(x, ...) {
        invr <= x$getinverse()
        if(!is.null(invr)) {
                message("Getting cached data...")
                return(invr)
        }
        matrx <- x$get()
        invr <- solve(matrx, ...)
        x$setinverse(invr)
        invr
}


#