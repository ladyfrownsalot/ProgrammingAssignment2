## R Programming
##B. Flores
# Fri Mar 25 16:02:25 2022 ------------------------------

## Pair of functions that will create a matrix, cache the inversions, and return the inversions

## Create matrix that will cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- invert
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute inverse of matrix created in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting inverse of matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
