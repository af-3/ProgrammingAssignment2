## These function combined make up assignment 2 of the R Programming course. 

## This function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(m = matrix()){
    i <- NULL
    set<- function(n) {
        m <<- n
        i <<- NULL
    }
    get <- function() m
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...){
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
