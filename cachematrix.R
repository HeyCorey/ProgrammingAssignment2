## These functions allow you to work with a matrix and its inverse
## With minimal processor requirements by caching the inverse once calculated

## The function sets/gets the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }                                                       ## define set function
  get <- function() x                                     ## define get function
  setinverse <- function(invmat) im <<- invmat            ## define setinverse function
  getinverse <- function() im                             ## define getinverse function
  
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)  ## return list
}


## This function return the inverse matrix, calculating it or retrieving it from cache

cacheSolve <- function(x, ...) {
  invmat <- x$getinverse()                                ## get inverse
  if (!is.null(invmat)){
    return(invmat)                                        ## if cached inverse exists, return it
  }
  m <- x$get()                                            ## if no cached inverse, get the matrix
  invmat <- solve(m)                                      ## get the inverse of the matrix
  x$setinverse(invmat)                                    ## add inverse to cache
  invmat                                                  ## return calculated inverse
}