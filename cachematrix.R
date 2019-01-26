## The functions here create a special matrix object that can cache and compute the inverse of the matrix

## This function creates the special matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
            x <<- y
            m <<- NULL
          }
          get <- function() x
          setmatrixInverse <- function(matrixInverse) m <<- matrixInverse
          getmatrixInverse <- function() m
          list(set=set, get=get, setmatrixInverse=setmatrixInverse, getmatrixInverse=getmatrixInverse)
}


## This function computes the inverse of the special matrix, if the inverse has already been calcutated, then the inverse is retreived from the cache

cacheSolve <- function(x, ...){
          m <- x$getmatrixInverse()
          if (!is.null(m)){
            message("getting cached data")
            return(m)
            }                            ## Return a matrix that is the inverse of 'x'
          data <- x$get()
          m <- solve(data)
          x$setmatrixInverse(m)
          m
}
