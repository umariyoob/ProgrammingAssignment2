## Below are a pair of functions that cache the inverse of a matrix.
## We assume that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
set <- function(y){
  x <<- y
  inverse <<- NULL
}
get <- function() x
setcache <- function(solve) inverse <<- solve
getcache <- function()inverse
list(set = set,get=get,setcache=setcache,getcache=getcache)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated   
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getcache()
        if(!is.null(inverse)){
          message("getting cached data")
          return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setcache(inverse)
        inverse
}
