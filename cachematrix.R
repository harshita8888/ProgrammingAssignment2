## The following function produce cached inverse of a matrix

## CacheMatrix function creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y){
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) 
  inversed <<- solveMatrix
  getInverse <- function() inversed
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

##  CacheSolve computes the inverse of the matrix returned by makeCacheMatrix()
cacheSolve <- function(x, ...) {
  inversed <- x$getInverse()
  if(!is.null(inversed)){
    message("getting cached data")
    return(inversed)
  }
  data <- x$get()
  inversed <- solve(data)
  x$setInverse(inversed)
  inversed      ## Returns a matrix that is the inverse of 'x' matrix

}
