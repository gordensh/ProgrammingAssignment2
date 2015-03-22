## Creates a special "matrix" object that can cache its inverse.
## Usage:
## $set - set the matrix
## $get - get the matrix
## $setInverse - set the inverse value of the matrix
## $getInverse - get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_value <- NULL
  
  # set matrix
  set <- function(y) {
    x <<- y
    inverse_value <<- NULL
  }
  # get matrix
  get <- function() x
  
  # set matrix inverse value
  setInverse <- function(v) inverse_value <<- v
  # get matrix inverse value
  getInverse <- function()  inverse_value
  
  # cahed matrix interface
  list(
       set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse
     )
}


## This function computes the inverse of the special "matrix"
## The function will not compute inverse if already cached for this matrix
cacheSolve <- function(x, ...) {
  
  # look for inverse value in cache 
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # compute inverse and cache it
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  
  i
}
