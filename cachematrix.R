## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse property
  inv <- NULL
  # Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Method to get the matrix
  get <- function() x
  # Method to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  # Method to get the inverse
  getInverse <- function() inv
  # List of methods 
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Initialize inverse object
  inv <- x$getInverse()
  # Verify that the object is not empty
  if (!is.null(inv)) {
    message("getting inverse matrix")
    return(inv)
  }
  # Get the matrix from our object 
  mat <- x$get()
  # Calculate the inverse (solve equation Ax = b)
  inv <- solve(mat, ...)
  # Set the inverse to the object
  x$setInverse(inv)
  # Return inverse
  inv
}
