## "Matrix inversion is usually a costly computation, and there may be
## some benefit to caching the inverse of a matrix rather than computing
## it repeatedly. This approach can significantly improve the efficiency
## of algorithms that require frequent access to the inverse, especially
## in applications involving large datasets or real-time processing.
## By storing the inverse matrix, we can avoid redundant calculations and
## reduce computational overhead, leading to faster and more efficient
## performance."

# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property
  
  # Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when the matrix is set
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Method to get the inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Retrieve the inverse from the cache
  
  # Return the inverse if it is already set
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Get the matrix from our object
  data <- x$get()
  
  # Calculate the inverse using solve function
  inv <- solve(data, ...)
  
  # Set the inverse to the object
  x$setInverse(inv)
  
  # Return the inverse
  inv
}

# Example usage:
example <- function() {
  # Create a matrix
  mat <- matrix(c(1, 2, 3, 4), 2, 2)
  
  # Create the special matrix object
  specialMatrix <- makeCacheMatrix(mat)
  
  # Compute the inverse
  inv1 <- cacheSolve(specialMatrix)
  print(inv1)
  
  # Retrieve the cached inverse
  inv2 <- cacheSolve(specialMatrix)
  print(inv2)
}

# Run the example
# example()
