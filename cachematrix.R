makeCacheMatrix <- function(matrix = matrix()) {
  cachedInverse <- NULL
  
  setMatrix <- function(newMatrix) {
    matrix <<- newMatrix
    cachedInverse <<- NULL  # Clear cached inverse when setting a new matrix
  }
  
  getMatrix <- function() matrix
  
  setInverse <- function(inverseMatrix) cachedInverse <<- inverseMatrix
  
  getInverse <- function() cachedInverse
  
  # Return a list of the above functions
  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


cacheSolve <- function(x, ...) {
  # Attempt to retrieve the cached inverse
  cachedInverse <- x$getInverse()
  
  # If the cached inverse exists, return it
  if (!is.null(cachedInverse)) {
    message("Returning cached inverse")
    return(cachedInverse)
  }
  
  # Otherwise, retrieve the matrix
  matrixData <- x$getMatrix()
  
  # Compute the inverse of the matrix
  computedInverse <- solve(matrixData, ...)
  
  # Cache the computed inverse for future use
  x$setInverse(computedInverse)
  
  # Return the newly computed inverse
  computedInverse
}
