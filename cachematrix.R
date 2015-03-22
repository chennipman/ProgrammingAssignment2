## These function implement caching of the inverse matrix

## Creates an matrix with the possiblity to keep an inverse matrix in cache
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse matrix
  inverse_matrix <- NULL
  
  # Define function to set the matrix
  set <- function(y) {
    x <<- y # Change the matrix with lexical scoping 
    inverse_matrix <<- NULL # Reset the inverse matrix, because the matrix is changed
  }
  
  get <- function() x # Function to return the matrix
  
  # Function to set the inverse matrix
  setinverse <- function(inv_matrix) {
    inverse_matrix <<- inv_matrix
  }
  
  getinverse <- function() inverse_matrix # Function to return the inverse matrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}  
  
 
## Returns the cached value of the inverse matrix or calculates the inverse matrix. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## If the inverse of 'x' is in cache, this one is used
  
  inverse_matrix <- x$getinverse()
  # If there is an inverse matrix, return this matrix
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  # If there is no inverse matrix, calculate the inverse matrix
  matrix <- x$get() # get the data from the matrix
  inverse_matrix <- solve(matrix, ...)
  x$setinverse(inverse_matrix)
  inverse_matrix
  
}
