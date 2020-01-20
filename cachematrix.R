## Caching the inverse of a Matrix
## functions do:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setting <- function(a) {
    x <<- a
    i <<- NULL
  }
  getting <- function() x
  set_inverse <- function(inverse_matrix) i <<- inverse_matrix
  get_inverse <- function() i
  list(setting = setting,
       getting = getting,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  i <- x$get_inverse()
  if (!is.null(i)) {
    message("get cached matrix_data")
    return(i)
  }
  matrix_data <- x$getting()
  i <- solve(matrix_data, ...)
  x$set_inverse(i)
  i
}

#B <- matrix(c(14,3,5,1),2,2)
#B1 <- makeCacheMatrix(B)
#cacheSolve(B1)
#cacheSolve(B1)



