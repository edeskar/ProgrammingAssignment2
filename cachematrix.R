## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create an empty matrix container that can cache the inverse matrix.
## set 

makeCacheMatrix <- function(x = matrix()) {
  ## create an empty container matrix
  inv <- NULL
  set <- function(y) {
    ## let x be accessed from the inner functions
    x <<- y
    ## let inv be accessed in the inner functions
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  ## store the functions in a list
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


## Executes and returns the iverse matrix. Also checks if the input matrix is already solved. If so it returns the iverse matrix form the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## call the stored function getinverse
  inv <- x$get_inverse()
  ## check if the matrix is already solved
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## compute the inverse matrix by calling the defined funcions in makeCacheMatrix
  mat <- x$get()
  inv <- solve(mat, ...)
  x$set_inverse(inv)
  inv
}
