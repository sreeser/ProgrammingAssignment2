# Function: makeCacheMatrix() 
#
# Description: Takes in a square matrix and creates a cached version
#              that also can store its inverse when set. 

makeCacheMatrix <- function(x = matrix()) {
  
  #first time called set the inverse to NULL
  inverse_matrix <- NULL
  
  # a 'set' caches the passed in matrix, and 
  # we also set the inverse of the cache to null
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  # return the original matrix
  get <- function() x
  
  # set the passed in inverse to the cached inverse
  setinverse <- function(inverse) inverse_matrix <<- inverse
  
  # return the cached version of the matrix if set
  getinverse <- function() inverse_matrix
  
  # list out available functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Function: cacheSolve() 
#
# Description: Takes in a the result of makeCacheMatrix and
#              will calculate the inverse to set back to the 
#              original cached object. 

cacheSolve <- function(x, ...) {
  
  # get cached inverse of passed in matrix
  inverse_matrix <- x$getinverse()
  
  # if its not null, lets get the cached value
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  
  # if the values null, get the original matrix
  # calc its inverse, and set that value to 
  # the original object. 
  original_matrix <- x$get()
  inverse_matrix <- solve(original_matrix)
  x$setinverse(inverse_matrix)

  # spit out the inverse matrix
  inverse_matrix
  
}
