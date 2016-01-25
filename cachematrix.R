## The functions will compute and cache an inverse of a matrix. 
## An inverse of a matrix is a costly computation hence caching it makes it readily available. 

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  mat.inv <- NULL
  set <- function(y) {
    x <<- y
    mat.inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mat.inv <<- inverse
  getinverse <- function() mat.inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  mat.inv = x$getinverse()
  
  # if the inverse has already been calculated
  if (!is.null(mat.inv)){
    # if the inverse of the matrix is already computed then retrieve it from the cache.
    message("Retrieving cached inverse")
    return(mat.inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  mat.inv= solve(mat.data, ...)
  
  # sets the value of inverse in the cache by using setinverse function.
  x$setinverse(mat.inv)
  
  return(mat.inv)
}
