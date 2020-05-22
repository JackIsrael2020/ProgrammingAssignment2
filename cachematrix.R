## This pair of functions take an invertible matrix, solves for its inverse and stores the
## result as a cache. Then if the calculation needs to be perfomed again, the original result
## can simply be retrieved from the cache. Since matrix inversion is a costly computation,
## this is good practise.


## We provide makeCacheMatrix with an invertible matrix and it creates a list of functions
## to cache and call the matrix and its inverse.  

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  ## The set function allows a user to interactively reset the cached of the original 
  ## matrix without calling makeCacheMatrix again.
  set <- function(y){
    x <<- y
    I <<- NULL
  }
  ## The get function calls the original matrix
  get <- function() x
  ## setInverse caches the value of the inverse matrix
  setInverse <- function(inv) I <<- inv
  ## And getInverse calls the inverse matrix
  getInverse <- function() I
  ## Finally, we return a list containing the functions that cacheSolve will need to
  ## cache the value of the inverse matrix and then call it again.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## cacheSolve is given the "vector" containing an invertible matrix and either calculates
## its inverse or, if there is already a suitable value cached, returns a solution 
## that was previously calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse()
  ## First we call the cached value of the inverse, which may have already been calculated.
  if(!is.null(I)) { 
    message("getting cached data")
    return(I) ## If there is a non-default value cached for I, we simply return it.
  }
  data <- x$get() 
  I <- solve(data, ...) ## If not, we call the original matrix using get() and solve
  ## for its inverse. Then we set
  x$setInverse(I)
  I
}
