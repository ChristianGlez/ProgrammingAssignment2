## These functions create a matrix and calculates the inverse of that matrix. 
## The inverse matrix, once calulated is cached and stored in the parent
## environment.  If the inverse of that matrix has been calculated before, the 
## inverse matrix is not calculated again.

## makeCacheMatrix: This funcion creates a defaul empty matrix.  It provides
## methods to set a matrix (set), get a matrix (get), set an inverse matrix 
## (setInverse), get the inverse matrix (getInverse).

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- 
    function (y) {
      x <<- y
      i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  
  getInverse <- function() i

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve:  This function calculates the inverse matrix of x, if the
## inverse matrix hasn't been calculated before and cached in the context 
## of makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse();
  
  if(!is.null(i)) {
    message("Getting cached Inverse Matrix")
    return(i)
  }
  
  message("Calculating Inverse Matrix ...")
  mtx <- x$get()
  i <- solve(mtx)
  x$setInverse(i);
  i
}
