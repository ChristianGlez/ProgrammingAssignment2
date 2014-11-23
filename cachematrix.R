## These functions create a matrix and calculates the inverse of that matrix. 
## The inverse matrix, once calulated, is cached and stored in the parent
## environment.  If the inverse of that matrix has been calculated before, the 
## inverse matrix is not calculated again and the cached version is returned.

## makeCacheMatrix: This funcion creates a defaul empty matrix or creates one 
## with the the argument.  It provides methods to set a new matrix (set), get 
## the set matrix (get), set an inverse matrix (setInverse), get the inverse matrix (getInverse).

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

## cacheSolve: This function calculates the inverse of matrix retrieved from x.
## If the inverse matrix retrieved from x is NULL, the inverse matrix is 
## is calculated.  If not, the inverse matrix is retrieved from the parent 
## environment.

cacheSolve <- function(x, ...) {
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
