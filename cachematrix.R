## Functin makeCacheMatrix creates a matrix. You can get inversion of this matrix with cacheSolve function.
## If you asked for the inversion next time, you obtain cached solution without computing.
## Example:
## mat <- matrix(1:4,2,2)
## special_mat <- makeCacheMatrix(mat)
## inverse_mat <- cacheSolve(special_mat)

## This funstion obtains matrix and creates a special matrix (which is in fact a list of 4 functions)

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Call this function when you need the inversion of you special matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

## Note: credit to original creator of makeVector and cacheMean functions; I just replaced several letters

