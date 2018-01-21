## Our functions here are used to cache the inverse of matrices, to avoid its 
## repeated computations

##  Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL ##for inverse, initially null
  set <- function(y){
    x <<- y
    a <- NULL ##incase of a new matrix
  }
  get <- function() x
  setinv <- function(inverse) a <<- inverse ##assigns inverse to parent env
  getinv <- function() a ##gets inverse val at called location
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## Computes the inverse of the special matrix returned
## by the above function. If inverse has already been
## calculated, cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  a <- x$getinv()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- mean(data, ...)
  x$setinv(a)
  a
}
