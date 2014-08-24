## These functions will create a special "matrix" object that can
## cache its inverse and computes the inverse of the special "matrix". 
##If the inverse has already been calculated, 
##then the inverse will be retreived from the cache

## "makeCacheMatrix" function creates a special 
##"matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) im <<- solve
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## "cacheSolve" function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
