## Below are two functions that are used to create a special object that stores
## a matrix and cache's its inverse.


## The first function, makeCacheMatrix creates a list containing a function to
## set the value of a matrix
## get the value of a matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  return(list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse))
}

## The following function takes as an argument the special list created with the above function.
## It first checks to see if the inverse of the matrix has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the
## value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(z, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- z$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  A <- z$get()
  m <- solve(A, ...)
  z$setinverse(m)
  return(m)
}  
  
   
