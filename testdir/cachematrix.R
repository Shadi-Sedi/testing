## makeCacheMatrix is a function which creates a "matrix" object that can 
## cache its inverse for the input 


makeCacheMatrix <- function(x = matrix()) {
  
  I <- NULL
  set <- function(y) {
    x <<- y
    I<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## ,then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}

## testing the result

m <- matrix(c(4,4,3,2,1,4,6,7,3), nrow = 3, ncol = 3)
testing <- makeCacheMatrix(m)
cacheSolve(testing)



