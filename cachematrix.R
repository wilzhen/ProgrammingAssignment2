## The makeCacheMatrix function maintains a matrix and a cache of its inverse matrix, while
## cacheSolve calculates the inverse matrix, if the inverse has not been cached.

## makeCacheMatrix contains 4 functions: (set) assigns a matrix and sets the inverse to NULL, 
## (get) retrieves the matrix assigned, (setinverse) assigns the inverse matrix and 
## (getinverse) retrieves the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve retrieves the inverse matrix from cache.  If the inverse matrix has not been set, then 
## cacheSolve will calculate (and sets) the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
  }
