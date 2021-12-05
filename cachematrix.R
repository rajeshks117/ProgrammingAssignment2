## Put comments here that give an overall description of what your
## functions do

## The function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to

## set the value of the matrix

## get the value of the matrix

## set the value of the matrix inverse

## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the setinverse function.

## Example is create a 2*2 matrix 
## c=rbind(c(1, -1/4), c(-1/4, 1))
## mat <- makeCacheMatrix(c)
## cacheSolve(mat) <-- Calculate and set it
## cacheSolve(mat) <-- get it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse matrix data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
