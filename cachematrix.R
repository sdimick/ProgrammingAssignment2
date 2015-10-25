## The two folowing functions are used together to skip the process
## of inversing a matrix if you have already solved the matrix.

## ---------------------------------------------------------------

## 'makeCacheMatrix' transforms a matrix into 'special matrix', which
## is really a list, that will store the inverse of a matrix once it
## is solved with 'cacheSolve'

## takes one argument, x, which is a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## ---------------------------------------------------------------

## casheSolve returnes the inverse of the 'specail matrix' created
## by 'makeCacheMatrix' by either a) printing the inverse from memory 
## if it has previously been solved, or b) solving the matrix and then
## returning it.

## 'cacheSolve' takes one argument, x, which is the 'specail matrix'
## created by 'makeCacheMatrix'
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
