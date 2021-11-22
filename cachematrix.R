## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix caches the inverse of a matrix with getter and setter functions within it
## cacheSolve either retrieves the cached value for the inverse of the matrix or solves it 
## if the inverse has not been calculated yet


## Write a short comment describing this function
## This function creates a matrix which is a list containing a 
## function to set the inverse of the matrix, get the inverse of
## the vector, set the value of the inverse, and get the value of
## the inverse

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function solves the inverse of the matrix
## First, it checks if the inverse has already been found, if
## it has, use the get function to get the inverse from cache and return a
## If it hasn't, compute the inverse of the matrix using the solve function in R
## It will cache this value as the inverse of matrix a and return a
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  a <- x$getinverse()
  if (!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}
