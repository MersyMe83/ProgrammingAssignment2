## MersyMe83
## Week 3: Assignment 2: Lexical Scoping
## June 6, 2020

## Assignment Instructions: Catch the Inverse of a Matrix

## The goal of this assignment is to write a pair of functions 
## that cache the value of a matrix so it can be looked up in the cache rather than computed repeatedly. 

## Create a function to compute the inverse of the special matrix returned
## by the makeCacheMatric function.The function will set the variable inv to NULL,
## return the matrix, store the calculated inverse, retrieve the cached inverse
## and return the special matrix when called.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Create a function that retrieves the inverse of the special matrix
## assuming the value has been cached 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}
