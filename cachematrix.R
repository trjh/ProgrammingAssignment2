## These functions create a special "matrix", and compute the inverse of that
## matrix.  The first function, makeCacheMatrix, creates a list containing
## functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
##
## the second function returns the inverse of the matrix set with
## makeCacheMatrix -- either by computing it, or by returning the cached
## inverse matrix calculated earlier.
##
## example usage:
##
## m <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## 
## ...where the first instance of cachesolve(m) computed the inverse of the
## matrix, but the second merely returned the cache value of the inverse

## makeCacheMatrix -- return a list of functions to
## 1. set the value of the matrix (at the outset, the input matrix x)
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
	x <<- y
	inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get,
	 setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve -- given a list of functions x, created by makeCacheMatrix,
##	return the inverse of the matrix contained in x, as computed by
##	solve().  Any additional arguments are passed to solve()

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
	return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
