## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. Your assignment is to write a pair of functions that cache
## the inverse of a matrix.

## This function makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
}

## The following function calculates the inverse of the special "matrix" 
## created with the function makeCacheMatrix. However, it first checks to see if,
## the inverse has already been calculated. If so, it gets the inverse matrix
## from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the inverse matrix in the cache
## via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

