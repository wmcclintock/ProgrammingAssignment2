## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {	# set function initiates x and m
                x <<- y		# assigns y to x and pushes x to the enclosing environment (CacheMatrix()
                m <<- NULL
        }
        get <- function() 
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

# create object by > obj <- makeCacheMatric(x)
# 'obj' is now a list of the above functions 

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(obj, ...) { # x refers to the x object input argument - list of functions from makeCacheMatrix (myval <- makeCacheMatrix())
        
        m <- obj$getsolve()	# 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- obj$get()
        m <- solve(data, ...)
        obj$setsolve(m)
        m
}
