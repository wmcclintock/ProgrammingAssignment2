## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list of functions
# set function initiates x and m
# assigns y to x and pushes x to the enclosing environment (CacheMatrix()
makeCacheMatrix <- function(x = matrix()) {
  
        m <- NULL
        
        set <- function(y) {	
                x <<- y		    
                m <<- NULL
        }
        
        get <- function() x
        
        setsolve <- function(solve) m <<- solve
        
        getsolve <- function() m

        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

# create object by > obj <- makeCacheMatric(x)
# 'obj' is now a list of the above functions 

## Return a matrix that is the inverse of 'x'
# obj refers to the object input argument created by the above makeCacheMatric function 
# input is a list of functions from makeCacheMatrix (myval <- makeCacheMatrix())
cacheSolve <- function(obj, ...) { 
        
        m <- obj$getsolve()	 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- obj$get()
        m <- solve(data, ...)
        obj$setsolve(m)
        m
}
