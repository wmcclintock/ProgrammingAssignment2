## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list of functions
# set function initiates x and m
# assigns y to x and pushes x to the enclosing environment (CacheMatrix()
makeCacheMatrix <- function(x = matrix()) {
        # initialise variables & push variables to closure environment 
        m <- NULL  
        
        set <- function(y) {	
                x <<- y		    
                m <<- NULL
        }
        
        get <- function() x
        
        setsolve <- function(solve) m <<- solve # solve and 'cache' 
        
        getsolve <- function() m # retrieve 'cached' value m

        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) # returns list of functions
}

# create object by > obj <- makeCacheMatric(x)
# 'obj' is now a list of the above functions 

## Return a matrix that is the inverse of 'x'
# obj refers to the object input argument created by the above makeCacheMatric function 
# input is a list of functions from makeCacheMatrix (myval <- makeCacheMatrix())
cacheSolve <- function(obj, ...) { 
        
        m <- obj$getsolve()	 # retrieve value m from obj
		
        if(!is.null(m)) {   # if m is not NULL return 'cached' m
                message("getting cached data")
                return(m) # return and exit function
        }
		
        data <- obj$get() # retrieve matrix defined in obj (makeCacheMatrix())
		
        m <- solve(data, ...) # solve matrix (invert)
		
        obj$setsolve(m) # cache solved (inverted) matrix
		
        m # return inverted matrix to the console
}
