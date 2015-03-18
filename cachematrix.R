## This script contains two functions. The first constructs a list of functions that 
## allows caching the inverse of a matrix. The second function is for finding the inverse:
## it first accesses the environment of the list created by the first function to see if
## the inverse has already been calculated. If so, it returns the cached value. If not, it
## calculates the inverse, and uses the functions created by the first function to cache
## it in the proper environment.

## Creates a matrix object that caches its inverse. The outer function makes an object
## consisting of 4 functions: set, get, setinv, and getinv. Takes advantage of lexical
## scoping to cache a matrix and its inverse within the environment defined by this
## object
makeCacheMatrix <- function(origMat = matrix()) {
        cachedInv <- NULL    # Set the cached inverse to null so the first run works
        
        set <- function(y) {
                origMat <<- y    # Sets origMat for the makeCacheMatrix environment to y
                cachedInv <<- NULL    # Sets cacheInv in the makeCacheMatrix environment to NULL.
        }
        
        get <- function() {    # Returns the original input matrix
                origMat
        }
        
        # Allows calling $setinv to set the inverse that is stored in this environment
        setinv <- function(invToCache) { 
                cachedInv <<- invToCache # sets the cached inverse in this environment to the one being passed from elsewhere
        }
        
        getinv <- function() {    # Returns the cached inverse
                cachedInv 
        }
        
        # Return the four functions in a list, so they can be called by cacheSolve
        return (list(set = set, get = get,
             setinv = setinv,
             getinv = getinv))
        
}


## Computes inverse of the matrix created by makeCacheMatrix. If inverse has already been 
## calculated, skip calculating and retreive from cache in the environment defined in
## makeCacheMatrix
cacheSolve <- function(matObj, ...) {
        # goes to the matrix object created by makeCacheMatrix, uses the getinv function
        # stored there to return the possibly previously calculated inverse
        invFromCache <- matObj$getinv()    
        
        # If the inverse has already been calculated, matObj$getinv will return something
        # other than NULL. Return this cached inverse
        if(!is.null(invFromCache)) {
                message("getting cached data")
                return(invFromCache)
        }
        
        # If the inverse has not been created yet, matObj$getinv will return NULL.
        # Calculate the inverse and save it to the matrix object
        else {
                data <- matObj$get()    # Get the original matrix from the matrix object, assign it to the local variable data
        
                invToCache <- solve(data, ...)    # Calculate the inverse
        
                matObj$setinv(invToCache)    # set the cached inverse in the matrix object to what was just calculated
        
                return (invToCache)    # Returns the calculated inverse
        }
}