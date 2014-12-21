
## Function makeCacheMatrix, when it is called with a matrix parameter,
## will hold that parameter and return a list object with references
## to each of the four API calls: set and get the original matrix,
## and setInverse and getInverse to service the cache when called from
## cacheSolve. Note that for non-square matrices, the ginv(A) function
## is called which requires the MASS package, i.e. the Moore-Penrose 
## Generalized Inverse of A.
## library("MASS")
## Note that if an attempt is made to inverse a non-inversible matrix,
## R will throw an error.
## One example of an inversible matrix is:
## m = rbind(c(1, -1/4), c(-1/4, 1))

makeCacheMatrix <- function(x = matrix()) {
    ## You can copy, source, and run this file to see the messages it spits out 
    ## in order to follow the flow.
    
    message("In makeCacheMatrix TOP, holding on to original matrix parameter.")
    
    cachedInverse <- NULL ## Initialized as empty.
    
    ## Note: functions are lazy and are not parsed until called.
    
    set <- function(y) {
        message("In makeCacheMatrix set function. The original matrix 
                is being replaced, and the cache invalidated.")
        x <<- y
        cachedInverse <<- NULL
    }
    
    get <- function() {
        message("In makeCacheMatrix get function, returning original matrix.")
        x
    }
    
    setInverse <- function(inv) {
        message("In makeCacheMatrix setInverse, caching.")
        cachedInverse <<- inv
    }
    
    getInverse <- function() {
        message("In makeCacheMatrix getInverse, retrieving cache.")
        cachedInverse
    }
    
    ## Returning API as a list object pointing to internals.
    list(
        set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse, 
        message("In makeCacheMatrix, returning API reference list")
    )
}

## Function cacheSolve returns the inverse of the matrix passed to it.
## If it has already produced the inverse, it retrieves the cached
## version. If the cache is empty, it produces the inverse and caches it.
## The cache, along with the original matrix, is stored and retrieved 
## from the makeCacheMatrix function environment which is referenced via 
## the x parameter which is a list of functions within the makeCacheMatrix 
## function environment.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    ## Cache is retrieved if it exists.
    if(!is.null(inv)) {
        message("In cacheSolve - returning cached data")
        return(inv)
    }
    
    ## Cache is empty, so we must produce it.
    ## Get original matrix from makeCacheMatrix function environment.
    origMtx <- x$get()
    
    ## For square matrices...
    if(nrow(origMtx) == ncol(origMtx)) {
        message("In cacheSolve, calling solve on square matrix.")
        inv <- solve(origMtx)
    }
    else {
        ## Attempting to call ginv(A), Moore-Penrose Generalized Inverse of A
        ## for non-square matrices. 
        ## ginv(A) requires loading the MASS package, so library("MASS") is
        ## called in hopes that if it has not already been loaded, it still
        ## might be.
        message("In cacheSolve, calling ginv function for non-square matrix.
                Loading MASS library as a precautionary measure.")
        library("MASS")
        inv <- ginv(origMtx)
    }
    x$setInverse(inv)
    message("In cacheSolve, Returning cached inverse.")
    inv
}
