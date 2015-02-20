# The module below uses the <<- R operation to cache the inverse of a Matrix in 
# a makeCacheMatrix object(function) The function cacheSolve, when invoked on a 
# makeCacheMatrix object, checks the cache first for the given object.
#
# If the cache is not null, then it returns the inverse from cache.
# If the cache is null, it computes the inverse using solve, caches the
# outcome and returns the inverted matrix
#
# All matrixes cannot be inverted, so for quick testing, I have created a test
# function createInversibleMatrix that takes an integer n and create a n x n
# inversible square matrix. The definition of this function is borrowed from
# function hilbert in R help section for solve
#
# The following would be a test
# matrix3 <- createInversibleMatrix(3)
# m3 <- makeCacheMatrix(matrix3)
# cacheSolve(m3)
# cacheSolve(m3)
#
# Both calls to the cacheSolve should return the same inverted matrix which is
# the same as solve(matrix3). From the message of cacheSolve, you can see that
# the second call did not have to compute (call solve), but simply returned the
# result from the existing cache from the first call


## The function makeCacheMatrix creates a class that contains a matrix, its 
## inverse, getters and setters. The inverse matrix object is set to null every
## time a new matrix is assigned to the class. Both the matrix and its inverse
## are assigned using <<- so they are set in the parent environment

makeCacheMatrix <- function(x = matrix()) {

        inverseMatrix <- NULL
        
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL 
        }
        
        get <- function() x
        
        setInverse <- function(inv) {
                inverseMatrix <<- inv
        }
        
        getInverse <- function() inverseMatrix
        
        list (
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## The cacheSolve function takes a makeCacheMatrix object and returns the
## inverse of the matrix. If the inverse is available (cached), it returns
## the cached value, otherwise it computes it, calls the set method on the 
## object to cache it and returns the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        if (! is.null(inv)) {
                message("Retrieving inverse matrix from cache...")
                return(inv)
        }
        
        myMatrix <- x$get()
        inv <- solve(myMatrix, ...)
        x$setInverse(inv)
        inv
}

## This function creates a square inversible matrix of size n that can be used
## to test the matrix caching

createInversibleMatrix <- function(n) { 
        i <- 1:n; 
        1 / outer(i - 1, i, "+") 
}
