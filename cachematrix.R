## Function makeCacheMatrix():
## This function creates a list of functions to set and get a given 
## invertible matrix and caches the inverse of the given matrix

## Function cacheSolve():
## This function gets the cached value of the inverse matrix when
## possible and if there is no cached value, computes the inverse 
## of the given matrix and saves the result in the cache.


## The function, makeCacheMatrix(), creates and caches the inverse of a matrix
## Assumptions: 
# 1. The matrix is always invertible, i.e. it is a square matrix
# 2. The matrix used is the same

makeCacheMatrix <- function(x = matrix()) {
        
        # Create an empty inverse matrix
        invMatrix <- NULL
        
        # Save the matrix passed as the argument, in the x variable
        setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        # Get the matrix
        getMatrix <- function() x
        
        # Save the inverse matrix
        setInvMatrix <- function(invMat) invMatrix <<- invMat
        
        # Get the inverse matrix
        getInvMatrix <- function() invMatrix
        
        # Create the list of functions
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,
             setInvMatrix = setInvMatrix, 
             getInvMatrix = getInvMatrix)
        
}


## The function, cacheSolve(),  computes the inverse of the given matrix
cacheSolve <- function(x, ...) {
        
        # Get the cached value of the matrix inverse
        invMatrix <- x$getInvMatrix()
        
        # Check if the returned matrix is empty
        # If not empty, then it's the cached value of the inverse matrix
        # So just return the cached matrix inverse
        if (!is.null(invMatrix)) {
                message("Getting cached value of inverse matrix...")
                return (invMatrix)
        }
        
        # if there is no cached value of inverse matrix
        # compute the inverse matrix and save it in the cache
        
        origMatrix <- x$getMatrix()             # Get the matrix
        invMatrix <- solve(origMatrix, ...)     # Compute the inverse
        x$setInvMatrix(invMatrix)               # Save to cache
        invMatrix                               # Return the value
        
}
