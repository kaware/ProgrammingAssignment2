## The following functions implement a cacheMatrix object, and the function
## that calculates the inverse of a cacheMatrix.  NOTE: These functions 
## assume that the matrix passed to the makeCacheMatrix function are
## invertible, there is not error checking to verify this in the fucntions.

#############################################################################
##
## function makeCacheMatrix
##
## Purpose:  Creates a cacheMatrix object.  The cacheMatrix object stores a 
##           copy of a matrix, and the inverse of the matrix, if it has been 
##           calculated.
##
## Input: A numeric matrix.  
##        
## Attributes:
##      x -- The matrix.
##      inv -- The inverse of the matrix x, if it exists and has been 
##             calculated with the cacheSolve routine below.
##
## Methods:
##      set() -- Allows the matrix stored in the object to be changed.
##      get() -- Retrieves the matrix stored in the object.
##      setInv() -- Sets the value of the inverse of the matrix stored in  the
##                  object.  Note, setInv should only be called through the
##                  cacheSolve routine below. Otherwise it may not be in sync
##                  with the matrix x.
##      getInv() -- Retrieves the inverse stored in the object.
##
##############################################################################

makeCacheMatrix <- function(x = matrix()) {
    # initialization of the inverse of the matrix x to NULL
    
    inv <- NULL
    
    # The set function that allows the matrix stored in the object to be 
    # changed.  If the matrix is changed, any existing inverse is cleared to
    # keep the inverse in sync with the matrix.
    
    set <- function(m) {
        x <<- m
        inv <<-NULL
    }
    
    # The get function returns the matrix.
    
    get <- function() x
    
    # The setInv function assigns an input matrix to the inv atribute.  This
    # function should not be called by anything other than the cacheSolve 
    # function listed below to keep the matrix x, and the inverse in sync.
    
    setInv <- function(inverse) inv <<- inverse
            
    # The getInv function returns the inverse of the matrix.
    
    getInv <- function() inv
    
    # The list is used to enable the methods of the object to be called via 
    # the obj$method syntax.
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

##############################################################################
##
## Function: cacheSolve
##
## Purpose:  This function returns the inverse of a matrix stored in a 
##           cacheMatrix object.  If the inverse of the matrix stored in the
##           object has not been cacluated, then the routine caclulates the 
##           inverse, stores it in the cacheMatrix object, before returning the
##           inverse.  Note, the routine does not do any error checking to 
##           verify that the matrix is invertible.  This is the responsibility
##           of the programmer/analyst using the routine.
##
## Input:  A cacheMatrix object
##
## Returns:  The inverse of the matrix stored in the cacheMatrix object.
##
##############################################################################

cacheSolve <- function(x, ...) {
    # Determine if the inverse of the matrix stored in the cacheMatrix 'x' is
    # also stored in 'x'.
    
    inverse <- x$getInv()
    
    # if so, return the inverse
    
    if (!is.null(inverse)) {
        return(inverse)
    }
    
    # if the inverse is not stored in 'x', then calculate the inverse and
    # store it in 'x'.

    inverse <- solve(x$get(), ...)
    x$setInv(inverse)
    
    # Return the inverse
    
    inverse
}
