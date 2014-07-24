## This function has getter and setter methods for 
## both the matrix and its inverse.
## This is a constructor class that returns a list
## with getter and setter functions.

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    
    set <- function(y){
        x <<- y
        invMatrix <<- NULL
    }
    
    get <- function() x
    
    setInvMatrix <- function(inverseMatrix){
        invMatrix <<- inverseMatrix
    }
    
    getInvMatrix <- function() invMatrix
    
    list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## This function calls the makeCacheMatrix constructor
## initially setting the inverse matrix to NULL.
## Once the inverse matrix is set using solve(), this
## function makes sure it is not recalculating the
## inverse again

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInvMatrix()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
 
    ## Since x is an object of makeCacheMatrix
    ## class, we need to use its get() method
    ## to get the matrix. solve() needs a numeric
    ## matrix as input and x is NOT a numeric matrix.
    
    inv <- solve(x$get())
    x$setInvMatrix(inv)
    inv
}
