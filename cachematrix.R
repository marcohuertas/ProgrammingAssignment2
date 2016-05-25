## Create a special object "matrix" that holds the values of a matrix and its
## inverse. This functions are used to quickly check if the inverse of the
## matrix has already been calculated and, if so, it takes the cached value and
## does not calculate it again.

## makeCacheMatrix is a function that receives a matrix as input and creates the 
## list of functions get(), set(), getinv() and setinv(). get() and getinv()
## output the matrix and its inverse, if this has been stored. set() and
## setinv() assignes the matrix and calculates it inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Return a list of functions used to check and compute the inverse of 
        ## a matrix, if this does not already exist
        
        # Check if the input is a matrix and if it is square        
        if (!is.matrix(x)) {
                print("Not a matrix")
                return()
        } else if (nrow(x) != ncol(x)) {
                print("Not a square matrix")
                return()
        }
        
        # Matrix inverse
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setinv <- function(matrixinverse) xinv <<- matrixinverse
        getinv <- function() xinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## cacheSolve is a function that receives a "matrix" and checks if the inverse
## has been calculated already and it has been stored. If it has it outputs 
## the inverse, if it has not is computes it and outputs the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        xinv
}
