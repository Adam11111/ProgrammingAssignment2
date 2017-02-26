########################################################################################################
## These functions calculate the inverse of a matrix.
##
## When it does so it checks to see if the inverse of the same matrix has
## already been calculated.
##
## If there is already a cached calculation of the inverse it skips the
## recalculation and returns the cached inverse
##
## Manual testing of these functions are commented out at the end of this script
##
## The program was developed from the following repo's "makeVector" and "cachemean" functions:
## https://github.com/rdpeng/ProgrammingAssignment2
##
## Further documentation and explanations of the functions in the "rdpeng" repo above can be found at:
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
########################################################################################################


## makeCacheMatrix(mat = matrix())
## This function creates a special "matrix" object that can cache its inverse.
## It creates 4 functions: "set", "get", "setinv", and "getinv"
## It also creates 2 variables: "mat", and "inv"
## These functions and variables are contained in the environment of the output from makeCacheMatrix

makeCacheMatrix <- function(mat = matrix()) {

    ## Initialize objects (mat initialized in function arguments above)
    inv <- NULL
    
    ## 1) set the value of the matrix to a different one
    set <- function(newmat) {
        mat <<- newmat
        inv <<- NULL
    }
    
    ## 2) get the value of the matrix
    get <- function() mat
    
    ## 3) set the value of the inverse of the matrix
    setinv <- function(solve) inv <<- solve
    
    ## 4) get the value of the inverse
    getinv <- function() inv
    
    ## Return a list with the functions
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv
    )
}


## cacheSolve(x, ...)
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Check to see if the inverse has already been calculated
    ## If true, then get the inverse from the cache and skip the computation
    inv <- x$getinv()
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    
    ## If the inverse has not already been calculated, then
    ## calculate the inverse of the matrix and set the value of the inverse in the cache via "setinv"
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


## Testing the output of the functions

## Test that makeCacheMatrix works correctly
#myMatrix <- makeCacheMatrix(matrix(c(2,2,3,2), nrow = 2, ncol = 2))
#myMatrix

## Test that the matrix inversion works and that a cached value is returned
#cacheSolve(myMatrix)
#cacheSolve(myMatrix)

## Test the "set" functionality with a different matrix
#myMatrix$set(matrix(c(6,2,8,4), nrow = 2, ncol = 2))
#cacheSolve(myMatrix)
#cacheSolve(myMatrix)

## Check the error handling
#errorTest <- makeCacheMatrix()
#errorTest
#cacheSolve(errorTest)
