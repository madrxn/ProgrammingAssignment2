############################################################
# R Programming Assignment 2 - Lexical Scoping
# Written by: Madrxn (https://github.com/madrxn)
#
# A) makeCacheMatrix function creates a special "matrix" 
#    object that can cache its inverse.
#
# B) cacheSolve function calculates the inverse of the 
#    special "matrix" created by makeCacheMatrix, if the 
#    inverse matix hasn't already been caclulated.  
############################################################


makeCacheMatrix <- function(x = matrix()) {
        # inv intitialized to null
        inv <- NULL
        # sets the matrix stored in the main function
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        # gets the matrix "x" stored in main function
        get <- function() x
        # sets the inverse of the matrix
        setinv <- function(matrix) inv <<- matrix
        # gets the inverse of the matrix
        getinv <- function() inv
        # stores the four functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)   
}


cacheSolve <- function(x, ...) {
        # gets stored value of inv (inverse matrix) from MakeCacheMatrix function
        inv <- x$getinv()
        # tests if stored value is not NULL 
        if(!is.null(inv)){
                #if stored value is not NULL prints message and returns cached value
                message("getting cached data...")
                return(inv)
        }
        # gets matrix from MackeCacheMatrix since stored inv value was NULL 
        data <- x$get()
        # calculates the inverse of the matrix
        inv <- solve(data, ...)
        # sets the inverse of the matrix
        x$setinv(inv)
        # returns the inverse matrix
        inv
}