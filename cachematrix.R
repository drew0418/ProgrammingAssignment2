## The pair of functions, makeCacheMatrix and cacheSolve will cache a matrix and return its inverse.
## 

# makeCacheMatrix will create a special matrix object that will be cached in "inv". It
# houses set, get, getinverse, and setinverse functions

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL         ## sets value of inv to NULL
    set <- function(y) {    ## set() will substitute matrix x in makeCacheMatrix with y
        x <<- y             
        inv <<- NULL        ## resets value NULL to inv
    }
    get <- function() x                  ## gets the matrix x from the main function
    setinverse <- function(solve) inv <<- solve   ## sets the inverse of matrix inv
    getinverse <- function() inv                  ## gets the inverse of matrix inv
    list(set = set, get = get,      ## stores the four functions
         setinverse = setinverse,
         getinverse = getinverse)
    

}


## cacheSolve will return the inverse of the matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()      ## retrieve inverse the inverse of matrix from makeCacheMatrix
    if(!is.null(inv)) {      ## check to see if the value of inv is not NULL.
        message("getting cached data")      ## if inv is not NULL return message and inv
        return(inv)
    }
    data <- x$get()      ## gets the matrix stored in makeCacheMatrix
    inv <- solve(data, ...)      ## assigns inverse of 'data' to inv
    x$setinverse(inv)      ## sets the inverse of inv in the global environment
    inv
}
