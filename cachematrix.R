## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL

    set <- function(new_matrix) {
        x <<- new_matrix
        inverted <<- NULL
    }

    get <- function() x
    
    set_inverted <- function(inv) inverted <<- inv
    
    get_inverted <- function() inverted
    
    list(
        set=set, 
        get=get, 
        set_inverted=set_inverted, 
        get_inverted=get_inverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverted <- x$get_inverted()

    if (!is.null(inverted)) {
        message("getting cached data")
        return(inverted)
    }
    
    data <- x$get()
    inverted <- solve(data)
    inverted
}
