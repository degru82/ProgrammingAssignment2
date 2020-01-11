## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    # This function will return a matrix with member variables & functions
    #   var x: original matrix
    #   var inverted: inverted matrix of x
    #   fun set: set passed value to x
    #   fun get: get x from envrionment
    #   fun set_inverted: set passed value to inverted
    #   fun get_inverted: get inverted from environment
    
    # var inverted
    inverted <- NULL

    # fun set
    set <- function(new_matrix) {
        x <<- new_matrix
        inverted <<- NULL
    }
    
    # fun get
    get <- function() x

    # fun set_inverted
    set_inverted <- function(inv) inverted <<- inv

    # fun get_inverted    
    get_inverted <- function() inverted
    
    # return function-jointed matrix
    list(
        set=set, 
        get=get, 
        set_inverted=set_inverted, 
        get_inverted=get_inverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # this function return inverted matrix of original matrix x
    # with following steps to go through
    # Step 1. get matrix from passed value
    # Step 2. check if inverted is alreadd set a specific value (not null)
    # Step 3-a. if so, return the inverted value
    # Step 3-b. if not, get original x matrix and set it to data
    # Step 3-b-1. calculate the inverted matrix
    # Step 3-b-2. set it to passed value and return the inverted matrix.
    
    # Step 1
    inverted <- x$get_inverted()

    # Step 2
    if (!is.null(inverted)) {
        
        # Step 3-a
        message("getting cached data")
        return(inverted)
    }

    # Step 3-b    
    data <- x$get()

    # Step 3-b-1
    inverted <- solve(data)

    # Step 3-b-2    
    x$set_inverted(inverted)
    inverted
}
