## These functions when used together can hold a matrix, while simultaneously holding a cache of it's calculated 
## inverse, both of which can be retrieved. The inverse must be calculated by the cacheSolve function in order
## to be stored by makeCacheMatrix. These functions assume the matrix has an inverse.

## makeCacheMatrix generates a list of functions which can then be used to find the matrix, find it's inverse,
## or change the matrix.

makeCacheMatrix <- function(x = matrix()) {
        # inv is variable representing the inverse as chached at the time
        # if makeCacheMatrix is called, any existing cached inverse should be removed:
        inv <- NULL
        # if the set() function is called, change the matrix, and reset inverse to null:
        set <- function(y) {
                x <<- y
                inv <<- NULL
                # <<- operator changes any old matrix values to the new values 
                # in the whole makeCacheMatrix function
        }
        get <- function() { x }
                # if get() function is called, will return x
        
        getinverse <- function(x) { inv }
                # one would use the getinverse() function to get the cached inverse
                # if cacheSolve has not been run for the matrix x, this will be NULL
        
        setinverse <- function(calculated_inverse) {
                # this function to only be used when called upon by cacheSolve()
                # calculated_inverse is a variable passed to this function by 
                # cacheSolve(). It is calculated by the cacheSolve function.
                inv <<- calculated_inverse
        }
        # prepare a list of these functions to be returned:
        list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## This function calculates the inverse of a matrix, acquired using the 
## get() function, and sets the cached inverse using the setinverse() 
## function, both of which are defined above.

cacheSolve <- function(x, ...) {
        # acquire the cached inverse from the makeCacheMatrix object.
        inv <-x$getinverse()
        # if the inverse has already been calculated, return the cached value.
        if(!is.null(inv)) {
                message("This value is already cached.")
                return(inv)
        }
        # if no cached inverse present, calculated the inverse, saved as inv.
        # use get() from the makeCacheMatrix object to get the values of the matrix.
        inv <- solve(x$get())
        # use the setinverse() function from the makeCacheMatrix object to cache the 
        # inverse of the matrix in the makeCacheMatrix object.
        x$setinverse(inv)
        # return the inverse
        inv
}
