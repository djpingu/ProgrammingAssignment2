## This function creates a special "vector", 
## which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the matrix
## get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invert) inv <<- invert
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## The following function calculates the inverse matrix of the 
## special "vector" created with the above function. However, 
## it first checks to see if the inverted matrix has already been 
## calculated. If so, it gets the inverted matrix from the cache 
## and skips the computation. Otherwise, it calculates the inverted
## matrix of the data and sets the value of the inverted matrix in 
## the cache via the setinv function.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
        
}
