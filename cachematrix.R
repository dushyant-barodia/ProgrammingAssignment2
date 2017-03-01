## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix funtion takes a matrix as an input and defines the set, get, setinverse and getinverse functions.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve functions takes input as an object of makeCacheMatrix function and tries to 
## get the inverse of the matrix defined in makeCacheMatrix, in case the inverse had been previously calculated it utilizes the same result.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}