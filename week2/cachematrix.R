## Put comments here that give an overall description of what your
## functions do
# Neal Gushue, 2014.  
# Sample code for assignement used as template.. 
## Write a short comment describing this function
## Build the constructor set
makeCacheMatrix <- function(x = matrix()) {
    n  <- NULL
    set  <- function(y){
        x <<- y
        n <<- NULL 
    }
    get  <- function() x
    setinv  <- function(inverse) n  <<- inverse
    getinv  <- function() n
    list(set= set, get = get, 
         setinv = setinv, 
         getinv = getinv)

}

## Write a short comment describing this function
# Check if the inverse is built & use, otherwise do it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    n  <- x$getinv()
    if (!is.null(n)){
        message("getting cached data")
        return(n)
    }
    data  <- x$get()
    n  <- solve(data, ...)
    x$setinv(n)
    n
}
