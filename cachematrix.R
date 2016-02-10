## The below functions can be used to create a special "matrix" object, solve it
## and cache its inverse in case it was previously calculated  

## makeCacheMatrix is used to create the special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL 
    }
    get <- function() x                         #used for returning the matrix
    setsolve <- function(solve) m <<- solve     #used by cacheSolve function to cache the inverse matrix, shouldn't be called directly
    getsolve <- function() m                    #used by cacheSolve function to return the inverse matrix, can be called directly
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## cacheSolve is used to return the inverse matrix of the object defined with makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {                       #checks whether the inverse matrix of x is cached
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()                         
    m <- solve(data, ...)                   #solves the matrix
    x$setsolve(m)                           #caches the solved value
    m
}
