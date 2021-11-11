## The purpose of these functions is to invert an arbitrary matrix and to cache the resulting matrix
## If the same function is to be inversted again, the result can be pulled from a cache such as to save 
## computing power

## This function creates a list of function objects that can get/set a matrix and get/set its inverse.

makeCacheMatrix <- function(x = matrix()) {

    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinvm <- function(inversematrix) invm <<- inversematrix
    getinvm <- function() invm
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
    
}


## The cacheSolve function returns the inverse of a matrix. In it operates in combination with the MakeCacheMatrix function.
## cacheSolve will return the inverse of a matrix, by either calculating the inverse or retrieving it from a cache, if it had 
## been calculated previously.


cacheSolve <- function(x, ...) {
    invm <- x$getinvm()
    print(invm)
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinvm(invm)
    invm
}
