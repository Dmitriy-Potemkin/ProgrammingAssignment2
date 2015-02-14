## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inver <<- inv
    getinv <- function() inver
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inver <- x$getinv()
    if(!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setinv(inver)
    inver
}
