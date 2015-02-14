## Functions create a special object "makeCacheMatrix", which is a matrix with methods to store its inverse

## Function creates a special object "makeCacheMatrix" with 4 methods: set and get value of the matrix and set and
## get its inverse

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

## Function returns inverse of "makeCacheMatrix" object by using a function solve() or by calling getinv() method

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
