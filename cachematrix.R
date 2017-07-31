## These two functions work together to compute the inverse of a matrix and to cache its value.

## makeCacheMatrix contains internal functions that set and get the value of the matrix, and get and set the value of its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(inv) m <<- inv
        getInv <- function() m
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve either returns the previously cached value of the inverse of the matrix, or computes and then caches it if it is not yet stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
