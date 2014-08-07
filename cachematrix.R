## makeCacheMatrix and cacheSolve find the inverse of a matrix, stores the inverse 
## in cache and returns the inverse when it is called.

## makeCacheMatrix allows us to store the inverse of a matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) m <<- Inv
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv

}


## cacheSolve returns the inverse of a matrix either if it is stored in cache. If 
## it is not stored in cache it will find the inverse.

cacheSolve <- function(x, ...) {
        
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m

}
