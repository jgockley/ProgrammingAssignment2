## This pair of functions takes an invertible matrix and determines whether its
## inverse has already been calculated and cached. If so, the inverse of the 
## matrix is returned from the cache. If not, the inverse is calculated, cached,
## and returned.

## Function makeCacheMatrix takes an invertible matrix and creates a special 
## object that can cache the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { 
                x <<- y
                m <<- NULL 
        }
        get <- function() x
        setinverse <- function(n) m <<- n
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## Function cacheSolve determines whether an inverse of an object created by
## the makeCacheMatrix function has already been cached. If so, it returns the 
## cached value of the inverse; if not, it calculates, caches, and returns 
## the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
