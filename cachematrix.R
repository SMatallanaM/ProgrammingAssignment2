# If the contents of a matrix are not changing, it may make sense to cache the
# value of its inverse so that when we need it again, it can be looked up in 
# the cache rather than recomputed. The following functions take advantage of
# the scoping rules of the R language and how they can be manipulated to 
# preserve state inside of an R object. That is:

# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# 2. cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cacheSolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
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