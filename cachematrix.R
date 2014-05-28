makeCacheMatrix <- function(x = matrix()) {
        #initialize. sets m to null
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

cacheSolve <- function(x, ...) {
        #chaches the inverse matrix
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #gets the nverse matrx
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
