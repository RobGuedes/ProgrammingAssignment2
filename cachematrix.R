makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        #function to set the inverse of a matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #function to get the inverse of the given matrix
        get <- function() x
        setinverse <- function(solve) m <<- solve   #procedure that actually inverts the matrix
        getinverse <- function() m                  #procedure that will return the inverted matrix
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
