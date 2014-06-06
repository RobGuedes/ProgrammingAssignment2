#Function that gets a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        #function to set the inverse of a matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #function to get the inverse of a given matrix
        get <- function() x
        setinverse <- function(solve) m <<- solve   #procedure that actually inverts the matrix
        getinverse <- function() m                  #procedure that will return the inverted matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        #gets the cached inverted matrix
        m <- x$getinverse()
        
        #if the cache is null, it calculates the inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
