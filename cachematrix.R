## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        memoize <- NULL
        set <- function(y) {
                x <<- y
                memoize <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) memoize <<- inverse
        getinverse <- function() memoize
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        memoize <- x$getinverse()
        if(!is.null(memoize)) {
                message("getting cached data")
                return(memoize)
        }

        matrix <- x$get()
        memoize <- solve(matrix)
        x$setinverse(memoize)

        memoize
}
