## Create list holder for cached (memoized) matrix calculations

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


## Solve matrix, contained in list holder.
## Use previously cached (memoized) calculated value, if any.

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
