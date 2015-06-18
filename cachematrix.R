## A cache object is created with makeCachematrix and then inserted into cache.
## The cacheSolve function create a new cache from input. If the cache object already exist, it returns
## existing cache. 

## This function return a cache object with a matrix as input. The return cache object is then
## used to perform set/get operation on matrix, e.g. bv<- makeCacheMatrix(k).
## It create a matrix inverse via setinverse() with solve(x) function.
## It returns the inverse , e.g. bv$getinverse(). 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function() m <<- solve(x)
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function create cache from existing matrix object, e.g. bc<-cacheSolve(bv),
## whereby the input is matrix object from makeCacheMatrix().
## If the input is cached previously, the existing value is returned, otherwise
## it calculates new inverse of matrix.
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
