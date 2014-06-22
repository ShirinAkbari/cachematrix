#this function set,get the matrix and set and get the inverse of matrix 
# The function includes two function makeCacheMatrix & Cachesolve
#I just do some modification in the example and change vector to matrix and mean to solve
# and also some change in name
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) m <<- inverse
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}
# this function return the inverse of matrix but it check at first whether the inverse in calculated perviousely or not

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
