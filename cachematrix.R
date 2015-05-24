# cachematrix.R
#
# Contains Two functions that together create a special "matrix" object that:
# given a matrix 'x' will compute the inverse of 'x' by calling solve(x) and
# cache the result.
#
# If the inverse of x is cached the result is retrieved from the cache and
# returned to the function, otherwise the result is calculated, cached, and the
# result is returned to the function.


# makeCacheMatrix() This function creates a special "matrix" object that can
# cache its inverse.
#
# @param 'x' a matrix
#
# @return a list of function - getter and setter for a matrix 'x' and it's
#   inverse set(), get(), setInv(), getInv()
#
# @examples
#  foo <- matrix(c(1,1,2,3,1,3,1,2,4),3,3)
#  bar <- makeCacheMatrix(foo)
#
#  getters and setter:
#  narf <- bar$set(foo)
#  narf <- bar$get(foo)
#  narf <- bar$setInv(foo)
#  narf <- bar$getInv(foo)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

# cacheSolve() This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix(). If the inverse has already been calculated (and
# the matrix has not changed), cacheSolve() retrieves the inverse from the
# cache.
#
# @param 'x', '...'  a matrix, a function
#
# @return a matrix 'm' where 'm' is a cached matrix object containing the
#   inverse of x
#
# @examples
#  foo <- matrix(c(1,1,2,3,1,3,1,2,4),3,3)
#  bar <- makeCacheMatrix(foo)
#  narf <- cacheSolve(bar)

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
