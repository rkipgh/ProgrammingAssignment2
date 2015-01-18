# January 2015

# These functions create a matrix and calculate the inverse of that
# matrix.  If the inverse has already been calculated and is stored
# in cache, the functions will use the cached value to reduce
# computational demands and save time.

# These functions are closely adapted from the functions 'makeVector'
# and 'cachemean' provided by Dr. Roger Peng for the Coursera / Johns
# Hopkins R Programming course.


# This function creates a list that uses functions to set and get
# the matrix and to set and get the inverse of that matrix.  The
# set functions obtain their arguments from another environment.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


# This function checks whether the inverse of the matrix created
# in the function 'makeCacheMatrix' has already been calculated and
# is stored in cache.  If so, it obtains the cached value.  If not,
# it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i    # Return a matrix that is the inverse of 'x'
}
