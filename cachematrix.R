## Overall description of Functions makeCacheMatrix and cacheSolve
#
#  These functions calculate the inverse of a matrix once the matrix is created
#  so that the results can be cached for quick retrieval later and save the time
#  needed to perform this costly calculation
#
#  To try the functions, use the following example code after loading/running both
#  functions in R:
#
#    BMatrix <- makeCacheMatrix(matrix(c(3,-3,3,1),nrow=2,ncol=2,byrow=TRUE))
#    cacheSolve(BMatrix)
#
#  To prove that this all works, here is some R code that just loads the same function
#  and uses the 'solve' function to calculate it's inverse:
#
#    B = matrix(c(3,-3,3,1),nrow=2,ncol=2,byrow=TRUE)
#    Binv = solve(B)
#    Binv
#
##

## makeCacheMatrix
#
#  This function is used to create the matrix and then cache its inverse
#  It also has the ability to set and get the value of the matrix and its inverse
#
##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}

## cacheSolve
#
#  This function calculates the inverse of the matrix created in using the
#  makeCacheMatrix function above, however, if first checks to see if the inverser
#  has already been calculated and gets it from the cache if so.
#
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

BMatrix <- makeCacheMatrix(matrix(c(3,-3,3,1),nrow=2,ncol=2,byrow=TRUE))
cacheSolve(BMatrix)
B = matrix(c(3,-3,3,1),nrow=2,ncol=2,byrow=TRUE)
Binv = solve(B)
Binv
