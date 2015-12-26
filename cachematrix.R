# Kenny Kyunghoon Lee (leehkenny@gmail.com)
# makeCacheMatrix creates a special list of the following 4 functions
# $get returns the matrix
# $set resets the matrix and mean globally
# $getInverse returns the inverse of the matrix
# $setInverse saves the inverse globally

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = get, get = get, 
       setInverse = setInverse,
       getInverser = getInverse)
}

# cacheSolve calculates the inverse matrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }   # Return the inverse matrix if there already inv cached
  d <- x$get()
  inv <- solve(d)
  x$setInverse(inv)
  inv
}

# test by the following code (use any matrix x) 
# a <- makeCacheMatrix(x) 
# b <- cacheSolve(a)
# a %*% b will return an identity matrix
# Voila!
