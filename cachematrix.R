## Returns matrix object with set/get properties 
## which can cache inverse value of its matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(new_inverse) inv <<- new_inverse
  get_inverse <- function() inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Returns inverse of a matrix object, returned by `makeCacheMatrix`
## using previously cached result or caching current computation if needed

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    return(inv)
  }
  
  matr <- x$get()
  inv <- solve(matr, ...)
  x$set_inverse(inv)
  inv
}

