## The below pair of functions are designed to cache
## and compute the inverse of a matrix

## This function can cache its inverse via a special object - "matrix"

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mtx)
  {
    x<<- mtx;
    inv<<-NULL;
  }
  
  get<- function() return(x);
  seti<- function(inverse) inv <<- inverse;
  geti<- function() return(inv);
  return(list(set = set, get = get, seti=seti, geti=geti))
  
}


## This function computes the inverse of "matrix" returned by
## above function.
## In case the inverse of the matrix is already calculated
## then this function will retrieve inverse of matrix from the cache

cacheSolve <- function(x, ...) {
  inv <- x$geti()
  if(!is.null(inv))
  {
    message("Getting cached data")
    return(inv)
  }
  data<- x$get()
  inv<- solve(data, ...)
  x$seti(inv)
  return(inv)
  
  

}
