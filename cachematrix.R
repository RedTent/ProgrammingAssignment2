## These two functions can be used to compute and cache an inverted matrix.

#' makeCacheMatrix is a function that transforms a matrix into an object that 
#' can cache the matrix' inverse. This can be useful to avoid repeatedly 
#' calculating the matrix' inverse and thereby improving speed. 
#' The function takes an invertible matrix as an argument
#' 
#' NOTE: An easier alternative would probably be to use the package memoise
#'

makeCacheMatrix <- function(x = matrix()) {
    # m <- NULL
    # set <- function(y) {
    #   x <<- y
    #   m <<- NULL
    # }
    # get <- function() x
    # setmean <- function(mean) m <<- mean
    # getmean <- function() m
    # list(set = set, get = get,
    #      setmean = setmean,
    #      getmean = getmean)
    # 
  
  inverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(inv) inverse <<- inv
  
  get_inverse <- function() inverse
  
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse
  )
  
}


#' This function takes an object made with makeCacheMatrix and returns the 
#' matrix' inverse. If there's a cached version of the inverse matrix available 
#' it will return the cached version instead of computing the inverse.
#' When the dots are use make sure to always provide exactly the same arguments.

cacheSolve <- function(x, ...) {

  inverse <- x$get_inverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix_orig <- x$get()
  
  inverse <- solve(matrix_orig, ...)
  x$set_inverse(inverse)
  inverse

}
