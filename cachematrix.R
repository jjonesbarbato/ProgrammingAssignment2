## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##Create variable as NULL to be used further in routine as cached variable
  b <- NULL
  ##Create sub-functions
  ##First sub-function will retrieve the matrix, set it to value x and clear the cache 
  set <- function(c) {
    x <<- c
    b <<- NULL
  }
    ##Sub-function 2 - Retrieve matrix
    get<- function () x
  ##Sub-function 3 - Set cached variable as inverse of the original  matrix
  set_cache_variable <- function(ginv) b <<- ginv
  ##Sub-function 4 - Retrieve cached variable
  get_cache_variable <- function() b
    list(set = set, get = get,
       set_cache_variable = set_cache_variable,
       get_cache_variable = get_cache_variable)
}

  ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$get_cache_variable()
 ##Write logical funtion to return text that the value is cached if it is currently
 ##non-null
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
   m <- ginv(data, ...)
  x$set_cache_variable(m)
  m
}