## cachematrix.R
## Assignment for R Programming, June 2014
## (c) 2014 John Iversen (jiversen@ucsd.edu)

## These functions allow the result of a potentially costly matrix inversion to be cached, so
##  that the next time the inverse of the matrix is required, it returns the precomputed value

## History
## JRI 6/4/14 began

## === makeCacheMatrix === ##
## Create a matrix cache. Returns a list of functions to get/set the matrix
##  and get/set the inverse of the matrix. This would not usually be called directly, but is a
##  helper for the function cacheSolve below. This is general purpose, and could be used for
##  any matrix operation

makeCacheMatrix <- function(x = matrix()) {
  resultcache <- NULL #the cached value of the computed result
  
  # set function to set the source matrix, which invalidates the result
  #  TODO: ?for even greater efficiency, we could improve this by first checking 
  #   if isidentical(y,x) and if so not invalidating the cached result
  #  Uses deep assignment opeator <<- to assign into the parent function's environment
  set <- function(y) {
    x <<- y
    resultcache <<- NULL
  }
  
  #get function to return the source matrix
  get <- function() {
    x
  }
  
  #setresult function to set the result cache
  setresult <- function(result) {
    resultcache <<- result
  }
  
  #getresult funtion to get the cached result
  getresult <- function () {
    resultcache
  }
  
  #now package up the functions as a list
  # Question: why do the elements have to be named?
  # Answer (?): so e can easilu call these functions by name, like x$getmean or x[["getmean"]]
  #   rather than x[[1]]
  list(set = set, get = get, setresult = setresult, getresult = getresult)
}

## === cacheSolve === ##
## returns the inverse of a matrix x, fetching it from the cache if we have alredy computed it, or
##  computing it if we havent done so already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #access the result cache
  result <- x$getresult()
  
  #if it's not NULL, return it
  if (!is.null(result)) {
    message("returning cached result")
    return(result)
  }
  
  #otherwise, the cache was NULL, so we need to calculate and cache the inverse before
  # returning it
  mat <- x$get()
  invmat <- solve(mat,...)
  x$setresult(invmat)
  invmat
}
