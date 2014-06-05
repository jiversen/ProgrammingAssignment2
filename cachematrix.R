## cachematrix.R
## Assignment for R Programming, June 2014
## (c) 2014 John Iversen (jiversen@ucsd.edu)

## These functions allow the result of a potentially costly matrix inversion to be cached, so
##  that the next time the inverse of the matrix is required, it returns the precomputed value, saving time

## Test Case
##  > mat <- rbind(c(8,1,6),c(3,5,7),c(4,9,2))  #a matrix we want to invert
##  > cm <- makeCacheMatrix(mat)                #a matrix cache containing mat and ultimately its cached inverse
##  > cacheSolve(cm)                            #first call will invert mat and save it in the cache
##    [,1]        [,2]       [,3]
##    [1,]  0.29444444 -0.28888889  0.1277778
##    [2,] -0.12222222  0.04444444  0.2111111
##    [3,] -0.03888889  0.37777778 -0.2055556
##  > cacheSolve(cm)                            # a second call returns the same result, but prints that it was taken from the cache!
##    returning cached result
##    [,1]        [,2]       [,3]
##    [1,]  0.29444444 -0.28888889  0.1277778
##    [2,] -0.12222222  0.04444444  0.2111111
##    [3,] -0.03888889  0.37777778 -0.2055556

## History
## JRI 6/4/14 began, finished

## === makeCacheMatrix === ##
## Create a matrix cache. Returns a list of functions to get/set the matrix
##  and get/set the cached result of a computation on the matrix. This would be used to first create an object
##  for use with the cacheSolve function below. 
## Note: This is general purpose, and could be used for any matrix operation by using a modified function like cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  resultcache <- NULL #the cached value of the computed result
  
  # set function to set the source matrix, which invalidates the result
  #  TODO: beyond the scope of assignment, but for even greater efficiency, we could improve this by first checking 
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
  # Answer (?): so we can easilu call these functions by name, like x$getmean or x[["getmean"]]
  #   rather than x[[1]]
  list(set = set, get = get, setresult = setresult, getresult = getresult)
}

## === cacheSolve === ##
## returns the inverse of a matrix x, fetching it from the cache if we have alredy computed it, or
##  computing it if we havent done so already
##
## TODO: this would be even cooler if we could simply pass a matrix, and this would check if it already had a chached result
##  for that matrix. If not, create a new cache matrix, and store it internally. Then it would be more transparent to the
##  user, who would not have to first use the function above
##  The tradeoff would be potentially larger use of memory, but the gain is speed and ease ofuse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of the matrix mat used to create 'x' (e.g. x<-makeCacheMatrix(mat))
  
  #access the cached result
  result <- x$getresult()
  
  #if the cached result is not NULL, return it
  if (!is.null(result)) {
    message("returning cached result")
    return(result)
  }
  
  #otherwise, the cache was NULL, so we need to calculate and cache the inverse before returning it
  mat <- x$get()            #fetch the original matrix
  invmat <- solve(mat,...)  #invert it
  x$setresult(invmat)       #cache the result for next time
  invmat                    #return the inverse
}
