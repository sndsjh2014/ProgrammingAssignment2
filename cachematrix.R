## This file contains two functions: 1. makeCacheMatrix, and 2. cacheSolve
## makeCacheMatrix takes a matrix as an argument returns a list of functions
## cacheSolve takes the list returned by makeCacheMatrix as an argument

## makeCacheMatrix function creates a special "matrix" object than can cache its inverse
## It allows the user to set a matrix, get a matrix, set an inverse and get the invers
## Note that cacheSolve as it is written does not require the "set a matrix" functionality

## naming of variables and functions: m_inv is the inverted matrix
## set_mat is a function that stores the original matrix
## get_mat is a function that returns the stored original matrix
## set_inv is a function that stores an inverted matrix
## get_inv is a function that returns the stored inverted matrix or NA

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NA ## m_inv is the inverted matrix. start with NA
  set_mat <- function(y) {
    x <<- y   ## over write x in the function argument to store a matrix
    m_inv <- NA ## reset m_inv to NA since we now have a new matrix
  }
  get_mat <- function() x ## retrieve the original matrix
  set_inv <- function(inv) m_inv <<- inv ## over write m_inv in the above 
                                        ##environment and store the inverted matrix
  get_inv <- function () m_inv ## retrieve the inverted matrix
  list(set_mat = set_mat, get_mat = get_mat, set_inv = set_inv, get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## cacheSolve receives a list of functions as the arguments
  ## each of these functions can be referenced with "argument$function" convention
  m_inv <- x$get_inv() ## retrieve what is stored in the inverted matrix variable
  if(!is.na(m_inv)) { ## if the inverted matrix is not NA, then we must have cached 
                    ## the inverted matrix
    message("getting cached data")
    return(m_inv) ## return the inverted matrix and exit
  }
  ## if we did not cache the inverted matrix, then do the following
  m <- x$get_mat() ## first get the matrix
  m_inv <- solve(m) ## use solve to invert the matrix. note this assumes that the
                    ## matrix is non-singular (i.e., invertible)
  x$set_inv(m_inv) ## now, store the inverted matrix so that the next time we can
                    ## use the cached data
  return(m_inv)         ## Return a matrix that is the inverse of 'x'
}
