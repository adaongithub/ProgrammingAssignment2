
## makeCacheMatrix( x ) and cachesolve() work together to compute the inverse
## of the matrix x while preventing unnecessary recomputation of that inverse.
## The matrix x is assumed to be numeric, square and invertable.
##
## Examples of use:
##  
##  # create a 2 by 2, square, numeric matrix (which just happens to be
#     invertable)
##  nrows <- 2
##  ncols <- 2
##  x <- matrix( 1:(nrows*ncols), nrows, ncols )
##  print( x )
##  
##  # create an intermediate list object to provide access to the
##  # cached inverse value of x
##  x_mMatrix <- makeCacheMatrix( x )
##  
##  # ask for and output the inverse of x (its inverse will be computed
##  # and saved within x_mMatrix)
##  inverse_1 <- cachesolve( x_mMatrix )
##  print( inverse_1 )
##  
##  # again ask for and output the inverse of x (the inverse need not be
##  # computed; it will be pulled from within x_mMatrix)
##  inverse_2 <- cachesolve( x_mMatrix )
##  print( inverse_2 )


## makeCacheMatrix() constructs an intermediate list which will hold functions
## that can be used to access (set/get) the matrix to be inverted and the
## cached value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Indicate with NULL that an inverse has not yet been computed and cached
  cachedVal <- NULL
  set <- function(newMatrix) {
    x         <<- newMatrix
    # we have a new underlying matrix x, so mark its inverse as not having
    # yet been computed
    cachedVal <<- NULL
  }
  get    <- function() x
  setInv <- function(inv) cachedVal <<- inv
  getInv <- function() cachedVal
  # return set/get for the matrix to be inverted and setInv/getInv for the
  # cached inverse value
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cachesolve() operates on the intermediate list returned by makeCacheMatric(x)
## to return the inverse of the matric x.  If the inverse of x has not been
## computed, it is computed here and returned.  If the inverse of x has been
## computed on a previous call to this function, the inverse value is returned
## and no extra computation of the inverse is preformed.

cachesolve <- function(x, ...) {
  cachedVal <- x$getInv()
  # cachedVal will be NULL iff we have not yet computed the inverse for the
  # matrix value currently in x$get().
  if(!is.null(cachedVal)) {
    # we already have an inverse value cached for this matrix - return it
    # we keep the output statement below for debugging purposes
    message("Getting cached solve(data): cachedVal=", cachedVal, ", data=", x$get())
    return(cachedVal)
  }
  # we don't yet have an inverse value cached for this matrix so compute it here
  # and stash the value using x$setInv()
  data <- x$get()
  cachedVal <- solve(data, ...)
  # we keep the output statement below for debugging purposes
  message("Computing new solve(data): cachedVal=", cachedVal, ", data=", data)
  x$setInv(cachedVal)
  cachedVal
}