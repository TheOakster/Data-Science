## These functions create a matrix object that can cache it's inverse
## This saves the need to repeatedly perform the same calculations
## Andy Oakley - September 2015

## This function takes a matrix as its argument
## It then creates a list object containing 4 functions

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } 
  
  get <- function() x
  setInv <- function (inverse) inv <<- inverse
  getInv <- function() inv
  
  list(set=set, 
       get = get, 
       setInv = setInv, 
       getInv = getInv)
}


## This function checks if the matrix inverse has already calculated
## If it has then it returns a message and retreives the cached value
## If not then it calculates the inverse and then caches it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInv()
  
  if(!is.null(inv)) {
    message("Retreiving cached value")
    return(inv)
  }
    
    mat <- x$get()
    inv <- solve(mat,...)
    x$setInv(inv)
    inv
  
}
