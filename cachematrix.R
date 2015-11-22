## makeCacheMatrix is a function that creates a special matrix 
## it is a special "matrix" where we can then cache its inverse
## it is a list containing a function to
## set and get value of a matrix
## set and get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix value
  inv <- NULL
  
  # set the value of the matrix
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  set_inverse <- function(inv_input) inv <<- inv_input
  # get the value of the inverse
  get_inverse <- function() inv
  
  # return a list of all the above functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve function calculates the inverse of the makeCacheMatrix function
## It runs a loop to check to see if the inverse has previously been calculated
## If so, it returns the inverse from the cache
## If not, it calculates the inverse of th ematrix
## It then sets the value of the determined inverse in the cache

cacheSolve <- function(x, ...) {
  ## check if the inverse is in the cache
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return()
  }
  
  ## else, get the matrix
  data <- x$get()
  ## calculate the inverse
  inv <- solve(data, ...)
  ## cache the inverse of the matrix
  x$set_inverse(inv)
  ## return result
  inv
}

