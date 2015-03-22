## Put comments here that give an overall description of what your
## functions do

## The functions in this R Script implement the caching capabilities for an 
## invertable matrix by keeping its inverse cached once calculated along 
## with matrix instance.  


## Write a short comment describing this function
## The makeCacheMatrix function is a blue print,that is, an abstract 
## representation of cacheable matrix creation. It creates an instance of 
## matrix object and update its inverse into this instance once calculated via 
## CacheSolve function
makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
        x <<- y
        inverse <<- NULL
      }
      get <- function() { x }
      setInv <- function(z) { inverse <<- z }
      getInv <- function() { inverse }
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
  }


## Write a short comment describing this function
## The casheSolve is reponsible for calculating and updating the inverse
## of the pre-created instance of matrix object if it is not already exists
## However if the inverse was already calculated and had assigned to the 
## pre-existing matrix instance, it simple returns that back.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInv()
        if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInv(inverse)
        inverse
}
