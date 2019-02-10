## makeCacheMatrix is a function that will create a matrix object that allows cahce of a calculated inverse matrix. 
## Assumes matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x} 
  setinv <- function(inverse) {inv <-- inverse}
  getinv <- function() {inv}
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve is a function that will calculate the inverse of a square matrix
## if inverse has already been calculated, it will returned cached value.
## if no value in cache, it will calculate the inverse as described.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("Cached value detected")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
