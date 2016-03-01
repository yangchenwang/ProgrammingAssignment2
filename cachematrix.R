## functions are used to create a special object that stores a matrix and 
## caches its inverse

##this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse
  )

}


## first the function computes the inverse of the special "matrix" created by
## makeCacheMateix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  m<-x$get()
  i<-solve(m,...)
  x$setInverse(i)
  i
}