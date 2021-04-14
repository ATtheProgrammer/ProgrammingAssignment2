## The assignment requires us to cache the inverse of a matrix
## The makeCacheMatrix function creates a matrix that will later be used to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
#x is a vector defined as 'matrix'

  a <- NULL
  set <- function(y){ ##setting value of the vector
    x <<- y
    a <<- NULL
  }  ##get function so you can set the inverse
  get <- function()x
  setInverse <- function(inverse) a <<- inverse ##Use the inverse function to minimise/ reducing computing the matrix repeatedly
  getInverse <- function() a 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
	   }


        ## Return a matrix that is the inverse of 'x'



#To return the inverse function, CacheSolve was used
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  a <- x$getInverse()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  mat <- x$get()
  a <- solve(mat,...)
  x$setInverse(a)
  a
}
