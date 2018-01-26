## Since matrix inverse is a computationally costly operation,instead of repeatedly
## calculating it we cache the results the first time the operation is done
## and retrieve from this cache when needed

## this function creates an object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set=function(y){
    x=y
    inv=NULL
  }
  get=function()x
  setinverse=function(inverse) inv=inverse
  getinverse=function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## this function calculates the inverse of the matrix in the above function,
##if the matrix inverse has already been calculated it retrieves it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  }
