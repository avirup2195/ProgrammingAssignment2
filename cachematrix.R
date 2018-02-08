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
  
  #to check we create a random matrix of order 3
  rmat <- matrix(rnorm(9),nrow=3)
  temp <- makeCacheMatrix(rmat)
  cacheSolve(temp)
  #verify the output
  solve(rmat)
  
  ###########OUTPUT#########################
  
  #"> #to check we create a random matrix of order 3
  #> rmat <- matrix(rnorm(9),nrow=3)
  #> temp <- makeCacheMatrix(rmat)
  #> cacheSolve(temp)
  #[,1]       [,2]      [,3]
  #[1,] -20.681111 -16.546997 -28.03137
  #[2,] -37.339538 -30.654831 -51.09537
  #[3,]  -9.057347  -7.177514 -13.15883
  #> #verify the output
  #> solve(rmat)
  #[,1]       [,2]      [,3]
  #[1,] -20.681111 -16.546997 -28.03137
  #[2,] -37.339538 -30.654831 -51.09537
  #[3,]  -9.057347  -7.177514 -13.15883"