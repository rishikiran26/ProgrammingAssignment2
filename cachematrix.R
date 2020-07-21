makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
    
  }
  get <- function()x
  setInverse <- function(inverse) {inv <<- inverse} 
  getInverse <- function(){inv}
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}

cacheSolve <- function(x, ...)
{
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting caching matrix")
    return(inv)
    
  }
  data <- x$get()
  inv <- solve(inv, ...)
  x$setInverse(inv)
  inv
}