## Instructions for testing makeCacheMatrix and CacheSolve functions
## I set the input x as a matrix
## then set the solved value "inv" as a null
## then changed every reference to "mean" to "solve"

## Set solve value "inv" as a null, and change "mean" to solve

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  getInverse<-function(){x}
  setInverse<-function(inverse){inv<<-inverse}
  getInverse<-function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## same here, changed "mean" to "inverse"
cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
        ## Return a matrix that is the inverse of 'x'
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}
