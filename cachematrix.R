## There are my functions that are used to create a special "matrix" object 
## and cache the inverse of matrix.

## This function create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {     
                   odw<-NULL

             set<-function(y){
                  x<<-y
                  odw<<-NULL
  
}
            get<-function() x
            setinverse<-function(inverse) 
                 odw<<-inverse
            
            getinverse<-function()odw

            list(set=set, get=get,
                 setinverse=setinverse,
                 getinverse=getinverse)
}




## This function computes the inverse of the matrix which was return 
## by my first function makeCacheMatrix.If the inverse has already been
## calculater than the cacheSolve restore the inverse from the cache.

cacheSolve <- function(x, ...){
        odw<-x$getinverse()
  
  if(!is.null(odw)) {
    message("Getting cached data")
    
    return(odw)
  }
  data<-x$get()
  odw<-solve(data,...)
  x$setinverse(odw)
  odw
  
}
