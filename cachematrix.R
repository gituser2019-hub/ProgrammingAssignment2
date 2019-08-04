## Overall description of the function:

## This function will compute the inverse of a matrix. But instead 
## of using the conventional computation method which is time
## consuming, this function will compute in a faster way using
## caching. The key concept is to cache the inverse of the matrix,
## and look up as needed as long as the matrix doesn't change. The
## inverse is only recalculated when the matrix changed. 
## This approach saves the run time by minimizing the un-neccessary 
## repeated computations


## This function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function() m
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m

}
