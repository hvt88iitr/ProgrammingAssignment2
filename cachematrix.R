## The function is intended to cache the inverse of a matrix

## The first function creates a special "matrix" which is
## a list containing 4 member function (set, get,setinverse and
## getinverse) allowing it to cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
i<-NULL
set<-function(y){
x<<-y
i<<-NULL
}
get<-function()x
setinverse<-function(inverse)i<<-inverse
getinverse<-function()i
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
i<-x$getinverse()
if(!is.null(i)){
message("getting cached data")
return(i)
}
data<-x$get()
i<-solve(data)
x$setinverse(i)
i
}
