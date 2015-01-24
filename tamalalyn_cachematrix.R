## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The following
## two functions are used to cache the inverse of a matrix.

## makeCache creates a list containing a function that:
## 1 sets the value of the matrix
## 2 gets the value of the matrix
## 3 sets the value of the inverse of the matrix
## 4 gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

## The following function returns the inverse of the matrix. It first checks
## if the inverse has already been computed. If so, it gets the result and skips
## the computation. If not, it computes the inverse and sets the value in the
## cache via the setinverse function.

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}


## Sample Run 1:
x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)
m$get()

## Sample Run 2:
cacheSolve(m)
