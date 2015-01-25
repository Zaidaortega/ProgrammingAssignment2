## This script includes 2 funcions, one for store an inverse matrix from a
## given matrix in cache (makeCacheMatrix), and the other one for printing the inverse
## matrix from cache (cacheSolve)

## MakeCacheMatrix is a function that makes a special objetc of the kind 
## "matrix" that can return its inverse from cache, so it is the first step
## to make the inverse matrix of a matrix storing it from cache (thus, saving time)

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL ## sets the value of m to NULL
    set<-function(y){ #set the value of the matrix
      x<<-y ## put the matrix in cache so we can go to it later
      m<<-NULL ## sets the value of m (the inverse matrix inverse when we use the nex function) to NULL
    }
    get<-function() x  ##set the value of inverse of the matrix
    setmatrix<-function(solve) m<<- solve ##solve the inverse matrix of m, the cached matrix
    getmatrix<-function() m ##displays the inverse matrix
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

cacheSolve <- function(x, ...) {  ##this is the function that returns the inverse matrix     
    m <- x$getinverse()  ## get the inverse of the matrix   
    if(!is.null(m)) {}   ## check if there is the matrix
      message("getting data from cache")
      return(m)
    }  
    data <- x$get() ## otherwise, it gets the inverse of the matrix 
    i <- solve(data, ...)
    x$setinverse(m)  ## setting the inverse of the matrix 
    m
  }
