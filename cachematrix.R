##Caching the inverse of a matrix saves more computation time than just computing the inverse of matrix repeatedly.
##We can generate the cache function based on the example of caching the mean of a vector
##The makeCacheMatrix function(same as makeVector function in caching mean of a vector) creates a special "matrix", which is a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of matrix
##get the value of the inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInvMat <- function(solve) i <<- solve
  getInvMat <- function() i
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}
## the cacheSolve function calculate the inverse of the matrix based on the makeCacheMatrix function.
## it will check if the inverse value has already been calculated. if so, it gets the inverse from the cache and just skips the computation,
## if not, it will calculate the inverse and set it in the cache via the setInvMat function.
cacheSolve <- function(x, ...) {
  i <- x$getInvMat()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInvMat(i)
  i
}

##*just a simple test to see whether it will work or not. the result copied from R console
##> a<-rbind(c(1,2),c(3,4))
##> print(a)
##[,1] [,2]
##[1,]    1    2
##[2,]    3    4
##> b<-makeCacheMatrix(a)
##> cacheSolve(b)
##[,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
##> cacheSolve(b)
##getting cached data
##[,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5