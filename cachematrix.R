## This set of methods allows users to cache a matrix and its inverse
## using lexical scoping so that the inverse of the matrix does not have to be recalulated. 
##
## makeCacheMatrix(matrix) returns a list containing matrix/inverse matrix getter/setter methods 
## cacheSolve(makeCacheMatrix_object) calculates/sets (if necessary) and returns the inverse matrix 


## Create a matrix object (list) containing getter and setter methods
## for a matrix and its inverse
## Args:
##   x: Invertible Matrix
## Returns:
##   List: containing getter/setter methods

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setInverse <- function(inverse = matrix()) inv <<-inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Return the cached version the inverse of the inputa matrix if it exists
## otherwise, calculate/cache and return the inverse matrix 
## Args:
##   x: makeCacheMatrix object
## Returns:
##   y: inverse matrix of matrix referenced in makeCacheMatrix object

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  
  data<-x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}


testCache <-function(){
  data<-c(5,7,-3,3,5,-2,-1,-2,1)
  dataMatrix<-matrix(data,3,3)
  identityData<-c(1,0,0,0,1,0,0,0,1)
  idMatrix<-matrix(identityData, 3,3)
  dataInv<-c(1,-1,1,-1,2,1,-1,3,4)
  dataIMatrix<-matrix(dataInv, 3,3)
  myMatrix<-makeCacheMatrix(dataMatrix)

  message("test get")
  if(all.equal(dataMatrix,myMatrix$get())) { message("get passed")} else {message("get failed")}

  message("test set")
  myMatrix$set(dataInv)
  if(all.equal(dataInv,myMatrix$get())) { message("set passed")} else {message("set failed")}
  
  message("test getInverse when no inverse")
  if (is.null(myMatrix$getInverse())) { message("getInverse passed")} else {message("getInverse failed")}
  
  message("test cacheSolve")
  myMatrix<-makeCacheMatrix(dataMatrix)
  invFromCache<-cacheSolve(myMatrix)
  if (all.equal(invFromCache, dataIMatrix)) { message("cacheSolve passed")} else {message("cacheSolve failed")}

  message("test getInverse and setinverse from previous test")
  invFromMyMatrix<-myMatrix$getInverse()
  if (all.equal(invFromMyMatrix, dataIMatrix)) { message("getInverse passed")} else {message("getInverse failed")}
 
  message("test for identity")
  res<- (myMatrix$get() %*% myMatrix$getInverse())
  if (all.equal(res, idMatrix)) { message("identity passed")} else {message("identity failed")}
  
}

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}