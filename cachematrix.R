# Function to create a matirx object with functions get,set and getmatrix and 
# setmatrix. The function takes a matrix as input and returns a list of functions

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

# Function to create an inverse of a given matirx. If the matrix inverse already 
# exists the function will use the cached value if not it will calculate the inverse
# of the matrix and update the cache 

cacheSolve <- function(x, ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    data<-x$get()
    m<-solve(data, ...)
    x$setmatrix(m)
    m
}

# Test Code 
matrix1 <- matrix(1:4, c(2,2))
matrix1

matrix2 <- matrix(12:15, c(2,2))
matrix2

# construct with default arguments
mkcm <- makeCacheMatrix()

cacheSolve(mkcm)

# use matrix1 
mkcm$set(matrix1)
mkcm$get()

# compute inverse
cacheSolve(mkcm)
# get cached inverse
cacheSolve(mkcm)

# construct with matrix2
mkcm <- makeCacheMatrix(matrix2)
mkcm$get()

# compute inverse
cacheSolve(mkcm)
# get cached inverse
cacheSolve(mkcm)
