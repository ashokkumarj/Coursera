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
# exists the function will use the cached value if not will calculate the inverser
# of the matrix and update the cache 

cacheSolve <- function(x, ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    datos<-x$get()
    m<-solve(datos, ...)
    x$setmatrix(m)
    m
}
