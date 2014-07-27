## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function defines getter and setter functions 
# for the matrix in an environment.
# The cached inverse is stored in this environment.
# When a call to get the inverse of the matrix happens,
# the inverse is caculated and stored in this environment.
# For repeated calls to the inverse of same matrix through the same
# variable, happen, cached inverse is returned.
#
makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
# get method returns the matrix
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)

}


## Write a short comment describing this function


## cacheSolve method computes the inverse of the matrix x and if inverse is
## already calculted then it returns the already calculated cacheed 
## value of the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # if inverse is already calculated then return it from cache.
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  # calculates the inverse
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
