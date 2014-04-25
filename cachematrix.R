## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(data = matrix()) {
  #inverse_data will be the cached result location
  inverse_data<-NULL
  
  set<-function(x)
  {
    data <<- x
    inverse_data <<- NULL
  }
  get<-function() {return(data)}
  
  set_inverse<-function(value) inverse_data<<-value
  get_inverse<-function() inverse_data
  
  #return a list object representing a special matrix with the necessary functions
  list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)

}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  #attempt to retrieve cached value
  inverse_data <- x$get_inverse()
  
  if(!is.null(inverse_data)) {
    message("getting cached data")
    return(inverse_data)
  }
  #get the raw matrix values
  stored_data <- x$get()
  #calculate the inverse
  result <- solve(stored_data, ...)
  #cache the result
  x$set_inverse(result)
  # Return a matrix that is the inverse of 'x'
  return(result)
}
