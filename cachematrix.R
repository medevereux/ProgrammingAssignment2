## The two functions below allow the user to calculate the inverse of a matrix (assumed)
## invertible matrix and store the value in memory - which can be retrieved if the same
## calculation is done again in succession. If the matrix is changed then the inverse
## has to be recalculated and overwritten in memory

## makeCacheMatrix() takes as input a matrix and creates a list of functions (not printed 
## by default) containing initalization of the inverse, storing the newly calculated inverse 
## in memory and being able to retrieve the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  #Define initial inverse and set to no value
  inv<-NULL
  
  #Define function 'set' to overwrite initial matrix and reset inverse
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  
  #Define function 'get' to check initial matrix has been inputted correctly
  get<-function() {
      x
  }
  
  #Define function 'setinv' so that new calculated inverse overwrites 
  ## makeCacheMatrix level 'inv'
  setinv<-function(inverse) {
      inv<<-inverse
  }
  
  #Define function 'getinv' to retrieve cached inverse
  getinv<-function() {
      inv
  }
  
  #Make format of function as a list to allow external access
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}

## cacheSolve() takes the function makeCacheMatrix() with original matrix as input and 
## returns the inverse of the matrix. If there is a cached inverse then that value will
## be returned with a message to indicate that the data is cached; otherwise the matrix
## is inverted and the solution stored in memory

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  
  #Initially get value of inverse from cache (NULL or non-NULL)
  inv<-x$getinv()
  
  #If matrix has been changed or inverse is NULL, calculate inverse and cache it. 
  #otherwise return cached value
  
  #If inv has already been calculated, return cached value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #Otherwise obtain matrix, calculate inverse and cache it
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}
